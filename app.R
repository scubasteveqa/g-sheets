library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)

# Authenticate with service account credentials from environment
creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
if (creds_json != "") {
  temp_key <- tempfile(fileext = ".json")
  writeLines(creds_json, temp_key)
  drive_auth(path = temp_key)
  gs4_auth(token = drive_token())
}

ui <- page_sidebar(
  title = "Google Sheets Editor",
  sidebar = sidebar(
    h4("Create New Sheet"),
    textInput("new_sheet_name", "Sheet Name", placeholder = "Enter new sheet name"),
    textInput("folder_id", "Folder ID (optional)", placeholder = "Paste shared folder ID here"),
    actionButton("create_sheet", "Create Sheet", class = "btn-success"),
    hr(),
    h4("Sheet Selection"),
    uiOutput("sheet_selector"),
    hr(),
    actionButton("refresh_sheets", "Refresh Sheet List", class = "btn-secondary"),
    hr(),
    h4("Data Controls"),
    actionButton("refresh_btn", "Load Data", class = "btn-primary"),
    hr(),
    h4("Add New Row"),
    uiOutput("add_row_ui"),
    br(),
    textOutput("last_updated")
  ),
  card(
    card_header("Current Sheet Data"),
    tableOutput("sheet_data")
  )
)

server <- function(input, output, session) {
  all_sheets <- reactiveVal(NULL)
  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)
  current_sheet_id <- reactiveVal(NULL)

  load_sheet_list <- function() {
    tryCatch({
      sheets <- gs4_find()
      all_sheets(sheets)
      showNotification("Sheet list loaded!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading sheets:", e$message), type = "error")
    })
  }

  clean_data_for_display <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(NoData = "Sheet is empty", stringsAsFactors = FALSE))
    }
    
    for (i in seq_along(data)) {
      if (is.list(data[[i]])) {
        data[[i]] <- sapply(data[[i]], function(x) {
          if (is.null(x) || length(x) == 0) return("")
          else if (length(x) == 1) return(as.character(x))
          else return(paste(as.character(x), collapse = ", "))
        })
      }
      data[[i]] <- as.character(data[[i]])
    }
    return(data)
  }

  read_sheet_data <- function() {
    req(current_sheet_id())
    tryCatch({
      data <- read_sheet(current_sheet_id(), col_types = "c")
      data <- clean_data_for_display(data)
      sheet_data(data)
      last_update(Sys.time())
      showNotification(paste("Data loaded! Rows:", nrow(data)), type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      sheet_data(data.frame(Error = e$message, stringsAsFactors = FALSE))
    })
  }

  # Try different creation methods based on authentication type
  observeEvent(input$create_sheet, {
    req(input$new_sheet_name)

    tryCatch({
      # Get service account email from credentials
      creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
      if (creds_json == "") {
        stop("No service account credentials found")
      }
      
      creds <- jsonlite::fromJSON(creds_json)
      service_email <- creds$client_email
      
      if (!is.null(input$folder_id) && input$folder_id != "") {
        # Method 1: Create in shared folder
        new_file <- drive_create(
          name = input$new_sheet_name,
          type = "spreadsheet",
          path = as_id(input$folder_id)
        )
        new_sheet_id <- as_id(new_file$id)
      } else {
        # Method 2: Create and then share with service account
        new_sheet_id <- gs4_create(name = input$new_sheet_name)
        
        # Share the new sheet with the service account
        drive_share(
          file = new_sheet_id,
          role = "writer",
          type = "user",
          emailAddress = service_email
        )
      }
      
      # Add initial data
      initial_data <- data.frame(
        Column1 = character(0),
        Column2 = character(0),
        stringsAsFactors = FALSE
      )
      
      sheet_write(initial_data, ss = new_sheet_id, sheet = "Sheet1")
      
      showNotification(paste("Sheet created:", input$new_sheet_name), type = "message")
      current_sheet_id(new_sheet_id)
      updateTextInput(session, "new_sheet_name", value = "")
      
      invalidateLater(2000, session)
      observe({ load_sheet_list() }, once = TRUE)

    }, error = function(e) {
      showNotification(paste("Failed to create sheet:", e$message), type = "error", duration = 10)
    })
  })

  observeEvent(TRUE, { load_sheet_list() }, once = TRUE)

  observeEvent(all_sheets(), {
    req(all_sheets())
    if (nrow(all_sheets()) > 0 && is.null(current_sheet_id())) {
      current_sheet_id(all_sheets()$id[1])
    }
  })

  output$sheet_selector <- renderUI({
    req(all_sheets())
    selectInput("selected_sheet", "Select a Sheet:",
                choices = setNames(all_sheets()$id, all_sheets()$name))
  })

  observeEvent(input$selected_sheet, {
    current_sheet_id(input$selected_sheet)
  })

  observeEvent(current_sheet_id(), {
    req(current_sheet_id())
    read_sheet_data()
  })

  observeEvent(input$refresh_sheets, {
    load_sheet_list()
  })

  observeEvent(input$refresh_btn, {
    read_sheet_data()
  })

  output$add_row_ui <- renderUI({
    req(sheet_data())
    cols <- names(sheet_data())

    if (length(cols) == 0 || any(cols %in% c("Error", "NoData"))) {
      return(p("Load sheet data first to see input fields."))
    }

    tagList(
      lapply(cols, function(col) {
        textInput(paste0("col_", col), label = col, value = "")
      }),
      actionButton("add_row", "Add Row", class = "btn-success")
    )
  })

  observeEvent(input$add_row, {
    req(sheet_data(), current_sheet_id())

    tryCatch({
      cols <- names(sheet_data())
      if (any(cols %in% c("Error", "NoData"))) {
        showNotification("Cannot add row: sheet not loaded properly", type = "error")
        return()
      }

      new_row <- lapply(cols, function(col) {
        val <- input[[paste0("col_", col)]]
        if (is.null(val) || val == "") "" else val
      })
      names(new_row) <- cols
      new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)

      sheet_append(current_sheet_id(), new_row_df)
      showNotification("Row added!", type = "message")

      lapply(cols, function(col) {
        updateTextInput(session, paste0("col_", col), value = "")
      })

      invalidateLater(2000, session)
      observe({ read_sheet_data() }, once = TRUE)

    }, error = function(e) {
      showNotification(paste("Error adding row:", e$message), type = "error")
    })
  })

  output$sheet_data <- renderTable({
    data <- sheet_data()
    if (is.null(data)) {
      return(data.frame(Status = "No data loaded", stringsAsFactors = FALSE))
    }
    return(data)
  })

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Not loaded yet"
    }
  })
}

shinyApp(ui = ui, server = server)
