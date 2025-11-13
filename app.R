library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(jsonlite)
library(httr)

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
    textInput("folder_id", "1LYng2Z02LpD04E5qYIzeSaUEBeVs7ENU", 
              placeholder = "Paste shared folder ID here"),
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

  # Load list of accessible sheets
  load_sheet_list <- function() {
    tryCatch({
      sheets <- gs4_find()
      all_sheets(sheets)
      showNotification("Sheet list loaded!", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error loading sheets:", e$message),
        type = "error"
      )
    })
  }

  # Read data using googlesheets4
  read_sheet_data <- function() {
    req(current_sheet_id())
    tryCatch({
      data <- read_sheet(current_sheet_id())
      if (nrow(data) == 0) {
        data <- data.frame(NoData = "Sheet is empty", stringsAsFactors = FALSE)
      }
      sheet_data(data)
      last_update(Sys.time())
      showNotification(paste("Data loaded! Rows:", nrow(data)), type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      sheet_data(data.frame(Error = e$message, stringsAsFactors = FALSE))
    })
  }

  # Create new spreadsheet with multiple approaches
  observeEvent(input$create_sheet, {
    req(input$new_sheet_name)

    tryCatch({
      # Method 1: Try creating in specified folder
      if (!is.null(input$folder_id) && input$folder_id != "") {
        new_file <- drive_create(
          name = input$new_sheet_name,
          type = "spreadsheet",
          path = as_id(input$folder_id)
        )
      } else {
        # Method 2: Try creating with gs4_create (sometimes has different permissions)
        new_sheet_id <- gs4_create(
          name = input$new_sheet_name,
          sheets = "Sheet1"
        )
        new_file <- list(id = new_sheet_id)
      }
      
      new_sheet_id <- as_id(new_file$id)
      
      # Add initial headers
      initial_data <- data.frame(
        Column1 = character(0),
        Column2 = character(0),
        stringsAsFactors = FALSE
      )
      
      sheet_write(
        initial_data,
        ss = new_sheet_id,
        sheet = "Sheet1"
      )

      showNotification(
        paste("Sheet created successfully:", input$new_sheet_name),
        type = "message"
      )

      current_sheet_id(new_sheet_id)
      updateTextInput(session, "new_sheet_name", value = "")
      
      # Refresh list after delay
      invalidateLater(2000, session)
      observe({
        load_sheet_list()
      }, once = TRUE)

    }, error = function(e) {
      error_msg <- paste0(
        "Failed to create sheet: ", e$message, "\n\n",
        "Try these solutions:\n",
        "1. Create a folder in Google Drive\n",
        "2. Share it with your service account email\n", 
        "3. Paste the folder ID in the 'Folder ID' field above\n",
        "4. Or try using gs4_create() method by leaving folder ID empty\n\n",
        "Service account needs 'Editor' permission on a shared folder or domain-wide delegation."
      )
      
      showNotification(error_msg, type = "error", duration = NULL)
    })
  })

  # Load sheets on startup
  observeEvent(TRUE, {
    load_sheet_list()
  }, once = TRUE)

  # Auto-select first sheet
  observeEvent(all_sheets(), {
    req(all_sheets())
    if (nrow(all_sheets()) > 0 && is.null(current_sheet_id())) {
      current_sheet_id(all_sheets()$id[1])
    }
  })

  # Render sheet selector
  output$sheet_selector <- renderUI({
    req(all_sheets())
    selectInput(
      "selected_sheet",
      "Select a Sheet:",
      choices = setNames(all_sheets()$id, all_sheets()$name)
    )
  })

  # Update current sheet ID
  observeEvent(input$selected_sheet, {
    current_sheet_id(input$selected_sheet)
  })

  # Auto-load data when sheet changes
  observeEvent(current_sheet_id(), {
    req(current_sheet_id())
    read_sheet_data()
  })

  # Refresh buttons
  observeEvent(input$refresh_sheets, {
    load_sheet_list()
  })

  observeEvent(input$refresh_btn, {
    read_sheet_data()
  })

  # Add row UI
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

  # Add new row
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
      showNotification("Row added! Refreshing data...", type = "message")

      # Clear inputs
      lapply(cols, function(col) {
        updateTextInput(session, paste0("col_", col), value = "")
      })

      # Refresh data after delay
      invalidateLater(2000, session)
      observe({
        read_sheet_data()
      }, once = TRUE)

    }, error = function(e) {
      showNotification(paste("Error adding row:", e$message), type = "error")
    })
  })

  # Display outputs
  output$sheet_data <- renderTable({
    sheet_data()
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
