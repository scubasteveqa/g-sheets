library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(jsonlite)
library(httr)

# Authenticate with service account credentials from environment
creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
if (creds_json != "") {
  # Write to temp file
  temp_key <- tempfile(fileext = ".json")
  writeLines(creds_json, temp_key)

  # Set gargle options (like customer.R does)
  options(
    gargle_oauth_cache = FALSE,
    gargle_oauth_email = TRUE
  )

  # Authenticate both services with the same path (like customer.R does)
  # This ensures both get the proper default scopes including drive.file
  drive_auth(path = temp_key)
  gs4_auth(path = temp_key)
}


ui <- page_sidebar(
  title = "Google Sheets Editor",
  sidebar = sidebar(
    h4("Create New Sheet"),
    textInput("new_sheet_name", "Sheet Name", placeholder = "Enter new sheet name"),
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
  poll_trigger <- reactiveVal(0)

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

  # Read data using googlesheets4 (simplified to use global auth)
  read_sheet_data <- function() {
    req(current_sheet_id())

    tryCatch({
      # Use the googlesheets4 package directly (it uses the global auth)
      # col_types = "c" forces all columns to character to avoid list column issues
      data <- read_sheet(current_sheet_id(), col_types = "c")

      # Handle empty sheets
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(NoData = "Sheet is empty", stringsAsFactors = FALSE)
      } else {
        # Convert any remaining list columns to character
        data <- as.data.frame(lapply(data, function(col) {
          if (is.list(col)) {
            sapply(col, function(x) paste(unlist(x), collapse = ", "))
          } else {
            as.character(col)
          }
        }), stringsAsFactors = FALSE)
      }

      sheet_data(data)
      last_update(Sys.time())
      showNotification(paste("Data loaded! Rows:", nrow(data), "Columns:", ncol(data)), type = "message")

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      sheet_data(data.frame(Error = e$message, stringsAsFactors = FALSE))
    })
  }

  # Create new spreadsheet (using same approach as customer.R)
  observeEvent(input$create_sheet, {
    req(input$new_sheet_name)

    tryCatch({
      # First, test if we can use drive operations
      cat("Testing drive access...\n")
      test_files <- drive_find(n_max = 1)
      cat("Drive access OK, found", nrow(test_files), "files\n")

      # Check if sheet with this name already exists
      all_sheets_check <- gs4_find()
      if (input$new_sheet_name %in% all_sheets_check$name) {
        showNotification(
          paste("A sheet named", input$new_sheet_name, "already exists!"),
          type = "warning",
          duration = 5
        )
        return()
      }

      # Create new sheet with template data (like customer.R does)
      initial_data <- data.frame(
        Column1 = character(0),
        Column2 = character(0),
        stringsAsFactors = FALSE
      )

      cat("Attempting to create sheet:", input$new_sheet_name, "\n")

      new_sheet <- gs4_create(
        name = input$new_sheet_name,
        sheets = list("Sheet1" = initial_data)
      )

      cat("Sheet created! ID:", new_sheet$spreadsheet_id, "\n")

      # Move to shared folder (like customer.R does)
      # NOTE: This will only work AFTER you fix the 403 error by adding IAM Editor role
      # Uncomment the line below once you can successfully create sheets:
      # drive_mv(new_sheet, path = "Test GDrive/")

      # Sheet creation succeeded
      showNotification(
        paste("Sheet created successfully:", input$new_sheet_name),
        type = "message",
        duration = 3
      )

      # Don't try to load the new sheet immediately - just refresh the list
      # The auto-load when changing sheets might fail if sheet isn't ready
      updateTextInput(session, "new_sheet_name", value = "")

      showNotification(
        "Refreshing sheet list... Select the new sheet from the dropdown.",
        type = "message",
        duration = 5
      )

      # Refresh list after a delay
      invalidateLater(2000, session)
      observe({
        load_sheet_list()
      }, once = TRUE)

    }, error = function(e) {
      # Log detailed error information
      cat("ERROR creating sheet:\n")
      cat("Message:", e$message, "\n")
      cat("Call:", deparse(e$call), "\n")

      showNotification(
        paste("Error creating sheet:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Poll for data updates with a delay
  poll_data <- function(delay_ms = 2000, max_attempts = 5) {
    attempt <- 0

    poll_once <- function() {
      attempt <<- attempt + 1

      invalidateLater(delay_ms, session)

      if (attempt <= max_attempts) {
        read_sheet_data()
      }
    }

    # Start first poll immediately
    invalidateLater(0, session)
    observe({
      poll_trigger()
      poll_once()
    })
  }

  # Load sheets on startup
  observeEvent(TRUE, {
    load_sheet_list()
  }, once = TRUE)

  # Auto-select first sheet after list loads
  observeEvent(all_sheets(), {
    req(all_sheets())
    if (nrow(all_sheets()) > 0 && is.null(current_sheet_id())) {
      current_sheet_id(all_sheets()$id[1])
    }
  })

  # Render sheet selector dropdown
  output$sheet_selector <- renderUI({
    req(all_sheets())
    selectInput(
      "selected_sheet",
      "Select a Sheet:",
      choices = setNames(all_sheets()$id, all_sheets()$name)
    )
  })

  # Update current sheet ID when selection changes
  observeEvent(input$selected_sheet, {
    current_sheet_id(input$selected_sheet)
  })

  # Auto-load data when sheet changes
  observeEvent(current_sheet_id(), {
    req(current_sheet_id())
    read_sheet_data()
  })

  # Refresh sheet list button
  observeEvent(input$refresh_sheets, {
    load_sheet_list()
  })

  # Reload button
  observeEvent(input$refresh_btn, {
    read_sheet_data()
  })

  # Render input fields for adding a new row
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

  # Add new row to sheet with polling
  observeEvent(input$add_row, {
  req(sheet_data(), current_sheet_id())

  tryCatch({
    cols <- names(sheet_data())
    if (any(cols %in% c("Error", "NoData"))) {
      showNotification("Cannot add row: sheet not loaded properly", type = "error")
      return()
    }

    # Collect input values
    new_row <- lapply(cols, function(col) {
      val <- input[[paste0("col_", col)]]
      if (is.null(val) || val == "") "" else val
    })
    names(new_row) <- cols
    new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)

    # Append to sheet
    sheet_append(current_sheet_id(), new_row_df)

    showNotification("Row added! Refreshing data...", type = "message")

    # Clear inputs
    lapply(cols, function(col) {
      updateTextInput(session, paste0("col_", col), value = "")
    })

    # Poll with delays to allow Google Sheets to update
    for (i in 1:5) {
      Sys.sleep(2)
      read_sheet_data()
    }

  }, error = function(e) {
    showNotification(
      paste("Error adding row:", e$message),
      type = "error",
      duration = 10
    )
  })
})

  # Display sheet data
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
