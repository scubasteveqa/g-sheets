library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(jsonlite)

# Create secret directory if it doesn't exist
if (!dir.exists("secret")) {
  dir.create("secret")
}

# Get credentials from environment variable and write to file
creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
if (creds_json != "" && !file.exists("secret/trackingauth.json")) {
  creds <- fromJSON(creds_json)
  write(toJSON(creds, auto_unbox = TRUE, pretty = TRUE),
        "secret/trackingauth.json")
}

# Authenticate with service account
drive_auth(path = "secret/trackingauth.json")
gs4_auth(path = "secret/trackingauth.json")
options(gargle_oauth_cache = "secret",
        gargle_oauth_email = TRUE)

ui <- page_sidebar(
  title = "Google Sheets Editor",
  sidebar = sidebar(
    h4("Sheet Selection"),
    uiOutput("sheet_selector"),
    hr(),
    actionButton("refresh_sheets", "Refresh Sheet List", class = "btn-secondary"),
    actionButton("refresh_data", "Reload Data", class = "btn-primary"),
    hr(),
    h4("Add New Row"),
    uiOutput("add_row_ui"),
    br(),
    textOutput("last_updated")
  ),
  card(
    card_header("Sheet Data"),
    tableOutput("sheet_data")
  )
)

server <- function(input, output, session) {

  all_sheets <- reactiveVal(NULL)
  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)

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

  # Load data from selected sheet with better error handling
  load_sheet_data <- function() {
    req(input$selected_sheet)
    tryCatch({
      # Try different approaches to read the sheet
      data <- tryCatch({
        # First attempt: use range_read with no column type specification
        range_read(input$selected_sheet)
      }, error = function(e1) {
        tryCatch({
          # Second attempt: read specific range starting from A1
          range_read(input$selected_sheet, range = "A:Z")
        }, error = function(e2) {
          tryCatch({
            # Third attempt: read with basic read_sheet and handle the error differently
            gs4_get(input$selected_sheet) # This will fail early if sheet is problematic
            read_sheet(input$selected_sheet, range = "A1:Z1000")
          }, error = function(e3) {
            # Fourth attempt: try to get sheet properties first
            sheet_properties <- gs4_get(input$selected_sheet)
            if (length(sheet_properties$sheets) > 0) {
              # Read the first sheet by name
              sheet_name <- sheet_properties$sheets[[1]]$properties$title
              read_sheet(input$selected_sheet, sheet = sheet_name)
            } else {
              stop("No readable sheets found")
            }
          })
        })
      })
      
      # Handle empty data
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
      }
      
      # Convert all columns to character to ensure consistency
      data <- data.frame(lapply(data, function(x) {
        if (is.list(x)) {
          # Handle list columns by converting to character
          sapply(x, function(y) if(is.null(y)) "" else as.character(y))
        } else {
          as.character(x)
        }
      }), stringsAsFactors = FALSE)
      
      # Replace NA values with empty strings for display
      data[is.na(data)] <- ""
      
      # Ensure column names are valid
      names(data) <- make.names(names(data), unique = TRUE)
      
      sheet_data(data)
      last_update(Sys.time())
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error",
        duration = 10
      )
      # Set empty data frame on error
      sheet_data(data.frame(Column1 = character(0), stringsAsFactors = FALSE))
    })
  }

  # Load sheets on startup
  observe({
    load_sheet_list()
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

  # Load data when sheet is selected
  observeEvent(input$selected_sheet, {
    load_sheet_data()
  })

  # Refresh sheet list button
  observeEvent(input$refresh_sheets, {
    load_sheet_list()
  })

  # Reload data button
  observeEvent(input$refresh_data, {
    load_sheet_data()
  })

  # Render input fields for adding a new row
  output$add_row_ui <- renderUI({
    req(sheet_data())
    cols <- names(sheet_data())
    
    if (length(cols) == 0) {
      return(p("No columns available. Please select a valid sheet."))
    }
    
    tagList(
      lapply(cols, function(col) {
        textInput(paste0("col_", col), label = col, value = "")
      }),
      actionButton("add_row", "Add Row", class = "btn-success")
    )
  })

  # Add new row to sheet
  observeEvent(input$add_row, {
    req(sheet_data(), input$selected_sheet)
    
    tryCatch({
      # Collect input values
      cols <- names(sheet_data())
      new_row <- lapply(cols, function(col) {
        val <- input[[paste0("col_", col)]]
        if (is.null(val) || val == "") "" else val
      })
      names(new_row) <- cols
      new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
      
      # Append to sheet
      sheet_append(input$selected_sheet, new_row_df)
      
      showNotification("Row added successfully!", type = "message")
      
      # Reload data
      load_sheet_data()
      
      # Clear inputs
      lapply(cols, function(col) {
        updateTextInput(session, paste0("col_", col), value = "")
      })
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
    data <- sheet_data()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame("No Data" = "Select a sheet and click Reload Data"))
    }
    data
  }, na = "")

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Select a sheet to load data"
    }
  })
}

shinyApp(ui = ui, server = server)
