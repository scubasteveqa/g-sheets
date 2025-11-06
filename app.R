library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(jsonlite)
library(httr)

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
    actionButton("load_sheet", "Load Sheet Data", class = "btn-primary"),
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

  # Load data using Google Sheets API directly to bypass googlesheets4 type inference
  load_sheet_data <- function() {
    req(current_sheet_id())
    
    tryCatch({
      # Try multiple fallback methods
      data <- tryCatch({
        # Method 1: Use range_read with specific range to avoid auto-detection
        range_read(current_sheet_id(), range = "A:Z", col_types = "c")
      }, error = function(e1) {
        tryCatch({
          # Method 2: Read with smaller range
          range_read(current_sheet_id(), range = "A1:Z1000", col_types = "c")
        }, error = function(e2) {
          tryCatch({
            # Method 3: Use read_sheet without col_types specification 
            read_sheet(current_sheet_id())
          }, error = function(e3) {
            tryCatch({
              # Method 4: Use range_read with no col_types
              range_read(current_sheet_id(), range = "A:Z")
            }, error = function(e4) {
              # Method 5: Try to read just the first few rows and columns
              range_read(current_sheet_id(), range = "A1:F100", col_types = "c")
            })
          })
        })
      })
      
      # Handle empty data
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
        showNotification("Sheet appears to be empty", type = "warning")
      } else {
        # Convert everything to character to ensure consistency
        data <- data.frame(lapply(data, function(col) {
          # Handle different column types
          if (is.list(col)) {
            # If it's a list column, extract the first element or convert to string
            sapply(col, function(x) {
              if (is.null(x)) {
                ""
              } else if (length(x) == 0) {
                ""
              } else {
                as.character(x[1])
              }
            })
          } else {
            as.character(col)
          }
        }), stringsAsFactors = FALSE)
        
        # Clean up the data
        data[is.na(data)] <- ""
        names(data) <- make.names(names(data), unique = TRUE)
        
        # Remove completely empty rows
        if (nrow(data) > 0) {
          empty_rows <- apply(data, 1, function(row) all(row == "" | is.na(row)))
          if (any(!empty_rows)) {
            data <- data[!empty_rows, , drop = FALSE]
          }
        }
      }
      
      sheet_data(data)
      last_update(Sys.time())
      showNotification(paste("Data loaded successfully! Rows:", nrow(data), "Columns:", ncol(data)), type = "message")
      
    }, error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error",
        duration = 15
      )
      # Create a diagnostic data frame
      sheet_data(data.frame(
        Error = paste("Could not load data:", e$message),
        Help = "Try refreshing or check if sheet has unusual formatting",
        stringsAsFactors = FALSE
      ))
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

  # Update current sheet ID when selection changes
  observeEvent(input$selected_sheet, {
    current_sheet_id(input$selected_sheet)
  })

  # Refresh sheet list button
  observeEvent(input$refresh_sheets, {
    load_sheet_list()
  })

  # Load sheet button
  observeEvent(input$load_sheet, {
    req(current_sheet_id())
    load_sheet_data()
  })

  # Render input fields for adding a new row
  output$add_row_ui <- renderUI({
    req(sheet_data())
    cols <- names(sheet_data())
    
    if (length(cols) == 0 || any(cols %in% c("Error", "Help"))) {
      return(p("No columns available. Please select a sheet and load data first."))
    }
    
    tagList(
      lapply(cols, function(col) {
        textInput(paste0("col_", col), label = col, value = "")
      }),
      actionButton("add_row", "Add Row", class = "btn-success"),
      br(),
      actionButton("refresh_data", "Refresh Data", class = "btn-secondary")
    )
  })

  # Add new row to sheet
  observeEvent(input$add_row, {
    req(sheet_data(), current_sheet_id())
    
    tryCatch({
      # Collect input values
      cols <- names(sheet_data())
      if (any(cols %in% c("Error", "Help"))) {
        showNotification("Cannot add row: sheet data not loaded properly", type = "error")
        return()
      }
      
      new_row <- lapply(cols, function(col) {
        val <- input[[paste0("col_", col)]]
        if (is.null(val) || val == "") "" else val
      })
      names(new_row) <- cols
      new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
      
      # Append to sheet
      sheet_append(current_sheet_id(), new_row_df)
      
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

  # Refresh data button
  observeEvent(input$refresh_data, {
    load_sheet_data()
  })

  # Display sheet data
  output$sheet_data <- renderTable({
    data <- sheet_data()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame("Message" = "Select a sheet and click Load Sheet Data", stringsAsFactors = FALSE))
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
