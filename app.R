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
    textInput("sheet_name", "Sheet Name/Tab:", value = "", placeholder = "Leave empty for first sheet"),
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

  # Load data using a simplified approach that avoids type inference
  load_sheet_data <- function() {
    req(input$selected_sheet)
    
    # Determine sheet name/tab
    sheet_name <- if (input$sheet_name != "") input$sheet_name else NULL
    
    tryCatch({
      # Try reading without any col_types specification first
      data <- tryCatch({
        if (!is.null(sheet_name)) {
          read_sheet(input$selected_sheet, sheet = sheet_name, range = "A1:Z1000")
        } else {
          read_sheet(input$selected_sheet, range = "A1:Z1000")
        }
      }, error = function(e1) {
        # If that fails, try with explicit character types but smaller range
        tryCatch({
          if (!is.null(sheet_name)) {
            read_sheet(input$selected_sheet, sheet = sheet_name, range = "A1:Z100", col_types = "c")
          } else {
            read_sheet(input$selected_sheet, range = "A1:Z100", col_types = "c")
          }
        }, error = function(e2) {
          # Final fallback: try very small range
          if (!is.null(sheet_name)) {
            read_sheet(input$selected_sheet, sheet = sheet_name, range = "A1:Z10")
          } else {
            read_sheet(input$selected_sheet, range = "A1:Z10")
          }
        })
      })
      
      # Handle empty data
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
      } else {
        # Convert all columns to character to ensure consistency
        data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
        
        # Replace NA values with empty strings
        data[is.na(data)] <- ""
        
        # Clean column names
        names(data) <- make.names(names(data), unique = TRUE)
        
        # Remove completely empty rows
        empty_rows <- apply(data, 1, function(row) all(row == "" | is.na(row)))
        if (any(!empty_rows)) {
          data <- data[!empty_rows, , drop = FALSE]
        }
      }
      
      sheet_data(data)
      last_update(Sys.time())
      showNotification(paste("Data loaded successfully! Rows:", nrow(data)), type = "message")
      
    }, error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error",
        duration = 10
      )
      sheet_data(data.frame(Error = paste("Could not load data:", e$message), stringsAsFactors = FALSE))
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

  # Auto-load data when sheet is selected
  observeEvent(input$selected_sheet, {
    if (!is.null(input$selected_sheet)) {
      load_sheet_data()
    }
  }, ignoreInit = TRUE)

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
    
    if (length(cols) == 0 || cols[1] == "Error") {
      return(p("No columns available. Please load a valid sheet first."))
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
      if (cols[1] == "Error") {
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
      sheet_name <- if (input$sheet_name != "") input$sheet_name else NULL
      if (!is.null(sheet_name)) {
        sheet_append(input$selected_sheet, new_row_df, sheet = sheet_name)
      } else {
        sheet_append(input$selected_sheet, new_row_df)
      }
      
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
      return(data.frame("Message" = "Select a sheet and click Reload Data", stringsAsFactors = FALSE))
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
