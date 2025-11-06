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
    uiOutput("tab_selector"),
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
  sheet_tabs <- reactiveVal(NULL)
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

  # Load tabs for selected sheet
  load_sheet_tabs <- function() {
    req(input$selected_sheet)
    tryCatch({
      sheet_info <- gs4_get(input$selected_sheet)
      tabs <- sapply(sheet_info$sheets, function(x) x$properties$title)
      sheet_tabs(tabs)
    }, error = function(e) {
      showNotification(
        paste("Error loading sheet tabs:", e$message),
        type = "error"
      )
      sheet_tabs(c("Sheet1")) # Default fallback
    })
  }

  # Load data from selected sheet with better error handling
  load_sheet_data <- function() {
    req(input$selected_sheet)
    sheet_name <- if (!is.null(input$selected_tab)) input$selected_tab else NULL
    
    tryCatch({
      # Use a completely different approach: read as raw values then convert
      data <- tryCatch({
        # Method 1: Read with explicit sheet name and range, all as text
        if (!is.null(sheet_name)) {
          range_read(input$selected_sheet, sheet = sheet_name, col_types = "c", .name_repair = "universal")
        } else {
          range_read(input$selected_sheet, col_types = "c", .name_repair = "universal")
        }
      }, error = function(e1) {
        tryCatch({
          # Method 2: Use sheet_values to get raw values
          if (!is.null(sheet_name)) {
            raw_values <- range_read_values(input$selected_sheet, sheet = sheet_name)
          } else {
            raw_values <- range_read_values(input$selected_sheet)
          }
          
          # Convert matrix to data frame
          if (nrow(raw_values) > 0) {
            col_names <- raw_values[1, ]
            data_rows <- raw_values[-1, , drop = FALSE]
            data <- as.data.frame(data_rows, stringsAsFactors = FALSE)
            names(data) <- col_names
            data
          } else {
            data.frame(Column1 = character(0), stringsAsFactors = FALSE)
          }
        }, error = function(e2) {
          # Method 3: Try with a specific small range first
          tryCatch({
            if (!is.null(sheet_name)) {
              range_read(input$selected_sheet, sheet = sheet_name, range = "A1:Z100", col_types = "c")
            } else {
              range_read(input$selected_sheet, range = "A1:Z100", col_types = "c")
            }
          }, error = function(e3) {
            stop(paste("All read methods failed. Last error:", e3$message))
          })
        })
      })
      
      # Handle empty data
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
      }
      
      # Convert all columns to character and clean up
      data <- data.frame(lapply(data, function(x) {
        if (is.list(x)) {
          sapply(x, function(y) if(is.null(y) || length(y) == 0) "" else as.character(y[1]))
        } else {
          as.character(x)
        }
      }), stringsAsFactors = FALSE)
      
      # Replace NA values with empty strings
      data[is.na(data)] <- ""
      
      # Clean column names
      names(data) <- make.names(names(data), unique = TRUE)
      
      # Remove completely empty rows
      if (nrow(data) > 0) {
        empty_rows <- apply(data, 1, function(row) all(row == "" | is.na(row)))
        data <- data[!empty_rows, , drop = FALSE]
      }
      
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

  # Render tab selector dropdown
  output$tab_selector <- renderUI({
    req(sheet_tabs())
    selectInput(
      "selected_tab",
      "Select a Tab:",
      choices = sheet_tabs(),
      selected = sheet_tabs()[1]
    )
  })

  # Load tabs when sheet is selected
  observeEvent(input$selected_sheet, {
    load_sheet_tabs()
  })

  # Load data when sheet or tab is selected
  observeEvent(c(input$selected_sheet, input$selected_tab), {
    if (!is.null(input$selected_sheet) && !is.null(input$selected_tab)) {
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
    
    if (length(cols) == 0) {
      return(p("No columns available. Please select a valid sheet and tab."))
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
    req(sheet_data(), input$selected_sheet, input$selected_tab)
    
    tryCatch({
      # Collect input values
      cols <- names(sheet_data())
      new_row <- lapply(cols, function(col) {
        val <- input[[paste0("col_", col)]]
        if (is.null(val) || val == "") "" else val
      })
      names(new_row) <- cols
      new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
      
      # Append to the specific sheet tab
      sheet_append(input$selected_sheet, new_row_df, sheet = input$selected_tab)
      
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
      return(data.frame("No Data" = "Select a sheet, tab, and click Reload Data"))
    }
    data
  }, na = "")

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Select a sheet and tab to load data"
    }
  })
}

shinyApp(ui = ui, server = server)
