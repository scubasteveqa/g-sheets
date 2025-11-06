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

  # Load data using a more robust approach
  load_sheet_data <- function() {
    req(input$selected_sheet)
    
    # Determine sheet name/tab
    sheet_name <- if (input$sheet_name != "") input$sheet_name else NULL
    
    tryCatch({
      # Strategy: Read a small sample first to get column structure
      sample_data <- tryCatch({
        if (!is.null(sheet_name)) {
          range_read(input$selected_sheet, sheet = sheet_name, range = "A1:Z10", col_types = "c")
        } else {
          range_read(input$selected_sheet, range = "A1:Z10", col_types = "c")
        }
      }, error = function(e1) {
        # If that fails, try manual approach with range_read_values
        tryCatch({
          if (!is.null(sheet_name)) {
            raw_matrix <- range_read_values(input$selected_sheet, sheet = sheet_name, range = "A1:Z10")
          } else {
            raw_matrix <- range_read_values(input$selected_sheet, range = "A1:Z10")
          }
          
          if (nrow(raw_matrix) > 0) {
            # First row as column names
            col_names <- as.character(raw_matrix[1, ])
            # Remove empty column names
            non_empty_cols <- which(col_names != "" & !is.na(col_names))
            if (length(non_empty_cols) > 0) {
              col_names <- col_names[non_empty_cols]
              data_matrix <- raw_matrix[-1, non_empty_cols, drop = FALSE]
              
              # Convert to data frame
              data <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
              names(data) <- make.names(col_names, unique = TRUE)
              data
            } else {
              data.frame(Column1 = character(0), stringsAsFactors = FALSE)
            }
          } else {
            data.frame(Column1 = character(0), stringsAsFactors = FALSE)
          }
        }, error = function(e2) {
          stop(paste("Could not read sheet:", e2$message))
        })
      })
      
      if (nrow(sample_data) > 0) {
        # Now read the full data using the same method that worked
        col_count <- ncol(sample_data)
        col_letters <- LETTERS[1:min(col_count, 26)]  # Handle up to 26 columns
        if (col_count > 26) col_letters <- c(col_letters, paste0("A", LETTERS[1:(col_count-26)]))
        
        full_range <- paste0("A1:", col_letters[col_count], "1000")  # Read up to 1000 rows
        
        full_data <- tryCatch({
          if (!is.null(sheet_name)) {
            range_read(input$selected_sheet, sheet = sheet_name, range = full_range, col_types = "c")
          } else {
            range_read(input$selected_sheet, range = full_range, col_types = "c")
          }
        }, error = function(e) {
          # Fallback to manual method
          if (!is.null(sheet_name)) {
            raw_matrix <- range_read_values(input$selected_sheet, sheet = sheet_name, range = full_range)
          } else {
            raw_matrix <- range_read_values(input$selected_sheet, range = full_range)
          }
          
          if (nrow(raw_matrix) > 0) {
            col_names <- as.character(raw_matrix[1, ])
            non_empty_cols <- which(col_names != "" & !is.na(col_names))
            
            if (length(non_empty_cols) > 0) {
              col_names <- col_names[non_empty_cols]
              data_matrix <- raw_matrix[-1, non_empty_cols, drop = FALSE]
              
              data <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
              names(data) <- make.names(col_names, unique = TRUE)
              data
            } else {
              sample_data  # Return the sample if we can't read more
            }
          } else {
            sample_data
          }
        })
        
        data <- full_data
      } else {
        data <- sample_data
      }
      
      # Clean up the data
      data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
      data[is.na(data)] <- ""
      
      # Remove completely empty rows
      if (nrow(data) > 0) {
        empty_rows <- apply(data, 1, function(row) all(row == "" | is.na(row)))
        data <- data[!empty_rows, , drop = FALSE]
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

  # Auto-load data when sheet is selected (with delay to allow sheet_name input)
  observeEvent(input$selected_sheet, {
    # Give a small delay to allow manual sheet name entry
    invalidateLater(1000, session)
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
