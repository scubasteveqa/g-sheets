library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(jsonlite)
library(httr)
library(readr)

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

  # Load data using CSV export to completely bypass googlesheets4 type inference
  load_sheet_data <- function() {
    req(current_sheet_id())
    
    tryCatch({
      # Method 1: Try to export as CSV and read that
      csv_url <- paste0("https://docs.google.com/spreadsheets/d/", 
                       current_sheet_id(), 
                       "/export?format=csv")
      
      # Get the authenticated session
      token <- gs4_token()
      
      # Make authenticated request to get CSV
      response <- tryCatch({
        GET(csv_url, config(token = token))
      }, error = function(e) {
        # Fallback: try the old method one more time
        read_sheet(current_sheet_id(), col_types = "c", n_max = 1000)
      })
      
      if (class(response)[1] == "response" && response$status_code == 200) {
        # Parse CSV content
        csv_content <- content(response, "text", encoding = "UTF-8")
        data <- read_csv(csv_content, col_types = cols(.default = "c"), 
                        show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
      } else {
        # If CSV export failed, fall back to simplified read
        data <- response  # This would be the fallback read_sheet result
      }
      
      # Handle empty data
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
        showNotification("Sheet appears to be empty", type = "warning")
      } else {
        # Convert to data frame and clean
        data <- as.data.frame(data, stringsAsFactors = FALSE)
        
        # Ensure all columns are character
        data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
        
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
        Help = "Check sheet permissions and formatting",
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
      
      # Append to sheet - this should still work even if reading doesn't
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
