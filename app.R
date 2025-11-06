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
    textInput("sheet_url", "Paste Google Sheet URL:", 
              value = "", 
              placeholder = "https://docs.google.com/spreadsheets/d/..."),
    hr(),
    actionButton("load_sheet", "Load Sheet", class = "btn-primary"),
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

  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)
  current_sheet_id <- reactiveVal(NULL)

  # Extract sheet ID from URL
  extract_sheet_id <- function(url) {
    if (grepl("docs.google.com/spreadsheets", url)) {
      # Extract ID from URL
      id <- regmatches(url, regexpr("/spreadsheets/d/([a-zA-Z0-9-_]+)", url))
      id <- gsub("/spreadsheets/d/", "", id)
      return(id)
    }
    return(url) # Assume it's already an ID
  }

  # Load data from sheet
  load_sheet_data <- function() {
    req(current_sheet_id())
    
    tryCatch({
      # Simple approach: read all data as character to avoid type issues
      data <- read_sheet(current_sheet_id(), col_types = "c")
      
      # Handle empty data
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
      } else {
        # Clean up the data
        data[is.na(data)] <- ""
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

  # Load sheet button
  observeEvent(input$load_sheet, {
    req(input$sheet_url)
    
    sheet_id <- extract_sheet_id(input$sheet_url)
    current_sheet_id(sheet_id)
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
      return(data.frame("Message" = "Enter a Google Sheet URL and click Load Sheet", stringsAsFactors = FALSE))
    }
    data
  }, na = "")

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Enter a sheet URL to load data"
    }
  })
}

shinyApp(ui = ui, server = server)
