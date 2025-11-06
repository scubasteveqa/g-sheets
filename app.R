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

  # Load list of accessible sheets (still use googlesheets4 for this)
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

  # Read data using direct Google Sheets API v4 calls
  read_sheet_data <- function() {
  req(current_sheet_id())
  
  tryCatch({
    # Use read_sheet with explicit options to avoid parsing issues
    data <- read_sheet(
      current_sheet_id(), 
      range = "A:Z",
      col_types = "c",  # Read all columns as character to avoid type detection
      .name_repair = "unique"  # Handle duplicate column names
    )
    
    if (is.null(data) || nrow(data) == 0) {
      data <- data.frame(NoData = "No values found in sheet", stringsAsFactors = FALSE)
    }
    
    sheet_data(data)
    last_update(Sys.time())
    showNotification(paste("Data loaded successfully! Rows:", nrow(data), "Columns:", ncol(data)), type = "message")
    
  }, error = function(e) {
    showNotification(
      paste("Error reading sheet:", e$message),
      type = "error",
      duration = 10
    )
    sheet_data(data.frame(
      Error = paste("Could not read sheet:", e$message),
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

  # Add new row to sheet (still use googlesheets4 for writing)
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
      
      # Try to append to sheet using googlesheets4
      sheet_append(current_sheet_id(), new_row_df)
      
      showNotification("Row added successfully!", type = "message")
      
      # Reload the data
      read_sheet_data()
      
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
