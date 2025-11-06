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
    actionButton("load_sheet", "Load Sheet Data", class = "btn-primary"),
    hr(),
    h4("Manual Data Entry"),
    p("If automatic loading fails, enter data manually:"),
    textInput("manual_headers", "Column Headers (comma-separated):", 
              placeholder = "country,continent,year,lifeExp,pop,gdpPercap"),
    actionButton("create_manual", "Create Manual Entry Form", class = "btn-warning"),
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

  # Create manual data structure when auto-loading fails
  create_manual_structure <- function() {
    req(input$manual_headers)
    
    headers <- trimws(strsplit(input$manual_headers, ",")[[1]])
    if (length(headers) > 0) {
      # Create empty data frame with specified headers
      empty_data <- data.frame(matrix(character(0), nrow = 0, ncol = length(headers)), 
                              stringsAsFactors = FALSE)
      names(empty_data) <- make.names(headers, unique = TRUE)
      
      sheet_data(empty_data)
      last_update(Sys.time())
      showNotification(paste("Manual structure created with", length(headers), "columns"), type = "message")
    }
  }

  # Simplified data loading - just try one simple method
  load_sheet_data <- function() {
    req(current_sheet_id())
    
    # Show loading notification
    showNotification("Attempting to load sheet data...", type = "message", duration = 3)
    
    tryCatch({
      # Single, simple attempt with minimal parameters
      # Force everything to be character and limit to reasonable range
      data <- read_sheet(
        ss = current_sheet_id(),
        range = "A1:Z1000",
        col_types = "c",
        na = c("", "NA"),
        trim_ws = TRUE
      )
      
      # If we got here, it worked!
      if (is.null(data) || nrow(data) == 0) {
        showNotification("Sheet is empty or no data found", type = "warning")
        data <- data.frame(Message = "No data found in sheet", stringsAsFactors = FALSE)
      } else {
        # Basic cleanup
        data <- as.data.frame(data, stringsAsFactors = FALSE)
        data[is.na(data)] <- ""
        names(data) <- make.names(names(data), unique = TRUE)
        
        showNotification(paste("Success! Loaded", nrow(data), "rows"), type = "message")
      }
      
      sheet_data(data)
      last_update(Sys.time())
      
    }, error = function(e) {
      showNotification(
        paste("Automatic loading failed:", e$message, 
              "\nTry using manual entry below."),
        type = "error",
        duration = 15
      )
      
      # Set a helpful error message
      sheet_data(data.frame(
        Error = "Could not load sheet automatically",
        Solution = "Use manual data entry below",
        Technical_Details = e$message,
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

  # Create manual structure button
  observeEvent(input$create_manual, {
    create_manual_structure()
  })

  # Render input fields for adding a new row
  output$add_row_ui <- renderUI({
    req(sheet_data())
    cols <- names(sheet_data())
    
    if (length(cols) == 0 || any(cols %in% c("Error", "Solution", "Technical_Details", "Message"))) {
      return(p("Load sheet data or create manual structure first."))
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
    req(sheet_data(), current_sheet_id())
    
    tryCatch({
      cols <- names(sheet_data())
      if (any(cols %in% c("Error", "Solution", "Technical_Details", "Message"))) {
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
      
      # Try to append to sheet
      sheet_append(current_sheet_id(), new_row_df)
      
      showNotification("Row added successfully!", type = "message")
      
      # Update local data
      current_data <- sheet_data()
      updated_data <- rbind(current_data, new_row_df)
      sheet_data(updated_data)
      last_update(Sys.time())
      
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
      return(data.frame("Message" = "Select a sheet and click Load Sheet Data", stringsAsFactors = FALSE))
    }
    data
  })

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Select a sheet to load data"
    }
  })
}

shinyApp(ui = ui, server = server)
