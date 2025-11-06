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

ui <- page_sidebar(
  title = "Google Sheets Editor",
  sidebar = sidebar(
    h4("Sheet Selection"),
    textInput("sheet_id_input", "Google Sheet ID:", 
              value = "", 
              placeholder = "Paste the sheet ID here"),
    p("Get the ID from the sheet URL between /d/ and /edit"),
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

  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)

  # Read data without authentication (for public sheets)
  read_sheet_data <- function() {
    req(input$sheet_id_input)
    
    tryCatch({
      # Deauthorize for reading (like your working example)
      gs4_deauth()
      
      # Read the sheet
      data <- read_sheet(input$sheet_id_input)
      sheet_data(data)
      last_update(Sys.time())
      showNotification("Data loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(
        paste("Error reading sheet:", e$message),
        type = "error"
      )
      sheet_data(data.frame(
        Error = paste("Could not read sheet:", e$message),
        Help = "Make sure the sheet is publicly viewable or shared with anyone with the link",
        stringsAsFactors = FALSE
      ))
    })
  }

  # Reload button
  observeEvent(input$refresh_btn, {
    read_sheet_data()
  })

  # Render input fields for adding a new row
  output$add_row_ui <- renderUI({
    req(sheet_data())
    cols <- names(sheet_data())
    
    if (length(cols) == 0 || any(cols %in% c("Error", "Help"))) {
      return(p("Load sheet data first to see input fields."))
    }
    
    tagList(
      lapply(cols, function(col) {
        textInput(paste0("col_", col), label = col, value = "")
      }),
      actionButton("add_row", "Add Row", class = "btn-success")
    )
  })

  # Add new row to sheet (with authentication)
  observeEvent(input$add_row, {
    req(sheet_data(), input$sheet_id_input)
    
    tryCatch({
      cols <- names(sheet_data())
      if (any(cols %in% c("Error", "Help"))) {
        showNotification("Cannot add row: sheet not loaded properly", type = "error")
        return()
      }
      
      # Re-authenticate for writing
      gs4_auth(path = "secret/trackingauth.json")
      
      # Collect input values
      new_row <- lapply(cols, function(col) {
        val <- input[[paste0("col_", col)]]
        if (is.null(val) || val == "") "" else val
      })
      names(new_row) <- cols
      new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
      
      # Try to append to sheet
      sheet_append(input$sheet_id_input, new_row_df)
      
      showNotification("Row added successfully!", type = "message")
      
      # Reload the data (deauth again for reading)
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
