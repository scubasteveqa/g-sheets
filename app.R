# app.R
library(shiny)
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

ui <- fluidPage(
  titlePanel("Google Sheets Auth Test"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data Controls"),
      actionButton("refresh_btn", "Reload Sheets", class = "btn-primary"),
      hr(),
      textOutput("last_updated")
    ),
    
    mainPanel(
      h3("Accessible Sheets"),
      tableOutput("sheet_list")
    )
  )
)

server <- function(input, output, session) {
  
  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)
  
  read_sheet_list <- function() {
    tryCatch({
      data <- gs4_find()
      sheet_data(data)
      last_update(Sys.time())
      showNotification("Sheets loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error reading sheets:", e$message),
        type = "error"
      )
    })
  }
  
  # Load data on startup
  observe({
    read_sheet_list()
  })
  
  # Reload button
  observeEvent(input$refresh_btn, {
    read_sheet_list()
  })
  
  output$sheet_list <- renderTable({
    if (!is.null(sheet_data())) {
      sheet_data()[, c("name", "id")]
    }
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
