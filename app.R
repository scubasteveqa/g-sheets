library(shiny)
library(bslib)
library(httr)
library(jsonlite)
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

ui <- page_sidebar(
  title = "Google Sheets Editor",
  sidebar = sidebar(
    h4("Sheet Access"),
    textInput("sheet_id", "Google Sheet ID:", 
              value = "", 
              placeholder = "Enter the sheet ID from the URL"),
    p("Extract the ID from: https://docs.google.com/spreadsheets/d/[SHEET_ID]/edit"),
    hr(),
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

  sheet_data <- reactiveVal(NULL)
  last_update <- reactiveVal(NULL)
  access_token <- reactiveVal(NULL)

  # Get OAuth token using service account
  get_access_token <- function() {
    tryCatch({
      # Read service account key
      service_key <- fromJSON("secret/trackingauth.json")
      
      # Create JWT for authentication
      header <- list(alg = "RS256", typ = "JWT")
      
      now <- as.numeric(Sys.time())
      payload <- list(
        iss = service_key$client_email,
        scope = "https://www.googleapis.com/auth/spreadsheets",
        aud = "https://oauth2.googleapis.com/token",
        exp = now + 3600,
        iat = now
      )
      
      # For simplicity, let's use httr to get token
      body <- list(
        grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
        assertion = "placeholder" # In real implementation, would create proper JWT
      )
      
      # Actually, let's use googlesheets4 minimally just for auth
      library(googlesheets4)
      gs4_auth(path = "secret/trackingauth.json")
      token <- gs4_token()
      access_token(token$credentials$access_token)
      
    }, error = function(e) {
      showNotification(paste("Auth error:", e$message), type = "error")
      return(NULL)
    })
  }

  # Load data using direct API call
  load_sheet_data <- function() {
    req(input$sheet_id)
    
    if (is.null(access_token())) {
      get_access_token()
    }
    
    req(access_token())
    
    tryCatch({
      # Use Google Sheets API v4 directly
      api_url <- paste0(
        "https://sheets.googleapis.com/v4/spreadsheets/",
        input$sheet_id,
        "/values/A:Z"
      )
      
      response <- GET(
        api_url,
        add_headers(Authorization = paste("Bearer", access_token()))
      )
      
      if (status_code(response) == 200) {
        content_data <- content(response, "parsed")
        values <- content_data$values
        
        if (length(values) > 0) {
          # First row as headers
          headers <- values[[1]]
          data_rows <- values[-1]
          
          # Convert to data frame
          max_cols <- max(length(headers), max(sapply(data_rows, length)))
          
          # Pad rows to same length
          data_matrix <- t(sapply(data_rows, function(row) {
            c(row, rep("", max_cols - length(row)))[1:max_cols]
          }))
          
          # Pad headers if needed
          headers <- c(headers, paste0("Column", (length(headers)+1):max_cols))[1:max_cols]
          
          data <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
          names(data) <- make.names(headers, unique = TRUE)
          
          # Clean up
          data[is.na(data)] <- ""
          
          # Remove empty rows
          empty_rows <- apply(data, 1, function(row) all(row == "" | is.na(row)))
          if (any(!empty_rows)) {
            data <- data[!empty_rows, , drop = FALSE]
          }
          
        } else {
          data <- data.frame(Column1 = character(0), stringsAsFactors = FALSE)
        }
        
        sheet_data(data)
        last_update(Sys.time())
        showNotification(paste("Data loaded! Rows:", nrow(data)), type = "message")
        
      } else {
        stop(paste("API returned status:", status_code(response)))
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
      sheet_data(data.frame(Error = paste("Could not load:", e$message), stringsAsFactors = FALSE))
    })
  }

  # Initialize auth on startup
  observe({
    get_access_token()
  })

  # Load sheet button
  observeEvent(input$load_sheet, {
    load_sheet_data()
  })

  # Render input fields for adding a new row
  output$add_row_ui <- renderUI({
    req(sheet_data())
    cols <- names(sheet_data())
    
    if (length(cols) == 0 || cols[1] == "Error") {
      return(p("Load sheet data first to see input fields."))
    }
    
    tagList(
      lapply(cols, function(col) {
        textInput(paste0("col_", col), label = col, value = "")
      }),
      actionButton("add_row", "Add Row", class = "btn-success")
    )
  })

  # Add new row using direct API
  observeEvent(input$add_row, {
    req(sheet_data(), input$sheet_id, access_token())
    
    tryCatch({
      cols <- names(sheet_data())
      if (cols[1] == "Error") return()
      
      # Collect values
      new_values <- sapply(cols, function(col) {
        val <- input[[paste0("col_", col)]]
        if (is.null(val) || val == "") "" else val
      })
      
      # Append via API
      api_url <- paste0(
        "https://sheets.googleapis.com/v4/spreadsheets/",
        input$sheet_id,
        "/values/A:Z:append?valueInputOption=RAW"
      )
      
      body_data <- list(
        values = list(as.list(new_values))
      )
      
      response <- POST(
        api_url,
        add_headers(
          Authorization = paste("Bearer", access_token()),
          `Content-Type` = "application/json"
        ),
        body = toJSON(body_data, auto_unbox = TRUE)
      )
      
      if (status_code(response) == 200) {
        showNotification("Row added successfully!", type = "message")
        # Clear inputs
        lapply(cols, function(col) {
          updateTextInput(session, paste0("col_", col), value = "")
        })
        # Reload data
        load_sheet_data()
      } else {
        stop(paste("Failed to add row. Status:", status_code(response)))
      }
      
    }, error = function(e) {
      showNotification(paste("Error adding row:", e$message), type = "error")
    })
  })

  # Display sheet data
  output$sheet_data <- renderTable({
    data <- sheet_data()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame("Message" = "Enter Sheet ID and click Load Sheet Data", stringsAsFactors = FALSE))
    }
    data
  })

  output$last_updated <- renderText({
    if (!is.null(last_update())) {
      paste("Last updated:", format(last_update(), "%Y-%m-%d %H:%M:%S"))
    } else {
      "Enter sheet ID to load data"
    }
  })
}

shinyApp(ui = ui, server = server)
