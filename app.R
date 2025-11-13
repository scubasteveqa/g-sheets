library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(jsonlite)
library(httr)

# Authenticate with service account credentials from environment
creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
if (creds_json != "") {
  # Write to temp file
  temp_key <- tempfile(fileext = ".json")
  writeLines(creds_json, temp_key)

  # Authenticate with path - include all necessary scopes
  drive_auth(
    path = temp_key,
    scopes = c(
      "https://www.googleapis.com/auth/spreadsheets",
      "https://www.googleapis.com/auth/drive.file"
    )
  )
  gs4_auth(token = drive_token())
}


ui <- page_sidebar(
  title = "Google Sheets Editor",
  sidebar = sidebar(
      h4("Create New Sheet"),
    textInput("new_sheet_name", "Sheet Name", placeholder = "Enter new sheet name"),
    actionButton("create_sheet", "Create Sheet", class = "btn-success"),
    hr(),
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
  poll_trigger <- reactiveVal(0)

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

  # Read data using direct Google Sheets API v4 calls
  read_sheet_data <- function() {
    req(current_sheet_id())

    tryCatch({
      # Get credentials from environment
      creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
      creds <- jsonlite::fromJSON(creds_json)

      # Create OAuth token manually
      token <- httr::oauth_service_token(
        endpoint = httr::oauth_endpoints("google"),
        secrets = creds,
        scope = c(
        "https://www.googleapis.com/auth/spreadsheets",
        "https://www.googleapis.com/auth/drive.file"
        )
      )

      # Direct API call
      api_url <- paste0(
        "https://sheets.googleapis.com/v4/spreadsheets/",
        current_sheet_id(),
        "/values/A:Z"
      )

      response <- httr::GET(
        api_url,
        httr::config(token = token)
      )

      if (httr::status_code(response) == 200) {
        content_data <- httr::content(response, "parsed")

        if (!is.null(content_data$values) && length(content_data$values) > 0) {
          values <- content_data$values

          if (length(values) < 1) {
            data <- data.frame(NoData = "Sheet is empty", stringsAsFactors = FALSE)
          } else {
            headers <- unlist(values[[1]])
            max_cols <- length(headers)

            if (length(values) == 1) {
              data <- data.frame(matrix(ncol = max_cols, nrow = 0))
              names(data) <- make.names(headers, unique = TRUE)
            } else {
              data_rows <- values[-1]
              num_rows <- length(data_rows)

              data_matrix <- matrix("", nrow = num_rows, ncol = max_cols)

              for (i in seq_len(num_rows)) {
                row <- unlist(data_rows[[i]])
                if (length(row) > 0) {
                  cols_to_fill <- min(length(row), max_cols)
                  data_matrix[i, seq_len(cols_to_fill)] <- row[seq_len(cols_to_fill)]
                }
              }

              data <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
              names(data) <- make.names(headers, unique = TRUE)
            }
          }
        } else {
          data <- data.frame(NoData = "No values found", stringsAsFactors = FALSE)
        }

        sheet_data(data)
        last_update(Sys.time())
        showNotification(paste("Data loaded! Rows:", nrow(data), "Columns:", ncol(data)), type = "message")
      } else {
        error_msg <- httr::content(response, "text", encoding = "UTF-8")
        stop(paste("API error:", httr::status_code(response), error_msg))
      }

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      sheet_data(data.frame(Error = e$message, stringsAsFactors = FALSE))
    })
  }

# Create new spreadsheet
observeEvent(input$create_sheet, {
  req(input$new_sheet_name)

  tryCatch({
    new_sheet <- gs4_create(
      name = input$new_sheet_name,
      sheets = list(
        list(
          name = "Sheet1",
          data = data.frame(
            Column1 = character(0),
            Column2 = character(0),
            stringsAsFactors = FALSE
          )
        )
      )
    )

    showNotification(
      paste("Sheet created successfully:", input$new_sheet_name),
      type = "message"
    )

    # Set the new sheet ID FIRST
    current_sheet_id(new_sheet$spreadsheet_id)

    # Clear input
    updateTextInput(session, "new_sheet_name", value = "")

    # Refresh list AFTER a delay
    invalidateLater(2000, session)
    observe({
      load_sheet_list()
    }, once = TRUE)

  }, error = function(e) {
    error_msg <- e$message
    
    # Provide helpful guidance for 403 errors
    if (grepl("403|PERMISSION_DENIED", error_msg)) {
      error_msg <- paste0(
        "Permission Denied (403): Cannot create sheet.\n\n",
        "This usually means the Google Drive API is not enabled.\n\n",
        "To fix:\n",
        "1. Go to Google Cloud Console\n",
        "2. Navigate to 'APIs & Services' > 'Library'\n",
        "3. Search for 'Google Drive API'\n",
        "4. Click 'ENABLE'\n\n",
        "Original error: ", e$message
      )
    }
    
    showNotification(
      error_msg,
      type = "error",
      duration = NULL  # Don't auto-dismiss
    )
  })
})

  # Poll for data updates with a delay
  poll_data <- function(delay_ms = 2000, max_attempts = 5) {
    attempt <- 0

    poll_once <- function() {
      attempt <<- attempt + 1

      invalidateLater(delay_ms, session)

      if (attempt <= max_attempts) {
        read_sheet_data()
      }
    }

    # Start first poll immediately
    invalidateLater(0, session)
    observe({
      poll_trigger()
      poll_once()
    })
  }

  # Load sheets on startup
  observeEvent(TRUE, {
    load_sheet_list()
  }, once = TRUE)

  # Auto-select first sheet after list loads
  observeEvent(all_sheets(), {
    req(all_sheets())
    if (nrow(all_sheets()) > 0 && is.null(current_sheet_id())) {
      current_sheet_id(all_sheets()$id[1])
    }
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

  # Auto-load data when sheet changes
  observeEvent(current_sheet_id(), {
    req(current_sheet_id())
    read_sheet_data()
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

  # Add new row to sheet with polling
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

    # Append to sheet
    sheet_append(current_sheet_id(), new_row_df)

    showNotification("Row added! Refreshing data...", type = "message")

    # Clear inputs
    lapply(cols, function(col) {
      updateTextInput(session, paste0("col_", col), value = "")
    })

    # Poll with delays to allow Google Sheets to update
    for (i in 1:5) {
      Sys.sleep(2)
      read_sheet_data()
    }

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
