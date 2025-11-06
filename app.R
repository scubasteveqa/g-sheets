##################################################### Test Tracking App for Posit Support ##################################################### 

#### Librarying ####

library(shiny)
library(tidyverse)
library(googledrive) # needed to move files within the tracking drive
library(googlesheets4) # googlesheets functionality
library(shinyWidgets) # pop up modal
library(shinyjs)
library(jsonlite)

# Create secret directory if it doesn't exist
if (!dir.exists("secret")) {
  dir.create("secret")
}

# Get credentials from environment variable and write to file
creds_json <- Sys.getenv("GOOGLE_CREDS_JSON")
if (creds_json != "" && !file.exists("secret/trackingauth.json")) {
  # Parse and re-write to ensure proper formatting
  creds <- fromJSON(creds_json)
  write(toJSON(creds, auto_unbox = TRUE, pretty = TRUE),
        "secret/trackingauth.json")
}# popups and other features needing js

drive_auth(path = "secret/trackingauth.json")
gs4_auth(path = "secret/trackingauth.json")
options(gargle_oauth_cache = "secret",
        gargle_oauth_email = TRUE)

#### Data Template ####

all_sheets <- gs4_find()
rowsToKeep <- c()
for(i in 1:nrow(all_sheets)) {
  perms <- c()
  perms_ln <- length(all_sheets$drive_resource[[i]]$permissions)
  for(j in 1:perms_ln) {
    perms <- c(perms, all_sheets$drive_resource[[i]]$permissions[[j]]$displayName)
  }
  if("nlltracking" %in% perms) {
    rowsToKeep <- c(rowsToKeep, i)
  }
}
all_sheets <- all_sheets[rowsToKeep, ]
# filters down to the template sheet
template_df <- all_sheets %>% 
  filter(name == "TEST TEMPLATE")
# reads that sheet
template_df <- read_sheet(template_df$id)

#### UI ####

ui <- fluidPage(
  
  # establishes the use of the pop-up modals used for error messages
  useSweetAlert(), 
  
  # need to use any shinyjs functions
  useShinyjs(), 
  
  br(), 
  
  fluidRow(
    
    column(
      width = 2, selectInput(inputId = "game", label = "Pick a Game", choices = c("Game A", "Game B", "Game C", "Game D"))
    ), 
    
    column(
      width = 3, uiOutput(outputId = "sheetStatus")
    ), 
    
    column(
      width = 2, actionButton(inputId = "createSheet", label = "Create New Sheet")
    ), 
    
    column(
      width = 3, actionButton(inputId = "addToSheet", label = "Add a New Row to the Sheet")
    ), 
    
    column(
      width = 2, actionButton(inputId = "update", label = "Refresh Data")
    )
    
  )
  
)

#### Server ####

server <- function(input, output, session) {
  
  #### Reactive Variables ####
  
  # Inputs that should trigger a data pull
  inputs_for_update <- reactive({
    list(input$update, input$game)
  })
  
  # Basic Game Info
  game_info <- reactiveValues("Game_Id" = "none")
  
  observeEvent(inputs_for_update(), {
    all_sheets <- gs4_find()
    rowsToKeep <- c()
    for(i in 1:nrow(all_sheets)) {
      perms <- c()
      perms_ln <- length(all_sheets$drive_resource[[i]]$permissions)
      for(j in 1:perms_ln) {
        perms <- c(perms, all_sheets$drive_resource[[i]]$permissions[[j]]$displayName)
      }
      if("nlltracking" %in% perms) {
        rowsToKeep <- c(rowsToKeep, i)
      }
    }
    all_sheets <- all_sheets[rowsToKeep, ]
    if(as.character(input$game) %in% all_sheets$name) {
      # filters down to the game selected
      current_sheet_info <- all_sheets %>% 
        filter(name == as.character(input$game))
      game_info$Game_Id <- current_sheet_info$id
    } 
  })
  
  #### Load Sheet ####
  df <- reactive({
    input$update
    
    # loads in a data frame with all of the sheets in the drive's names and id's
    all_sheets <- gs4_find()
    rowsToKeep <- c()
    for(i in 1:nrow(all_sheets)) {
      perms <- c()
      perms_ln <- length(all_sheets$drive_resource[[i]]$permissions)
      for(j in 1:perms_ln) {
        perms <- c(perms, all_sheets$drive_resource[[i]]$permissions[[j]]$displayName)
      }
      if("nlltracking" %in% perms) {
        rowsToKeep <- c(rowsToKeep, i)
      }
    }
    all_sheets <- all_sheets[rowsToKeep, ]
    if(as.character(input$game) %in% all_sheets$name) {
      # filters down to the game selected
      current_sheet_info <- all_sheets %>% 
        filter(name == as.character(input$game))
      # reads that sheet, and since it's the last line, it goes into df, a reactive data frame
      read_sheet(current_sheet_info$id)
    } 
  })
  
  #### On Create New Sheet Button ####
  observeEvent(input$createSheet, {
    showModal(modalDialog("Creating...", footer = NULL))
    
    # loads in a data frame of all of the sheets in the drive and their names
    all_sheets <- gs4_find()
    rowsToKeep <- c()
    for(i in 1:nrow(all_sheets)) {
      perms <- c()
      perms_ln <- length(all_sheets$drive_resource[[i]]$permissions)
      for(j in 1:perms_ln) {
        perms <- c(perms, all_sheets$drive_resource[[i]]$permissions[[j]]$displayName)
      }
      if("nlltracking" %in% perms) {
        rowsToKeep <- c(rowsToKeep, i)
      }
    }
    all_sheets <- all_sheets[rowsToKeep, ]
    # this if statement checks if the entered game name 
    if(as.character(input$game) %in% all_sheets$name) {
      removeModal()
      
      sendSweetAlert(
        session = session,
        title = "Sheet for this Game Already Exists",
        text = "Check the NLL Tracking Google Drive for this Game's Sheet",
        type = "error", 
        html = TRUE
      )
    } else {
      # Creates new sheet whose name is the game title and whose only sheet is the template
      df <- gs4_create(name = input$game, sheets = list("Tracking" = template_df))
      # Moves that sheet to the shared Game Sheets folder
      drive_mv(df, path = "TESTING LOCATION/")
      
      all_sheets_sub <- gs4_find()
      rowsToKeep <- c()
      for(i in 1:nrow(all_sheets_sub)) {
        perms <- c()
        perms_ln <- length(all_sheets_sub$drive_resource[[i]]$permissions)
        for(j in 1:perms_ln) {
          perms <- c(perms, all_sheets_sub$drive_resource[[i]]$permissions[[j]]$displayName)
        }
        if("nlltracking" %in% perms) {
          rowsToKeep <- c(rowsToKeep, i)
        }
      }
      all_sheets_sub <- all_sheets_sub[rowsToKeep, ]
      df_sub <- all_sheets_sub %>% 
        filter(name == input$game)
      game_info$Game_Id <- df_sub$id
      
      removeModal()
      
      sendSweetAlert(
        session = session,
        title = "Sheet Created!",
        text = tags$div(
          tags$h3("Created for this game:"), 
          tags$b(paste(as.character(input$game))), 
          tags$h5("Click the Update from Manual Changes 
                  button if screen does not change automatically.")
        ),
        type = "success"
      )
    }
    
    click("update")
  })
  
  #### Clicking to Add to Sheet #### 
  
  observeEvent(input$addToSheet, {
    
    showModal(modalDialog("Submitting...", footer = NULL))
    
    all_sheets <- gs4_find()
    rowsToKeep <- c()
    for(i in 1:nrow(all_sheets)) {
      perms <- c()
      perms_ln <- length(all_sheets$drive_resource[[i]]$permissions)
      for(j in 1:perms_ln) {
        perms <- c(perms, all_sheets$drive_resource[[i]]$permissions[[j]]$displayName)
      }
      if("nlltracking" %in% perms) {
        rowsToKeep <- c(rowsToKeep, i)
      }
    }
    all_sheets <- all_sheets[rowsToKeep, ]
    current_sheet_info <- all_sheets %>% 
      filter(name == as.character(input$game))
    sheet_append(game_info$Game_Id, template_df, sheet = 1)
    
    # Need to simulate a click to the Update from Manual Changes button AFTER the new rows have been added
    click("update")
    
    removeModal()
    
  })
  
  #### Basic Text Output ####
  
  output$sheetStatus <- renderUI({
    if(game_info$Game_Id == "none") {
      paste("No Sheet")
    } else {
      paste("Rows in Data: ", nrow(df()))
    } 
  })
  
}

shinyApp(ui, server)
