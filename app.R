library(shiny)
library(shinyauthr)
library(shinydashboard)
library(httr)
library(dplyr)
library(rjson)
library(tidyverse)

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("taycohort"),
  password = sapply(c("1"), sodium::password_store),
  permissions = c("admin"),
  name = c("TAYCohort")
)

ui <- fluidPage(

  # login section
  shinyauthr::loginUI(id = "login"),
  
  fluidRow(
    # Conditionally render input and button based on authentication
    uiOutput("input_container"),
    
    div(style = "height: 30px;"),
    
    # Render data table based on ID entry
    dataTableOutput("missing_table")
  )
)

server <- function(input, output, session) {
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))
  
  output$input_container <- renderUI({
    req(credentials()$user_auth)
    div(
      textInput("input_id", label = "Enter Participant ID:", value = "TAY01_CMH_00000"),
      actionButton("run_button", "Search")
    )
  })
  
  source("redcap_missing_functions.R")
  source("redcap_missing_parameters.R")
  
  # Read REDCap token
  token <- fromJSON(file = "../../credentials.json")$main_token
  
  # Read REDCap metadata (data dictionary) from REDCap API
  url <- "https://edc.camhx.ca/redcap/api/"
  formData <- list(
    "token" = token,
    content = 'metadata',
    format = 'csv',
    returnFormat = 'csv'
  )
  datadict <-
    httr::content(httr::POST(url, body = formData, encode = "form"), col_types = cols())
  
  # Clean up data dictionary
  meta_data <- datadict %>%
    filter(
      !field_type %in% c("descriptive", "calc"),
      (
        field_name == "record_id" |
          form_name %in% instruments_to_check
      ),!field_name %in% fields_to_exclude,!str_ends(field_name, "notes1|notes2|_redcap_id|_dode")
    )
  
  meta_data_v2 <- meta_data %>%
    filter(!field_label %in% c("Was this form completed?", "Was the form completed?"))
  
  # Translate REDCap branching logic into R expression
  logic <- parseBranchingLogic(meta_data$branching_logic)
  names(logic) <- meta_data$field_name
  
  logic_v2 <- parseBranchingLogic(meta_data_v2$branching_logic)
  names(logic_v2) <- meta_data_v2$field_name
  
  # Create the forms[i] list
  forms_list <-
    as.list(setNames(instruments_to_check, paste0(
      "forms[", seq_along(instruments_to_check) - 1, "]"
    )))
  
  filtered_data <- eventReactive(input$run_button, {
    req(credentials()$user_auth)
    req(input$input_id)
    
    # Create the list of data export parameters
    export_pars <- list(
      "token" = token,
      content = 'record',
      action = 'export',
      format = 'csv',
      type = 'flat',
      csvDelimiter = '',
      'records[0]' = input$input_id,
      'fields[0]' = 'record_id',
      'events[0]' = event,
      rawOrLabel = 'raw',
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'true',
      exportDataAccessGroups = 'false',
      returnFormat = 'csv'
    )
    
    # Join export_pars and forms_list
    formData <- c(export_pars, forms_list)
    
    # Read research data from REDCap API
    records_orig <-
      httr::content(
        httr::POST(url, body = formData, encode = "form"),
        col_types = cols(),
        na = ''
      )
    
    # Find missing data
    records <-
      .missingSummary_isMissingInField(records_orig, meta_data_v2, logic_v2)
    
    records <-
      .missingSummary_excludeMissingForm(records, meta_data, logic)
    
    # Output results to a dataframe
    out_df <-
      .missingSummary_makeSingleResultFrame(records, meta_data)
    
    out_df
  })
  
  output$missing_table <- renderDataTable({
    filtered_data()
  })
  
}

shinyApp(ui, server)
