library(shiny)
library(shinyauthr)
library(shinydashboard)
library(httr)
library(dplyr)
library(rjson)
library(tidyverse)

# Load App Credentials
shiny_username <- fromJSON(file = "../../config/credentials.json")$shiny_username
shiny_password <- fromJSON(file = "../../config/credentials.json")$shiny_password

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c(shiny_username),
  password = sapply(c(shiny_password), sodium::password_store),
  permissions = c("admin"),
  name = c("TAYCohort")
)

ui <- fluidPage(
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
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
      selectInput("event", label = "Select Event:", 
                  choices = c("01_selfreport_base_arm_1", 
                              "01_interview_basel_arm_1",
                              "02_selfreport_6mon_arm_1",
                              "02_interview_6mont_arm_1",
                              "03_selfreport_12mo_arm_1",
                              "03_interview_12mon_arm_1",
                              "04_selfreport_18mo_arm_1", 
                              "04_interview_18mon_arm_1",
                              "05_selfreport_24mo_arm_1",
                              "05_interview_24mon_arm_1"),
                  selected = "01_selfreport_base_arm_1"),
      actionButton("run_button", "Search")
    )
  })
  
  source("redcap_missing_functions.R")
  source("redcap_missing_parameters.R")
  
  # Read REDCap token
  main_token <- fromJSON(file = "../../config/credentials.json")$main_token
  cg_token <- fromJSON(file = "../../config/credentials.json")$cg_token
  
  # Read REDCap metadata (data dictionary) from REDCap API
  url <- "https://edc.camhx.ca/redcap/api/"
  formData <- list(
    "token" = main_token,
    content = 'metadata',
    format = 'csv',
    returnFormat = 'csv'
  )
  
  datadict <-
    httr::content(httr::POST(url, body = formData, encode = "form"), col_types = cols())
  
  formData <- list(
    "token" = cg_token,
    content = 'metadata',
    format = 'csv',
    returnFormat = 'csv'
  )
  
  cg_datadict <-
    httr::content(httr::POST(url, body = formData, encode = "form"), col_types = cols())
  
  filtered_data <- eventReactive(input$run_button, {
    req(credentials()$user_auth)
    req(input$input_id)
    req(input$event)
    
    event <- input$event
    instruments <- getInstrumentInfo(event)
    instruments_to_check <- instruments$instruments_to_check
    fields_to_exclude <- instruments$fields_to_exclude
    
    # Clean up data dictionary
    exclude_pattern = "notes1|notes2|_redcap_id|_redcap_id_fu|_dode|_dode_fu"
    meta_data <- datadict %>%
      filter(
        !field_type %in% c("descriptive", "calc"),
        (field_name == "record_id" | form_name %in% instruments_to_check),
        !field_name %in% fields_to_exclude, 
        !str_ends(field_name, exclude_pattern)
      ) %>%
      mutate(form_name = case_when(
        form_name == 'survey_queue' ~ 'education_attainment_questionnaire_yearly_followup',
        TRUE ~ form_name
      ))
    
    meta_data$branching_logic <- ifelse(meta_data$field_name == 'eaq_q3_tmp', NA, meta_data$branching_logic)
    
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
    
    # Create the list of data export parameters
    export_pars <- list(
      "token" = main_token,
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
      .missingSummary_makeSingleResultFrame(records, meta_data) %>%
      mutate(participant = 'Youth')
    
    if (event %in% c("01_selfreport_base_arm_1", 
                     "02_selfreport_6mon_arm_1",
                     "03_selfreport_12mo_arm_1",
                     "05_selfreport_24mo_arm_1"
    )) {
      
      instruments <- getCaregiverInstrumentInfo(event)
      instruments_to_check <- instruments$instruments_to_check
      fields_to_exclude <- instruments$fields_to_exclude
      
      # Clean up data dictionary
      exclude_pattern = "notes1|notes2|_redcap_id|_redcap_id_fu|_dode|_dode_fu"
      meta_data <- cg_datadict %>%
        filter(
          !field_type %in% c("descriptive", "calc"),
          (field_name == "record_id" | form_name %in% instruments_to_check),
          !field_name %in% fields_to_exclude, 
          !str_ends(field_name, exclude_pattern)
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
      
      # Create the list of data export parameters
      export_pars <- list(
        "token" = cg_token,
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
      out_df_cg <-
        .missingSummary_makeSingleResultFrame(records, meta_data) %>%
        mutate(participant = 'Caregiver')
      
      out_df <- rbind(out_df, out_df_cg)
      
    }
    
    out_df
  })
  
  output$missing_table <- renderDataTable({
    filtered_data()
  })
  
}

shinyApp(ui, server)
