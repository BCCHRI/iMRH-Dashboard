library(shiny)
library(bslib)
library(DBI)
library(dplyr)
library(gt)
library(readr)
library(tidyr)
library(shinyjs)  
library(plotly)   
library(yaml)
library(stringr)
source('utils/utils.R')
source("utils/init_sql_db.R")

######################################################################
init_sql_db()
###################################

server <- function(input, output, session) {
  ###################################
  # Database connection
  con <- create_db_connection()
  
  ###################################
  # Data refresh 
  autoInvalidate <- reactiveTimer(3600000, session)
  AutoRefreshData <- reactive({
    autoInvalidate()         # invalidate every 1h 
    input$refreshData        # invalidate on click

    scan_data     <- dbReadTable(con, "scan_data")
    baseline_data <- dbReadTable(con, "baseline_data")

    list(scan_data = scan_data,
         baseline_data = baseline_data)
  })
  
  ###################################
  # Reactive expressions 
  .ReactiveData <- reactive({
    data <- AutoRefreshData()
    scan_data <- data$scan_data
    baseline_data <- data$baseline_data
    
    if (input$subsetVarOverview == "All") {
      subset_data <- scan_data
    } else {
      subset_ids <- 
        baseline_data %>% 
        filter(category == input$subsetVarOverview)
      
      subset_data <- 
        scan_data %>% 
        filter(record_id %in% subset_ids$record_id)
    }
    if (input$subsetVarOverview == "Respiratory Disease" &&
        !is.null(input$diseaseSubsets) &&
        length(input$diseaseSubsets) > 0) {
      
      # build a regex to match any selected label as a whole word
      pat <- paste0("\\b(", paste(input$diseaseSubsets, collapse = "|"), ")\\b")
      
      # find all IDs whose multi‑label string contains at least one selection
      disease_ids <- baseline_data %>%
        filter(str_detect(resp_disease, pat)) %>%
        pull(record_id)
      
      # keep only those records
      subset_data <- subset_data %>%
        filter(record_id %in% disease_ids)
    }
    
    subset_data
  })
  
  .FinalReactiveData <- reactive({
    data_subset <- .ReactiveData()
    
    final_subset <- data_subset
    
    if (!is.null(input$sequenceType) && length(input$sequenceType) > 0) {
      final_subset <- 
        final_subset %>% 
        filter(sequence_name %in% input$sequenceType)
    }
    
    if (!is.null(input$compType) && length(input$compType) > 0) {
      final_subset <- 
        final_subset %>% 
        mutate(drop_row = 
                 case_when(
                   comp1 %in% input$compType | 
                     comp2 %in% input$compType |
                     comp3 %in% input$compType |
                     comp4 %in% input$compType ~ 0,
                   TRUE ~ 1
                 )) %>% 
        filter(drop_row == 0) 
    }
    
    final_subset
  })
  
  .DownloadData <-  reactive({
    data <- AutoRefreshData()
    baseline_data <- data$baseline_data
    data_download <- .FinalReactiveData()
    left_join(data_download, baseline_data, by = "record_id")
  })
  
  ###################################
  # Dynamic Dropdowns 
  output$resp_disease_dropdown_overview <- renderUI({
    req(input$subsetVarOverview)
    if (input$subsetVarOverview == "Respiratory Disease") {
      selectInput(
        inputId  = "diseaseSubsets",
        label    = "Select Disease(s):",
        choices  = c(
          "Cystic fibrosis",
          "Asthma",
          "Primary Ciliary Dyskinesia (PCD)",
          "Ex-prematurity (< 35 weeks gestation)",
          "Bone marrow transplant (with or without pulmonary cGvHD)",
          "Bronchiolitis obliterans (non-BMT-associated)",
          "Chronic obstructive pulmonary disease",
          "Interstitial lung disease",
          "Congenital lung abnormality",
          "Other"
        ),
        multiple = TRUE
      )
    }
  })
  
  output$sequence_type_dropdown_overview <- renderUI({
    data_subset <- .ReactiveData()
    unique_sequence_types <- unique(data_subset$sequence_name)
    selectInput("sequenceType", "Select Sequence Type(s):",
                choices = unique_sequence_types, 
                multiple = TRUE)
  })
  
  output$study_comp_dropdown_overview <- renderUI({
    data_subset <- .ReactiveData()
    study_id_types <- 
      data_subset %>% 
      pivot_longer(cols = contains("comp"), names_to = "study_comp", values_to = "value") %>% 
      distinct(value) %>% 
      drop_na()
    selectInput("compType", "Select Component Study Type(s):",
                choices = study_id_types$value,
                multiple = TRUE)
  })
  
  ###################################
  # Dynamic Download 
  output$downloadDataOverview <- downloadHandler(
    filename = function() {
      paste("data-subset-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_download <- .DownloadData()
      write_csv(data_to_download, file)
    }
  )
  
  ###################################
  # Tables and figures
  output$demographics_table <- renderUI({
    data <- AutoRefreshData()
    as_gt(DemographicsTable(data$scan_data, data$baseline_data))
  })
  
  output$overview_figure <- renderPlotly({
    p <- OverviewGroupedBarWithCounts(.FinalReactiveData())
    ggplotly(p) 
  })
  
  # New output for data explorer
  output$data_explorer_table <- DT::renderDataTable({
    DT::datatable(.DownloadData(), 
                  options = list(pageLength = 10, 
                                 scrollX = TRUE,
                                 scrollY = "500px"),
                  filter = 'top')
  })
  
  # Observe changes in subsetVarOverview to reset other inputs
  observeEvent(input$subsetVarOverview, {
    if(input$subsetVarOverview != "Respiratory Disease") {
      updateSelectInput(session, "diseaseSubsets", selected = character(0))
    }
    updateSelectInput(session, "sequenceType", selected = character(0))
    updateSelectInput(session, "compType", selected = character(0))
  })
}