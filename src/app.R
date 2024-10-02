library(shiny)
library(bslib)
library(DBI)
library(dplyr)
library(gt)
library(readr)
library(tidyr)
library(shinyjs)  
library(plotly)   

source('utils.R')
source("init_sql_db.R")

# SQLite database path
DB_PATH <- "../data/iMRH_db.sqlite"

# Function to create the database connection
create_db_connection <- function() {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), DB_PATH)
    return(con)
  }, error = function(e) {
    stop("Error connecting to the SQLite database: ", e$message)
  })
}

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly", version = 5),  
  titlePanel("iMRH Dashboard"),
  
  # Toggle button for the navbar
  actionButton("toggle", " Menu"),
  
  # Collapsible navbar
  conditionalPanel(
    condition = "input.toggle > 0",
    navbarPage("", id = "all_tabs",
      tabPanel("Database Summary",
               fluidRow(
                 h2("Summary Table"),
                 htmlOutput("demographics_table")
               )
      ),
      tabPanel("Custom Overview",
               sidebarLayout(
                 sidebarPanel(
                   h3("Data Filter"),
                   selectInput("subsetVarOverview", "Select Subset:", 
                               choices = c("All","Healthy Control", "Respiratory Disease"),
                               selected = "All"),
                   uiOutput("resp_disease_dropdown_overview"),
                   uiOutput("sequence_type_dropdown_overview"),
                   uiOutput("study_comp_dropdown_overview"),
                   downloadButton("downloadDataOverview", "Download Data"),
                   width = 3
                 ),
                 mainPanel(
                   plotlyOutput("overview_figure"),
                   width = 9
                 )
               )
      ),
      tabPanel("Data Explorer",
               fluidRow(
                 column(12,
                        h2("Data Explorer"),
                        DT::dataTableOutput("data_explorer_table")
                 )
               )
      )
    )
  )
)

server <- function(input, output, session) {
  ###################################
  # Database connection
  con <- create_db_connection()
  
  observeEvent(input$toggle, {
    if (input$toggle %% 2 == 1) {
      shinyjs::show("all_tabs")  
    } else {
      shinyjs::hide("all_tabs")  
    }
  })
  
  ###################################
  # Data refresh 
  AutoRefreshData <- reactivePoll(3600000, session,
                                  checkFunc = function() {
                                    # Placeholder for actual check logic
                                    return(Sys.time())
                                  },
                                  valueFunc = function() {
                                    # Re-read data from SQLite
                                    scan_data <- dbReadTable(con, "scan_data")
                                    baseline_data <- dbReadTable(con, "baseline_data")
                                    list(scan_data = scan_data, baseline_data = baseline_data)
                                  }
  )
  
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
    if (input$subsetVarOverview == "Respiratory Disease" && !is.null(input$diseaseSubsets) && length(input$diseaseSubsets) > 0) {
      disease_ids <- 
        baseline_data %>% 
        filter(resp_disease %in% input$diseaseSubsets)
      
      subset_data <- 
        subset_data %>% 
        filter(record_id %in% disease_ids$record_id)
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
    if (input$subsetVarOverview == "Respiratory Disease") {
      selectInput("diseaseSubsets", "Select Disease(s):",
                  choices = c("CF", "CF-Asthma", "Chronic obstructive pulmonary disease",
                              "Congenital lung abnormality", "Interstitial lung disease", 
                              "None", "Other/mixed", "PCD"), 
                  multiple = TRUE)
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
    ggplotly(p)  # Convert ggplot to plotly for interactivity
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

# Run the app
shinyApp(ui = ui, server = server)
