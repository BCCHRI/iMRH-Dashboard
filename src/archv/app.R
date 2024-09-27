library(shiny)
source('utils.R')
source("init_sql_db.R")


# Define UI
ui <- fluidPage(
  titlePanel("iMRH Dashboard"),
  navbarPage("Navigation", id = "all_tabs",
             position = "fixed-bottom",
             tabPanel("Database Summary",
                      fluidRow(
                        h2("Summary Table"),
                        htmlOutput("demographics_table")
                        )
                      ),
             tabPanel("Custom Overview",
                      fluidRow(
                        column(2,
                               h2("Data filter"),
                               selectInput("subsetVarOverview", "Select Subset:", 
                                           choices = c("All","Healthy Control", "Respiratory Disease"),
                                           selected = "All"),
                               uiOutput("resp_disease_dropdown_overview"),
                               uiOutput("sequence_type_dropdown_overview"),
                               uiOutput("study_comp_dropdown_overview"),
                               downloadButton("downloadDataOverview", "Download Data")
                        ),
                        column(10,
                          div(style = "margin-top: 100px;", 
                            plotOutput("overview_figure")
                            )
                          )
                        )
                      ),
             )
)

server <- function(input, output, session) {
  ###################################
  # Data refresh 
  # Set up a reactive expression that automatically refreshes
  AutoRefreshData <- reactive({
    print("----------- FETCHING LATEST DATA -----------")
    invalidateLater(3600 * 1000, session) 
    raw_data <- ReadREDCapData(TOKEN_PATH, URI)
    baseline_data <- ProcessBaselineData(raw_data, DATA_DICT_PATH)
    scan_data <- ProcessScanData(raw_data, DATA_DICT_PATH)
    agg_seq_data <- AggSeqData(scan_data)
    print("----------- WRITING DATA TO SQL -----------")
    WriteToSQLite(raw_data, DB_PATH, "raw_data")
    WriteToSQLite(baseline_data, DB_PATH, "baseline_data")
    WriteToSQLite(scan_data, DB_PATH, "scan_data")
    WriteToSQLite(agg_seq_data, DB_PATH, "agg_seq_data")
    print("----------- FINISHED FETCHING LATEST DATA -----------")
  })
  
  observe({
    AutoRefreshData()
  })
  ###################################
  # Database connection
  con <- dbConnect(SQLite(),"iMRH_db.sqlite")
  scan_data <- dbReadTable(con, "scan_data")
  baseline_data <- dbReadTable(con, "baseline_data")
  ###################################
  # Reactive expressions 
  .ReactiveData <- reactive({
    if (input$subsetVarOverview == "All") {
      subset_data <- scan_data
    } else {
      subset_ids <- 
        baseline_data |> 
        filter(category == input$subsetVarOverview)
      
      subset_data <- 
        scan_data |> 
        filter(record_id %in% subset_ids$record_id)
    }
    if (input$subsetVarOverview == "Respiratory Disease" && !is.null(input$diseaseSubsets) && length(input$diseaseSubsets) > 0) {
      disease_ids <- 
        baseline_data |> 
        filter(resp_disease %in% input$diseaseSubsets)
      
      subset_data <- 
        subset_data |> 
        filter(record_id %in% disease_ids$record_id)
    }
    
    subset_data
  })
  
  .FinalReactiveData <- reactive({
    data_subset <- .ReactiveData()
    
    final_subset <- data_subset
    
    if (!is.null(input$sequenceType) && length(input$sequenceType) > 0) {
      final_subset <- 
        final_subset |> 
        filter(sequence_name %in% input$sequenceType)
    }
    
    if (!is.null(input$compType) && length(input$compType) > 0) {
      final_subset <- 
        final_subset |> 
        mutate(drop_row = 
                 case_when(
                   comp1 %in% input$compType | 
                     comp2 %in% input$compType |
                     comp3 %in% input$compType |
                     comp4 %in% input$compType ~ 0,
                   TRUE ~ 1
                 )) |> 
        filter(drop_row == 0) 
    }
    
    final_subset
  })
  
  .DownloadData <-  reactive({
    data_download <- .FinalReactiveData()
    left_join(data_download,baseline_data,
              by = "record_id")
  })


  ###################################
  # Dynamic Dropdowns 
  output$resp_disease_dropdown_overview <- renderUI({
    if (input$subsetVarOverview == "Respiratory Disease") {
      selectInput("diseaseSubsets", "Select Disease(s):",
                  choices = c("CF", "CF-Asthma", "Chronic obstructive pulmonary disease",
                              "Congenital lung abnormality", "Interstitial lung disease", 
                              "None", "Other/mixed", "PCD"), 
                  multiple = T)
    }
  })

  output$sequence_type_dropdown_overview <- renderUI({
    data_subset <- .ReactiveData()
    unique_sequence_types <- unique(data_subset$sequence_name)
    selectInput("sequenceType", "Select Sequence Type(s):",
                choices = unique_sequence_types, 
                multiple = T)
  })
  
  output$study_comp_dropdown_overview <- renderUI({
    data_subset <- .ReactiveData()
    study_id_types <- 
      data_subset |> 
      pivot_longer(cols = contains("comp"),names_to = "study_comp",values_to = "value") |> 
      distinct(value) |> 
      drop_na()
    selectInput("compType", "Select Component Study Type(s):",
                choices = study_id_types, 
                multiple = T)
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
    as_gt(DemographicsTable(scan_data, baseline_data))
  })
  
  output$overview_figure <- renderPlot({
    OverviewGroupedBarWithCounts(.FinalReactiveData())
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
