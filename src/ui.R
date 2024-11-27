library(shiny)
library(bslib)
library(shinyjs)
library(plotly)
library(DT)
library(shinymanager)

ui <- secure_app(
  fluidPage(
    useShinyjs(),
    theme = bs_theme(bootswatch = "flatly", version = 5),  
    titlePanel("iMRH Dashboard"),
    
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