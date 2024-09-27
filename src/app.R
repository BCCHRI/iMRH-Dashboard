library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "iMRH Dashboard",
  sidebar = "Data Filter",
  "Main content area "
)

shinyApp(ui, function(input, output) {})
