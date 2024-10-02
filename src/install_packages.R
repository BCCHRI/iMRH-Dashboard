# List of packages to install
packages <- c(
  'shiny', 
  'bslib', 
  'DBI', 
  'dplyr', 
  'gt', 
  'readr', 
  'tidyr', 
  'shinyjs', 
  'plotly', 
  'DT', 
  'RSQLite',
  'tidyverse',
  'REDCapR',
  'glue'
)

# Install packages
install.packages(packages, repos = 'https://cloud.r-project.org/')