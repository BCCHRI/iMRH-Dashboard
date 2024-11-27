#####################
# Libraries
#####################
library(tidyverse)
library(ggthemes)
library(scales)
library(kableExtra)
library(gtsummary)
library(lubridate)
#####################
# Helper functions
#####################
#' Creates a Summary Table of Demographic and Clinical Characteristics
#'
#' @description This function generates a summary table that combines demographic and clinical characteristics
#' from baseline data with scan data, providing a comprehensive overview of the study population.
#' @param scan_data A tibble or data frame containing scan information.
#' @param baseline_data A tibble or data frame containing baseline demographic information.
#' @return A gtsummary table object representing the demographic and clinical characteristics summary.
DemographicsTable <- function(scan_data, baseline_data){
  
  scan_summary <- 
    scan_data |> 
    group_by(record_id) |> 
    summarise(total_scans = n(), 
              total_dummy = as.integer(sum(dummy_scan, na.rm = T)), 
              total_non_dummy = total_scans - total_dummy)
    
  

  demo_table <- 
    baseline_data |> 
    left_join(scan_summary) |> 
    as_tibble() |> 
    filter(record_id %in% scan_data$record_id) |> 
    mutate(
      across(where(is.character),as.factor),
      est_dob = dmy(est_dob), 
      est_age = round(time_length(today() - est_dob, unit = "years"),1)
    ) |> 
    select(-record_id,-contains("ob")) |> 
    tbl_summary(
      by = category, 
      statistic = list(
        total_scans ~ "{sum}",
        total_dummy ~ "{sum}",
        total_non_dummy ~ "{sum}",
        est_age ~ "{median} [{p25}, {p75}]"
      ),
      type = list(
        est_age ~ "continuous",
        total_scans ~ "continuous",
        total_dummy ~ "continuous",
        total_non_dummy ~ "continuous"
      ),
      label = list(
        total_scans = "Total Scans",
        total_dummy = "Total Dummy Scans",
        total_non_dummy = "Total Non-dummy Scans",
        est_age = "Estimated Age",
        sex = "Sex",
        ethnicity = "Ethnicity",
        smoking = "Smoking Status",
        second_hand_smoke = "Second-hand Smoke Exposure",
        pancreatic_status = "Pancreatic Status",
        resp_disease = "Respiratory Disease",
        genotype = "Genotype"
      )) |> 
    modify_header(
      label = "**Variable**",
      stat_0 = "**Category**") |> 
    modify_caption("**Demographic and Clinical Characteristics Summary**") |> 
    add_n() |> 
    bold_labels() 
  
  return(demo_table)
}

#' Generates a Summary Table of Scan Data
#'
#' @description This function summarizes scan data for all records in the dataset, providing counts of total scans,
#' dummy scans, and non-dummy scans.
#' @param scan_data A tibble or data frame containing scan information.
#' @return A gtsummary table object with the summary of scan data.
OverviewTable <- function(scan_data){
  
  overview_table <- 
    scan_data |> 
    group_by(record_id) |> 
    summarise(total_scans = n(), 
              total_dummy = as.integer(sum(dummy_scan, na.rm = T)), 
              total_non_dummy = total_scans - total_dummy) |> 
    select(-record_id) |> 
    tbl_summary(
      statistic = list(
        total_scans ~ "{sum}",
        total_dummy ~ "{sum}",
        total_non_dummy ~ "{sum}"
      ),
      type = list(
        total_scans ~ "continuous",
        total_dummy ~ "continuous",
        total_non_dummy ~ "continuous"
      ),
      label = list(
        total_scans = "Total Scans",
        total_dummy = "Total Dummy Scans",
        total_non_dummy = "Total Non-Dummy Scans")) |> 
    modify_caption("**iMRH Overview**") |> 
    bold_labels() 
  
  return(overview_table)
}

#' Creates a Summary Table of Scan Data and Demographic Information for a Subset of Records
#'
#' @description This function generates a summary table for a subset of records, combining scan data with demographic
#' information, to provide a detailed overview of specific segments of the study population.
#' @param scan_data A tibble or data frame containing scan information.
#' @param baseline_data A tibble or data frame containing baseline demographic information.
#' @return A gtsummary table object with the combined summary of scan data and demographic information.
OverviewTableSubset <- function(scan_data, baseline_data){
  
  scan_summary <- 
    scan_data |> 
    group_by(record_id) |> 
    summarise(total_scans = n(), 
              total_dummy = as.integer(sum(dummy_scan, na.rm = T)), 
              total_non_dummy = total_scans - total_dummy)
  
  demo_table <- 
    scan_summary |> 
    left_join(baseline_data) |> 
    as_tibble() |> 
    mutate(
      across(where(is.character),as.factor),
      est_dob = dmy(est_dob), 
      est_age = round(time_length(today() - est_dob, unit = "years"),1)
    ) |> 
    select(-record_id,-contains("ob")) |> 
    tbl_summary(
      statistic = list(
        total_scans ~ "{sum}",
        total_dummy ~ "{sum}",
        total_non_dummy ~ "{sum}",
        est_age ~ "{median} [{p25}, {p75}]"
      ),
      type = list(
        est_age ~ "continuous",
        total_scans ~ "continuous",
        total_dummy ~ "continuous",
        total_non_dummy ~ "continuous"
      ),
      label = list(
        total_scans = "Total Scans",
        total_dummy = "Total Dummy Scans",
        total_non_dummy = "Total Non-dummy Scans",
        est_age = "Estimated Age",
        sex = "Sex",
        ethnicity = "Ethnicity",
        smoking = "Smoking Status",
        second_hand_smoke = "Second-hand Smoke Exposure",
        pancreatic_status = "Pancreatic Status",
        resp_disease = "Respiratory Disease",
        genotype = "Genotype"
      )) |> 
    modify_header(
      label = "**Variable**",
      stat_0 = "**Category**") |> 
    modify_caption("**Demographic and Clinical Characteristics Summary**") |> 
    add_n() |> 
    bold_labels() 
  
  return(demo_table)
}

#' Generates a Grouped Bar Chart with Counts of Scans
#'
#' @description This function creates a grouped bar chart displaying the counts of total scans, dummy scans, and
#' non-dummy scans, providing a visual summary of the scan data.
#' @param scan_data A tibble or data frame containing scan information.
#' @return A ggplot object representing the grouped bar chart of scan counts.
OverviewGroupedBarWithCounts <- function(scan_data){
  # Preparing the data
  figure_data <- 
    scan_data |> 
    summarise(total_scans = n(), 
              total_dummy = as.integer(sum(dummy_scan, na.rm = TRUE)), 
              total_non_dummy = total_scans - total_dummy) |> 
    pivot_longer(cols = c(total_dummy, total_non_dummy),
                 names_to = "category",
                 values_to = "count") |>  
    filter(category == "total_non_dummy") |> 
    mutate(category = factor(category, levels = c("total_non_dummy"),
                             labels = c("Total Scans")))
  
  total_row <- data.frame(category = "Total Scans", count = sum(figure_data$count))
  figure_data <- bind_rows(figure_data, total_row)
  
  n_study <- n_distinct(scan_data$record_id)
  
  p <- 
    figure_data |> 
    ggplot(aes(x = category, y = count, fill = category)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_text(aes(label = count), hjust = 1.5, position = position_dodge(width = 0.7)) + 
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = paste("Overview of Scans: N =", n_study),
         y = "Count",
         x = "",
         fill = "Category") +
    theme_wsj() +
    theme(axis.text.x = element_text(angle = 45,hjust = 1),
          legend.position = "none") +
    coord_flip()
  
  return(p)
}