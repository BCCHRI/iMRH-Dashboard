#####################
# Libraries
#####################
library(tidyverse)
library(REDCapR)
library(DBI)
library(RSQLite)
library(glue)
#####################
# Functions
#####################
#' Read data from REDCap
#'
#' @description Fetches data from a specified REDCap project using API credentials.
#' @return A tibble containing the project data.
ReadREDCapData <- function(token_path, uri){
  token <- read_file(token_path)
  raw_data <- redcap_read_oneshot(redcap_uri = uri,token = token)
  return(raw_data$data)
}

#' Clean and filter baseline data
#'
#' @description Cleans and filters baseline data based on specified conditions and transformations.
#' @param data Raw data tibble to be cleaned and filtered.
#' @param data_dict_path Path to the data dictionary for variable and form names.
#' @return A tibble of cleaned and filtered baseline data.
ProcessBaselineData <- function(data, data_dict_path){
  # get data dict 
  baseline_vars <- 
    read_csv(data_dict_path) |> 
    janitor::clean_names() |> 
    filter(form_name %in% c("demographics","medical_history")) 

  # define maps
  sex_map <- c("1" = "Male", "2" = "Female", "0" = "Not collected")
  ethnicity_map <- c(
    "1" = "Caucasian",
    "2" = "African American",
    "3" = "North East Asian",
    "4" = "South East Asian",
    "5" = "Other/Mixed",
    "6" = "Not collected"
  )
  smoking_map <- c(
    "1" = "Not collected",
    "2" = "Current smoker",
    "3" = "Ex-smoker",
    "4" = "Never-smoker",
    "0" = "Not collected"
  )
  second_hand_smoke_map <- c("0" = "No", "1" = "Yes", "2" = "Not collected")
  category_map <- c("1" = "Healthy Control", "2" = "Respiratory Disease")
  pancreatic_status_map <- c("1" = "Sufficient", "2" = "Insufficient")

  # (If you want to recode resp_disease later, fill this in similarly)
  resp_disease_map <- c(
    "0"   = "None",
    "99"  = "Other/mixed",
    "1"   = "Congenital lung abnormality",
    "2"   = "Interstitial lung disease",
    "3"   = "Interstitial lung disease",
    "4"   = "Chronic obstructive pulmonary disease",
    "5"   = "Chronic obstructive pulmonary disease",
    "6"   = "PCD",
    "7"   = "CF",
    "8"   = "CF-Asthma"
  )

  # Pack your maps into a named list for easier iteration
  mapping_list <- list(
    sex                 = sex_map,
    ethnicity           = ethnicity_map,
    smoking             = smoking_map,
    second_hand_smoke   = second_hand_smoke_map,
    category            = category_map,
    pancreatic_status   = pancreatic_status_map
  )

  # 3) Build the baseline_data pipeline
  baseline_data <- data |> 
    # keep only the baseline arm
    filter(redcap_event_name == "baseline_data_arm_1") |> 
    # pull in only your variables of interest
    select(record_id, contains(baseline_vars$variable_field_name)) |> 
    # 3a) Turn all the _dx / _report flags into Yes/No
    mutate(across(
      c(dysplasia_dx, ild_biopsy, ild_biopsy_report,
        pcd_emicroscopy, emicroscopy_report,
        pcd_hsvm, hsvm_report,
        pcd_nasal_no, nasal_no_report,
        p_cgvhd),
      ~ case_when(
          .x == 1 ~ "Yes",
          .x == 0 ~ "No",
          TRUE     ~ NA_character_
        )
    )) |> 
    # 3b) keep only the columns we actually want for the final dataset
    select(
      record_id, mob, yob,
      names(mapping_list),
      cf_allele_1, cf_allele_2,
      pancreatic_status
    ) |> 
    # 3c) apply all maps in one go
    mutate(
      across(
        names(mapping_list),
        ~ factor(
            as.character(.x),
            levels = names(mapping_list[[cur_column()]]),
            labels = mapping_list[[cur_column()]]
          )
      ),
      # 3f) clean up the CF‐allele fields into genotype categories
      genotype1_clean = if_else(
        str_detect(cf_allele_1, regex("f508", ignore_case = TRUE)),
        "F508del", cf_allele_1
      ),
      genotype2_clean = if_else(
        str_detect(cf_allele_2, regex("f508", ignore_case = TRUE)),
        "F508del", cf_allele_2
      ),
      genotype = case_when(
        genotype1_clean == "F508del" & genotype2_clean == "F508del" ~ "Two copies F508del",
        xor(genotype1_clean == "F508del", genotype2_clean == "F508del") ~ "One copy F508del",
        genotype1_clean != "F508del" & genotype2_clean != "F508del" ~ "No copies F508del",
        is.na(genotype1_clean) & is.na(genotype2_clean)         ~ "Unknown",
        TRUE                                                    ~ NA_character_
      ),
      
      # 3g) estimate a “middle‐of‐month” DOB string
      est_dob = case_when(
        !is.na(mob) & !is.na(yob) ~ paste0("15-", mob, "-", yob),
        is.na(mob)  & !is.na(yob) ~ paste0("15-06-",   yob),
        TRUE                     ~ NA_character_
      ),
      
      # ensure record_id stays character
      record_id = as.character(record_id)
    ) |> 
    # 4) drop intermediate helper columns
    select(
      -starts_with("cf_allele"),
      -ends_with("_clean")
    )

    # 1) your code → label map
  resp_disease_map <- c(
    "1"  = "Cystic fibrosis",
    "2"  = "Asthma",
    "3"  = "Primary Ciliary Dyskinesia (PCD)",
    "4"  = "Ex-prematurity (< 35 weeks gestation)",
    "5"  = "Bone marrow transplant (with or without pulmonary cGvHD)",
    "6"  = "Bronchiolitis obliterans (non-BMT-associated)",
    "7"  = "Chronic obstructive pulmonary disease",
    "8"  = "Interstitial lung disease",
    "9"  = "Congenital lung abnormality",
    "99" = "Other"
  )

  # 2) the flag columns
  resp_cols <- paste0("resp_disease_cat___", c(1:9, 99))

  # 3) build a little table of record_id → combined labels
  baseline_resp <- data |> 
    select(record_id, all_of(resp_cols)) %>%
    pivot_longer(
      cols      = all_of(resp_cols),
      names_to  = "flag",
      values_to = "v"
    ) |> 
    filter(v == 1) |> 
    mutate(
      code  = str_remove(flag, "resp_disease_cat___"),
      label = resp_disease_map[code],
      record_id = as.character(record_id)
    ) |> 
    group_by(record_id) |> 
    summarise(
      resp_disease  = str_c(label, collapse = "; "),
      .groups = "drop"
    )

  baseline_data_combined <- left_join(baseline_data,baseline_resp,by="record_id") |> 
    mutate(resp_disease  = replace_na(resp_disease, "None"))
  return(baseline_data_combined)
}

#' Process and export scan data
#'
#' @description Extracts and processes MRI scan data from REDCap.
#' @param data Raw data tibble to be cleaned and filtered.
#' @param data_dict_path Path to the data dictionary for variable and form names.
#' @return A tibble of processed scan data.
ProcessScanData <- function(data, data_dict_path){
  
  # get data dict 
  scan_vars <- 
    read_csv(data_dict_path)  |> 
    janitor::clean_names() |> 
    filter(form_name %in% c("scans")) |> 
    filter(!str_detect(variable_field_name,"notes")) |> 
    filter(is.na(section_header)) |> 
    select(variable_field_name,field_type)
  
  # get study_id vars
  study_id_vars <- 
    tibble(variable_field_name = c("comp1","comp2","comp3","comp4"),
           field_type = rep("dropdown",4))
  
  
  # lets clean this thing up 
  sequence_label_mapping <- c(
    "1" = "Localizer", 
    "2" = "Cal - FID CSI (phantom)", 
    "3" = "Cal - FIDALL (human)", 
    "4" = "Xe - 2D SPGR (vent; JW)", 
    "5" = "Xe - 3D SPGR (vent; JW)", 
    "6" = "Xe - DWCS (diffusion; JW)", 
    "7" = "Xe - Dissolved Phase (4pt; JW)", 
    "8" = "Structure - PROTON mask", 
    "9" = "PREFUL - FIESTA", 
    "10" = "Structure - GE ZTE", 
    "11" = "PREFUL - SPGR v1", 
    "12" = "PREFUL - SPGR v2", 
    "13" = "Xe - 3D SPGR (vent; AO)", 
    "14" = "32 Channel Torso Coil Calibration", 
    "15" = "3D Ax T1 FSPGR")
  
  mri_label_mapping <- c(
    "1" = "FB", 
    "2" = "Full inhalation", 
    "3" = "Full exhalation", 
    "4" = "FRC + bag", 
    "5" = "RV + bag"
  )
  
  yes_no_mapping <- c(
    "1" = "Yes",
    "0" = "No"
  )
  
  study_id_mapping <- c(
    "1" = "varMRI",
    "2" = "TriMRI",
    "3" = "imPCR",
    "4" = "CANUCK",
    "5" = "CDH-MRI",
    "6" = "Cross-MRI",
    "7" = "Var-MRI PCD",
    "8" = "IEI-MRI"
  )
  # now select fields that we want from the raw data
  scan_data <- 
    data |> 
    filter(redcap_event_name == "visit_data_arm_1") |> 
    select(record_id,redcap_repeat_instance,visit_date,
           contains(scan_vars$variable_field_name),
           contains(study_id_vars$variable_field_name)) |> 
    mutate_all(as.character) |> 
    pivot_longer(cols = c(-record_id,-redcap_repeat_instance,-visit_date,-contains("comp")),
                 values_to = "scan_parameter_value",
                 names_to = "scan_parameter") |> 
    filter(!is.na(scan_parameter_value)) |> 
    mutate(
      scan_number = str_extract(scan_parameter, "\\d+"), 
      scan_parameter_name = str_remove(str_extract(scan_parameter, "[^\\d]+"),"_$" )) |> 
    select(record_id,visit_number = redcap_repeat_instance,visit_date,contains("comp"),
           scan_number,scan_parameter_name,scan_parameter_value) |> 
    pivot_wider(
      names_from = scan_parameter_name,
      values_from = scan_parameter_value) |> 
    mutate(sequence_name = factor(sequence_name, 
                                  levels = names(sequence_label_mapping), 
                                  labels = sequence_label_mapping),
           mri_vol = factor(mri_vol,
                            levels = names(mri_label_mapping),
                            labels = mri_label_mapping),
           xe = factor(xe,
                       levels = names(yes_no_mapping),
                       labels = yes_no_mapping),
           bag = factor(bag,
                        levels = names(yes_no_mapping),
                        labels = yes_no_mapping),
           vxe = as.numeric(vxe),
           vtotal = as.numeric(vtotal),
           xe_pol = as.numeric(xe_pol),
           acc = factor(acc,
                        levels = names(yes_no_mapping),
                        labels = yes_no_mapping),
           ae = factor(ae,
                       levels = names(yes_no_mapping),
                       labels = yes_no_mapping),
           comp1 = factor(comp1,
                          levels = names(study_id_mapping),
                          labels = study_id_mapping),
           comp2 = factor(comp2,
                          levels = names(study_id_mapping),
                          labels = study_id_mapping),
           comp3 = factor(comp3,
                          levels = names(study_id_mapping),
                          labels = study_id_mapping),
           comp4 = factor(comp4,
                          levels = names(study_id_mapping),
                          labels = study_id_mapping),
           tg = as.numeric(tg),
           slices = as.numeric(slices),
           phase_fov = as.numeric(phase_fov),
           visit_number = (as.numeric(visit_number)),
           scan_number = (as.numeric(scan_number))) |> 
    group_by(record_id,visit_number) |> 
    mutate(dummy_scan = 
             case_when(
               str_detect(sequence_name,"Xe") & (xe == "No" | is.na(xe)) ~ 1,
               TRUE ~ 0)) |> 
    ungroup() |>
    filter(dummy_scan == 0)
  
  return(scan_data)
}

#' Aggregate sequence data
#'
#' Aggregates sequence data by record ID and sequence name, counting occurrences.
#' @param scan_data The scan data tibble to aggregate.
#' @return A tibble of aggregated sequence data.
AggSeqData <- function(scan_data){
  agg_seq_data <- 
    scan_data |> 
    select(record_id, visit_number, scan_number, 
           sequence_name,dummy_scan) |> 
    arrange(record_id) |> 
    mutate(
      seq = factor(case_when(
        dummy_scan == 1 ~ glue("{sequence_name} [dummy scan]"), 
        TRUE ~ as.character(sequence_name)
      ))) |> 
    group_by(record_id,seq) |> 
    summarise(count = n())
  return(agg_seq_data)
}

#' Export processed data to an SQLite database
#'
#' @description Creates an SQLite database (if it doesn't exist) and writes the processed data to it.
#' @param data Processed data tibble to be written to the database.
#' @param db_path Path where the SQLite database is stored.
#' @param table_name Name of the table within the database to write data into.
WriteToSQLite <- function(data, db_path, table_name){
  con <- dbConnect(RSQLite::SQLite(), db_path)
  dbWriteTable(con, table_name, data, overwrite = TRUE, row.names = FALSE)
  dbDisconnect(con)
}


#' Process Study Components
#'
#' @description Cleans and processes study component data from REDCap, mapping numeric component values 
#' to descriptive names and reshaping the data.
#'
#' @param data A tibble containing raw REDCap data, including study components and component IDs.
#' @return A tibble with `record_id`, `visit_number`, `component_study_number`, 
#' `component_study_name`, and `component_study_id`.
#'
#' @details
#' This function:
#' - Converts numeric component columns to labeled factors.
#' - Pivots the component columns from wide to long format.
#' - Joins component names with their respective study IDs.
#'
#' @example
#' \dontrun{
#'   raw_data <- dbReadTable(db, "raw_data")
#'   processed_data <- ProcessStudyComponents(raw_data)
#' }
#'
#' @export
ProcessStudyComponents <- function(data){
  
  study_names <- data |> 
    select(
      record_id,visit_number = redcap_repeat_instance,
      comp1, comp2, comp3, comp4
    ) |> 
    mutate(
      across(c(comp1,comp2,comp3,comp4),
             ~ factor(.x, 
                      levels = 1:8,
                      labels = c( "varMRI","TriMRI","imPCR","CANUCK","CHD-MRI","Cross-MRI","Var-MRI-PCD","IEI-MRI")
             ))) |> 
    pivot_longer(cols = contains("comp"), names_to = "component_study_number",values_to = "component_study_name") |> 
    drop_na(component_study_name) |> 
    mutate(component_study_number = factor(component_study_number,labels = c("comp_number1","comp_number2","comp_number3")))
  
  study_names_combined <- data |> 
    select(
      record_id, visit_number = redcap_repeat_instance,
      comp_number1, comp_number2, comp_number3, comp_number4) |> 
    mutate(across(contains("comp"), as.character)) |> 
    pivot_longer(cols = contains("comp"), names_to = "component_study_number", values_to = "component_study_id") |> 
    drop_na(component_study_id) |> 
    right_join(study_names, by = join_by(record_id, visit_number, component_study_number)) |> 
    distinct(record_id,component_study_name,component_study_id)
  
  return(study_names_combined)
}

