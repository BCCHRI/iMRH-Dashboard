#####################
# Libraries
#####################
pacman::p_load(tidyverse,REDCapR,RSQLite,DBI,glue)
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
    readxl::read_excel(data_dict_path) |> 
    janitor::clean_names() |> 
    filter(form_name %in% c("demographics","medical_history")) 
  
  # look at baseline data 
  baseline_data <- 
    data |> 
    filter(redcap_event_name == "baseline_data_arm_1") |> 
    select(record_id,contains(baseline_vars$variable_field_name)) |> 
    mutate(across(c(dysplasia_dx, ild_biopsy, ild_biopsy_report, pcd_emicroscopy, emicroscopy_report, 
                    pcd_hsvm, hsvm_report, pcd_nasal_no, nasal_no_report, p_cgvhd),
                  ~ case_when(
                    .x == 1 ~ "Yes",
                    .x == 0 ~ "No",
                    TRUE ~ NA
                  ))) |> 
    select(record_id,
           mob,yob,
           sex,ethnicity,
           smoking,second_hand_smoke,
           category,contains("resp_disease_cat"),
           cf_allele_1,cf_allele_2,
           pancreatic_status) |> 
    mutate(
      sex = factor(sex,
                   labels = c("Male","Female")),
      ethnicity = factor(ethnicity,
                         labels = c("Caucasian","African American",
                                    "North East Asian", "South East Asian",
                                    "Other/Mixed","Not collected")),
      smoking = factor(smoking,
                       labels = c("Not collected","Current smoker",
                                  "Ex-smoker","Never-smoker")),
      second_hand_smoke = factor(second_hand_smoke,
                                 labels = c("No","Yes","Not collected")),
      category = factor(category,
                        labels = c("Healthy Control",
                                   "Respiratory Disease")),
    ) |> 
    rowwise() |> 
    mutate(combined_resp_disease_cat = str_c(
      resp_disease_cat___1 * 1,
      resp_disease_cat___2 * 2,
      resp_disease_cat___3 * 3,
      resp_disease_cat___4 * 4,
      resp_disease_cat___5 * 5,
      resp_disease_cat___6 * 6,
      resp_disease_cat___7 * 7,
      resp_disease_cat___8 * 8,
      resp_disease_cat___9 * 9,
      resp_disease_cat___99 * 99,
      sep = ",", collapse = NULL
    )) |> 
    ungroup() |> 
    mutate(resp_disease = factor(combined_resp_disease_cat,
                                 labels = c("None","Other/mixed",
                                            "Congenital lung abnormality",
                                            rep("Interstitial lung disease",2),
                                            rep("Chronic obstructive pulmonary disease",2),
                                            "PCD","CF","CF-Asthma")),
           pancreatic_status = factor(pancreatic_status,
                                      labels = c("Sufficient",
                                                 "Insufficient")),
           genotype1_clean = 
             ifelse(str_detect(cf_allele_1, regex("f508", ignore_case = TRUE)),
                    "F508del", cf_allele_1),
           genotype2_clean = 
             ifelse(str_detect(cf_allele_2, regex("f508", ignore_case = TRUE)),
                    "F508del", cf_allele_2),
           genotype =
             case_when(genotype1_clean == "F508del" & genotype2_clean == "F508del" ~ "Two copies F508del", 
                       (genotype1_clean == "F508del" & genotype2_clean != "F508del") | 
                         (genotype1_clean != "F508del" & genotype2_clean == "F508del") ~  "One copy F508del",
                       genotype1_clean != "F508del" & genotype2_clean != "F508del" ~ "No copies F508del", 
                       is.na(genotype1_clean) & is.na(genotype2_clean) ~ "Unknown",
                       TRUE ~ NA),
           est_dob = case_when(
             !is.na(mob) & !is.na(yob) ~ paste0("15-",mob,"-",yob),
             is.na(mob) & !is.na(yob) ~ paste0("15-","06-","-",yob),
             is.na(mob) & is.na(yob) ~ NA
           ), 
           record_id = as.character(record_id)) |> 
    select(-contains("resp_disease_cat"),
           -contains("cf_allele"),
           -contains("clean"))
  
  return(baseline_data)
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
    readxl::read_excel(data_dict_path)  |> 
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
    ungroup()
  
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
