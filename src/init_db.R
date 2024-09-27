# Author: Akshdeep Sandhu
# Date: 2024-02-29
# Description: This script automates the extraction, processing, and storage of REDCap project data into an SQLite database.
#####################
# MAIN SCRIPT
#####################
source('src/db_utils.R')
# constants
TOKEN_PATH <- "src/iMRHToken"
URI <- "https://rc.bcchr.ca/redcap/api/"
DATA_DICT_PATH <- 'data/data_dictionary.xlsx'
DB_PATH <- "data/iMRH_db.sqlite"

# Read redcap data
raw_data <- ReadREDCapData(TOKEN_PATH, URI)
baseline_data <- ProcessBaselineData(raw_data, DATA_DICT_PATH)
scan_data <- ProcessScanData(raw_data, DATA_DICT_PATH)
agg_seq_data <- AggSeqData(scan_data)
study_names <- ProcessStudyComponents(raw_data)
# write to db
WriteToSQLite(raw_data, DB_PATH, "raw_data")
WriteToSQLite(baseline_data, DB_PATH, "baseline_data")
WriteToSQLite(scan_data, DB_PATH, "scan_data")
WriteToSQLite(agg_seq_data, DB_PATH, "agg_seq_data")
WriteToSQLite(study_names, DB_PATH, "study_names")
# clear 
rm(list=ls())
