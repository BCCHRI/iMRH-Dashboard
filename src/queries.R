pacman::p_load(tidyverse,SQLite, DBI)

db <- dbConnect(RSQLite::SQLite(), 'data/iMRH_db.sqlite')

tbl(db, 'scan_data') |> 
  group_by(sequence_name) |> 
  count() |> 
  print(n=100)

# Total number of: 
# - Ventilation? (What is this)
# - ADC (What is this)
# - Dissolved phase scans 
# - Number of participants

# 1. Dissolved phase scans 
tbl(db, 'scan_data') |> 
  filter(dummy_scan == 0) |> 
  filter(sequence_name %in% c("Xe - Dissolved Phase (4pt; JW)", "Xe - DWCS (diffusion; JW)")) |> 
  group_by(sequence_name) |> 
  count()

# N = 59

# 2. Ventilation 
tbl(db, 'scan_data') |> 
  filter(dummy_scan == 0) |> 
  filter(sequence_name %in% 
    c("Xe - 2D SPGR (vent; JW)",
      "Xe - 3D SPGR (vent; AO)",
      "Xe - 3D SPGR (vent; JW)")) |>
  group_by(sequence_name) |> 
  count()

# 3. Number of participants (staratifed by CF)
tbl(db, 'baseline_data') |> 
  group_by(resp_disease) |> 
  count() |> 
  collect() |> 
  ungroup() |> 
  summarise(total = sum(n))
  
