source('utils.R')
baseline_data <- ExportBaselineData()
scan_data <- ExportScanData()


# Let's make a database


# 
# dummy_scans <- 
#   scan_data |> 
#   select(record_id,visit_number,scan_number,sequence_name,bag,xe) |> 
#   group_by(record_id,visit_number) |> 
#   mutate(dummy_scan = 
#            case_when(
#             bag == "Yes" &  xe == "No" &
#             lead(bag) == "Yes" & lead(xe) == "Yes" ~ 1, 
#              TRUE ~ 0
#            ))


# Team might have sample queries such as: 
# Number of scans with respiratory disease that have a given sequence type 

# Feature 1: Aggregate table 
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

# we want for each record id the number of scans of a certain sequence name
left_join(agg_seq_data, baseline_data)


scan_data |> 
  mutate(drop_row = 
           case_when(
             comp1 %in% c("varMRI") | 
             comp2 %in% c("CANUCK") |
             comp3 %in% c("varMRI") |
             comp4 %in% c("varMRI") ~ 0,
             TRUE ~ 1
           )) |> 
  filter(drop_row == 0) |> 
  tibble() |> 
  view()

# PLOT
# pct pol
pct_pol <- 
  scan_data |> 
  select(visit_date,xe_pol,vxe) |>
  filter(vxe >= 200) |> 
  tibble() |> 
  mutate(visit_date = as_date(visit_date)) |> 
  drop_na(xe_pol) |> 
  arrange(visit_date) |> 
  mutate(week = floor_date(visit_date,"month")) |> 
  group_by(week) |> 
  mutate(weekly_ma = mean(xe_pol)) |> 
  ungroup()


# Plot
pct_pol |> 
  ggplot() +
  geom_line(aes(x = visit_date, y = weekly_ma, colour = "Monthly Moving Average"), size = 1) +
  geom_point(aes(x = visit_date, y = xe_pol, colour = "Actual Values")) +
  labs(title = "Moving Average of xe_pol", x = "Date", y = "xe_pol") +
  scale_color_brewer(palette = "Set2")+
  theme_minimal()


pct_pol |> 
  ggplot() +
  geom_point(aes(x = visit_date, y = weekly_ma, colour = "Weekly Moving Average"))+
  geom_line(aes(x = visit_date, y = weekly_ma, colour = "Weekly Moving Average"), size = 1, linetype = "dashed") +
  #geom_point(aes(x = visit_date, y = xe_pol, colour = "Actual Values")) +
  labs(title = "Moving Average of xe_pol", x = "Date", y = "xe_pol") +
  scale_color_brewer(palette = "Set2")+
  theme_minimal()


# vol ze
pct_xe <- 
  scan_data |> 
  select(visit_date,vxe) |> 
  tibble() |> 
  mutate(visit_date = as_date(visit_date)) |> 
  drop_na(vxe) |> 
  arrange(visit_date) |> 
  mutate(week = floor_date(visit_date,"week")) |> 
  group_by(week) |> 
  mutate(weekly_ma = mean(vxe)) |> 
  ungroup()


# Plot
pct_xe |> 
  ggplot() +
  geom_line(aes(x = visit_date, y = weekly_ma, colour = "Weekly Moving Average"), size = 1, linetype = "dashed") +
  geom_point(aes(x = visit_date, y = vxe, colour = "Actual Values")) +
  labs(title = "Moving Average of vxe", x = "Date", y = "xe_pol") +
  scale_color_brewer(palette = "Set2")+
  theme_minimal()


pct_xe |> 
  ggplot() +
  geom_point(aes(x = visit_date, y = weekly_ma, colour = "Weekly Moving Average"))+
  geom_line(aes(x = visit_date, y = weekly_ma, colour = "Weekly Moving Average"), size = 1, linetype = "dashed") +
  labs(title = "Moving Average of xe_pol", x = "Date", y = "xe_pol") +
  scale_color_brewer(palette = "Set2")+
  theme_minimal()
