# Code for connecting to historical data

# Note: This can change once database structure is implemented

# Clinical Pathology --------
## Find latest dates from OAO cloud database tables
oao_cloud_db <- "OAO Cloud DB Kate"

oao_conn <- dbConnect(odbc(), oao_cloud_db)

chemistry_default_date <- tbl(oao_conn, "CP_DAILY_REPO") %>%
  filter(DIVISION %in% "Chemistry") %>%
  summarize(RESULT_DATE = max(RESULT_DATE)) %>%
  collect()

chemistry_default_date <- as.Date(chemistry_default_date$RESULT_DATE)

hematology_default_date <- tbl(oao_conn, "CP_DAILY_REPO") %>%
  filter(DIVISION %in% "Hematology") %>%
  summarize(RESULT_DATE = max(RESULT_DATE)) %>%
  collect()

hematology_default_date <- as.Date(hematology_default_date$RESULT_DATE)

micro_default_date <- tbl(oao_conn, "CP_DAILY_REPO") %>%
  filter(DIVISION %in% "Microbiology RRL") %>%
  summarize(RESULT_DATE = max(RESULT_DATE)) %>%
  collect()

micro_default_date <- as.Date(micro_default_date$RESULT_DATE)

infusion_default_date <- tbl(oao_conn, "CP_DAILY_REPO") %>%
  filter(DIVISION %in% "Infusion") %>%
  summarize(RESULT_DATE = max(RESULT_DATE)) %>%
  collect()

infusion_default_date <- as.Date(infusion_default_date$RESULT_DATE)

missing_collect_default_date <- tbl(oao_conn, "CP_DAILY_REPO") %>%
  summarize(RESULT_DATE = max(RESULT_DATE)) %>%
  collect()

missing_collect_default_date <- as.Date(missing_collect_default_date$RESULT_DATE)

dbDisconnect(oao_conn)

# 
# 
# # Tables will show data for latest date in repository be default
# cp_resulted_date <- max(cp_daily_repo$ResultDate)
# 
# cp_submitted_daily_summary <- cp_daily_repo %>%
#   filter(ResultDate == cp_resulted_date) %>%
#   arrange(Site, ResultDate) %>%
#   ungroup()
# 
# 
# # Anatomic Pathology ---------
# 
# 
# # Operations and Quality Indicators -------
# ops_qlty_data <- read_excel(
#   paste0(user_directory,
#          "/Quality and Ops Indicators Form Responses",
#          "/MSHS Lab KPI Operations and Quality Indicators Forms 07252023.xlsx"))
# 
# ops_qlty_responses <- ops_qlty_data
# 
# colnames(ops_qlty_responses) <- c(
#   "ID", "StartTime", "CompletionTime", "Email", "Name", "Facility",
#   "LabCorp", "VendorServices", "Environment", "Equipment", "IT",
#   "ServiceChanges", "Volume", "Staffing", "Comments",
#   "NeverEvents", "NeverEventsComments", "GoodCatch",
#   "LIS_Staffing", "LIS_UnplannedService", "LIS_PreplannedDowntime"
# )
# 
# ops_qlty_responses <- ops_qlty_responses %>%
#   select(-contains("LIS_")) %>%
#   arrange(Facility, -ID) %>%
#   mutate(CompletionDate = as.Date(CompletionTime),
#          CompletionHour = hour(CompletionTime)) %>%
#   filter(!duplicated(cbind(Facility, CompletionDate)))
# 
# 
# ops_qlty_date <- max(date(ops_qlty_responses$CompletionDate))
# 
# ops_qlty_responses_today <- ops_qlty_responses %>%
#   filter(CompletionDate == ops_qlty_date|
#            (CompletionDate == ops_qlty_date - 1 &
#               CompletionHour >= 17))
# 
# # Ensure all facilities are in dataset even if there weren't any responses
# ops_qlty_responses_today <- left_join(ops_indicators_facility_df,
#                                       ops_qlty_responses_today,
#                                       by = c("Facility" = "Facility"))
# 
# ops_qlty_responses_today <- ops_qlty_responses_today %>%
#   select(-Facility) %>%
#   rename("Facility" = FacilitySimple)
# 
# 
# 
# never_events_responses <- ops_qlty_responses_today %>%
#   select(Facility, NeverEvents, NeverEventsComments)
# 
# good_catch_responses <- ops_qlty_responses_today %>%
#   select(Facility, GoodCatch)
# 
# 
# # Custom function for formatting -----
# ops_qlty_formatting <- function(x) {
#   x <- x %>%
#     mutate(Status = ifelse(is.na(Status),
#                            cell_spec(Status, "html", color = "lightgray"),
#                            ifelse(Status %in% c("Safe", "None"),
#                                   cell_spec(Status, "html", color = "green"),
#                                   ifelse(Status %in% c("Not Safe", "Present"),
#                                          cell_spec(Status, "html", color = "red"),
#                                          cell_spec(Status, "html", color = "orange")))))
#   
#   
# }
# 
# 
# 
# # Never Events ------
# never_event_options <- c("Specimen(s) Lost",
#                          "QNS - specimen that cannot be recollected",
#                          "Treatment based on mislabeled/misidentified specimen",
#                          "Treatment based on false positive/false negative results")
# 
# never_events_responses$NeverEvents <- c("None",
#                                         paste0(never_event_options[1],
#                                                ";",
#                                                never_event_options[3],
#                                                ";"),
#                                         paste0(never_event_options[2], ";"),
#                                         "None",
#                                         "None",
#                                         "None",
#                                         "None",
#                                         "None",
#                                         "None",
#                                         "None",
#                                         "None")
# never_events_responses <- never_events_responses %>%
#   mutate(SpecimenLost = ifelse(str_detect(NeverEvents, "Specimen\\(s\\) Lost"),
#                                "Present", "None"),
#          QNS = ifelse(str_detect(NeverEvents, "QNS"), "Present", "None"),
#          Mislabeled = ifelse(str_detect(NeverEvents,
#                                         "Treatment based on mislabeled"),
#                              "Present", "None"),
#          FalsePositive = ifelse(str_detect(NeverEvents,
#                                            "Treatment based on false positive"),
#                                 "Present", "None"),
#          NeverEventsComments = ifelse(is.na(NeverEventsComments) |
#                                         toupper(NeverEventsComments) %in%
#                                         c("NONE", "N/A", "NA"),
#                                       "No Issues Reported",
#                                       NeverEventsComments)
#   ) %>%
#   relocate(NeverEventsComments, .after = last_col()) %>%
#   select(-NeverEvents)
# 
# 
# 
# never_events_status <- never_events_responses %>%
#   select(-NeverEventsComments) %>%
#   pivot_longer(cols = !Facility,
#                names_to = "Metric",
#                values_to = "Status")
# 
# never_events_status <- ops_qlty_formatting(never_events_status)
# 
# never_events_status <- never_events_status %>%
#   pivot_wider(names_from = "Metric",
#               values_from = "Status")
# 
# never_events_comments <- never_events_responses %>%
#   select(Facility, NeverEventsComments)
# 
# good_catch_responses <- good_catch_responses %>%
#   mutate(GoodCatch = ifelse(is.na(GoodCatch) |
#                               toupper(GoodCatch) %in%
#                               c("NONE", "N/A", "NA", 0),
#                             "No Issues Reported", GoodCatch)
#   )
# 
# # Kables
# # kable(ops_indicators_status, escape = FALSE, align = "c",
# #       col.names = c("Facility", "Lab Corp Consumables",
# #                     "Vendor Services", "Environment", "Equipment",
# #                     "IT", "Service Change", "Acute Volume Increase",
# #                     "Staffing")) %>%
# #   kable_styling(bootstrap_options = "hover", full_width = FALSE,
# #                  position = "center", row_label_position = "c",
# #                  font_size = 11) %>%
# #   row_spec(row = 0, font_size = 13)
# # 
# # kable(ops_indicators_comments, escape = F, align = "c",
# #       col.names = c("Facility", "Comments if At Risk or Not Safe")) %>%
# #   kable_styling(bootstrap_options = "hover", full_width = TRUE,
# #                 position = "center", row_label_position = "c",
# #                 font_size = 11) %>%
# #   row_spec(row = 0, font_size = 13)
# # 
# # kable(never_events_status, escape = F, align = "c",
# #       col.names = c("Facility",
# #                     "Specimen(s) Lost",
# #                     "QNS - specimen that cannot be recollected",
# #                     "Treatment based on mislabeled/misidentified specimen",
# #                     "Treatment based on false positive/false negative results")) %>%
# #   kable_styling(bootstrap_options = "hover", full_width = FALSE,
# #                 position = "center", row_label_position = "c",
# #                 font_size = 11) %>%
# #   row_spec(row = 0, font_size = 13)
