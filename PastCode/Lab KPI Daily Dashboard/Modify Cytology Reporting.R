library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
library(kableExtra)
library(formattable)
library(rmarkdown)
library(writexl)
library(gsubfn)
library(stringr)

#Clear existing history
rm(list = ls())
#-------------------------------holiday/weekend-------------------------------#
# Get today and yesterday's date

today <- Sys.Date()

# Select file/folder path for easier file selection and navigation

if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}

test_pp_data <- data.frame(read_excel(
  path = paste0(user_directory,
                "\\AP & Cytology Signed Cases Reports\\",
                "KPI REPORT - RAW DATA V4_V2 2021-05-08.xls"),
           skip = 1))

test_pp_data <- test_pp_data[-nrow(test_pp_data),]

epic_data <- read_excel(
  path = paste0(user_directory,
                "\\EPIC Cytology\\",
                "MSHS Pathology Orders EPIC 2021-05-08.xlsx"))
# 
# cyto_sign_out_df <- test_pp_data %>%
#   filter(spec_sort_order == "A" &
#            spec_group %in% c("CYTO NONGYN", "CYTO GYN")) %>%
#   mutate(SignOutDept = ifelse(str_detect(signed_out_Pathologist, "Interface"),
#                               "Micro", "Cyto"))

# Filter Epic data to only include cases with final sign out
epic_data_final <- epic_data %>%
  filter(LAB_STATUS %in% c("Final result", "Edited Result - FINAL"))

# Identify case numbers of cases with final sign out for use with PowerPath data
epic_data_spec <- epic_data_final %>%
  mutate(Case_no = SPECIMEN_ID) %>%
  select(Case_no)

epic_data_spec <- unique(epic_data_spec)

pp_data <- test_pp_data %>%
  mutate(Facility_Old = Facility,
         Facility = ifelse(Facility_Old == "MSS", "MSH",
                           ifelse(Facility_Old == "STL", "SL",
                                  Facility_Old)))

# cyto_raw <- pp_data %>%
#   filter(spec_sort_order == "A" &
#            spec_group %in% c("CYTO NONGYN", "CYTO GYN"))
# 
# cyto_final <- merge(x = cyto_raw, y = epic_data_spec)

cyto_raw_new <- pp_data %>%
  filter(spec_sort_order == "A" &
           spec_group %in% c("CYTO NONGYN", "CYTO GYN")) %>%
  mutate(SignOutDept = ifelse(str_detect(signed_out_Pathologist, "Interface"),
                              "Microbiology", "Cytology"),
         FinalizedCase = Case_no %in% epic_data_spec$Case_no)

sp_raw <- test_pp_data

sp_raw <- merge(x = sp_raw, y = gi_codes, all.x = TRUE)

sp_raw <- sp_raw %>%
  mutate(Facility_Old = Facility,
         Facility = ifelse(Facility_Old == "MSS", "MSH",
                           ifelse(Facility_Old == "STL", "SL",
                                  Facility_Old)),
         spec_group = ifelse(spec_group == "BREAST", "Breast", spec_group))

exclude_gi_codes_df <- sp_raw %>%
  filter(GI_Code_InclExcl %in% c("Exclude"))

exclude_case_num <- unique(exclude_gi_codes_df$Case_no)

sp_raw <- sp_raw %>%
  filter(# Select primary specimens only
    spec_sort_order == "A" &
      # Select GI specimens with codes that are included and any breast specimens
      ((spec_group == "GI" & !(Case_no %in% exclude_case_num)) |
         (spec_group == "Breast" )) &
      # Exclude NYEE
      Facility != "NYEE") %>%
  mutate(SignOutDept = NA,
         FinalizedCase = TRUE)

cyto_weekday_preprocessed <- cyto_weekday_final

cyto_weekday_preprocessed <- merge(x = cyto_weekday_preprocessed,
                                   y = patient_setting,
                                   all.x = TRUE)
cyto_weekday_preprocessed <- cyto_weekday_preprocessed %>%
  mutate(Patient.Setting = ifelse(Rev_ctr == "MSBK" &
                                    patient_type %in% c("A", "O"),
                                  "Amb",
                                  ifelse(Rev_ctr = "MSBK" &
                                           patient_type %in% c("I"), "IP",
                                         Patient.Setting)))

cyto_weekday_preprocessed <- merge(x = cyto_weekday_preprocessed,
                                   y = tat_targets_ap,
                                   all.x = TRUE,
                                   by = c("spec_group", "Patient.Setting"))

# check if any of the dates were imported as characters
if (is.character(cyto_weekday_preprocessed$Collection_Date)) {
  cyto_weekday_preprocessed2 <- cyto_weekday_preprocessed %>%
    mutate(Collection_Date = as.numeric(Collection_Date)) %>%
    mutate(Collection_Date = as.Date(Collection_Date,
                                     origin = "1899-12-30"))
} else {
  cyto_weekday_preprocessed2 <- cyto_weekday_preprocessed %>%
    mutate(Collection_Date = Collection_Date)
}

#Change all Dates into POSIXct format to start the calculations
cyto_weekday_preprocessed2[c("Case_created_date",
               "Collection_Date",
               "Received_Date",
               "signed_out_date")] <-
  lapply(cyto_weekday_preprocessed2[c("Case_created_date",
                        "Collection_Date",
                        "Received_Date",
                        "signed_out_date")],
         as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p")