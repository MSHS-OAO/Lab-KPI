#######
# Code for importing the needed packages, constants, reference files, and
# data templates for the lab KPI dashboard pre-processing -----
#######

#Install packages only the first time you run the code
# install.packages("timeDate")
# install.packages("lubridate")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("knitr")
# install.packages("gdtools")
# install.packages("kableExtra")
# install.packages("formattable")
# install.packages("bizdays")
# install.packages("rmarkdown")
# install.packages("stringr")
# install.packages("writexl")
# install.packages("gsubfn")
# install.packages("tidyr")
#-------------------------------Required packages------------------------------#

#Required packages: run these every time you run the code
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
# library(gdtools)
# library(kableExtra)
library(kableExtra,  "~/R/x86_64-pc-linux-gnu-library/4.2")
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)
library(here)
library(odbc)
library(dbplyr)
library(DBI)
library(glue)
library(foreach)
library(doParallel)
library(parallel)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

#Clear existing history
rm(list = ls())
#-------------------------------holiday/weekend-------------------------------#
# Get today and yesterday's date
# today <- as.Date("5/9/23", format("%m/%d/%y"))
today <- Sys.Date()

#Determine if yesterday was a holiday/weekend
#get yesterday's DOW
yesterday <- today - 1
# Determine date of resulted labs/specimens
resulted_date <- yesterday


#Get yesterday's DOW
yesterday_day <- wday(yesterday, label = TRUE, abbr = TRUE)

#Remove Good Friday from MSHS Holidays
nyse_holidays <- as.Date(holidayNYSE(year = 1990:2100))
good_friday <- as.Date(GoodFriday())
mshs_holiday <- nyse_holidays[good_friday != nyse_holidays]

#Determine whether yesterday was a holiday/weekend
#holiday_det <- isHoliday(yesterday, holidays = mshs_holiday)
holiday_det <- isHoliday(as.timeDate(yesterday), holidays = mshs_holiday)

#Set up a calendar for collect to received TAT calculations for Path & Cyto
create.calendar("MSHS_working_days", mshs_holiday,
                weekdays = c("saturday", "sunday"))
bizdays.options$set(default.calendar = "MSHS_working_days")


# Determine root directory using custom function
# Function to determine path to share drive on R Workbench or R Studio
define_root_path <- function(){
  #Check if directory is from R Workbench; starts with '/home'
  if(grepl("^/home", dirname(getwd()))){
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files("/SharedDrive/") == "Presidents",
           #Define prefix of path to share drive with R Workbench format
           output <- "/SharedDrive/Presidents/", 
           output <- "/SharedDrive/deans/Presidents/")
  }#Check if directory is from R Studio; starts with an uppercase letter than ':'
  else if(grepl("^[[:upper:]]+:", dirname(getwd()))){
    #Determine which drive is mapped to Sharedrive (x)
    for(i in LETTERS){
      if(any(grepl("deans|Presidents", list.files(paste0(i, "://"))))){x <- i}
    }
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files(paste0(x, "://")) == "Presidents",
           #Define prefix of path to share drive with R Studio format
           output <- paste0(x, ":/Presidents/"),
           output <- paste0(x, ":/deans/Presidents/"))
    
  }
  return(output)
}

# user_directory <- paste0(define_root_path(),
#                          "HSPI-PM/",
#                          "Operations Analytics and Optimization/Projects/",
#                          "Service Lines/Lab Kpi/Data")
# 
# daily_repo <- readRDS(
#   paste0(define_root_path(),
#          "HSPI-PM/",
#          "Operations Analytics and Optimization/Projects/",
#          "Service Lines/Lab Kpi/Data/",
#          "CP Repositories/DailyRepo/",
#          "Daily Repo 12-01-20 to 05-21-23 as of 05-22-23.RDS")
# )

# # AP Summary Import ----
# ap_summary <- readRDS(paste0(user_directory,
#                              "/Shiny App Repo/APDailySummary",
#                              "/APRepo180Days.rds"))
# 
# # AP Backlog Import 
# backlog_daily_repo <- readRDS(paste0(user_directory,
#                                      "/Shiny App Repo/APDailySummary",
#                                      "/BacklogRepo180Days.rds"))

# CP Reference Tables -------------------
oao_cloud_db <- "OAO Cloud DB Kate"

## Begin connection -------
oao_personal_conn <- dbConnect(odbc(), oao_cloud_db)

## MSHS Sites -----------
cp_mshs_sites <- tbl(oao_personal_conn, "CP_SITE_NAMES") %>%
  collect() %>%
  rename(Site = SITE)

## Test Codes -----------

cp_scc_test_codes <- tbl(oao_personal_conn, "CP_SCC_TEST_CODES") %>%
  collect()

cp_sun_test_codes <- tbl(oao_personal_conn, "CP_SUN_TEST_CODES") %>%
  collect()

## TAT Targets ---------
cp_tat_targets <- tbl(oao_personal_conn, "CP_TAT_TARGETS") %>%
  collect() %>%
  mutate(Concate = ifelse(
    PRIORITY == "All" & PT_SETTING == "All", paste(TEST, DIVISION),
    ifelse(PRIORITY != "All" & PT_SETTING == "All",
           paste(TEST, DIVISION, PRIORITY),
           paste(TEST, DIVISION, PRIORITY, PT_SETTING))))

## Patient Setting and ICU -----------
cp_scc_setting <- tbl(oao_personal_conn, "CP_SCC_CLINIC_TYPE") %>%
  collect()

cp_sun_setting <- tbl(oao_personal_conn, "CP_SUN_LOC_TYPE") %>%
  collect()

cp_scc_icu <- tbl(oao_personal_conn, "CP_SCC_ICU") %>%
  collect() %>%
  mutate(SiteCodeName = paste(SITE, WARD, WARD_NAME))

cp_sun_icu <- tbl(oao_personal_conn, "CP_SUN_ICU") %>%
  collect() %>%
  mutate(SiteCodeName = paste(SITE, LOC_CODE, LOC_NAME))

## Disconnect -----------
dbDisconnect(oao_personal_conn)


# Create data frame of CP tests and divisions
cp_test_divisions <-
  unique(
    rbind(unique(cp_scc_test_codes[, c("TEST", "DIVISION")]),
          unique(cp_sun_test_codes[, c("TEST", "DIVISION")])))

# tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")
#
# Add a column concatenating test, priority, and setting for matching later
# # tat_targets <- tat_targets %>%
# #   mutate(Concate = ifelse(
# #     Priority == "All" & `PtSetting` == "All", paste(Test, Division),
# #     ifelse(Priority != "All" & `PtSetting` == "All",
# #            paste(Test, Division, Priority),
# #            paste(Test, Division, Priority, `PtSetting`))))
# 
# scc_icu <- read_excel(reference_file, sheet = "SCC_ICU")
# sun_icu <- read_excel(reference_file, sheet = "SUN_ICU")
# 
# scc_icu <- scc_icu %>%
#   mutate(SiteCodeName = paste(Site, Ward, Ward_Name))
# 
# sun_icu <- sun_icu %>%
#   mutate(SiteCodeName = paste(Site, LocCode, LocName))
# 
# scc_setting <- read_excel(reference_file, sheet = "SCC_ClinicType")
# sun_setting <- read_excel(reference_file, sheet = "SUN_LocType")
# 
# mshs_site <- read_excel(reference_file, sheet = "SiteNames")

###code to be moved to the CP code
#scc_wday <- scc_weekday
#sun_wday <- sq_weekday

test_names <- cp_test_divisions$TEST

cp_micro_lab_order <- c("Troponin",
                        "Lactate WB",
                        "BUN",
                        "HGB",
                        "PT",
                        "Rapid Flu",
                        "C. diff")

all_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN", "RTC")
hosp_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN")
infusion_sites <- c("RTC")

pt_setting_order <- c("ED", "ICU", "IP Non-ICU", "Amb", "Other")
pt_setting_order2 <- c("ED & ICU", "IP Non-ICU", "Amb", "Other")
dashboard_pt_setting <- c("ED & ICU", "IP Non-ICU", "Amb")

dashboard_priority_order <- c("All", "Stat", "Routine")

cp_division_order <- c("Chemistry", "Hematology", "Microbiology RRL", "Infusion")

ops_indicators_facility_df <- data.frame(
  "Facility" = 
    c("MSH (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSQ (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSBI (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSB (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSW (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSSL (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "NYEE (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSSN (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
      "MSH - Anatomic Pathology (Centralized)",
      "MSH - Central Processing & Accessioning",
      "4LABS - Client Services"),
  
  "FacilitySimple" = c("MSH",
                       "MSQ",
                       "MSBI",
                       "MSB",
                       "MSW",
                       "MSSL",
                       "NYEE",
                       "MSSN",
                       "Anatomic Pathology (Centralized)",
                       "Central Processing & Accessioning (CPA)",
                       "Client Services - 4LABS")
)

# Create template dataframes for combinations of tests, priority, and settings
# that will be used in TAT and volume look back tables. These templates ensure
# all relevant combinations are included in the tables regardless of resulted
# volume
# Create template data frames for combinations of tests, priority and settings
# that will be used in TAT tables and volume lookback tables
tat_base_template <-
  expand.grid(
    "Test" = test_names,
    "Site" = all_sites,
    "DashboardPriority" = dashboard_priority_order,
    "DashboardSetting" = dashboard_pt_setting,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
  arrange(Test, Site) %>%
  left_join(cp_test_divisions, by = c("Test" = "TEST"))

vol_base_template <- 
  expand.grid(
    "Test" = test_names,
    "Site" = all_sites,
    "DashboardPriority" = dashboard_priority_order,
    "PtSetting" = pt_setting_order,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
  arrange(Test, Site) %>%
  left_join(cp_test_divisions, by = c("Test" = "TEST")) %>%
  arrange(Test, Site, DashboardPriority, PtSetting)

# Select applicable test, priority, setting combinations based on lab operations
tat_dashboard_templ <- tat_base_template %>%
  rename(Division = DIVISION) %>%
  mutate(
    # Update Division for RTC to Infusion
    Division = ifelse(Site %in% c("RTC"), "Infusion", Division),
    # Create column for applicable combinations
    Incl = ifelse(
      # Remove ED & ICU labs with Routine priority since all labs in these
      # these settings are treated as stat
      (DashboardPriority %in% c("Routine") &
         DashboardSetting %in% c("ED & ICU")) |
        # Remove ambulatory troponin and lactate since these labs are collected
        # in ambulatory settings. Remove stat and routine stratification for
        # these labs since all are treated as stat
        (Test %in% c("Troponin", "Lactate WB") &
           (DashboardPriority %in% c("Stat", "Routine") |
              DashboardSetting %in% c("Amb"))) |
        # Remove "all" priority for BUN, PT, and HGB labs for non-infusion
        # settings
        (Test %in% c("BUN", "PT", "HGB") & DashboardPriority %in% c("All") &
           !(Division %in% c("Infusion"))) |
        # Remove priority stratification for rapid flu and c. diff since all
        # are treated as stat
        (Test %in% c("Rapid Flu", "C. diff") &
           !(DashboardPriority %in% c("All"))) |
        # Remove any labs other than BUN and HGB for infusion since those are
        # the only labs processed there. Remove stat and routine stratification
        # for infusion labs since all labs treated the same
        (Division %in% c("Infusion") & (!(Test %in% c("BUN", "HGB")) |
           !(DashboardSetting %in% c("Amb")) |
           !(DashboardPriority %in% c("All")))), "Excl", "Incl")) %>%
  filter(Incl == "Incl")

vol_dashboard_templ <- vol_base_template %>%
  rename(Division = DIVISION) %>%
  mutate(
    # Update Division for RTC to Infusion
    Division = ifelse(Site %in% c("RTC"), "Infusion", Division),
    # Create column for applicable combinations
    Incl = ifelse(
      # Remove ED & ICU labs with Routine priority since all labs in these
      # these settings are treated as stat
      (DashboardPriority %in% c("Routine") &
         PtSetting %in% c("ED", "ICU")) |
        # Remove stat and routine stratification for troponin and lactate labs
        # these labs since all are treated as stat
        (Test %in% c("Troponin", "Lactate WB") &
           (DashboardPriority %in% c("Stat", "Routine"))) |
        # Remove "all" priority for BUN, PT, and HGB labs
        (Test %in% c("BUN", "PT", "HGB") & DashboardPriority %in% c("All") &
           !(Division %in% c("Infusion"))) |
        # Remove Microbiology RRL since resulted volume is included already in
        # TAT tables
        (Division %in% c("Microbiology RRL")) |
        # Remove any labs other than BUN and HGB from RTC since those are the
        # only labs processed there. Also remove non-amb setting
        (Division %in% c("Infusion") & (!(Test %in% c("BUN", "HGB")) |
                                          !(PtSetting %in% c("Amb")) |
                                          !(DashboardPriority %in% c("All")))),
      "Excl", "Incl")) %>%
  filter(Incl == "Incl")


# #-----------Patient Setting Excel File-----------#
# #Using Rev Center to determine patient setting
# patient_setting <- data.frame(read_excel(reference_file,
#                                          sheet = "AP_Patient Setting"),
#                               stringsAsFactors = FALSE)
# 
# #-----------Anatomic Pathology Targets Excel File-----------#
# tat_targets_ap <- data.frame(read_excel(reference_file,
#                                         sheet = "AP_TAT Targets"),
#                              stringsAsFactors = FALSE)
# 
# #-----------GI Codes Excel File-----------#
# #Upload the exclusion vs inclusion criteria associated with the GI codes
# gi_codes <- data.frame(read_excel(reference_file, sheet = "GI_Codes"),
#                        stringsAsFactors = FALSE)
# 
# #-----------Create table template for Cyto/Path-----------#
# #The reason behind the table templates is to make sure all the variables and
# #patient settings are included
# 
# #Cyto
# #this template for cyto is with an assumption that received to result is not
# #centralized
# Spec_group <- c("CYTO GYN","CYTO NONGYN")
# Patient_setting <- c("IP", "Amb")
# 
# table_temp_cyto <- expand.grid(Spec_group, Patient_setting)
# 
# colnames(table_temp_cyto) <- c("Spec_group", "Patient_setting")
# 
# other_cols_table_temp_cyto <- c("no_cases_signed",
#                                 "MSH.x", "BIMC.x", "MSQ.x", "NYEE.x",
#                                 "PACC.x", "R.x", "SL.x", "KH.x", "BIMC.y",
#                                 "MSH.y", "MSQ.y", "NYEE.y", "PACC.y", "R.y",
#                                 "SL.y", "KH.y")
# table_temp_cyto <- table_temp_cyto %>% 
#   mutate(!!!setNames(rep(NA, length(other_cols_table_temp_cyto)), other_cols_table_temp_cyto))
# 
# 
# #this template for cyto is with an assumption that received to result is
# #centralized
# # for "!!!" syntax refer: https://stackoverflow.com/questions/18214395/add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector
# # table_temp_cyto_v2 <- expand.grid(Spec_group, Patient_setting)
# # 
# # colnames(table_temp_cyto_v2) <- c("Spec_group", "Patient_setting")
# # 
# # 
# # other_cols_table_temp_cyto_v2 <- c("no_cases_signed",
# #                                    "received_to_signed_out_within_target",
# #                                    "BIMC", "MSH", "MSQ", "NYEE", "PACC",
# #                                    "R", "SL", "KH")
# # 
# # table_temp_cyto_v2 <- table_temp_cyto_v2 %>% 
# #   mutate(!!!setNames(rep(NA, length(other_cols_table_temp_cyto_v2)), other_cols_table_temp_cyto_v2))
# # 
# # #this table template is for cytology volume
# # table_temp_cyto_vol <- expand.grid(Spec_group, Patient_setting)
# # 
# # colnames(table_temp_cyto_vol) <- c("Spec_group", "Patient_setting")
# # 
# # 
# # other_cols_table_temp_cyto_vol <- c("BIMC", "MSH", "MSQ", "NYEE", "PACC",
# #                                     "R", "SL", "KH")
# # 
# # table_temp_cyto_vol <- table_temp_cyto_vol %>% 
# #   mutate(!!!setNames(rep(NA, length(other_cols_table_temp_cyto_vol)), other_cols_table_temp_cyto_vol))
# # 
# # #Patho
# # #this template for patho (sp) is with an assumption that received to result is
# # #not centralized
# # # Centralized vs Non centralized table creation.
# # 
# # Spec_group <- c("Breast", "GI")
# # Patient_setting <- c("IP", "Amb")
# # 
# # table_temp_patho <- expand.grid(Spec_group, Patient_setting)
# # 
# # colnames(table_temp_patho) <- c("Spec_group", "Patient_setting")
# # 
# # 
# # other_cols_table_temp_patho <- c("no_cases_signed",
# #                                  "MSH.x", "BIMC.x", "MSQ.x", "PACC.x",
# #                                  "R.x", "SL.x", "KH.x", "BIMC.y", "MSH.y",
# #                                  "MSQ.y", "PACC.y", "R.y", "SL.y", "KH.y")
# # 
# # table_temp_patho <- table_temp_patho %>% 
# #   mutate(!!!setNames(rep(NA, length(other_cols_table_temp_patho)), other_cols_table_temp_patho))
# # 
# # #this table template is for surgical pathology volume
# # table_temp_patho_vol <- expand.grid(Spec_group, Patient_setting)
# # 
# # colnames(table_temp_patho_vol) <- c("Spec_group", "Patient_setting")
# # 
# # 
# # other_cols_table_temp_patho_vol <- c("BIMC", "MSH", "MSQ", "PACC",
# #                                      "R", "SL", "KH")
# # 
# # table_temp_patho_vol <- table_temp_patho_vol %>% 
# #   mutate(!!!setNames(rep(NA, length(other_cols_table_temp_patho_vol)), other_cols_table_temp_patho_vol))
# # 
# # sp_vol_column_order <- c("Spec_group", "Patient_setting",
# #                          "MSH", "MSQ", "BIMC", "PACC", "KH", "R", "SL")
# # 
# # cyto_vol_column_order <- c("Spec_group", "Patient_setting",
# #                            "MSH", "MSQ", "BIMC", "PACC", "KH", "R", "SL",
# #                            "NYEE")
# # 
# # sp_standardized_column_names <-
# #   c("Case Type", "Target", "Setting", "No. Cases Signed Out",
# #     "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL",
# #     "MSH", "MSQ", "MSBI", "PACC",
# #     "MSB", "MSW", "MSSL")
# # 
# # sp_vol_column_names <- c("Case Type", "Setting",
# #                          "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL")
# # 
# # cyto_vol_column_names <- c("Case Type", "Setting",
# #                            "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL",
# #                            "NYEE")
# # cyto_spec_group <- c("CYTO GYN", "CYTO NONGYN")
# # patho_spec_group <- c("Breast", "GI")
# 
# 
# # Creating Mapping Template for AP Display ----
# SPEC_GROUP <- c("Breast", "GI","CYTO GYN", "CYTO NONGYN")
# PATIENT_SETTING <- c("IP", "Amb")
# SITES <- c("MSB","MSBI","PACC","MSW","MSH","MSM","MSSN","MSQ","NYEE")
# TAB <- c("Efficiency Indicators","24 Hour Volume")
# METRIC <- c("received_to_signed_out_within_target",
#             "avg_collection_to_signed_out",
#             "no_cases_signed")
# 
# table_ap_template <- expand.grid(SPEC_GROUP, PATIENT_SETTING,SITES,TAB,METRIC)
# 
# colnames(table_ap_template) <- c("SPECIMEN_GROUP", "PATIENT_SETTING","SITE","TAB","METRIC")
# 
# table_ap_template <- table_ap_template %>%
#   mutate(DIVISION = case_when(SPECIMEN_GROUP %in% c("Breast", "GI") ~ "SURGICAL PATHOLOGY",
#                               SPECIMEN_GROUP %in% c("CYTO GYN", "CYTO NONGYN") ~ "CYTOLOGY"))
# 
# 
# table_ap_template <- table_ap_template %>%
#   filter(!((DIVISION == "SURGICAL PATHOLOGY") & 
#              (SPECIMEN_GROUP %in% c("CYTO GYN", "CYTO NONGYN"))))
# 
# table_ap_template <- table_ap_template %>%
#   filter(!((DIVISION == "CYTOLOGY") & 
#              (SPECIMEN_GROUP %in% c("Breast", "GI"))))
# 
# 
# 
# table_ap_template <- table_ap_template %>%
#   filter(!((DIVISION == "CYTOLOGY") & 
#              (TAB == "Efficiency Indicators") & 
#              (METRIC %in% c("received_to_signed_out_within_target",
#                             "no_cases_signed"))))
# 
# table_ap_template <- table_ap_template %>%
#   filter(!((DIVISION == "SURGICAL PATHOLOGY") & 
#              (TAB == "Efficiency Indicators") & 
#              (METRIC == "no_cases_signed")))
# 
# 
# table_ap_template <- table_ap_template %>%
#   filter(!((DIVISION %in% c("SURGICAL PATHOLOGY","CYTOLOGY")) & 
#              (TAB == "24 Hour Volume") & 
#              (METRIC %in% c("received_to_signed_out_within_target",
#                             "avg_collection_to_signed_out"))))
# 
# # Efficiency Indicators template Pathology ----
# table_ap_template_surgical_pathology <- table_ap_template %>%
#   filter(DIVISION == "SURGICAL PATHOLOGY" &
#            !SITE %in% c("NYEE","MSSN")) %>%
#   filter(TAB == "Efficiency Indicators") %>%
#   select(-DIVISION,-TAB)
# 
# # Efficiency Indicators template Cytology ----
# table_ap_template_cytology <- table_ap_template %>%
#   filter(DIVISION == "CYTOLOGY" &
#            !SITE %in% c("MSSN")) %>%
#   filter(TAB == "Efficiency Indicators") %>%
#   select(-DIVISION,-TAB)
# 
# 
# # 24 Hour Volume template Pathology ----
# table_ap_template_surgical_pathology_24 <- table_ap_template %>%
#   filter(DIVISION == "SURGICAL PATHOLOGY" &
#            !SITE %in% c("NYEE","MSSN")) %>%
#   filter(TAB == "24 Hour Volume") %>%
#   select(-DIVISION,-TAB)
# 
# # 24 Hour Volume template Cytology ----
# table_ap_template_cytology_24 <- table_ap_template %>%
#   filter(DIVISION == "CYTOLOGY" &
#            !SITE %in% c("MSSN")) %>%
#   filter(TAB == "24 Hour Volume") %>%
#   select(-DIVISION,-TAB)
# 
# 
