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
library(kableExtra)
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)

#Clear existing history
rm(list = ls())
#-------------------------------holiday/weekend-------------------------------#
# Get today and yesterday's date
today <- as.Date("5/9/23", format("%m/%d/%y"))

#Determine if yesterday was a holiday/weekend
#get yesterday's DOW
yesterday <- today - 1

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

user_directory <- paste0(define_root_path(),
                         "HSPI-PM/",
                         "Operations Analytics and Optimization/Projects/",
                         "Service Lines/Lab Kpi/Data")

# Import analysis reference data
reference_file <- paste0(user_directory,
                         "/Code Reference/",
                         "Analysis Reference 2022-06-21.xlsx")

# CP and Micro --------------------------------
scc_test_code <- read_excel(reference_file, sheet = "SCC_TestCodes")
sun_test_code <- read_excel(reference_file, sheet = "SUN_TestCodes")

# Create data frame of CP tests and divisions
cp_test_divisions <- 
  unique(
    rbind(unique(scc_test_code[, c("Test", "Division")]),
          unique(sun_test_code[, c("Test", "Division")])))

tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")
#
# Add a column concatenating test, priority, and setting for matching later
tat_targets <- tat_targets %>%
  mutate(Concate = ifelse(
    Priority == "All" & `PtSetting` == "All", paste(Test, Division),
    ifelse(Priority != "All" & `PtSetting` == "All",
           paste(Test, Division, Priority),
           paste(Test, Division, Priority, `PtSetting`))))

scc_icu <- read_excel(reference_file, sheet = "SCC_ICU")
sun_icu <- read_excel(reference_file, sheet = "SUN_ICU")

scc_icu <- scc_icu %>%
  mutate(SiteCodeName = paste(Site, Ward, Ward_Name))

sun_icu <- sun_icu %>%
  mutate(SiteCodeName = paste(Site, LocCode, LocName))

scc_setting <- read_excel(reference_file, sheet = "SCC_ClinicType")
sun_setting <- read_excel(reference_file, sheet = "SUN_LocType")

mshs_site <- read_excel(reference_file, sheet = "SiteNames")

###code to be moved to the CP code
#scc_wday <- scc_weekday
#sun_wday <- sq_weekday

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

# Create template dataframes for combinations of tests, priority, and settings
# that will be used in TAT and volume look back tables. These templates ensure
# all relevant combinations are included in the tables regardless of resulted
# volume
# Create template data frames for combinations of tests, priority and settings
# that will be used in TAT tables and volume lookback tables
test_name_division <- unique(cp_test_divisions[, c("Division", "Test")])

test_names <- cp_test_divisions$Test

# Create data frame of test and site combinations
rep_test_site <- sort(rep(test_names, length(all_sites)))

rep_sites <- rep(all_sites, length(test_names))

test_site_comb <- data.frame("Test" = rep_test_site,
                             "Site" = rep_sites,
                             stringsAsFactors = FALSE)

# Create data frame of test and priority combinations
rep_test_priority <- sort(rep(test_names, length(dashboard_priority_order)))

rep_priority <- rep(dashboard_priority_order, length(test_names))

test_priority_comb <- data.frame("Test" = rep_test_priority,
                                 "DashboardPriority" = rep_priority,
                                 stringsAsFactors = FALSE)

# Create data frame of test and setting combinations for TAT tables
rep_test_setting_tat <- sort(rep(test_names, length(dashboard_pt_setting)))

rep_setting_tat <- rep(dashboard_pt_setting, length(test_names))

test_setting_comb_tat <- data.frame("Test" = rep_test_setting_tat,
                                    "DashboardSetting" = rep_setting_tat,
                                    stringsAsFactors = FALSE)

# Create data frame of test and setting combinations for volume lookback tables
rep_test_setting_vol <- sort(rep(test_names, length(pt_setting_order)))

rep_setting_vol <- rep(pt_setting_order, length(test_names))

test_setting_comb_vol <- data.frame("Test" = rep_test_setting_vol,
                                    "PtSetting" = rep_setting_vol,
                                    stringsAsFactors = FALSE)

# Combine data frames to create data frame with all combinations of tests,
# sites, priority, and settings for both TAT tables and lookback tables
test_site_prty <- left_join(test_site_comb,
                            test_priority_comb,
                            by = c("Test" = "Test"))

test_site_prty_setting_tat <- left_join(test_site_prty,
                                        test_setting_comb_tat,
                                        by = c("Test" = "Test"))

test_site_prty_setting_tat <- left_join(test_site_prty_setting_tat,
                                        cp_test_divisions,
                                        by = c("Test" = "Test"))

test_site_prty_setting_vol <- left_join(test_site_prty,
                                        test_setting_comb_vol,
                                        by = c("Test" = "Test"))

test_site_prty_setting_vol <- left_join(test_site_prty_setting_vol,
                                        cp_test_divisions,
                                        by = c("Test" = "Test"))

# Select applicable test, priority, setting combinations based on lab operations
tat_dashboard_templ <- test_site_prty_setting_tat %>%
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

vol_dashboard_templ <- test_site_prty_setting_vol %>%
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


#-----------Patient Setting Excel File-----------#
#Using Rev Center to determine patient setting
patient_setting <- data.frame(read_excel(reference_file,
                                         sheet = "AP_Patient Setting"),
                              stringsAsFactors = FALSE)

#-----------Anatomic Pathology Targets Excel File-----------#
tat_targets_ap <- data.frame(read_excel(reference_file,
                                        sheet = "AP_TAT Targets"),
                             stringsAsFactors = FALSE)

#-----------GI Codes Excel File-----------#
#Upload the exclusion vs inclusion criteria associated with the GI codes
gi_codes <- data.frame(read_excel(reference_file, sheet = "GI_Codes"),
                       stringsAsFactors = FALSE)

#-----------Create table template for Cyto/Path-----------#
#The reason behind the table templates is to make sure all the variables and
#patient settings are included

#Cyto
#this template for cyto is with an assumption that received to result is not
#centralized
table_temp_cyto <- data.frame(matrix(ncol = 19, nrow = 4))

colnames(table_temp_cyto) <- c("Spec_group", "Patient_setting",
                               "no_cases_signed",
                               "MSH.x", "BIMC.x", "MSQ.x", "NYEE.x",
                               "PACC.x", "R.x", "SL.x", "KH.x", "BIMC.y",
                               "MSH.y", "MSQ.y", "NYEE.y", "PACC.y", "R.y",
                               "SL.y", "KH.y")

table_temp_cyto[1] <- c("CYTO GYN", "CYTO GYN", "CYTO NONGYN", "CYTO NONGYN")
table_temp_cyto[2] <- c("IP", "Amb")

#this template for cyto is with an assumption that received to result is
#centralized
table_temp_cyto_v2 <- data.frame(matrix(ncol = 12, nrow = 4))

colnames(table_temp_cyto_v2) <- c("Spec_group", "Patient_setting",
                                  "no_cases_signed",
                                  "received_to_signed_out_within_target",
                                  "BIMC", "MSH", "MSQ", "NYEE", "PACC",
                                  "R", "SL", "KH")

table_temp_cyto_v2[1] <- c("CYTO GYN", "CYTO GYN", "CYTO NONGYN", "CYTO NONGYN")
table_temp_cyto_v2[2] <- c("IP", "Amb")

#this table template is for cytology volume
table_temp_cyto_vol <- data.frame(matrix(ncol = 10, nrow = 4))

colnames(table_temp_cyto_vol) <- c("Spec_group", "Patient_setting",
                                   "BIMC", "MSH", "MSQ", "NYEE", "PACC",
                                   "R", "SL", "KH")

table_temp_cyto_vol[1] <- c("CYTO GYN", "CYTO GYN",
                            "CYTO NONGYN", "CYTO NONGYN")
table_temp_cyto_vol[2] <- c("IP", "Amb")

#Patho
#this template for patho (sp) is with an assumption that received to result is
#not centralized
table_temp_patho <- data.frame(matrix(ncol = 17, nrow = 4))
colnames(table_temp_patho) <- c("Spec_group", "Patient_setting",
                                "no_cases_signed",
                                "MSH.x", "BIMC.x", "MSQ.x", "PACC.x",
                                "R.x", "SL.x", "KH.x", "BIMC.y", "MSH.y",
                                "MSQ.y", "PACC.y", "R.y", "SL.y", "KH.y")

table_temp_patho[1] <- c("Breast", "Breast", "GI", "GI")
table_temp_patho[2] <- c("IP", "Amb")

#this table template is for surgical pathology volume
table_temp_patho_vol <- data.frame(matrix(ncol = 9, nrow = 4))
colnames(table_temp_patho_vol) <- c("Spec_group", "Patient_setting",
                                    "BIMC", "MSH", "MSQ", "PACC",
                                    "R", "SL", "KH")

table_temp_patho_vol[1] <- c("Breast", "Breast", "GI", "GI")
table_temp_patho_vol[2] <- c("IP", "Amb")

sp_vol_column_order <- c("Spec_group", "Patient_setting",
                         "MSH", "MSQ", "BIMC", "PACC", "KH", "R", "SL")

cyto_vol_column_order <- c("Spec_group", "Patient_setting",
                           "MSH", "MSQ", "BIMC", "PACC", "KH", "R", "SL",
                           "NYEE")

sp_standardized_column_names <-
  c("Case Type", "Target", "Setting", "No. Cases Signed Out",
    "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL",
    "MSH", "MSQ", "MSBI", "PACC",
    "MSB", "MSW", "MSSL")

sp_vol_column_names <- c("Case Type", "Setting",
                         "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL")

cyto_vol_column_names <- c("Case Type", "Setting",
                           "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL",
                           "NYEE")
cyto_spec_group <- c("CYTO GYN", "CYTO NONGYN")
patho_spec_group <- c("Breast", "GI")