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
library(kableExtra, "~/R/x86_64-pc-linux-gnu-library/4.2")
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)

#-------------------------------holiday/weekend-------------------------------#
# Get today and yesterday's date
today <- Sys.Date()
# today <- as.Date("9/21/2023", format = "%m/%d/%Y")

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
holiday_det <- isHoliday(as.timeDate(yesterday), holidays = mshs_holiday)

#Set up a calendar for collect to received TAT calculations for Path & Cyto
create.calendar("MSHS_working_days", mshs_holiday,
                weekdays = c("saturday", "sunday"))
bizdays.options$set(default.calendar = "MSHS_working_days")


user_directory <- paste0("/Pathology/")

# Import analysis reference data
reference_file <- paste0(user_directory,
                         "/Code Reference/",
                         "Analysis Reference 2023-09-15.xlsx")

# CP and Micro --------------------------------
cp_scc_test_codes <- read_excel(reference_file, sheet = "SCC_TestCodes")
cp_sun_test_codes <- read_excel(reference_file, sheet = "SUN_TestCodes")

# Create data frame of CP tests and divisions
cp_test_divisions <- 
  unique(
    rbind(unique(cp_scc_test_codes[, c("TEST", "DIVISION")]),
          unique(cp_sun_test_codes[, c("TEST", "DIVISION")])))

test_names <- cp_test_divisions$TEST

cp_tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")
#
# Add a column concatenating test, priority, and setting for matching later
cp_tat_targets <- cp_tat_targets %>%
  mutate(Concate = ifelse(
    PRIORITY == "All" & PT_SETTING == "All", paste(TEST, DIVISION),
    ifelse(PRIORITY != "All" & PT_SETTING == "All",
           paste(TEST, DIVISION, PRIORITY),
           paste(TEST, DIVISION, PRIORITY, PT_SETTING))))

cp_scc_icu <- read_excel(reference_file, sheet = "SCC_ICU")
cp_sun_icu <- read_excel(reference_file, sheet = "SUN_ICU")

cp_scc_icu <- cp_scc_icu %>%
  mutate(SiteCodeName = paste(SITE, WARD, WARD_NAME))

cp_sun_icu <- cp_sun_icu %>%
  mutate(SiteCodeName = paste(SITE, LOC_CODE, LOC_NAME))

cp_scc_setting <- read_excel(reference_file, sheet = "SCC_ClinicType")
cp_sun_setting <- read_excel(reference_file, sheet = "SUN_LocType")

cp_mshs_sites <- read_excel(reference_file, sheet = "SiteNames")

cp_mshs_sites <- cp_mshs_sites %>%
  rename(Site = SITE)

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

SPEC_GROUP <- c("Breast", "GI","CYTO GYN", "CYTO NONGYN")
PATIENT_SETTING <- c("IP", "Amb")
SITES <- c("MSB","MSBI","PACC","MSW","MSH","MSM","MSSN","MSQ","NYEE")
TAB <- c("Efficiency Indicators","24 Hour Volume")
METRIC <- c("received_to_signed_out_within_target",
            "avg_collection_to_signed_out",
            "no_cases_signed")

table_ap_template <- expand.grid(SPEC_GROUP, PATIENT_SETTING,SITES,TAB,METRIC)

colnames(table_ap_template) <- c("SPECIMEN_GROUP", "PATIENT_SETTING","SITE","TAB","METRIC")

table_ap_template <- table_ap_template %>%
  mutate(DIVISION = case_when(SPECIMEN_GROUP %in% c("Breast", "GI") ~ "SURGICAL PATHOLOGY",
                              SPECIMEN_GROUP %in% c("CYTO GYN", "CYTO NONGYN") ~ "CYTOLOGY"))


table_ap_template <- table_ap_template %>%
  filter(!((DIVISION == "SURGICAL PATHOLOGY") & 
             (SPECIMEN_GROUP %in% c("CYTO GYN", "CYTO NONGYN"))))

table_ap_template <- table_ap_template %>%
  filter(!((DIVISION == "CYTOLOGY") & 
             (SPECIMEN_GROUP %in% c("Breast", "GI"))))



table_ap_template <- table_ap_template %>%
  filter(!((DIVISION == "CYTOLOGY") & 
             (TAB == "Efficiency Indicators") & 
             (METRIC %in% c("received_to_signed_out_within_target",
                            "no_cases_signed"))))

table_ap_template <- table_ap_template %>%
  filter(!((DIVISION == "SURGICAL PATHOLOGY") & 
             (TAB == "Efficiency Indicators") & 
             (METRIC == "no_cases_signed")))


table_ap_template <- table_ap_template %>%
  filter(!((DIVISION %in% c("SURGICAL PATHOLOGY","CYTOLOGY")) & 
             (TAB == "24 Hour Volume") & 
             (METRIC %in% c("received_to_signed_out_within_target",
                            "avg_collection_to_signed_out"))))

# Efficiency Indicators template Pathology ----
table_ap_template_surgical_pathology <- table_ap_template %>%
  filter(DIVISION == "SURGICAL PATHOLOGY" &
           !SITE %in% c("NYEE","MSSN")) %>%
  filter(TAB == "Efficiency Indicators") %>%
  select(-DIVISION,-TAB)

# Efficiency Indicators template Cytology ----
table_ap_template_cytology <- table_ap_template %>%
  filter(DIVISION == "CYTOLOGY" &
           !SITE %in% c("MSSN")) %>%
  filter(TAB == "Efficiency Indicators") %>%
  select(-DIVISION,-TAB)


# 24 Hour Volume template Pathology ----
table_ap_template_surgical_pathology_24 <- table_ap_template %>%
  filter(DIVISION == "SURGICAL PATHOLOGY" &
           !SITE %in% c("NYEE","MSSN")) %>%
  filter(TAB == "24 Hour Volume") %>%
  select(-DIVISION,-TAB)

# 24 Hour Volume template Cytology ----
table_ap_template_cytology_24 <- table_ap_template %>%
  filter(DIVISION == "CYTOLOGY" &
           !SITE %in% c("MSSN")) %>%
  filter(TAB == "24 Hour Volume") %>%
  select(-DIVISION,-TAB)


# Creating Mapping Template for AP Display Backlog ----
SPEC_GROUP <- c("CYTO GYN", "CYTO NONGYN")
METRIC <- c("total_accessioned_volume",
            "cyto_backlog",
            "percentile_25th",
            "percentile_50th",
            "maximum")

table_backlog_template <- expand.grid(SPEC_GROUP,METRIC)


colnames(table_backlog_template) <- c("Spec_group",
                                      "METRIC")
sp_vol_column_names <- c("Case Type",
                         "Setting",
                         "MSH",
                         "MSQ",
                         "MSBI",
                         "PACC",
                         "MSB",
                         "MSW",
                         "MSM")

cyto_vol_column_names <- c("Case Type",
                           "Setting",
                           "MSH",
                           "MSQ",
                           "MSBI",
                           "PACC",
                           "MSB",
                           "MSW",
                           "MSM",
                           "NYEE")