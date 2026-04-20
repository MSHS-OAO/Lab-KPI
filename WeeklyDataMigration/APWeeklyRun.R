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
library(pool)
library(DBI)
library(odbc)
library(dbplyr)
library(glue)
library(assertr)
library(doParallel)
library(blastula)
library(readr)
library(zip)
library(here)


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

# Set working directory ------
dsn <- "OAO Cloud DB Production"
dsn_oracle <- paste0(dsn, " Oracle")

conn <- dbConnect(odbc(), dsn)
if ("Presidents" %in% list.files("/SharedDrive/")) {
  user_directory <- paste0("/SharedDrive/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab KPI/Data")
} else {
  user_directory <- paste0("/SharedDrive/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab KPI/Data")
}
# user_directory <- paste0("/Pathology/")
user_path <- paste0(user_directory, "\\*.*")

# Import analysis reference data
# reference_file <- paste0(user_directory,
#                          "/Code Reference/",
#                          "Analysis Reference 2025-02-05.xlsx")

# CP and Micro --------------------------------
# cp_scc_test_codes <- read_excel(reference_file, sheet = "SCC_TestCodes")
# cp_sun_test_codes <- read_excel(reference_file, sheet = "SUN_TestCodes")

cp_scc_test_codes <- tbl(conn,"LAB_KPI_SCC_TEST_CODES") %>%
  collect()
cp_sun_test_codes <- tbl(conn,"LAB_KPI_SUN_TEST_CODES") %>%
  collect()

# Create data frame of CP tests and divisions
cp_test_divisions <- 
  unique(
    rbind(unique(cp_scc_test_codes[, c("TEST", "DIVISION")]),
          unique(cp_sun_test_codes[, c("TEST", "DIVISION")])))

test_names <- cp_test_divisions$TEST

# cp_tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")
cp_tat_targets <- tbl(conn,"LAB_KPI_TURNAROUND_TARGETS") %>%
  collect()
#
# Add a column concatenating test, priority, and setting for matching later
cp_tat_targets <- cp_tat_targets %>%
  mutate(Concate = ifelse(
    PRIORITY == "All" & PT_SETTING == "All", paste(TEST, DIVISION),
    ifelse(PRIORITY != "All" & PT_SETTING == "All",
           paste(TEST, DIVISION, PRIORITY),
           paste(TEST, DIVISION, PRIORITY, PT_SETTING))))

# cp_scc_icu <- read_excel(reference_file, sheet = "SCC_ICU")
# cp_sun_icu <- read_excel(reference_file, sheet = "SUN_ICU")
cp_scc_icu <- tbl(conn,"LAB_KPI_SCC_ICU") %>%
  collect()
cp_sun_icu <-  tbl(conn,"LAB_KPI_SUN_ICU") %>%
  collect()

cp_scc_icu <- cp_scc_icu %>%
  mutate(SiteCodeName = paste(SITE, WARD, WARD_NAME))

cp_sun_icu <- cp_sun_icu %>%
  mutate(SiteCodeName = paste(SITE, LOC_CODE, LOC_NAME))

# cp_scc_setting <- read_excel(reference_file, sheet = "SCC_ClinicType")
# cp_sun_setting <- read_excel(reference_file, sheet = "SUN_LocType")
cp_scc_setting <- tbl(conn,"LAB_KPI_SCC_CLINICTYPE") %>%
  collect()
cp_sun_setting <- tbl(conn,"LAB_KPI_SUN_LOCTYPE") %>%
  collect()

# cp_mshs_sites <- read_excel(reference_file, sheet = "SiteNames")
cp_mshs_sites <- tbl(conn,"LAB_KPI_SITE_NAMES") %>%
  collect()

cp_mshs_sites <- cp_mshs_sites %>%
  rename(Site = SITE)

cp_micro_lab_order <- c("Troponin",
                        "Lactate WB",
                        "BUN",
                        "HGB",
                        "PT",
                        "Rapid Flu",
                        "C. diff",
                        "Antibody Screen")

all_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN", "RTC")
hosp_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN")
infusion_sites <- c("RTC")

pt_setting_order <- c("ED", "ICU", "IP Non-ICU", "Amb", "Other")
pt_setting_order2 <- c("ED & ICU", "IP Non-ICU", "Amb", "Other")
dashboard_pt_setting <- c("ED & ICU", "IP Non-ICU", "Amb")

dashboard_priority_order <- c("All", "Stat", "Routine")

cp_division_order <- c("Chemistry", "Hematology", "Microbiology RRL", "Infusion" ,"Blood Bank")

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
                                          !(DashboardPriority %in% c("All")))) |
        #Remove any All stratitifcation for Blood Bank 
        (Division %in% c("Blood Bank") & ((DashboardPriority %in% c("All")))), "Excl", "Incl")) %>%
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
                                          !(DashboardPriority %in% c("All")))) |
        #Remove any All stratitifcation for Blood Bank 
        (Division %in% c("Blood Bank") & ((DashboardPriority %in% c("All")))),"Excl", "Incl")) %>%
  filter(Incl == "Incl")

#-----------Patient Setting Excel File-----------#
#Using Rev Center to determine patient setting
# patient_setting <- data.frame(read_excel(reference_file,
#                                          sheet = "AP_Patient Setting"),
#                               stringsAsFactors = FALSE)
patient_setting <- tbl(conn,"LAB_KPI_AP_PATIENT_SETTING") %>%
  collect()

#-----------Anatomic Pathology Targets Excel File-----------#
# tat_targets_ap <- data.frame(read_excel(reference_file,
#                                         sheet = "AP_TAT Targets"),
#                              stringsAsFactors = FALSE)
tat_targets_ap <- tbl(conn,"LAB_KPI_AP_TAT_TARGETS") %>%
  collect()

#-----------GI Codes Excel File-----------#
#Upload the exclusion vs inclusion criteria associated with the GI codes
# gi_codes <- data.frame(read_excel(reference_file, sheet = "GI_Codes"),
#                        stringsAsFactors = FALSE)
gi_codes <-  tbl(conn,"LAB_KPI_GI_CODES") %>%
  collect()

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



# Import Powerpath file for cases signed out yesterday, if any exists
resulted_date <- yesterday

# Format yesterday's date for dashboard visualization
result_date_text <- format(resulted_date,
                           format = "%a %m/%d/%y")

# Create regular expressions for beginning each file type
pp_pattern_start <- "^(KPI REPORT - RAW DATA V4_V2){1}.*"
# Find Powerpath file for cases signed out yesterday
todays_date <- as.Date(Sys.Date(), format = "%Y-%m-%d")

pp_date_range <- seq(from = as.Date("2025-04-26")+1,
                      to = todays_date,
                      by = "day")


pattern_pp = paste0("^(KPI REPORT - RAW DATA V4_V2){1}.*",
                     pp_date_range,
                     ".xls",
                     collapse = "|")

file_list_pp <- list.files(
  path = paste0(user_directory, "/AP & Cytology Signed Cases Reports"),
  pattern = pattern_pp)




# Find Epic Cytology file for cases signed out yesterday
# epic_data_file <- list.files(
#   path = paste0(user_directory, "/EPIC Cytology"),
#   pattern = paste0(epic_pattern_start,
#                    "(",
#                    report_date_file_format,
#                    ".xlsx)$"),
#   ignore.case = TRUE)




# Processing Function ----
cyto_prep <- function(pp_data) {
  if (is.null(pp_data) ||  nrow(pp_data) == 0) {
    summarized_table <- NULL
    return(summarized_table)
  } else {
    
    pp_data <- pp_data %>%
      mutate(Case_no =  str_replace_all(Case_no,"\\*failed to decode utf16\\*",""))
    
    
    # Subset PowerPath data to keep Cyto Gyn and Cyto NonGyn and primary
    # specimens only
    cyto_data <- pp_data %>%
      filter(spec_group %in% c("CYTO NONGYN","CYTO GYN","BREAST"),
             spec_sort_order == 'A')
    
    cyto_data <- cyto_data %>%
      mutate(TEST_NAME =  case_when(Spec_code == 'BREA1' & spec_group == 'BREAST' ~ 'Breast Biopsy',
                                    spec_group == 'CYTO NONGYN' ~ 'Cytology NONGYN',
                                    spec_group == 'CYTO GYN' ~ 'Cytology GYN',
                                    TRUE ~ 'Breast Large/Resection'))
    
    
    # cyto_data <- merge(x = cyto_data, y = epic_data_specimens,
    #                     by.x = "Case_no",
    #                     by.y = "SPECIMEN_ID")
    
    
    
    # Crosswalk Rev_ctr and patient setting for PowerPath data
    cyto_data <- left_join(cyto_data,
                           patient_setting,
                           by = c('Rev_ctr' = 'REV_CTR'))
    
    # update sites/facilities
    cyto_data <- site_mapping(cyto_data)
    
    # Update MSB patient setting based on patient type column
    cyto_data <- cyto_data %>%
      mutate(PATIENT_SETTING = case_when(Rev_ctr == "MSBK" & (patient_type == "A" | patient_type == "O") ~ "Amb",
                                         Rev_ctr == "MSBK" &  patient_type == "IN" ~ "IP",
                                         TRUE ~ PATIENT_SETTING))
    
    # Crosswalk TAT targets based on spec_group and patient setting
    # cyto_data <- left_join(cyto_data,
    #                        tat_targets_ap,
    #                        by = c('spec_group' = 'SPEC_GROUP', 'PATIENT_SETTING' = 'PATIENT_SETTING'))
    
    #Change all Dates into POSIXct format to start the calculations
    cyto_data <- format_dates(cyto_data)
    
    # Add columns for turnaround time calculations:
    # Collection to signed out (in calendar days) and
    # received to signed out (in business days)
    cyto_data <- cyto_data %>%
      mutate(
        # Add column for collected to signed out turnaround time in calendar days
        Collection_to_signed_out =
          as.numeric(difftime(signed_out_date,Collection_Date,units="days")),
        # Add column for received to signed out turnaround time in business days
        Received_to_signed_out = bizdays(Received_Date, signed_out_date),
        # Prepare data for accessioned volume analysis
        # First find the date of the last weekday and add 1 for report date
        report_date_only = as.Date(signed_out_date) + 1,
        MONTH_ROLLUP = floor_date(signed_out_date, "month"),
        #  Find the accessioned date and use this for determining accessioned volume
        acc_date_only = as.Date(Received_Date),
        TAT_TARGET =  case_when(TEST_NAME == 'Breast Biopsy' ~ 3,
                                TEST_NAME == 'Breast Large/Resection' ~ 5,
                                TEST_NAME == 'Cytology GYN' ~ 8,
                                TRUE ~ 8),
        IS_TAT_WITHIN_TARGET =  if_else(Received_to_signed_out <= TAT_TARGET,1,0)) 
    
    cyto_data <- cyto_data %>%
      select(Facility,
             Priority,
             Spec_code,
             Specimen_description,
             CPT_code,
             Rev_ctr,
             Encounter_no,
             MRN,
             Case_no,
             Case_created_date,
             Collection_Date,
             Received_Date,
             signed_out_date,
             Refmd_code,
             spec_sort_order,
             TEST_NAME,
             PATIENT_SETTING,
             Collection_to_signed_out,
             Received_to_signed_out,
             TAT_TARGET,
             IS_TAT_WITHIN_TARGET,
             spec_group,
             Refmd_name)
    names(cyto_data) <- toupper(names(cyto_data))
    
    
    #summarize the data to be used for analysis and to be stored as historical
    #repo
    # summarized_table <- cyto_data %>%
    #   group_by(Spec_code,
    #            spec_group,
    #            Facility,
    #            Patient.Setting,
    #            Rev_ctr,
    #            as.Date(signed_out_date),
    #            weekdays(as.Date(signed_out_date)),
    #            Received.to.signed.out.target..Days.,
    #            Collected.to.signed.out.target..Days.,
    #            acc_date_only,
    #            weekdays(acc_date_only),
    #            report_date_only,
    #            weekdays(report_date_only)) %>%
    #   summarise(no_cases_signed = n(),
    #             lab_metric_tat_avg = round(mean(Received_to_signed_out,
    #                                             na.rm = TRUE), 0),
    #             lab_metric_tat_med = round(median(Received_to_signed_out,
    #                                               na.rm = TRUE), 0),
    #             lab_metric_tat_sd = round(sd(Received_to_signed_out, na.rm = TRUE), 1),
    #             lab_metric_within_target = as.numeric(format(
    #               round(
    #                 sum(Received_to_signed_out <= Received.to.signed.out.target..Days.,
    #                     na.rm = TRUE) / sum(
    #                       Received_to_signed_out >= 0, na.rm = TRUE), 2))),
    #             patient_metric_tat_avg = as.numeric(format(
    #               ceiling(mean(Collection_to_signed_out, na.rm = TRUE)))),
    #             patient_metric_tat_med = round(median(Collection_to_signed_out,
    #                                                   na.rm = TRUE), 0),
    #             patient_metric_tat_sd = round(sd(Collection_to_signed_out,
    #                                              na.rm = TRUE), 1),
    #             cyto_acc_vol = as.numeric(sum((report_date_only - 1) == acc_date_only,
    #                                           na.rm = TRUE)))
    
    
    
    return(cyto_data)
  }
}


site_mapping <- function(data){
  
  
  data <- data %>%
    mutate(Facility = case_when(Facility == "KH" ~ "MSB",
                                Facility == "R" ~ "MSW",
                                Facility == "STL" ~ "MSM",
                                Facility == "BIMC" ~ "MSBI",
                                Facility == "SNCH" ~ "MSSN",
                                Facility == "MSS" ~ "MSH",
                                Facility == "SL" ~ "MSM",
                                #Facility == "PACC" ~ "MS USQ",
                                TRUE ~ Facility ))
  
  
  
}


format_dates <- function(data){
  
  data[c("Case_created_date",
         "Collection_Date",
         "Received_Date",
         "signed_out_date")] <-
    lapply(data[c("Case_created_date",
                  "Collection_Date",
                  "Received_Date",
                  "signed_out_date")],
           as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p" ,origin = "1970-01-01")
  
  data
  
}



# Function to push data to database ----
get_values_updated <- function(x, columns,table_name){
  
  values <- glue("INTO \"{table_name}\" ({columns}) 
                 VALUES{x}")
  
  return(values)
}

write_temporary_table_to_database_and_merge_updated <- function(data, key_columns, destination_table_name, source_table_name, update_columns) {
  
  if(destination_table_name == 'LAB_KPI_PREPROCESSED_DAILY'){
    process_data <- data %>% mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, function(x) gsub("\'", "''", x)) %>%
      mutate_if(is.character, function(x) gsub("&", "' || chr(38) || '", x)) %>%
      mutate_if(is.character, function(x) paste0("'", x, "'")) %>%
      mutate_if(is.Date, function(x) paste0("TO_DATE('", x, "', 'YYYY-MM-DD')")) %>%
      mutate(ORDERTIME = paste0("TO_TIMESTAMP('", ORDERTIME, "', 'YYYY-MM-DD HH24:MI:SS')"),
             COLLECTTIME = paste0("TO_TIMESTAMP('", COLLECTTIME, "', 'YYYY-MM-DD HH24:MI:SS')"),
             RECEIVETIME = paste0("TO_TIMESTAMP('", RECEIVETIME, "', 'YYYY-MM-DD HH24:MI:SS')"),
             RESULTTIME = paste0("TO_TIMESTAMP('", RESULTTIME, "', 'YYYY-MM-DD HH24:MI:SS')")) %>%
      mutate(across(where(is.numeric),~ replace_na(as.character(.x), "''")))
    
  }else{
    process_data <- data %>% mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, function(x) gsub("\'", "''", x)) %>%
      mutate_if(is.character, function(x) gsub("&", "' || chr(38) || '", x)) %>%
      mutate_if(is.character, function(x) paste0("'", x, "'")) %>%
      mutate_if(is.Date, function(x) paste0("TO_DATE('", x, "', 'YYYY-MM-DD')")) %>%
      mutate(CASE_CREATED_DATE = paste0("TO_TIMESTAMP('", CASE_CREATED_DATE, "', 'YYYY-MM-DD HH24:MI:SS')"),
             COLLECTION_DATE = paste0("TO_TIMESTAMP('", COLLECTION_DATE, "', 'YYYY-MM-DD HH24:MI:SS')"),
             RECEIVED_DATE = paste0("TO_TIMESTAMP('", RECEIVED_DATE, "', 'YYYY-MM-DD HH24:MI:SS')"),
             SIGNED_OUT_DATE = paste0("TO_TIMESTAMP('", SIGNED_OUT_DATE, "', 'YYYY-MM-DD HH24:MI:SS')")) %>%
      mutate(across(where(is.numeric),~ replace_na(as.character(.x), "''")))
    
  }
  
  
  columns <- paste(colnames(process_data), collapse = ",")
  
  values <- process_data %>% mutate(values = paste0("(", col_concat(., sep = ","), ")")) %>%
    select(values)
  
  values$values <- gsub('NA', "", values$values)
  
  data_records <- split(values ,1:nrow(values))
  
  
  registerDoParallel()
  inserts <- foreach(record = data_records) %dopar% {
    tmp <- as.list(record)
    tmp <- as.character(tmp)
    tmp <- get_values_updated(tmp,columns = columns, table_name = source_table_name)
  }
  registerDoSEQ()
  
  chunk_length <- 500
  split_queries <- split(inserts, ceiling(seq_along(inserts)/chunk_length))
  
  
  split_queries_sql_statements <- list()
  for (i in 1:length(split_queries)) {
    row <- glue_collapse(split_queries[[i]], sep = "\n\n")
    # row <- gsub('NA', "", row)
    # row <- gsub("&", " ' || chr(38) || ' ", row)
    sql <- glue('INSERT ALL {row} SELECT 1 FROM DUAL;')
    split_queries_sql_statements <- append(split_queries_sql_statements, gsub("\\n", "", sql))
  }
  
  
  
  # glue statement for dropping table
  truncate_query <- glue('TRUNCATE TABLE "{source_table_name}";')
  
  #glue statement to copy empty table
  copy_table_query <- glue('CREATE TABLE {source_table_name}
                      AS
                      SELECT *
                      FROM {destination_table_name} WHERE 1=0;')
  
  #glue statement to drop table
  drop_query <- glue('DROP TABLE {source_table_name};')
  
  
  # Clear the staging data
  tryCatch({
    ch = dbConnect(odbc(), dsn)
    dbBegin(ch)
    if(dbExistsTable(ch,source_table_name)){
      dbExecute(ch,drop_query)
    }
    dbExecute(ch,copy_table_query)
    dbExecute(ch,truncate_query)
    
    dbCommit(ch)
    dbDisconnect(ch)
  },
  error = function(err){
    print(err)
    print("Table Copy Failed")
    dbRollback(ch)
    dbDisconnect(ch)
    
  })
  
  
  registerDoParallel()
  system.time(
    outputPar <- foreach(i = 1:length(split_queries_sql_statements), .packages = c("DBI", "odbc"))%dopar%{
      #Connecting to database through DBI
      ch = dbConnect(odbc(), dsn)
      #Test connection
      tryCatch({
        dbBegin(ch)
        dbExecute(ch, split_queries_sql_statements[[i]])
        dbCommit(ch)
      },
      error = function(err){
        #print("error2")split_queries_sql_statements[[1]]
        dbRollback(ch)
        dbDisconnect(ch)
        print("Staging Data Write Error")
      })
    }
  )
  registerDoSEQ()
  
  
  ###MErge Statement
  # key_columns <- c("FUNCTION", "CATEGORY", "SITE", "CC", "NAME", "EXPTYPE", "SUB_ACCOUNT", "SUB_ACCOUNT_DESCRIPTION", "SUPPLY_MAPPING_FILE_CATEGORY", "MONTH")
  merge_on_cols <- paste(paste0("DT.",key_columns, " = ST.",key_columns), sep="", collapse = ",")
  merge_on_cols <- gsub(',', " AND ", merge_on_cols)
  
  # update_columns <- c("SUM_OF_MONTH_BUDGET", "SUM_OF_MONTH_ACTUAL", "SUM_OF_YTD_BUDGET", "SUM_OF_YTD_ACTUAL", "SUM_OF_ANNUAL_BUDGET")
  update_on_cols <- paste(paste0("DT.",update_columns, " = ST.",update_columns), sep="", collapse = ",")
  
  
  get_dest_table_cols <-  paste(paste0("DT.",unique(c(key_columns,update_columns))), sep="", collapse = ",")
  get_source_table_values <- paste(paste0("ST.",unique(c(key_columns,update_columns))), sep="", collapse = ",")
  
  # destination_table_name <- "BSC_FINANCE_TABLE"
  # source_table_name <- "BSC_FINANCE_TABLE_TESTING"
  
  merge_query <- glue("MERGE INTO \"{destination_table_name}\" DT 
                        USING \"{source_table_name}\" ST 
                        ON ({merge_on_cols})
                      WHEN MATCHED THEN UPDATE SET
                      {update_on_cols}
                      WHEN NOT MATCHED THEN
                      INSERT ({get_dest_table_cols})
                      VALUES({get_source_table_values})
                      ")
  
  
  ch = dbConnect(odbc(), dsn_oracle)
  #Test connection
  tryCatch({
    dbBegin(ch)
    dbExecute(ch, merge_query)
    dbExecute(ch,drop_query)
    dbCommit(ch)
    dbDisconnect(ch)
    # print("success")
    print("Merge Succesfull")
    
  },
  error = function(err){
    #print("error")
    dbRollback(ch)
    dbDisconnect(ch)
    print("Merge Failed")
    
  })
  
}

# Process AP files and push the data to DB -----
for (file in file_list_pp){
  print(file)
  pp_data_raw <- read_excel(path =
                              paste0(user_directory,
                                     "/AP & Cytology Signed Cases Reports/",
                                     file),
                            skip = 1, 1)
  
  pp_data_raw <- data.frame(pp_data_raw[-nrow(pp_data_raw), ],
                            stringsAsFactors = FALSE)
  resulted_date <- as.Date(epic_extract_date) -1
  cyto_daily_preprocessed <- cyto_prep(pp_data_raw)
  key_cols <- c("CASE_NO","TEST_NAME", "SIGNED_OUT_DATE")
  update_cols <- names(cyto_daily_preprocessed)
  update_cols <- update_cols[! update_cols %in% key_cols]
  write_temporary_table_to_database_and_merge_updated(cyto_daily_preprocessed,
                                                      key_columns = key_cols,
                                                      update_columns = update_cols,
                                                      source_table_name="LAB_KPI_POWERPATH_CYTO_BIOPSY_ST",
                                                      destination_table_name="LAB_KPI_POWERPATH_CYTO_BIOPSY")
  
  
}
  
