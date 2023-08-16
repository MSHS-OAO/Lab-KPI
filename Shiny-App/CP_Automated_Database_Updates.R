# Code for identifying missing dates of data and updating repositories -----

# Load packages ---------------
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

#Clear existing history
rm(list = ls())

# Define root path ---------------
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

today <- Sys.Date()

# OAO Cloud Database schema
oao_cloud_db <- "OAO Cloud DB Kate"

# CP Reference Tables -------------------
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

# Raw Data 60 Days ---------
## Query raw data repo -------
last_60_days <- data.frame(DATE = 
                         seq.Date(from = today - 60, to = today - 1, by = 1)
)

oao_personal_conn <- dbConnect(odbc(), oao_cloud_db)

scc_raw_data_repo_dates <-tbl(oao_personal_conn, "CP_RAW_DATA_60DAYS") %>%
  filter(SITE %in% c("MSQ", "MSH")) %>%
  select(RESULT_DATE) %>%
  distinct() %>%
  collect() %>%
  mutate(RESULT_DATE = as.Date(RESULT_DATE))

sun_raw_data_repo_dates <-tbl(oao_personal_conn, "CP_RAW_DATA_60DAYS") %>%
  filter(!(SITE %in% c("MSQ", "MSH"))) %>%
  select(RESULT_DATE) %>%
  distinct() %>%
  collect() %>%
  mutate(RESULT_DATE = as.Date(RESULT_DATE))

dbDisconnect(oao_personal_conn)

scc_missing_data <- last_60_days %>%
  mutate(MissingInDB = !(DATE %in% scc_raw_data_repo_dates$RESULT_DATE)) %>%
  filter(MissingInDB) %>%
  select(DATE)

# scc_missing_data <- scc_missing_data[1:3, ]
# scc_missing_data <- (scc_missing_data$DATE)

scc_folder <- paste0(user_directory,
                        "/SCC CP Reports")

missing_scc_files <- list.files(scc_folder,
                                pattern = paste0(
                                  "^(Doc).+(",
                                  paste0(scc_missing_data$DATE, collapse = "|"),
                                  ").xlsx$"))
# sun_missing_data <- sun_missing_data[1:3, ]  

sun_missing_data <- last_60_days %>%
  mutate(MissingInDB = !(DATE %in% sun_raw_data_repo_dates$RESULT_DATE)) %>%
  filter(MissingInDB) %>%
  select(DATE)

sun_folder <- paste0(user_directory, "/SUN CP Reports")

missing_sun_files <- list.files(sun_folder,
                                pattern = paste0(
                                  "^(KPI_Daily_TAT_Report_Updated )",
                                  paste0(sun_missing_data$DATE, collapse = "|"),
                                  ".xls$"))

if (length(missing_scc_files) > 0) {
  scc_raw_reports <- lapply(
    missing_scc_files, function(x) read_excel(
      path = paste0(scc_folder, "/", x)
    )
  )
} else {
  scc_raw_reports <- NULL
}

if (length(missing_sun_files) > 0) {
  sun_raw_reports <- lapply(
    missing_sun_files, function(x) 
      read_excel(
        path = paste0(sun_folder, "/", x),
        col_types = c("text", "text", "text", "text", "text",
                      "text", "text", "text", "text",
                      "numeric", "numeric", "numeric", "numeric", "numeric",
                      "text", "text", "text", "text", "text",
                      "text", "text", "text", "text", "text",
                      "text", "text", "text", "text", "text",
                      "text", "text", "text", "text", "text", "text")
      )
  )
} else {
  sun_raw_reports <- NULL
}

# Custom function for preprocessing SCC data ---------------------------------
preprocess_scc <- function(raw_scc)  {
  # Preprocess SCC data -------------------------------
  # Remove any duplicates
  raw_scc <- unique(raw_scc)
  # Correct and format any timestamps that were not imported correctly
  raw_scc[c("ORDERING_DATE",
            "COLLECTION_DATE",
            "RECEIVE_DATE",
            "VERIFIED_DATE")] <-
    lapply(raw_scc[c("ORDERING_DATE",
                     "COLLECTION_DATE",
                     "RECEIVE_DATE",
                     "VERIFIED_DATE")],
           function(x)
             ifelse(!is.na(x) & str_detect(x, "\\*.*\\*"),
                    str_replace(x, "\\*.*\\*", ""), x))
  
  raw_scc[c("ORDERING_DATE",
            "COLLECTION_DATE",
            "RECEIVE_DATE",
            "VERIFIED_DATE")] <-
    lapply(raw_scc[c("ORDERING_DATE",
                     "COLLECTION_DATE",
                     "RECEIVE_DATE",
                     "VERIFIED_DATE")],
           as.POSIXct, tz = "UTC",
           format = "%Y-%m-%d %H:%M:%OS",
           options(digits.sec = 1))
  
  # SCC lookup references ----------------------------------------------
  # Crosswalk in scope labs
  raw_scc <- left_join(raw_scc,
                       cp_scc_test_codes,
                       by = c("TEST_ID" = "SCC_TEST_ID"))
  
  # Determine if test is included based on crosswalk results
  raw_scc <- raw_scc %>%
    mutate(TestIncl = !is.na(TEST)) %>%
    filter(TestIncl)
  
  # Crosswalk unit type
  raw_scc <- left_join(raw_scc, cp_scc_setting,
                       by = c("CLINIC_TYPE" = "CLINIC_TYPE"))
  # Crosswalk site name
  raw_scc <- left_join(raw_scc, cp_mshs_sites,
                       by = c("SITE" = "DATA_SITE"))
  
  # Preprocess SCC data and add any necessary columns
  raw_scc <- raw_scc %>%
    mutate(
      # Subset HGB and BUN tests completed at RTC as a separate site since they
      # are processed at RTC
      Site = ifelse(TEST %in% c("HGB", "BUN") &
                      str_detect(replace_na(WARD_NAME, ""),
                                 "Ruttenberg Treatment Center"),
                    "RTC", Site),
      # Update division to Infusion for RTC
      DIVISION = ifelse(Site %in% c("RTC"), "Infusion", DIVISION),
      # Determine if unit is an ICU based on site mappings
      ICU = paste(Site, Ward, WARD_NAME) %in% cp_scc_icu$SiteCodeName,
      # Create a column for resulted date
      RESULT_DATE = date(VERIFIED_DATE),
      # Create master setting column to identify ICU and IP Non-ICU units
      DETAILED_SETTING = ifelse(SETTING_ROLL_UP == "IP" & ICU, "ICU",
                             ifelse(SETTING_ROLL_UP == "IP" & !ICU,
                                    "IP Non-ICU", SETTING_ROLL_UP)),
      # Create dashboard setting column to roll up master settings based on
      # desired dashboard grouping (ie, group ED and ICU together)
      DASHBOARD_SETTING = ifelse(DETAILED_SETTING %in% c("ED", "ICU"),
                                "ED & ICU", DETAILED_SETTING),
      # Create column with adjusted priority based on assumption that all ED and
      # ICU labs are treated as stat per operational leadership
      ADJ_PRIORITY = ifelse(DETAILED_SETTING %in% c("ED", "ICU") |
                             PRIORITY %in% "S", "Stat", "Routine"),
      # Calculate turnaround times
      COLLECT_TO_RECEIVE_TAT =
        as.numeric(RECEIVE_DATE - COLLECTION_DATE, units = "mins"),
      RECEIVE_TO_RESULT_TAT =
        as.numeric(VERIFIED_DATE - RECEIVE_DATE, units = "mins"),
      COLLECT_TO_RESULT_TAT =
        as.numeric(VERIFIED_DATE - COLLECTION_DATE, units = "mins"),
      #
      # Determine if order was an add on or original order based on time between
      # order and receive times
      ADD_ON_FINAL = ifelse(as.numeric(ORDERING_DATE - RECEIVE_DATE,
                                      units = "mins")
                           > 5, "AddOn", "Original"),
      # Determine if collection time is missing
      MISSING_COLLECT = COLLECT_TIME %in% RECEIVE_TIME, # COLLECT_TO_RECEIVE_TAT %in% 0,
      #
      # Determine TAT based on test, division, priority, and patient setting
      # Create column concatenating test and division to determine TAT targets
      Concate1 = paste(TEST, DIVISION),
      #
      # Create dashboard priority column
      DASHBOARD_PRIORITY = ifelse(
        cp_tat_targets$PRIORITY[match(
          Concate1, 
          paste(cp_tat_targets$TEST, cp_tat_targets$DIVISION))] == "All",
        "All", ADJ_PRIORITY),
      # Create column concatenating test, division, and priority to determine
      # TAT targets
      Concate2 = paste(TEST, DIVISION, DASHBOARD_PRIORITY),
      # Create column concatenating test, division, priority, and setting to
      # determine TAT targets
      Concate3 = paste(TEST, DIVISION, DASHBOARD_PRIORITY, DETAILED_SETTING),
      #
      # Determine Receive to Result TAT target using this logic:
      # 1. Try to match test, division, priority, and setting (applicable for
      # labs with different TAT targets based on patient setting and order priority)
      # 2. Try to match test, division, and priority (applicable for labs with
      # different TAT targets based on order priority)
      # 3. Try to match test and division - (applicable for labs with
      # TAT targets that are independent of patient setting or priority)
      #
      # Determine Receive to Result TAT target based on above logic/scenarios
      RECEIVE_RESULT_TARGET =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, cp_tat_targets$Concate)),
               cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                 match(Concate3, cp_tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, cp_tat_targets$Concate)),
                      cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate2, cp_tat_targets$Concate)],
                      # Match on scenario 3
                      cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate1, cp_tat_targets$Concate)])),
      #
      # Determine Collect to Result TAT target based on above logic/scenarios
      COLLECT_RESULT_TARGET =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, cp_tat_targets$Concate)),
               cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                 match(Concate3, cp_tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, cp_tat_targets$Concate)),
                      cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate2, cp_tat_targets$Concate)],
                      # Match on scenario 3
                      cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate1, cp_tat_targets$Concate)])),
      #
      # Determine if Receive to Result and Collect to Result TAT meet targets
      RECEIVE_RESULT_IN_TARGET = ifelse(is.na(RECEIVE_TO_RESULT_TAT), FALSE,
                                        RECEIVE_TO_RESULT_TAT <= RECEIVE_RESULT_TARGET),
      COLLECT_RESULT_IN_TARGET = ifelse(is.na(COLLECT_TO_RESULT_TAT), FALSE,
                                        COLLECT_TO_RESULT_TAT <= COLLECT_RESULT_TARGET),
      # Create column with patient name, order ID, test, collect, receive, and
      # result date and determine if there is a duplicate; order time excluded
      Concate4 = paste(LAST_NAME, FIRST_NAME,
                       ORDER_ID, TEST_NAME,
                       COLLECTION_DATE, RECEIVE_DATE, VERIFIED_DATE),
      DuplTest = duplicated(Concate4),
      # Determine whether or not to include this particular lab in TAT analysis
      # Exclusion criteria:
      # 1. Add on orders
      # 2. Orders from "Other" settings
      # 3. Orders with collect or receive times after result time
      # 4. Orders with missing collect, receive, or result timestamps
      # 5. Orders with missing collection times are excluded from
      # collect-to-result and collect-to-receive turnaround time analyis
      RECEIVE_TIME_TAT_INCL = ifelse(ADD_ON_FINAL == "AddOn" |
                                       DETAILED_SETTING == "Other" |
                                       COLLECT_TO_RECEIVE_TAT < 0 |
                                       COLLECT_TO_RESULT_TAT < 0 |
                                       RECEIVE_TO_RESULT_TAT < 0 |
                                       is.na(COLLECT_TO_RESULT_TAT) |
                                       is.na(RECEIVE_TO_RESULT_TAT), FALSE, TRUE),
      COLLECT_TIME_TAT_INCL = ifelse(MISSING_COLLECT |
                                        ADD_ON_FINAL == "AddOn" |
                                        DETAILED_SETTING == "Other" |
                                        COLLECT_TO_RECEIVE_TAT < 0 |
                                        COLLECT_TO_RESULT_TAT < 0 |
                                        RECEIVE_TO_RESULT_TAT < 0 |
                                        is.na(COLLECT_TO_RESULT_TAT) |
                                        is.na(RECEIVE_TO_RESULT_TAT), FALSE, TRUE))
  
  # Remove duplicate tests
  raw_scc <- raw_scc %>%
    filter(!DuplTest)
  
  # Determine volume of labs associated with each date and identify correct date
  scc_resulted_dates_vol <- raw_scc %>%
    group_by(RESULT_DATE) %>%
    summarize(VolLabs = n()) %>%
    arrange(desc(VolLabs)) %>%
    ungroup()
  
  scc_correct_date <- scc_resulted_dates_vol$RESULT_DATE[1]
  
  raw_scc <- raw_scc %>%
    filter(RESULT_DATE %in% scc_correct_date)
  
  # Select columns
  scc_processed <- raw_scc %>%
    select(Ward, WARD_NAME, ORDER_ID, `REQUESTING_DOC NAME`, MPI, `WORK SHIFT`,
           TEST_NAME, TEST, DIVISION, PRIORITY,
           Site, ICU, CLINIC_TYPE,
           SETTING, SETTING_ROLL_UP, DETAILED_SETTING, DASHBOARD_SETTING,
           ADJ_PRIORITY, DASHBOARD_PRIORITY,
           ORDERING_DATE, COLLECTION_DATE, RECEIVE_DATE, VERIFIED_DATE,
           RESULT_DATE,
           COLLECT_TO_RECEIVE_TAT, RECEIVE_TO_RESULT_TAT, COLLECT_TO_RESULT_TAT,
           ADD_ON_FINAL, MISSING_COLLECT,
           RECEIVE_RESULT_TARGET, COLLECT_RESULT_TARGET,
           RECEIVE_RESULT_IN_TARGET, COLLECT_RESULT_IN_TARGET,
           RECEIVE_TIME_TAT_INCL, COLLECT_TIME_TAT_INCL
    ) %>%
    rename(LOC_CODE = Ward,
           LOC_NAME = WARD_NAME,
           REQUEST_MD = `REQUESTING_DOC NAME`,
           MSMRN = MPI,
           WORK_SHIFT = `WORK SHIFT`,
           ORDER_PRIORITY = PRIORITY,
           SITE = Site,
           LOC_TYPE = CLINIC_TYPE,
           ORDER_TIME = ORDERING_DATE,
           COLLECT_TIME = COLLECTION_DATE,
           RECEIVE_TIME = RECEIVE_DATE,
           RESULT_TIME = VERIFIED_DATE)
  
  return(scc_processed)
  
}

# Custom function for preprocessing Sunquest data -----------------
preprocess_sun <- function(raw_sun) {
  
  # Preprocess Sunquest data --------------------------------
  # Remove any duplicates
  raw_sun <- unique(raw_sun)
  # Correct and format any timestamps that were not imported correctly
  raw_sun[c("OrderDateTime",
            "CollectDateTime",
            "ReceiveDateTime",
            "ResultDateTime")] <-
    lapply(raw_sun[c("OrderDateTime",
                     "CollectDateTime",
                     "ReceiveDateTime",
                     "ResultDateTime")],
           function(x) ifelse(!is.na(x) & str_detect(x, "\\*.*\\*")  == TRUE,
                              str_replace(x, "\\*.*\\*", ""), x))
  
  raw_sun[c("OrderDateTime",
            "CollectDateTime",
            "ReceiveDateTime",
            "ResultDateTime")] <-
    lapply(raw_sun[c("OrderDateTime",
                     "CollectDateTime",
                     "ReceiveDateTime",
                     "ResultDateTime")],
           as.POSIXct, tz = "UTC", format = "%m/%d/%Y %H:%M:%S")
  
  # Sunquest lookup references
  # Crosswalk labs included and remove out of scope labs
  raw_sun <- left_join(raw_sun, cp_sun_test_codes,
                       by = c("TestCode" = "SUN_TEST_CODE"))
  
  # Determine if test is included based on crosswalk results
  raw_sun <- raw_sun %>%
    mutate(TestIncl = !is.na(TEST)) %>%
    filter(TestIncl)
  
  
  # Crosswalk unit type
  raw_sun <- left_join(raw_sun, cp_sun_setting,
                       by = c("LocType" = "LOC_TYPE"))
  
  # Crosswalk site name
  raw_sun <- left_join(raw_sun, cp_mshs_sites,
                       by = c("HospCode" = "DATA_SITE"))
  
  # # Sunquest data formatting-----------------------------
  # Preprocess Sunquest data and add any necessary columns
  raw_sun <- raw_sun %>%
    mutate(
      # Determine if unit is an ICU based on site mappings
      ICU = paste(Site, LocCode, LocName) %in% cp_sun_icu$SiteCodeName,
      # Create a column for resulted date
      RESULT_DATE = as.Date(ResultDateTime, format = "%m/%d/%Y"),
      # Create master setting column to identify ICU and IP Non-ICU units
      DETAILED_SETTING = ifelse(SETTING_ROLL_UP == "IP" & ICU, "ICU",
                                ifelse(SETTING_ROLL_UP == "IP" & !ICU,
                                       "IP Non-ICU", SETTING_ROLL_UP)),
      # Create dashboard setting column to roll up master settings based on
      # desired dashboard grouping(ie, group ED and ICU together)
      DASHBOARD_SETTING = ifelse(DETAILED_SETTING %in% c("ED", "ICU"),
                                 "ED & ICU", DETAILED_SETTING),
      #
      # Create column with adjusted priority based on operational assumption
      # that all ED and ICU labs are treated as stat
      ADJ_PRIORITY = ifelse(DETAILED_SETTING %in% c("ED", "ICU") |
                             SpecimenPriority %in% "S", "Stat", "Routine"),
      #
      # Calculate turnaround times
      COLLECT_TO_RECEIVE_TAT =
        round(as.numeric(ReceiveDateTime - CollectDateTime, units = "mins"),
              digits = 3),
      RECEIVE_TO_RESULT_TAT =
        round(as.numeric(ResultDateTime - ReceiveDateTime, units = "mins"),
              digits = 3),
      COLLECT_TO_RESULT_TAT =
        round(as.numeric(ResultDateTime - CollectDateTime, units = "mins"),
              digits = 3),
      #
      # Determine if order was an add on or original order based on time between
      # order and receive times
      ADD_ON_FINAL = ifelse(as.numeric(OrderDateTime - ReceiveDateTime,
                                      units = "mins") > 5, "AddOn", "Original"),
      #
      # Determine if collection time is missing
      MISSING_COLLECT = CollectDateTime %in% OrderDateTime,
      #
      # Determine TAT target based on test, priority, and patient setting
      # Create column concatenating test and division to determine TAT targets
      Concate1 = paste(TEST, DIVISION),
      #
      # Create dashboard priority column
      DASHBOARD_PRIORITY = ifelse(
        cp_tat_targets$PRIORITY[match(
          Concate1,
          paste(cp_tat_targets$TEST, cp_tat_targets$DIVISION))] == "All",
        "All", ADJ_PRIORITY),
      # Create column concatenating test, division, and priority to determine
      # TAT targets
      Concate2 = paste(TEST, DIVISION, DASHBOARD_PRIORITY),
      # Create column concatenating test, division, priority, and setting to
      # determine TAT targets
      Concate3 = paste(TEST, DIVISION, DASHBOARD_PRIORITY, DETAILED_SETTING),
      #
      # Determine Receive to Result TAT target using this logic:
      # 1. Try to match test, priority, and setting (applicable for labs with
      # different TAT targets based on patient setting and order priority)
      # 2. Try to match test and priority (applicable for labs with different
      # TAT targets based on order priority)
      # 3. Try to match test - this is for tests with (applicable for labs with
      # TAT targets that are independent of patient setting or priority)
      #
      # Determine Receive to Result TAT target based on above logic/scenarios
      RECEIVE_RESULT_TARGET =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, cp_tat_targets$Concate)),
               cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                 match(Concate3, cp_tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, cp_tat_targets$Concate)),
                      cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate2, cp_tat_targets$Concate)],
                      # Match on scenario 3
                      cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate1, cp_tat_targets$Concate)])),
      #
      # Determine Collect to Result TAT target based on above logic/scenarios
      COLLECT_RESULT_TARGET =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, cp_tat_targets$Concate)),
               cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                 match(Concate3, cp_tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, cp_tat_targets$Concate)),
                      cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate2, cp_tat_targets$Concate)],
                      # Match on scenario 3
                      cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate1, cp_tat_targets$Concate)])),
      #
      # Determine if Receive to Result and Collect to Result TAT meet targets
      RECEIVE_RESULT_IN_TARGET = ifelse(is.na(RECEIVE_TO_RESULT_TAT), FALSE,
                                        RECEIVE_TO_RESULT_TAT <= RECEIVE_RESULT_TARGET),
      COLLECT_RESULT_IN_TARGET = ifelse(is.na(COLLECT_TO_RESULT_TAT), FALSE,
                                        COLLECT_TO_RESULT_TAT <= COLLECT_RESULT_TARGET),
      #
      # Create column with patient name, order ID, test, collect, receive, and
      # result date and determine if there is a duplicate; order time excluded
      Concate4 = paste(PtNumber, HISOrderNumber, TSTName,
                       CollectDateTime, ReceiveDateTime, ResultDateTime),
      DuplTest = duplicated(Concate4),
      #
      # Determine whether or not to include this particular lab in TAT analysis
      # Exclusion criteria:
      # 1. Add on orders
      # 2. Orders from "Other" settings
      # 3. Orders with collect or receive times after result time
      # 4. Orders with missing collect, receive, or result timestamps
      # 5. Orders with missing collection times are excluded from
      # collect-to-result and collect-to-receive turnaround time analyis
      RECEIVE_TIME_TAT_INCL = ifelse(ADD_ON_FINAL == "AddOn" |
                                       DETAILED_SETTING == "Other" |
                                       COLLECT_TO_RECEIVE_TAT < 0 |
                                       COLLECT_TO_RESULT_TAT < 0 |
                                       RECEIVE_TO_RESULT_TAT < 0 |
                                       is.na(COLLECT_TO_RESULT_TAT) |
                                       is.na(RECEIVE_TO_RESULT_TAT), FALSE, TRUE),
      COLLECT_TIME_TAT_INCL = ifelse(MISSING_COLLECT |
                                        ADD_ON_FINAL == "AddOn" |
                                        DETAILED_SETTING == "Other" |
                                        COLLECT_TO_RECEIVE_TAT < 0 |
                                        COLLECT_TO_RESULT_TAT < 0 |
                                        RECEIVE_TO_RESULT_TAT < 0 |
                                        is.na(COLLECT_TO_RESULT_TAT) |
                                        is.na(RECEIVE_TO_RESULT_TAT), FALSE, TRUE))
  
  # Remove duplicate tests
  raw_sun <- raw_sun %>%
    filter(!DuplTest)
  
  # Determine volume of labs associated with each date and identify correct date
  sun_resulted_dates_vol <- raw_sun %>%
    group_by(RESULT_DATE) %>%
    summarize(VolLabs = n()) %>%
    arrange(desc(VolLabs)) %>%
    ungroup()
  
  sun_correct_date <- sun_resulted_dates_vol$RESULT_DATE[1]
  
  raw_sun <- raw_sun %>%
    filter(RESULT_DATE %in% sun_correct_date)
  
  # Select columns
  sun_processed <- raw_sun %>%
    select(LocCode, LocName,
           HISOrderNumber, PhysName,
           PtNumber, SHIFT,
           TSTName, TEST, DIVISION, SpecimenPriority,
           Site, ICU, LocType,
           SETTING, SETTING_ROLL_UP,
           DETAILED_SETTING, DASHBOARD_SETTING,
           ADJ_PRIORITY, DASHBOARD_PRIORITY,
           OrderDateTime, CollectDateTime,
           ReceiveDateTime, ResultDateTime,
           RESULT_DATE,
           COLLECT_TO_RECEIVE_TAT, RECEIVE_TO_RESULT_TAT,
           COLLECT_TO_RESULT_TAT,
           ADD_ON_FINAL, MISSING_COLLECT,
           RECEIVE_RESULT_TARGET, COLLECT_RESULT_TARGET,
           RECEIVE_RESULT_IN_TARGET, COLLECT_RESULT_IN_TARGET,
           RECEIVE_TIME_TAT_INCL, COLLECT_TIME_TAT_INCL
    ) %>%
    rename(LOC_CODE = LocCode,
           LOC_NAME = LocName,
           ORDER_ID = HISOrderNumber,
           REQUEST_MD = PhysName,
           MSMRN = PtNumber,
           WORK_SHIFT = SHIFT,
           TEST_NAME = TSTName,
           ORDER_PRIORITY = SpecimenPriority,
           SITE = Site,
           LOC_TYPE = LocType,
           ORDER_TIME = OrderDateTime,
           COLLECT_TIME = CollectDateTime,
           RECEIVE_TIME = ReceiveDateTime,
           RESULT_TIME = ResultDateTime
    )
  
  return(sun_processed)
}

test_new_scc_function <- lapply(
  X = scc_raw_reports, FUN = preprocess_scc)

test_new_sun_function <- lapply(
  X = sun_raw_reports, FUN = preprocess_sun
)

test_scc_insert <- test_new_scc_function[[1]]
