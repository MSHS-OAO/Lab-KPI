#Install packages only the first time you run the code
#install.packages("timeDate")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("formattable")
#install.packages("bizdays")
#install.packages("rmarkdown")
#install.packages("stringr")
#install.packages("writexl")

#-------------------------------Required packages-----------------------------#

#Required packages: run these everytime you run the code
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
library(stringr)
library(writexl)
library(tidyr)

rm(list = ls())

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

# Import data for two scenarios - first time compiling repo and updating repo
initial_run <- TRUE

# Determine today's date to determine last possible data report
todays_date <- as.Date(Sys.Date(), format = "%Y-%m-%d")

# Determine date range for reports to include in repository
if (initial_run == TRUE) {
  # Provide start date for new data repository
  repo_start_date <- as.Date("2021-01-01", format = "%Y-%m-%d")
  # Create vector with date range for new data repository
  repo_date_range <- seq(from = repo_start_date + 1,
                         to = todays_date,
                         by = "day")
  scc_date_range <- repo_date_range
  sun_date_range <- repo_date_range
  
} else {
  # Import existing troponin historical repository file
  existing_repo <- read_excel(
    path = choose.files(default = paste0(user_directory,
                                         "/OI Data Exports/Troponin Repo/*.*"),
                        caption = "Select historical repository for troponin labs"))
  #
  # Find last date of resulted lab data in historical repo for SCC and Sunquest sites
  last_dates <- data.frame(
    "SCCSites" = as.Date(
      max(existing_repo[
        which(existing_repo$Site %in% c("MSH", "MSQ")), ]$ResultDate),
      format = "%Y-%m-%d"),
    "SunSites" = as.Date(
      max(existing_repo[
        which(!(existing_repo$Site %in% c("MSH", "MSQ"))), ]$ResultDate),
      format = "%Y-%m-%d"))
  # Create vector with possible data report dates for SCC and Sunquest sites
  scc_date_range <- seq(from = last_dates$SCCSites + 2,
                        to = todays_date,
                        by = "day")
  sun_date_range <- seq(from = last_dates$SunSites + 2,
                        to = todays_date,
                        by = "day")
}

# Find list of SCC data reports within date range
file_list_scc <- list.files(
  path = paste0(user_directory, "\\SCC CP Reports"),
  pattern = paste0("^(Doc){1}.+",
                   scc_date_range,
                   ".xlsx",
                   collapse = "|"))

# Find list of daily Sunquest data reports within date range
file_list_sun <- list.files(
  path = paste0(user_directory, "\\SUN CP Reports"),
  pattern = paste0("^(KPI_Daily_TAT_Report){1}.*",
                   sun_date_range,
                   ".xls", collapse = "|"))

# Read daily SCC reports from date range, if any exist --------
if (length(file_list_scc) > 0) {
  scc_raw_data_list <- lapply(
    file_list_scc, function(x) read_excel(
      path = paste0(user_directory, "\\SCC CP Reports\\", x)))
} else {
  scc_raw_data_list <- NULL
}

# Read daily Sunquest reports, if any exist --------
if (length(file_list_sun) > 0) {
  sun_raw_data_list <- lapply(
    file_list_sun, function(x) (read_excel(
      path = paste0(user_directory, "\\SUN CP Reports\\", x),
      col_types = c("text", "text", "text", "text", "text",
                    "text", "text", "text", "text",
                    "numeric", "numeric", "numeric", "numeric", "numeric",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text", "text"))))
} else {
  sun_raw_data_list <- NULL
}

# Import Clinical Pathology analysis reference data ---------------
reference_file <- paste0(user_directory,
                         "/Code Reference/",
                         "Analysis Reference 2022-06-21.xlsx")

scc_test_code <- read_excel(reference_file, sheet = "SCC_TestCodes")
sun_test_code <- read_excel(reference_file, sheet = "SUN_TestCodes")

tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")

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
                       scc_test_code,
                       by = c("TEST_ID" = "SCC_TestID"))
  
  # Determine if test is included based on crosswalk results
  raw_scc <- raw_scc %>%
    mutate(TestIncl = !is.na(Test)) %>%
    filter(TestIncl)
  
  # Crosswalk unit type
  raw_scc <- left_join(raw_scc, scc_setting,
                       by = c("CLINIC_TYPE" = "Clinic_Type"))
  # Crosswalk site name
  raw_scc <- left_join(raw_scc, mshs_site,
                       by = c("SITE" = "DataSite"))
  
  # Preprocess SCC data and add any necessary columns
  raw_scc <- raw_scc %>%
    mutate(
      # Subset HGB and BUN tests completed at RTC as a separate site since they
      # are processed at RTC
      Site = ifelse(Test %in% c("HGB", "BUN") &
                      str_detect(replace_na(WARD_NAME, ""),
                                 "Ruttenberg Treatment Center"),
                    "RTC", Site),
      # Update division to Infusion for RTC
      Division = ifelse(Site %in% c("RTC"), "Infusion", Division),
      # Determine if unit is an ICU based on site mappings
      ICU = paste(Site, Ward, WARD_NAME) %in% scc_icu$SiteCodeName,
      # Create a column for resulted date
      ResultedDate = date(VERIFIED_DATE),
      # Create master setting column to identify ICU and IP Non-ICU units
      MasterSetting = ifelse(SettingRollUp == "IP" & ICU, "ICU",
                             ifelse(SettingRollUp == "IP" & !ICU,
                                    "IP Non-ICU", SettingRollUp)),
      # Create dashboard setting column to roll up master settings based on
      # desired dashboard grouping (ie, group ED and ICU together)
      DashboardSetting = ifelse(MasterSetting %in% c("ED", "ICU"),
                                "ED & ICU", MasterSetting),
      # Create column with adjusted priority based on assumption that all ED and
      # ICU labs are treated as stat per operational leadership
      AdjPriority = ifelse(MasterSetting %in% c("ED", "ICU") |
                             PRIORITY %in% "S", "Stat", "Routine"),
      # Calculate turnaround times
      CollectToReceive =
        as.numeric(RECEIVE_DATE - COLLECTION_DATE, units = "mins"),
      ReceiveToResult =
        as.numeric(VERIFIED_DATE - RECEIVE_DATE, units = "mins"),
      CollectToResult =
        as.numeric(VERIFIED_DATE - COLLECTION_DATE, units = "mins"),
      #
      # Determine if order was an add on or original order based on time between
      # order and receive times
      AddOnMaster = ifelse(as.numeric(ORDERING_DATE - RECEIVE_DATE,
                                      units = "mins")
                           > 5, "AddOn", "Original"),
      # Determine if collection time is missing
      MissingCollect = CollectToReceive == 0,
      #
      # Determine TAT based on test, division, priority, and patient setting
      # Create column concatenating test and division to determine TAT targets
      Concate1 = paste(Test, Division),
      #
      # Create dashboard priority column
      DashboardPriority = ifelse(
        tat_targets$Priority[match(
          Concate1, 
          paste(tat_targets$Test, tat_targets$Division))] == "All",
        "All", AdjPriority),
      # Create column concatenating test, division, and priority to determine
      # TAT targets
      Concate2 = paste(Test, Division, DashboardPriority),
      # Create column concatenating test, division, priority, and setting to
      # determine TAT targets
      Concate3 = paste(Test, Division, DashboardPriority, MasterSetting),
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
      ReceiveResultTarget =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, tat_targets$Concate)),
               tat_targets$ReceiveToResultTarget[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$ReceiveToResultTarget[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$ReceiveToResultTarget[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine Collect to Result TAT target based on above logic/scenarios
      CollectResultTarget =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, tat_targets$Concate)),
               tat_targets$CollectToResultTarget[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$CollectToResultTarget[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$CollectToResultTarget[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine if Receive to Result and Collect to Result TAT meet targets
      ReceiveResultInTarget = ReceiveToResult <= ReceiveResultTarget,
      CollectResultInTarget = CollectToResult <= CollectResultTarget,
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
      TATInclude = ifelse(AddOnMaster == "AddOn" |
                            MasterSetting == "Other" |
                            CollectToReceive < 0 |
                            CollectToResult < 0 |
                            ReceiveToResult < 0 |
                            is.na(CollectToResult) |
                            is.na(ReceiveToResult), FALSE, TRUE))
  
  # Remove duplicate tests
  raw_scc <- raw_scc %>%
    filter(!DuplTest)
  
  # Select columns
  scc_master <- raw_scc[, c("Ward", "WARD_NAME",
                            "ORDER_ID", "REQUESTING_DOC NAME",
                            "MPI", "WORK SHIFT",
                            "TEST_NAME", "Test", "Division", "PRIORITY",
                            "Site", "ICU", "CLINIC_TYPE",
                            "Setting", "SettingRollUp",
                            "MasterSetting", "DashboardSetting",
                            "AdjPriority", "DashboardPriority",
                            "ORDERING_DATE", "COLLECTION_DATE",
                            "RECEIVE_DATE", "VERIFIED_DATE",
                            "ResultedDate",
                            "CollectToReceive", "ReceiveToResult",
                            "CollectToResult",
                            "AddOnMaster", "MissingCollect",
                            "ReceiveResultTarget", "CollectResultTarget",
                            "ReceiveResultInTarget", "CollectResultInTarget",
                            "TATInclude")]
  # Rename columns
  colnames(scc_master) <- c("LocCode", "LocName",
                            "OrderID", "RequestMD",
                            "MSMRN", "WorkShift",
                            "TestName", "Test", "Division", "OrderPriority",
                            "Site", "ICU", "LocType",
                            "Setting", "SettingRollUp",
                            "MasterSetting", "DashboardSetting",
                            "AdjPriority", "DashboardPriority",
                            "OrderTime", "CollectTime",
                            "ReceiveTime", "ResultTime",
                            "ResultDate",
                            "CollectToReceiveTAT", "ReceiveToResultTAT",
                            "CollectToResultTAT",
                            "AddOnMaster", "MissingCollect",
                            "ReceiveResultTarget", "CollectResultTarget",
                            "ReceiveResultInTarget", "CollectResultInTarget",
                            "TATInclude")
  
  
  scc_daily_list <- list(raw_scc, scc_master)
  
}


# Custom function for preprocessing Sunquest data -----------------
preprocess_daily_sun <- function(raw_sun) {
  
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
  raw_sun <- left_join(raw_sun, sun_test_code,
                       by = c("TestCode" = "SUN_TestCode"))
  
  # Determine if test is included based on crosswalk results
  raw_sun <- raw_sun %>%
    mutate(TestIncl = !is.na(Test)) %>%
    filter(TestIncl)
  
  
  # Crosswalk unit type
  raw_sun <- left_join(raw_sun, sun_setting,
                       by = c("LocType" = "LocType"))
  
  # Crosswalk site name
  raw_sun <- left_join(raw_sun, mshs_site,
                       by = c("HospCode" = "DataSite"))
  
  # # Sunquest data formatting-----------------------------
  # Preprocess Sunquest data and add any necessary columns
  raw_sun <- raw_sun %>%
    mutate(
      # Determine if unit is an ICU based on site mappings
      ICU = paste(Site, LocCode, LocName) %in% sun_icu$SiteCodeName,
      # Create a column for resulted date
      ResultedDate = as.Date(ResultDateTime, format = "%m/%d/%Y"),
      # Create master setting column to identify ICU and IP Non-ICU units
      MasterSetting = ifelse(SettingRollUp == "IP" & ICU, "ICU",
                             ifelse(SettingRollUp == "IP" & !ICU,
                                    "IP Non-ICU", SettingRollUp)),
      # Create dashboard setting column to roll up master settings based on
      # desired dashboard grouping(ie, group ED and ICU together)
      DashboardSetting = ifelse(MasterSetting %in% c("ED", "ICU"), "ED & ICU",
                                MasterSetting),
      #
      # Create column with adjusted priority based on operational assumption
      # that all ED and ICU labs are treated as stat
      AdjPriority = ifelse(MasterSetting %in% c("ED", "ICU") |
                             SpecimenPriority %in% "S", "Stat", "Routine"),
      #
      # Calculate turnaround times
      CollectToReceive =
        as.numeric(ReceiveDateTime - CollectDateTime, units = "mins"),
      ReceiveToResult =
        as.numeric(ResultDateTime - ReceiveDateTime, units = "mins"),
      CollectToResult =
        as.numeric(ResultDateTime - CollectDateTime, units = "mins"),
      #
      # Determine if order was an add on or original order based on time between
      # order and receive times
      AddOnMaster = ifelse(as.numeric(OrderDateTime - ReceiveDateTime,
                                      units = "mins") > 5, "AddOn", "Original"),
      #
      # Determine if collection time is missing
      MissingCollect = CollectDateTime == OrderDateTime,
      #
      # Determine TAT target based on test, priority, and patient setting
      # Create column concatenating test and division to determine TAT targets
      Concate1 = paste(Test, Division),
      #
      # Create dashboard priority column
      DashboardPriority = ifelse(
        tat_targets$Priority[match(
          Concate1,
          paste(tat_targets$Test, tat_targets$Division))] == "All",
        "All", AdjPriority),
      # Create column concatenating test, division, and priority to determine
      # TAT targets
      Concate2 = paste(Test, Division, DashboardPriority),
      # Create column concatenating test, division, priority, and setting to
      # determine TAT targets
      Concate3 = paste(Test, Division, DashboardPriority, MasterSetting),
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
      ReceiveResultTarget =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, tat_targets$Concate)),
               tat_targets$ReceiveToResultTarget[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$ReceiveToResultTarget[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$ReceiveToResultTarget[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine Collect to Result TAT target based on above logic/scenarios
      CollectResultTarget =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, tat_targets$Concate)),
               tat_targets$CollectToResultTarget[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$CollectToResultTarget[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$CollectToResultTarget[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine if Receive to Result and Collect to Result TAT meet targets
      ReceiveResultInTarget = ReceiveToResult <= ReceiveResultTarget,
      CollectResultInTarget = CollectToResult <= CollectResultTarget,
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
      TATInclude = ifelse(AddOnMaster == "AddOn" |
                            MasterSetting == "Other" |
                            CollectToReceive < 0 |
                            CollectToResult < 0 |
                            ReceiveToResult < 0 |
                            is.na(CollectToResult) |
                            is.na(ReceiveToResult), FALSE, TRUE))
  
  # Remove duplicate tests
  raw_sun <- raw_sun %>%
    filter(!DuplTest)
  
  # Select columns
  sun_master <- raw_sun[, c("LocCode", "LocName",
                            "HISOrderNumber", "PhysName",
                            "PtNumber", "SHIFT",
                            "TSTName", "Test", "Division", "SpecimenPriority",
                            "Site", "ICU", "LocType",
                            "Setting", "SettingRollUp",
                            "MasterSetting", "DashboardSetting",
                            "AdjPriority", "DashboardPriority",
                            "OrderDateTime", "CollectDateTime",
                            "ReceiveDateTime", "ResultDateTime",
                            "ResultedDate",
                            "CollecttoReceive", "ReceivetoResult",
                            "CollecttoResult",
                            "AddOnMaster", "MissingCollect",
                            "ReceiveResultTarget", "CollectResultTarget",
                            "ReceiveResultInTarget", "CollectResultInTarget",
                            "TATInclude")]
  
  colnames(sun_master) <- c("LocCode", "LocName",
                            "OrderID", "RequestMD",
                            "MSMRN", "WorkShift",
                            "TestName", "Test", "Division", "OrderPriority",
                            "Site", "ICU", "LocType",
                            "Setting", "SettingRollUp",
                            "MasterSetting", "DashboardSetting",
                            "AdjPriority", "DashboardPriority",
                            "OrderTime", "CollectTime",
                            "ReceiveTime", "ResultTime",
                            "ResultDate",
                            "CollectToReceiveTAT", "ReceiveToResultTAT",
                            "CollectToResultTAT",
                            "AddOnMaster", "MissingCollect",
                            "ReceiveResultTarget", "CollectResultTarget",
                            "ReceiveResultInTarget", "CollectResultInTarget",
                            "TATInclude")
  
  sun_daily_list <- list(raw_sun, sun_master)
  
}

# Custom function to determine resulted lab date from preprocessed data -------
# SCC data often has a few labs with incorrect result date
correct_result_dates <- function(data, number_days) {
  all_resulted_dates_vol <- data %>%
    group_by(ResultDate) %>%
    summarize(VolLabs = n()) %>%
    arrange(desc(VolLabs)) %>%
    ungroup()
  
  
  correct_dates <- all_resulted_dates_vol$ResultDate[1:number_days]
  
  new_data <- data %>%
    filter(ResultDate %in% correct_dates)
  return(new_data)
}


# Compile processed SCC daily data ------------------------------
if (!is.null(scc_raw_data_list)) {
  # Preprocess SCC daily raw data using custom function
  scc_daily_preprocessed <- lapply(scc_raw_data_list,
                                   preprocess_scc)
  # Select the second element of the list of lists
  scc_preprocessed_data <- lapply(scc_daily_preprocessed, function(x) x[[2]])
  
  # Remove any labs with incorrect dates then bind daily reports into one data frame
  scc_preprocessed_data <- lapply(scc_preprocessed_data, 
                                  function(x) correct_result_dates(x, number_days = 1))
  
  # Bind all data together
  scc_daily_bind <- data.frame(bind_rows(scc_preprocessed_data))
  
} else {
  scc_daily_bind <- NULL
}


# Compile preprocessed Sunquest daily data, if any exists ---------------
if (!is.null(sun_raw_data_list)) {
  # Preprocess Sunquest daily raw data using custom function
  sun_daily_preprocessed <- lapply(sun_raw_data_list,
                                   preprocess_daily_sun)
  # Select the second element of the list of lists
  sun_preprocessed_data <- lapply(sun_daily_preprocessed, function(x) x[[2]])
  
  # Bind daily reports into one data frame
  sun_daily_bind <- data.frame(bind_rows(sun_preprocessed_data))
  
} else {
  sun_daily_bind <- NULL
}

# Bind together all SCC and Sunquest data --------------------------------------
bind_all_data <- rbind(sun_daily_bind, scc_daily_bind)

new_troponin_data <- bind_all_data %>%
  filter(Test %in% c("Troponin"))

# Combine new troponin data with historical repository
if (initial_run == TRUE) {
  troponin_repo <- new_troponin_data
} else {
  # Convert ResultDate from date-time to date
  existing_repo <- existing_repo %>%
    mutate(ResultDate = date(ResultDate)) %>%
    filter(!(ResultDate %in% new_troponin_data$ResultDate))
  # 
  # Bind new data with repository
  troponin_repo <- rbind(existing_repo, new_troponin_data)
}

# Export repository to file
start_date <- format(min(troponin_repo$ResultDate), "%m-%d-%y")
end_date <- format(max(troponin_repo$ResultDate), "%m-%d-%y")

write_xlsx(troponin_repo,
           paste0(user_directory,
                  "/OI Data Exports/Troponin Repo",
                  "/Troponin Labs ",
                  start_date,
                  " to ",
                  end_date,
                  " Created ",
                  format(Sys.Date(), "%m-%d-%y"),
                  ".xlsx"))


new_excel_import <- read_excel(path = choose.files(default = paste0(user_directory,
                                                                    "/OI Data Exports/Troponin Repo/*.*"),
                                                   caption = "Select historical repository for troponin labs"))
