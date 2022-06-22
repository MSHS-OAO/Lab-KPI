# This code is used to create and maintain a data repository with:
# 1. Raw data for in scope analytes for 60 days
# 2. Daily summary based on dashboard stratifications
# 3. Weekly summary based on dashboard stratifications
# 4. Monthly summary based on dashboard stratifications

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
#install.packages("tidyr")

#-------------------------------Required packages-------------------------------#

#Required packages: run these every time you run the code
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

# Set working directory -------------------------------

if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}

user_path <- paste0(user_directory, "\\*.*")

# Import data for two scenarios - first time compiling repo and updating repo ----------
initial_run <- FALSE

# Determine today's date to determine last possible data report
todays_date <- as.Date(Sys.Date(), format = "%Y-%m-%d")
this_month <- month(todays_date)

# Determine date range for reports to include in repository
if (initial_run == TRUE) {
  # Provide start date for new data repository
  # repo_start_date <- as.Date(paste0(this_month, "/",
  #                                   1, "/",
  #                                   year(Sys.Date())), format = "%m/%d/%Y")
  # repo_start_date <- todays_date - 60
  repo_start_date <- as.Date("12/1/2020", format = "%m/%d/%Y")
  # Create vector with date range for new data repository
  repo_date_range <- seq(from = repo_start_date + 1,
                         to = todays_date,
                         by = "day")
  scc_date_range <- repo_date_range
  sun_date_range <- repo_date_range
  
} else {
  # Import existing historical repositories
  
  # Import raw data repository by finding file with the latest creation time
  raw_data_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/RawDataRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  raw_data_repo_files <- raw_data_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_raw_data_repo <- raw_data_repo_files[1, "FileName"]
  
  raw_data_repo <- readRDS(latest_raw_data_repo)
  
  # Import daily summary repository by finding file with the latest creation time
  daily_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/DailyRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  daily_repo_files <- daily_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_daily_repo <- daily_repo_files[1, "FileName"]
  
  daily_repo <- readRDS(latest_daily_repo)
  
  # Import weekly summary repository by finding file with the latest creation time
  weekly_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/WeeklyRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  weekly_repo_files <- weekly_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_weekly_repo <- weekly_repo_files[1, "FileName"]
  
  weekly_repo <- readRDS(latest_weekly_repo)
  
  # Import monthly summary repository by finding file with the latest creation time
  monthly_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/MonthlyRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  monthly_repo_files <- monthly_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_monthly_repo <- monthly_repo_files[1, "FileName"]
  
  monthly_repo <- readRDS(latest_monthly_repo)

  #
  # Find last date of resulted lab data in historical repositories for
  # SCC and Sunquest sites
  last_dates <- data.frame(
    "SCCSites" = as.Date(
      max(raw_data_repo[
        which(raw_data_repo$Site %in% c("MSH", "MSQ")), ]$ResultDate),
      format = "%Y-%m-%d"),
    "SunSites" = as.Date(
      max(raw_data_repo[
        which(!(raw_data_repo$Site %in% c("MSH", "MSQ"))), ]$ResultDate),
      format = "%Y-%m-%d"))
  # Create vector with possible data report dates for SCC and Sunquest sites
  if (last_dates$SCCSites + 2 > todays_date) {
    scc_date_range <- seq(from = todays_date,
                          to = todays_date,
                          by = "day")
  } else {
    scc_date_range <- seq(from = last_dates$SCCSites + 2,
                          to = todays_date,
                          by = "day")
  }
  
  if (last_dates$SunSites + 2 > todays_date) {
    sun_date_range <- seq(from = todays_date,
                          to = todays_date,
                          by = "day")
  } else {
    sun_date_range <- seq(from = last_dates$SunSites + 2,
                          to = todays_date,
                          by = "day")
  }
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
  sun_daily_raw_data_list <- lapply(
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
  sun_daily_raw_data_list <- NULL
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
      # 5. Orders with missing collection times are excluded from
      # collect-to-result and collect-to-receive turnaround time analyis
      ReceiveTime_TATInclude = ifelse(AddOnMaster == "AddOn" |
                            MasterSetting == "Other" |
                            CollectToReceive < 0 |
                            CollectToResult < 0 |
                            ReceiveToResult < 0 |
                            is.na(CollectToResult) |
                            is.na(ReceiveToResult), FALSE, TRUE),
      CollectTime_TATInclude = ifelse(MissingCollect |
                                        AddOnMaster == "AddOn" |
                                        MasterSetting == "Other" |
                                        CollectToReceive < 0 |
                                        CollectToResult < 0 |
                                        ReceiveToResult < 0 |
                                        is.na(CollectToResult) |
                                        is.na(ReceiveToResult), FALSE, TRUE))
  
  # Remove duplicate tests
  raw_scc <- raw_scc %>%
    filter(!DuplTest)
  
  # Determine volume of labs associated with each date and identify correct date
  scc_resulted_dates_vol <- raw_scc %>%
    group_by(ResultedDate) %>%
    summarize(VolLabs = n()) %>%
    arrange(desc(VolLabs)) %>%
    ungroup()
  
  scc_correct_date <- scc_resulted_dates_vol$ResultedDate[1]
  
  raw_scc <- raw_scc %>%
    filter(ResultedDate %in% scc_correct_date)
  
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
                            "ReceiveTime_TATInclude",
                            "CollectTime_TATInclude")]
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
                            "ReceiveTime_TATInclude", "CollectTime_TATInclude")
  
  
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
      # 5. Orders with missing collection times are excluded from
      # collect-to-result and collect-to-receive turnaround time analyis
      ReceiveTime_TATInclude = ifelse(AddOnMaster == "AddOn" |
                                        MasterSetting == "Other" |
                                        CollectToReceive < 0 |
                                        CollectToResult < 0 |
                                        ReceiveToResult < 0 |
                                        is.na(CollectToResult) |
                                        is.na(ReceiveToResult), FALSE, TRUE),
      CollectTime_TATInclude = ifelse(MissingCollect |
                                        AddOnMaster == "AddOn" |
                                        MasterSetting == "Other" |
                                        CollectToReceive < 0 |
                                        CollectToResult < 0 |
                                        ReceiveToResult < 0 |
                                        is.na(CollectToResult) |
                                        is.na(ReceiveToResult), FALSE, TRUE))
  
  # Remove duplicate tests
  raw_sun <- raw_sun %>%
    filter(!DuplTest)
  
  # Determine volume of labs associated with each date and identify correct date
  sun_resulted_dates_vol <- raw_sun %>%
    group_by(ResultedDate) %>%
    summarize(VolLabs = n()) %>%
    arrange(desc(VolLabs)) %>%
    ungroup()
  
  sun_correct_date <- sun_resulted_dates_vol$ResultedDate[1]
  
  raw_sun <- raw_sun %>%
    filter(ResultedDate %in% sun_correct_date)
  
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
                            "ReceiveTime_TATInclude", "CollectTime_TATInclude")]
  
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
                            "ReceiveTime_TATInclude", "CollectTime_TATInclude")
  
  sun_daily_list <- list(raw_sun, sun_master)
  
}

# # Custom function to determine resulted lab date from preprocessed data -------
# # SCC data often has a few labs with incorrect result date
# correct_result_dates <- function(data, number_days) {
#   all_resulted_dates_vol <- data %>%
#     group_by(ResultDate) %>%
#     summarize(VolLabs = n()) %>%
#     arrange(desc(VolLabs)) %>%
#     ungroup()
#   
#   
#   correct_dates <- all_resulted_dates_vol$ResultDate[1:number_days]
#   
#   new_data <- data %>%
#     filter(ResultDate %in% correct_dates)
#   return(new_data)
# }


# Compile processed SCC daily data ------------------------------
if (!is.null(scc_raw_data_list)) {
  # Preprocess SCC daily raw data using custom function
  scc_daily_preprocessed <- lapply(scc_raw_data_list,
                                   preprocess_scc)
  # Select the second element of the list of lists
  scc_preprocessed_data <- lapply(scc_daily_preprocessed, function(x) x[[2]])
  
  # # Remove any labs with incorrect dates then bind daily reports into one data frame
  # scc_preprocessed_data <- lapply(scc_preprocessed_data, 
  #                                 function(x) correct_result_dates(x, number_days = 1))
  
  # Bind all data together
  scc_daily_bind <- bind_rows(scc_preprocessed_data)
  
} else {
  scc_daily_bind <- NULL
}


# Compile preprocessed Sunquest daily data, if any exists ---------------
if (!is.null(sun_daily_raw_data_list)) {
  # Preprocess Sunquest daily raw data using custom function
  sun_daily_preprocessed <- lapply(sun_daily_raw_data_list,
                                   preprocess_daily_sun)
  # Select the second element of the list of lists
  sun_preprocessed_data <- lapply(sun_daily_preprocessed, function(x) x[[2]])
  
  # Bind daily reports into one data frame
  sun_daily_bind <- bind_rows(sun_preprocessed_data)
  
} else {
  sun_daily_bind <- NULL
}

# Bind together preprocessed SCC and Sunquest data --------------------------------------
bind_all_data <- rbind(sun_daily_bind, scc_daily_bind)

# Add columns for month and week number
bind_all_data <- bind_all_data %>%
  mutate(
    # Find month name from result date
    Year = year(ResultDate),
    MonthNo = month(ResultDate),
    MonthName = month(ResultDate, label = TRUE, abbr = TRUE),
    MonthRollUp = as.Date(paste0(MonthNo, "/",
                   1, "/",
                   Year),
            format = "%m/%d/%Y"),
    # WeekNo = format(ResultDate, "%U"),
    WeekStart = ResultDate - (wday(ResultDate) - 1),
    WeekEnd = ResultDate + (7 - wday(ResultDate)),
    WeekOf = paste0(format(WeekStart, "%m/%d/%y"),
                   "-",
                   format(WeekEnd, "%m/%d/%y")))

# Combine raw data repo with new data
if (initial_run == TRUE) {
  latest_raw_data_repo <- bind_all_data
} else {
  # Remove dates more than 60 days back and remove any dates that are in the
  # latest data imports
  mod_raw_data_repo <- raw_data_repo %>%
    filter(ResultDate >= (todays_date - 60) &
             !(ResultDate %in% unique(bind_all_data$ResultDate))) %>%
    mutate(CompleteWeek = NULL,
           CompleteMonth = NULL)
  
  latest_raw_data_repo <- rbind(mod_raw_data_repo, bind_all_data)
  
}

# Create data frame of start and end dates of weeks and months
# This will be used to determine if the data for a complete week or month is present
# If the week or month is not complete, that data will not be included in the repositories
week_dates <- unique(latest_raw_data_repo[, c("WeekStart", "WeekEnd")])

week_dates <- week_dates %>%
  mutate(StartInData = WeekStart %in% unique(latest_raw_data_repo$ResultDate),
         EndInData = WeekEnd %in% unique(latest_raw_data_repo$ResultDate),
         CompleteWeek = StartInData & EndInData)

month_dates <- unique(latest_raw_data_repo[, c("MonthNo", "Year")])

month_dates <- month_dates %>%
  mutate(MonthRollUp = as.Date(paste0(MonthNo, "/",
                                      1, "/",
                                      Year),
                               format = "%m/%d/%Y"),
         MonthStart = floor_date(MonthRollUp, unit = "month"),
         MonthEnd = ceiling_date(MonthRollUp, unit = "month") - 1,
         StartInData = MonthStart %in% unique(latest_raw_data_repo$ResultDate),
         EndInData = MonthEnd %in% unique(latest_raw_data_repo$ResultDate),
         CompleteMonth = StartInData & EndInData)

latest_raw_data_repo <- left_join(latest_raw_data_repo,
                           week_dates[, c("WeekStart",
                                          "WeekEnd",
                                          "CompleteWeek")],
                           by = c("WeekStart" = "WeekStart",
                                  "WeekEnd" = "WeekEnd"))

latest_raw_data_repo <- left_join(latest_raw_data_repo,
                           month_dates[, c("MonthRollUp", "CompleteMonth")],
                           by = c("MonthRollUp" = "MonthRollUp"))

# Summarize data for each day
cp_daily_summary <- latest_raw_data_repo %>%
  group_by(
    Site,
    ResultDate,
    WeekStart,
    WeekEnd,
    WeekOf,
    MonthNo,
    MonthName,
    Year,
    Test,
    Division,
    SettingRollUp,
    MasterSetting,
    DashboardSetting,
    AdjPriority,
    DashboardPriority,
    ReceiveResultTarget,
    CollectResultTarget) %>%
  summarize(
    # Calculate total number of labs resulted
    TotalResulted = n(),
    # Calculate number of labs with valid receive times and collection times
    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
    CollectTime_VolIncl = sum(CollectTime_TATInclude),
    # Calculate number of labs within target TAT
    TotalReceiveResultInTarget =
      sum(ReceiveResultInTarget[ReceiveTime_TATInclude], na.rm = TRUE),
    TotalCollectResultInTarget =
      sum(CollectResultInTarget[CollectTime_TATInclude], na.rm = TRUE),
    TotalAddOnOrder = sum(AddOnMaster %in% c("AddOn"), na.rm = TRUE),
    TotalMissingCollections = sum(MissingCollect),
    # Calculate key statistics for collect-to-receive TAT
    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                 na.rm = TRUE),
    # Calculate key statistics for receive-to-result TAT
    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    # Calculate key statistics for collect-to-result TAT
    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    .groups = "keep") %>%
  ungroup()

# Summarize data for each week
# Filter out data for incomplete weeks
cp_weekly_summary <- latest_raw_data_repo %>%
  filter(CompleteWeek) %>%
  group_by(
    Site,
    WeekStart,
    WeekEnd,
    WeekOf,
    Test,
    Division,
    MasterSetting,
    DashboardSetting,
    DashboardPriority,
    ReceiveResultTarget,
    CollectResultTarget) %>%
  summarize(
    # Calculate total number of labs resulted
    TotalResulted = n(),
    # Calculate number of labs with valid receive times and collection times
    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
    CollectTime_VolIncl = sum(CollectTime_TATInclude),
    # Calculate number of labs within target TAT
    TotalReceiveResultInTarget =
      sum(ReceiveResultInTarget[ReceiveTime_TATInclude], na.rm = TRUE),
    TotalCollectResultInTarget =
      sum(CollectResultInTarget[CollectTime_TATInclude], na.rm = TRUE),
    TotalAddOnOrder = sum(AddOnMaster %in% c("AddOn"), na.rm = TRUE),
    TotalMissingCollections = sum(MissingCollect),
    # Calculate key statistics for collect-to-receive TAT
    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                 na.rm = TRUE),
    # Calculate key statistics for receive-to-result TAT
    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    # Calculate key statistics for collect-to-result TAT
    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    .groups = "keep") %>%
  ungroup()

# Summarize data for each month
# Filter out data for incomplete months
cp_monthly_summary <- latest_raw_data_repo %>%
  filter(CompleteMonth) %>%
  group_by(
    Site,
    MonthRollUp,
    MonthNo,
    MonthName,
    Year,
    Test,
    Division,
    MasterSetting,
    DashboardSetting,
    DashboardPriority,
    ReceiveResultTarget,
    CollectResultTarget) %>%
  summarize(
    # Calculate total number of labs resulted and those with valid TAT
    TotalResulted = n(),
    # Calculate number of labs with valid receive times and collection times
    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
    CollectTime_VolIncl = sum(CollectTime_TATInclude),
    # Calculate number of labs within target TAT
    TotalReceiveResultInTarget =
      sum(ReceiveResultInTarget[ReceiveTime_TATInclude], na.rm = TRUE),
    TotalCollectResultInTarget =
      sum(CollectResultInTarget[CollectTime_TATInclude], na.rm = TRUE),
    TotalAddOnOrder = sum(AddOnMaster %in% c("AddOn"), na.rm = TRUE),
    TotalMissingCollections = sum(MissingCollect),
    # Calculate key statistics for collect-to-receive TAT
    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                 na.rm = TRUE),
    # Calculate key statistics for receive-to-result TAT
    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    # Calculate key statistics for collect-to-result TAT
    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    .groups = "keep") %>%
  ungroup()

# Update repositories with latest data
if (initial_run == TRUE) {
  latest_daily_repo <- cp_daily_summary
  latest_weekly_repo <- cp_weekly_summary
  latest_monthly_repo <- cp_monthly_summary
  
} else {
  # Remove data from daily repo for dates that are in current daily summary
  mod_daily_repo <- daily_repo %>%
    filter(!(ResultDate %in% cp_daily_summary$ResultDate))
  # Bind repository with current daily summary
  latest_daily_repo <- rbind(mod_daily_repo, cp_daily_summary)
  
  # Remove data from weekly repo for weeks that are in current weekly summary
  mod_weekly_repo <- weekly_repo %>%
    filter(!(WeekStart %in% cp_weekly_summary$WeekStart))
  # Bind repository with current weekly summary
  latest_weekly_repo <- rbind(mod_weekly_repo, cp_weekly_summary)
  
  # Remove data from monthly_repo for months that are in current monthly summary
  mod_monthly_repo <- monthly_repo %>%
    filter(!(MonthRollUp %in% cp_monthly_summary$MonthRollUp))
  # Bind repository with current monthly summary
  latest_monthly_repo <- rbind(mod_monthly_repo, cp_monthly_summary)
}

# Save repositories in appropriate folder
# First ensure that raw data repository only includes 60 days of data
if (initial_run == TRUE) {
  raw_data_repo_export <- latest_raw_data_repo %>%
    filter(ResultDate >= todays_date - 60)
} else {
  raw_data_repo_export <- latest_raw_data_repo
}

# Test code for comparing existing repos to new repos; want to make sure historical data remains unchanged
# old_monthly_repo_months <- unique(monthly_repo$MonthRollUp)
# 
# check_new_monthly_repo <- latest_monthly_repo %>%
#   filter(MonthRollUp %in% old_monthly_repo_months)
# 
# new_monthly_repo_data <- latest_monthly_repo %>%
#   filter(!(MonthRollUp %in% old_monthly_repo_months))
# 
# old_weekly_repo_weeks <- unique(weekly_repo$WeekOf)
# 
# check_new_weekly_repo <- latest_weekly_repo %>%
#   filter(WeekOf %in% old_weekly_repo_weeks)
# 
# weekly_repo <- weekly_repo %>%
#   arrange(Site,
#           WeekStart,                 
#           WeekEnd,                  
#           WeekOf,                    
#           Test,                      
#           Division,                 
#           MasterSetting,             
#           DashboardSetting,          
#           DashboardPriority,        
#           ReceiveResultTarget,       
#           CollectResultTarget,       
#           TotalResulted,            
#           ReceiveTime_VolIncl,       
#           CollectTime_VolIncl,       
#           TotalReceiveResultInTarget,
#           TotalCollectResultInTarget,
#           TotalAddOnOrder,           
#           TotalMissingCollections,  
#           CollectReceive_Avg,        
#           CollectReceive_Median,     
#           CollectReceive_95,        
#           ReceiveResult_Avg,         
#           ReceiveResult_Median,      
#           ReceiveResult_95,         
#           CollectResult_Avg,         
#           CollectResult_Median,      
#           CollectResult_95)
# 
# check_new_weekly_repo <- check_new_weekly_repo %>%
#   arrange(Site,
#           WeekStart,                 
#           WeekEnd,                  
#           WeekOf,                    
#           Test,                      
#           Division,                 
#           MasterSetting,             
#           DashboardSetting,          
#           DashboardPriority,        
#           ReceiveResultTarget,       
#           CollectResultTarget,       
#           TotalResulted,            
#           ReceiveTime_VolIncl,       
#           CollectTime_VolIncl,       
#           TotalReceiveResultInTarget,
#           TotalCollectResultInTarget,
#           TotalAddOnOrder,           
#           TotalMissingCollections,  
#           CollectReceive_Avg,        
#           CollectReceive_Median,     
#           CollectReceive_95,        
#           ReceiveResult_Avg,         
#           ReceiveResult_Median,      
#           ReceiveResult_95,         
#           CollectResult_Avg,         
#           CollectResult_Median,      
#           CollectResult_95)

saveRDS(raw_data_repo_export,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/RawDataRepo",
                      "/Labs Resulted ",
                      format(min(latest_raw_data_repo$ResultDate), "%m-%d-%y"),
                      " to ",
                      format(max(latest_raw_data_repo$ResultDate), "%m-%d-%y"),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))

saveRDS(latest_daily_repo,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/DailyRepo",
                      "/Daily Repo ",
                      format(min(latest_daily_repo$ResultDate), "%m-%d-%y"),
                      " to ",
                      format(max(latest_daily_repo$ResultDate), "%m-%d-%y"),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))

saveRDS(latest_weekly_repo,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/WeeklyRepo",
                      "/Weekly Repo ",
                      format(min(latest_weekly_repo$WeekStart), "%m-%d-%y"),
                      " to ",
                      format(max(latest_weekly_repo$WeekEnd), "%m-%d-%y"),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))

saveRDS(latest_monthly_repo,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/MonthlyRepo",
                      "/Monthly Repo ",
                      paste0(month(min(latest_monthly_repo$MonthRollUp), 
                                   label = TRUE, abbr = TRUE), 
                             year(min(latest_monthly_repo$MonthRollUp))),
                      " to ",
                      paste0(month(max(latest_monthly_repo$MonthRollUp), 
                                   label = TRUE, abbr = TRUE), 
                             year(max(latest_monthly_repo$MonthRollUp))),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))
