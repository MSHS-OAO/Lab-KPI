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
  
  raw_scc <- raw_scc %>%
    mutate(across(where(is.character), gsub,
                  pattern = "\\*.*\\*",
                  replacement = ""))
  
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
                       by = c("TEST_ID" = "SCC_TEST_ID"))
  
  # Determine if test is included based on crosswalk results
  raw_scc <- raw_scc %>%
    mutate(TestIncl = !is.na(TEST)) %>%
    filter(TestIncl)
  
  # Crosswalk unit type
  raw_scc <- left_join(raw_scc, scc_setting,
                       by = c("CLINIC_TYPE" = "CLINIC_TYPE"))
  # Crosswalk site name
  raw_scc <- left_join(raw_scc, mshs_site,
                       by = c("SITE" = "DATA_SITE"))
  
  raw_scc <- raw_scc%>%
    select(-SITE) %>%
    rename(SITE = SITE.y)
  
  # Preprocess SCC data and add any necessary columns
  raw_scc <- raw_scc %>%
    mutate(
      # Subset HGB and BUN tests completed at RTC as a separate site since they
      # are processed at RTC
      SITE = ifelse(TEST %in% c("HGB", "BUN") &
                      str_detect(replace_na(WARD_NAME, ""),
                                 "Ruttenberg Treatment Center"),
                    "RTC", SITE),
      # Update division to Infusion for RTC
      DIVISION = ifelse(SITE %in% c("RTC"), "Infusion", DIVISION),
      # Determine if unit is an ICU based on site mappings
      ICU = paste(SITE, Ward, WARD_NAME) %in% scc_icu$SiteCodeName,
      # Create a column for resulted date
      ResultedDate = date(VERIFIED_DATE),
      # Create master setting column to identify ICU and IP Non-ICU units
      MasterSetting = ifelse(SETTING_ROLL_UP == "IP" & ICU, "ICU",
                             ifelse(SETTING_ROLL_UP == "IP" & !ICU,
                                    "IP Non-ICU", SETTING_ROLL_UP)),
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
      MissingCollect = ifelse(CollectToReceive == 0,'TRUE','FALSE'),
      #
      # Determine TAT based on test, division, priority, and patient setting
      # Create column concatenating test and division to determine TAT targets
      Concate1 = paste(TEST, DIVISION),
      #
      # Create dashboard priority column
      DashboardPriority = ifelse(
        tat_targets$PRIORITY[match(
          Concate1, 
          paste(tat_targets$TEST, tat_targets$DIVISION))] == "All",
        "All", AdjPriority),
      # Create column concatenating test, division, and priority to determine
      # TAT targets
      Concate2 = paste(TEST, DIVISION, DashboardPriority),
      # Create column concatenating test, division, priority, and setting to
      # determine TAT targets
      Concate3 = paste(TEST, DIVISION, DashboardPriority, MasterSetting),
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
               tat_targets$RECEIVE_TO_RESULT_TARGET[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine Collect to Result TAT target based on above logic/scenarios
      CollectResultTarget =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, tat_targets$Concate)),
               tat_targets$COLLECT_TO_RESULT_TARGET[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine if Receive to Result and Collect to Result TAT meet targets
      ReceiveResultInTarget = ifelse(ReceiveToResult <= ReceiveResultTarget, 'TRUE','FALSE'),
      CollectResultInTarget = ifelse(CollectToResult <= CollectResultTarget, 'TRUE','FALSE'),
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
                                        is.na(ReceiveToResult), 'FALSE', 'TRUE'),
      CollectTime_TATInclude = ifelse(MissingCollect == 'TRUE' |
                                        AddOnMaster == "AddOn" |
                                        MasterSetting == "Other" |
                                        CollectToReceive < 0 |
                                        CollectToResult < 0 |
                                        ReceiveToResult < 0 |
                                        is.na(CollectToResult) |
                                        is.na(ReceiveToResult), 'FALSE', 'TRUE'))
  
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
    filter(ResultedDate %in% scc_correct_date) %>%
    mutate(ICU = as.character(ICU))
  
  # Select columns
  scc_master <- raw_scc[, c("Ward", "WARD_NAME",
                            "ORDER_ID", "REQUESTING_DOC NAME",
                            "MPI", "WORK SHIFT",
                            "TEST_NAME", "TEST", "DIVISION", "PRIORITY",
                            "SITE", "ICU", "CLINIC_TYPE",
                            "SETTING", "SETTING_ROLL_UP",
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
  
  colnames(scc_master) <- toupper(colnames(scc_master))
  
  scc_master <- scc_master %>%
    mutate(
      # Find month name from result date
      YEAR = year(RESULTDATE),
      MONTHNO = month(RESULTDATE),
      MONTHNAME = month(RESULTDATE, label = TRUE, abbr = TRUE),
      MONTHROLLUP = as.Date(paste0(MONTHNO, "/",
                                   1, "/",
                                   YEAR),
                            format = "%m/%d/%Y"),
      # WeekNo = format(ResultDate, "%U"),
      WEEKSTART = RESULTDATE - (wday(RESULTDATE) - 1),
      WEEKEND = RESULTDATE + (7 - wday(RESULTDATE)),
      WEEKOF = paste0(format(WEEKSTART, "%m/%d/%y"),
                      "-",
                      format(WEEKEND, "%m/%d/%y")))
  
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
  
  raw_sun <- raw_sun %>%
    mutate(across(where(is.character), gsub,
                  pattern = "\\*.*\\*",
                  replacement = ""))
  
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
                       by = c("TestCode" = "SUN_TEST_CODE"))
  
  # Determine if test is included based on crosswalk results
  raw_sun <- raw_sun %>%
    mutate(TestIncl = !is.na(TEST)) %>%
    filter(TestIncl)
  
  
  # Crosswalk unit type
  raw_sun <- left_join(raw_sun, sun_setting,
                       by = c("LocType" = "LOC_TYPE"))
  
  # Crosswalk site name
  raw_sun <- left_join(raw_sun, mshs_site,
                       by = c("HospCode" = "DATA_SITE"))
  
  # # Sunquest data formatting-----------------------------
  # Preprocess Sunquest data and add any necessary columns
  raw_sun <- raw_sun %>%
    mutate(
      # Determine if unit is an ICU based on site mappings
      ICU = paste(SITE, LocCode, LocName) %in% sun_icu$SiteCodeName,
      # Create a column for resulted date
      ResultedDate = as.Date(ResultDateTime, format = "%m/%d/%Y"),
      # Create master setting column to identify ICU and IP Non-ICU units
      MasterSetting = ifelse(SETTING_ROLL_UP == "IP" & ICU, "ICU",
                             ifelse(SETTING_ROLL_UP == "IP" & !ICU,
                                    "IP Non-ICU", SETTING_ROLL_UP)),
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
      MissingCollect = ifelse(CollectDateTime == OrderDateTime, 'TRUE', 'FALSE'),
      #
      # Determine TAT target based on test, priority, and patient setting
      # Create column concatenating test and division to determine TAT targets
      Concate1 = paste(TEST, DIVISION),
      #
      # Create dashboard priority column
      DashboardPriority = ifelse(
        tat_targets$PRIORITY[match(
          Concate1,
          paste(tat_targets$TEST, tat_targets$DIVISION))] == "All",
        "All", AdjPriority),
      # Create column concatenating test, division, and priority to determine
      # TAT targets
      Concate2 = paste(TEST, DIVISION, DashboardPriority),
      # Create column concatenating test, division, priority, and setting to
      # determine TAT targets
      Concate3 = paste(TEST, DIVISION, DashboardPriority, MasterSetting),
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
               tat_targets$RECEIVE_TO_RESULT_TARGET[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$RECEIVE_TO_RESULT_TARGET[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine Collect to Result TAT target based on above logic/scenarios
      CollectResultTarget =
        # Match on scenario 1
        ifelse(!is.na(match(Concate3, tat_targets$Concate)),
               tat_targets$COLLECT_TO_RESULT_TARGET[
                 match(Concate3, tat_targets$Concate)],
               # Match on scenario 2
               ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                      tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate2, tat_targets$Concate)],
                      # Match on scenario 3
                      tat_targets$COLLECT_TO_RESULT_TARGET[
                        match(Concate1, tat_targets$Concate)])),
      #
      # Determine if Receive to Result and Collect to Result TAT meet targets
      ReceiveResultInTarget = ifelse(ReceiveToResult <= ReceiveResultTarget, 'TRUE', 'FALSE'),
      CollectResultInTarget = ifelse(CollectToResult <= CollectResultTarget, 'TRUE', 'FALSE'),
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
                                        is.na(ReceiveToResult), 'FALSE', 'TRUE'),
      CollectTime_TATInclude = ifelse(MissingCollect == 'TRUE' |
                                        AddOnMaster == "AddOn" |
                                        MasterSetting == "Other" |
                                        CollectToReceive < 0 |
                                        CollectToResult < 0 |
                                        ReceiveToResult < 0 |
                                        is.na(CollectToResult) |
                                        is.na(ReceiveToResult), 'FALSE', 'TRUE'))
  
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
    filter(ResultedDate %in% sun_correct_date)%>%
    mutate(ICU = as.character(ICU))
  
  # Select columns
  sun_master <- raw_sun[, c("LocCode", "LocName",
                            "HISOrderNumber", "PhysName",
                            "PtNumber", "SHIFT",
                            "TSTName", "TEST", "DIVISION", "SpecimenPriority",
                            "SITE", "ICU", "LocType",
                            "SETTING", "SETTING_ROLL_UP",
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
  
  colnames(sun_master) <- toupper(colnames(sun_master))
  
  sun_master <-  sun_master %>%
    mutate(
      # Find month name from result date
      YEAR = year(RESULTDATE),
      MONTHNO = month(RESULTDATE),
      MONTHNAME = month(RESULTDATE, label = TRUE, abbr = TRUE),
      MONTHROLLUP = as.Date(paste0(MONTHNO, "/",
                                   1, "/",
                                   YEAR),
                            format = "%m/%d/%Y"),
      # WeekNo = format(ResultDate, "%U"),
      WEEKSTART = RESULTDATE - (wday(RESULTDATE) - 1),
      WEEKEND = RESULTDATE + (7 - wday(RESULTDATE)),
      WEEKOF = paste0(format(WEEKSTART, "%m/%d/%y"),
                      "-",
                      format(WEEKEND, "%m/%d/%y")))
  
}