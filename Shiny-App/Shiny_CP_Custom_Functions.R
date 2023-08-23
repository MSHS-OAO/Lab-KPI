# Code for preprocessing, analyzing, and displaying Clinical Pathology KPI -----
# CP includes Chemistry, Hematology, and Microbiology RRL divisions


# Custom function for preprocessing raw data from SCC and Sunquest -------
preprocess_scc <- function(raw_scc) {
  
  # Preprocess SCC data -------------------------------
  if (is.null(raw_scc) || nrow(raw_scc) == 0) {
    scc_processed <- NULL
  } else {
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
        DetailedSetting = ifelse(SettingRollUp == "IP" & ICU, "ICU",
                              ifelse(SettingRollUp == "IP" & !ICU,
                                     "IP Non-ICU", SettingRollUp)),
        # Create dashboard setting column to roll up master settings based on
        # desired dashboard grouping (ie, group ED and ICU together)
        DashboardSetting = ifelse(DetailedSetting %in% c("ED", "ICU"),
                                  "ED & ICU", DetailedSetting),
        # Create column with adjusted priority based on assumption that all ED and
        # ICU labs are treated as stat per operational leadership
        AdjPriority = ifelse(DetailedSetting %in% c("ED", "ICU") |
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
        AddOnFinal = ifelse(as.numeric(ORDERING_DATE - RECEIVE_DATE,
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
        Concate3 = paste(Test, Division, DashboardPriority, DetailedSetting),
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
        ReceiveTime_TATInclude = ifelse(AddOnFinal == "AddOn" |
                                          DetailedSetting == "Other" |
                                          CollectToReceive < 0 |
                                          CollectToResult < 0 |
                                          ReceiveToResult < 0 |
                                          is.na(CollectToResult) |
                                          is.na(ReceiveToResult), FALSE, TRUE),
        CollectTime_TATInclude = ifelse(MissingCollect |
                                          AddOnFinal == "AddOn" |
                                          DetailedSetting == "Other" |
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
    scc_processed <- raw_scc[, c("Ward", "WARD_NAME",
                                 "ORDER_ID", "REQUESTING_DOC NAME",
                                 "MPI", "WORK SHIFT",
                                 "TEST_NAME", "Test", "Division", "PRIORITY",
                                 "Site", "ICU", "CLINIC_TYPE",
                                 "Setting", "SettingRollUp",
                                 "DetailedSetting", "DashboardSetting",
                                 "AdjPriority", "DashboardPriority",
                                 "ORDERING_DATE", "COLLECTION_DATE",
                                 "RECEIVE_DATE", "VERIFIED_DATE",
                                 "ResultedDate",
                                 "CollectToReceive", "ReceiveToResult",
                                 "CollectToResult",
                                 "AddOnFinal", "MissingCollect",
                                 "ReceiveResultTarget", "CollectResultTarget",
                                 "ReceiveResultInTarget", "CollectResultInTarget",
                                 "ReceiveTime_TATInclude",
                                 "CollectTime_TATInclude")]
    # Rename columns
    colnames(scc_processed) <- c("LocCode", "LocName",
                                 "OrderID", "RequestMD",
                                 "MSMRN", "WorkShift",
                                 "TestName", "Test", "Division", "OrderPriority",
                                 "Site", "ICU", "LocType",
                                 "Setting", "SettingRollUp",
                                 "DetailedSetting", "DashboardSetting",
                                 "AdjPriority", "DashboardPriority",
                                 "OrderTime", "CollectTime",
                                 "ReceiveTime", "ResultTime",
                                 "ResultDate",
                                 "CollectToReceiveTAT", "ReceiveToResultTAT",
                                 "CollectToResultTAT",
                                 "AddOnFinal", "MissingCollect",
                                 "ReceiveResultTarget", "CollectResultTarget",
                                 "ReceiveResultInTarget", "CollectResultInTarget",
                                 "ReceiveTime_TATInclude", "CollectTime_TATInclude")
  }
  
  scc_return_list <- list(scc_processed, scc_correct_date)
  
  return(scc_return_list)
}

preprocess_sun <- function(raw_sun)  {
  
  # Preprocess Sunquest data --------------------------------
  if (is.null(raw_sun) || nrow(raw_sun) == 0) {
    sun_processed <- NULL
  } else {
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
        DetailedSetting = ifelse(SettingRollUp == "IP" & ICU, "ICU",
                              ifelse(SettingRollUp == "IP" & !ICU,
                                     "IP Non-ICU", SettingRollUp)),
        # Create dashboard setting column to roll up master settings based on
        # desired dashboard grouping(ie, group ED and ICU together)
        DashboardSetting = ifelse(DetailedSetting %in% c("ED", "ICU"), "ED & ICU",
                                  DetailedSetting),
        #
        # Create column with adjusted priority based on operational assumption
        # that all ED and ICU labs are treated as stat
        AdjPriority = ifelse(DetailedSetting %in% c("ED", "ICU") |
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
        AddOnFinal = ifelse(as.numeric(OrderDateTime - ReceiveDateTime,
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
        Concate3 = paste(Test, Division, DashboardPriority, DetailedSetting),
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
        ReceiveTime_TATInclude = ifelse(AddOnFinal == "AddOn" |
                                          DetailedSetting == "Other" |
                                          CollectToReceive < 0 |
                                          CollectToResult < 0 |
                                          ReceiveToResult < 0 |
                                          is.na(CollectToResult) |
                                          is.na(ReceiveToResult), FALSE, TRUE),
        CollectTime_TATInclude = ifelse(MissingCollect |
                                          AddOnFinal == "AddOn" |
                                          DetailedSetting == "Other" |
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
    sun_processed <- raw_sun[, c("LocCode", "LocName",
                                 "HISOrderNumber", "PhysName",
                                 "PtNumber", "SHIFT",
                                 "TSTName", "Test", "Division", "SpecimenPriority",
                                 "Site", "ICU", "LocType",
                                 "Setting", "SettingRollUp",
                                 "DetailedSetting", "DashboardSetting",
                                 "AdjPriority", "DashboardPriority",
                                 "OrderDateTime", "CollectDateTime",
                                 "ReceiveDateTime", "ResultDateTime",
                                 "ResultedDate",
                                 "CollecttoReceive", "ReceivetoResult",
                                 "CollecttoResult",
                                 "AddOnFinal", "MissingCollect",
                                 "ReceiveResultTarget", "CollectResultTarget",
                                 "ReceiveResultInTarget", "CollectResultInTarget",
                                 "ReceiveTime_TATInclude", "CollectTime_TATInclude")]
    
    colnames(sun_processed) <- c("LocCode", "LocName",
                                 "OrderID", "RequestMD",
                                 "MSMRN", "WorkShift",
                                 "TestName", "Test", "Division", "OrderPriority",
                                 "Site", "ICU", "LocType",
                                 "Setting", "SettingRollUp",
                                 "DetailedSetting", "DashboardSetting",
                                 "AdjPriority", "DashboardPriority",
                                 "OrderTime", "CollectTime",
                                 "ReceiveTime", "ResultTime",
                                 "ResultDate",
                                 "CollectToReceiveTAT", "ReceiveToResultTAT",
                                 "CollectToResultTAT",
                                 "AddOnFinal", "MissingCollect",
                                 "ReceiveResultTarget", "CollectResultTarget",
                                 "ReceiveResultInTarget", "CollectResultInTarget",
                                 "ReceiveTime_TATInclude", "CollectTime_TATInclude")
    
  }
  
  sun_return_list <- list(sun_processed, sun_correct_date)
  
  return(sun_return_list)
}


# Custom function to subset and summarize data for each lab division ----------
summarize_cp_tat <- function(x, lab_division) {
  if (is.null(x) || nrow(x) == 0) {
    lab_div_df <- NULL
    lab_summary <- NULL
    lab_dashboard_melt <- NULL
    lab_dashboard_cast <- NULL
  } else {
    # Subset data to be included based on lab division, whether or not TAT
    # meets inclusion criteria, and site location
    lab_summary <- x %>%
      filter(DIVISION == lab_division) %>%
      group_by(TEST,
               SITE,
               DASHBOARD_PRIORITY,
               DASHBOARD_SETTING,
               RECEIVE_RESULT_TARGET,
               COLLECT_RESULT_TARGET) %>%
      summarize(ResultedVolume = sum(TOTAL_RESULTED),
                ResultedVol_ReceiveTAT = sum(RECEIVE_TIME_VOL_INCL),
                ResultedVol_CollectTAT = sum(COLLECT_TIME_VOL_INCL),
                ReceiveResultInTarget = sum(TOTAL_RECEIVE_RESULT_IN_TARGET),
                CollectResultInTarget = sum(TOTAL_COLLECT_RESULT_IN_TARGET),
                ReceiveResultPercent = round(
                  ReceiveResultInTarget / ResultedVol_ReceiveTAT, digits = 3),
                CollectResultPercent = round(
                  CollectResultInTarget / ResultedVol_CollectTAT, digits = 3),
                .groups = "keep") %>%
      rename(Test = TEST,
             Site = SITE,
             DashboardPriority = DASHBOARD_PRIORITY,
             DashboardSetting = DASHBOARD_SETTING,
             ReceiveResultTarget = RECEIVE_RESULT_TARGET,
             CollectResultTarget = COLLECT_RESULT_TARGET) %>%
      ungroup()
    #
    # Subset template data frame for this division
    lab_div_df_templ <- tat_dashboard_templ %>%
      mutate(Incl = NULL) %>%
      filter(Division == lab_division)
    #
    # Combine lab summary with template data frame for this division for
    # dashboard visualization
    lab_summary <- left_join(lab_div_df_templ, lab_summary,
                             by = c("Test" = "Test",
                                    "Site" = "Site",
                                    "DashboardPriority" = "DashboardPriority",
                                    "DashboardSetting" = "DashboardSetting"))
    #
    # Format relevant columns as factors, look up target TAT for labs with 0
    # resulted volume, add formatting for percent within targets
    lab_summary <- lab_summary %>%
      mutate(
        #
        # Set test, site, priority, and setting as factors
        Test = droplevels(factor(Test, levels = test_names, ordered = TRUE)),
        Site = droplevels(factor(Site, levels = all_sites, ordered = TRUE)),
        DashboardPriority = droplevels(factor(DashboardPriority,
                                              levels = dashboard_priority_order,
                                              ordered = TRUE)),
        DashboardSetting = droplevels(factor(DashboardSetting,
                                             levels = dashboard_pt_setting,
                                             ordered = TRUE)),
        #
        # Determine TAT target for sites with 0 resulted labs
        # Create column concatenating test and division to determine TAT targets
        Concate1 = paste(Test, Division),
        # Create column concatenating test, division, and priority to determine
        # TAT targets
        Concate2 = paste(Test, Division, DashboardPriority),
        # Create column concatenating test, division, priority, and setting to
        # determine TAT targets
        Concate3 = paste(Test, Division, DashboardPriority, DashboardSetting),
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
          # If TAT target is known, keep TAT target
          ifelse(!is.na(ReceiveResultTarget), ReceiveResultTarget,
                 # Try to match on scenario 1
                 ifelse(
                   !is.na(match(Concate3, cp_tat_targets$Concate)),
                   cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                     match(Concate3, cp_tat_targets$Concate)],
                   # Try to match on scenario 2
                   ifelse(
                     !is.na(match(Concate2, cp_tat_targets$Concate)),
                     cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                       match(Concate2, cp_tat_targets$Concate)],
                     # Try to match on scenario 3
                     cp_tat_targets$RECEIVE_TO_RESULT_TARGET[
                       match(Concate1, cp_tat_targets$Concate)]))),
        #
        # Determine Collect to Result TAT target based on above logic/scenarios
        # Determine Receive to Result TAT target based on above logic/scenarios
        CollectResultTarget =
          # If TAT target is known, keep TAT target
          ifelse(!is.na(CollectResultTarget), CollectResultTarget,
                 # Try to match on scenario 1
                 ifelse(
                   !is.na(match(Concate3, cp_tat_targets$Concate)),
                   cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                     match(Concate3, cp_tat_targets$Concate)],
                   # Try to match on scenario 2
                   ifelse(
                     !is.na(match(Concate2, cp_tat_targets$Concate)),
                     cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                       match(Concate2, cp_tat_targets$Concate)],
                     # Try to match on scenario 3
                     cp_tat_targets$COLLECT_TO_RESULT_TARGET[
                       match(Concate1, cp_tat_targets$Concate)]))),
        #
        # Format target TAT for tables from numbers to "<=X min"
        ReceiveResultTarget = paste0("<=", ReceiveResultTarget, " min"),
        CollectResultTarget = paste0("<=", CollectResultTarget, " min"),
        #
        # Format percentage of labs in target
        ReceiveResultPercent = formattable::percent(ReceiveResultPercent, digits = 0),
        CollectResultPercent = formattable::percent(CollectResultPercent, digits = 0),
        #
        # Apply conditional color formatting to TAT percentages based on status
        # definitions for each lab division
        #
        # Chemistry & Hematology:
        # Green: >= 95%, Yellow: >= 80% & < 95%, Red: < 80%
        # Microbiology:
        # Green: 100%, Yellow: >= 90% & < 100%, Red: < 90%
        #
        ReceiveResultPercent = cell_spec(
          ReceiveResultPercent, "html",
          color = ifelse(is.na(ReceiveResultPercent), "lightgray",
                         ifelse(
                           (ReceiveResultPercent >= 0.95 &
                              lab_division %in% c("Chemistry", "Hematology")) |
                             (ReceiveResultPercent == 1.00 &
                                lab_division %in% c("Microbiology RRL")) |
                             (ReceiveResultPercent >= 0.90 &
                                lab_division %in% c("Infusion")),
                           "green",
                           ifelse(
                             (ReceiveResultPercent >= 0.8 &
                                lab_division %in%
                                c("Chemistry", "Hematology", "Infusion")) |
                               (ReceiveResultPercent >= 0.9 &
                                  lab_division %in% c("Microbiology RRL")),
                             "orange", "red")))),
        CollectResultPercent = cell_spec(
          CollectResultPercent, "html",
          color = ifelse(is.na(CollectResultPercent), "lightgray",
                         ifelse(
                           (CollectResultPercent >= 0.95 &
                              lab_division %in% c("Chemistry", "Hematology")) |
                             (CollectResultPercent == 1.00 &
                                lab_division %in% c("Microbiology RRL")) |
                             (CollectResultPercent >= 0.90 &
                                lab_division %in% c("Infusion")),
                           "green",
                           ifelse(
                             (CollectResultPercent >= 0.8 &
                                lab_division %in%
                                c("Chemistry", "Hematology", "Infusion")) |
                               (CollectResultPercent >= 0.9 &
                                  lab_division %in% c("Microbiology RRL")),
                             "orange", "red")))),
        #
        # Create a new column with test and priority to be used in tables later
        TestAndPriority = paste(Test, "-", DashboardPriority, "Labs"),
        #
        # Remove concatenated columns used for matching
        Concate1 = NULL,
        Concate2 = NULL,
        Concate3 = NULL) %>%
      arrange(Test, Site, DashboardPriority, DashboardSetting)
    #
    # Melt summarized data into a long dataframe
    lab_dashboard_melt <- melt(lab_summary,
                               id.var = c("Test",
                                          "Site",
                                          "DashboardPriority",
                                          "TestAndPriority",
                                          "DashboardSetting",
                                          "ReceiveResultTarget",
                                          "CollectResultTarget"),
                               measure.vars = c("ReceiveResultPercent",
                                                "CollectResultPercent"))
    #
    # Cast dataframe into wide format for use in tables later
    lab_dashboard_cast <- dcast(lab_dashboard_melt,
                                Test +
                                  DashboardPriority +
                                  TestAndPriority +
                                  DashboardSetting +
                                  ReceiveResultTarget +
                                  CollectResultTarget ~
                                  variable +
                                  Site,
                                value.var = "value")
    #
    # Rearrange columns based on desired dashboard aesthetics
    col_order <- c("Test", "DashboardPriority", "TestAndPriority",
                   "ReceiveResultTarget", "DashboardSetting",
                   "ReceiveResultPercent_MSH", "ReceiveResultPercent_MSQ",
                   "ReceiveResultPercent_MSBI", "ReceiveResultPercent_MSB",
                   "ReceiveResultPercent_MSW", "ReceiveResultPercent_MSM",
                   "ReceiveResultPercent_MSSN", "ReceiveResultPercent_RTC",
                   "CollectResultTarget", "DashboardSetting2",
                   "CollectResultPercent_MSH", "CollectResultPercent_MSQ",
                   "CollectResultPercent_MSBI", "CollectResultPercent_MSB",
                   "CollectResultPercent_MSW", "CollectResultPercent_MSM",
                   "CollectResultPercent_MSSN", "CollectResultPercent_RTC")
    
    lab_dashboard_cast <- lab_dashboard_cast %>%
      mutate(DashboardSetting2 = DashboardSetting) %>%
      select(intersect(col_order, names(.)))
    
    # Microbiology RRL: Manually remove C. diff ambulatory TAT since only volume
    # is monitored for this lab/setting combination
    if (lab_division == "Microbiology RRL") {
      lab_dashboard_cast <- lab_dashboard_cast %>%
        filter(!(Test == "C. diff" & DashboardSetting == "Amb"))
      row.names(lab_dashboard_cast) <- seq_len(nrow(lab_dashboard_cast))
    }
  }
  #
  # # Save outputs in a list
  # lab_sub_output <- list(lab_div_df,
  #                        lab_summary,
  #                        lab_dashboard_melt,
  #                        lab_dashboard_cast)
  # #
  # lab_sub_output
  return(lab_dashboard_cast)
}

# Custom function for creating kables for each CP lab division ----------------
kable_cp_tat <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    asis_output(
      paste("<i>",
            "No data available to determine turnaround times.",
            "</i>")
    )
  } else {
    #
    # Select columns 3 and on
    data <- x[, c(3:ncol(x))]
    
    if (any(str_detect(colnames(data), "_RTC"))) {
      kable_col_names <- c("Test & Priority",
                           "Target", "Setting",
                           "RTC",
                           "Target", "Setting",
                           "RTC")
    } else {
      kable_col_names <- c("Test & Priority",
                           "Target", "Setting",
                           "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN",
                           "Target", "Setting",
                           "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN")
    }
    
    num_col <- length(kable_col_names)
    #
    # Format kable
    kable(data, format = "html", escape = FALSE, align = "c",
          col.names = kable_col_names) %>%
      kable_styling(bootstrap_options = "hover", position = "center",
                    font_size = 11,
                    full_width = FALSE) %>%
      column_spec(column = c(1, (num_col - 1) / 2 + 1, num_col),
                  border_right = "thin solid lightgray") %>%
      add_header_above(c(" " = 1,
                         "Receive to Result Within Target" =
                           (num_col - 1) / 2,
                         "Collect to Result Within Target" =
                           (num_col - 1) / 2),
                       background = c("white", "#00AEEF", "#221f72"),
                       color = "white", line = FALSE, font_size = 13) %>%
      column_spec(column = 2:((num_col - 1) / 2 + 1), background = "#E6F8FF", color = "black") %>%
      column_spec(column = ((num_col - 1) / 2 + 2):num_col, background = "#EBEBF9", color = "black") %>%
      #column_spec(column = 2:17, background = "inherit", color = "inherit") %>%
      column_spec(column = 1, width_min = "125px") %>%
      column_spec(column = c(3, (num_col - 1) / 2 + 3), width_min = "100px") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = c(1, 2, ((num_col - 1) / 2 + 2)))
  }
}

# Custom function for summarizing resulted lab volume from prior day(s) --------
summarize_cp_vol <- function(x, lab_division) {
  if (is.null(x) || nrow(x) == 0) {
    lab_div_vol_cast <- NULL
  } else {
    # Subset data to be included based on lab division and site location
    lab_div_vol_df <- x %>%
      filter(Division == lab_division) %>%
      group_by(Site,
               Test,
               DashboardPriority,
               DetailedSetting) %>%
      summarize(ResultedLabs = sum(TotalResulted),
                .groups = "keep")
    #
    # Subset volume dataframe template for this division
    lab_div_vol_templ <- vol_dashboard_templ %>%
      filter(Division == lab_division) %>%
      mutate(Incl = NULL)
    #
    # Combine two dataframes to ensure all combinations are accounts for
    lab_div_vol_df <- left_join(lab_div_vol_templ, lab_div_vol_df,
                                by = c("Test" = "Test",
                                       "Site" = "Site",
                                       "DashboardPriority" = "DashboardPriority",
                                       "PtSetting" = "DetailedSetting"))
    #
    lab_div_vol_df <- lab_div_vol_df %>%
      mutate(
        # Set test, site, priority, and setting as factors
        Test = droplevels(factor(Test, levels = test_names, ordered = TRUE)),
        Site = droplevels(factor(Site, levels = all_sites, ordered = TRUE)),
        DashboardPriority = droplevels(factor(DashboardPriority,
                                              levels = dashboard_priority_order,
                                              ordered = TRUE)),
        PtSetting = droplevels(factor(PtSetting,
                                      levels = pt_setting_order,
                                      ordered = TRUE)),
        #
        # Replace NA with 0
        ResultedLabs = ifelse(is.na(ResultedLabs), 0, ResultedLabs),
        #
        # Create column with test name and priority
        TestAndPriority = paste(Test, "-", DashboardPriority, "Labs"))
    #
    # Cast dataframe
    lab_div_vol_cast <- dcast(lab_div_vol_df,
                              Test +
                                DashboardPriority +
                                TestAndPriority +
                                PtSetting ~
                                Site,
                              value.var = "ResultedLabs")
    # Remove test and priority columns
    lab_div_vol_cast <- lab_div_vol_cast[, c(3:ncol(lab_div_vol_cast))]
  }
  #
  return(lab_div_vol_cast)
}

# Custom function for creating a kable of lab volume from prior day(s)----------
kable_cp_vol <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    asis_output(
      paste("<i>",
            "Not data available.",
            "</i>")
    )
  } else {
    if (any(str_detect(colnames(x), "RTC"))) {
      kable_cp_vol_cols <- c("Test & Priority", "Setting", "RTC")
    } else {
      kable_cp_vol_cols <- c("Test & Priority", "Setting",
                             "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN")
    }
    
    
    kable(x, format = "html", escape = FALSE, align = "c",
          col.names = kable_cp_vol_cols) %>%
      kable_styling(bootstrap_options = "hover",
                    position = "center",
                    font_size = 11) %>%
      column_spec(column = c(1, length(kable_cp_vol_cols)),
                  border_right = "thin solid lightgray") %>%
      add_header_above(c(" " = 1,
                         "Resulted Lab Volume" = (ncol(x) - 1)),
                       background = c("white", "#00AEEF"),
                       color = "white",
                       line = FALSE,
                       font_size = 13) %>%
      column_spec(column = 2:length(kable_cp_vol_cols), background = "#E6F8FF", color = "black") %>%
      # column_spec(column = 2:8,
      #             background = "inherit",
      #             color = "inherit") %>%
      # column_spec(column = 1,
      #             width_min = "125px",
      #             include_thead = TRUE) %>%
      # column_spec(column = c(3, 11),
      #             width_min = "100px",
      #             include_thead = TRUE) %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = c(1, 2))
  }
}

# Custom function for creating a kable of labs with missing collections --------
kable_missing_collections <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    asis_output(
      paste("<i>",
            "No data available to determine missing collections.",
            "</i>")
    )
  } else {
    # Filter data for city sites and summarize
    missing_collect <- x %>%
      group_by(Site) %>%
      summarize(ResultedVolume = sum(TotalResulted),
                MissingCollection = sum(TotalMissingCollections, na.rm = TRUE),
                Percent = formattable::percent(MissingCollection / ResultedVolume,
                                  digits = 0),
                .groups = "keep") %>%
      ungroup() %>%
      mutate(
        # Apply conditional formatting based on percentage of labs with missing
        # collections
        Percent = cell_spec(
          Percent, "html",
          color = ifelse(is.na(Percent), "grey",
                         ifelse(Percent <= 0.05, "green",
                                ifelse(Percent <= 0.15, "orange", "red")))),
        # Format site as factors
        Site = factor(Site, levels = all_sites, ordered = TRUE))
    #
    # Create template to ensure all sites are included
    missing_collect <- left_join(data.frame("Site" = factor(all_sites,
                                                            levels = all_sites,
                                                            ordered = TRUE)),
                                 missing_collect,
                                 by = c("Site" = "Site"))
    #
    # Cast missing collections into table format
    missing_collect_table <- dcast(missing_collect,
                                   "Percentage of Specimens" ~ Site,
                                   value.var = "Percent")
    # Create kable with summarized data
    missing_collect_table %>%
      kable(format = "html", escape = FALSE, align = "c",
            col.names = c("Site",
                          "MSH", "MSQ", "MSBI", "MSB", "MSW",
                          "MSM", "MSSN", "RTC")) %>%
      kable_styling(
        bootstrap = "hover",
        position = "center",
        font_size = 11,
        full_width = FALSE) %>%
      add_header_above(
        c(" " = 1,
          "Percentage of Labs Missing Collect Times" =
            ncol(missing_collect_table) - 1),
        background = c("white", "#00AEEF"),
        color = "white",
        line = FALSE,
        font_size = 13) %>%
      column_spec(column = c(1, ncol(missing_collect_table)),
                  border_right = "thin solid lightgray") %>%
      column_spec(column = c(2:ncol(missing_collect_table)),
                  background = "#E6F8FF",
                  color = "black") %>%
      # column_spec(column = c(2:ncol(missing_collect_table)),
      #             background = "inherit",
      #             color = "inherit",
      #             width_max = 0.15) %>%
      row_spec(row = 0, font_size = 13)
  }
}

# Custom function for creating a kable of add-on order volume
kable_add_on_volume <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    asis_output(
      paste("<i>",
            "No data available to determine add on order volume.",
            "</i>")
    )
  } else {
    # Filter data for city sites and summarize
    add_on_volume <- x %>%
      group_by(Test, Site) %>%
      summarize(AddOnVolume = sum(TotalAddOnOrder, na.rm = TRUE),
                .groups = "keep") %>%
      ungroup()
    
    add_on_volume <- left_join(test_site_comb, add_on_volume,
                               by = c("Site" = "Site",
                                      "Test" = "Test"))
    
    add_on_volume <- add_on_volume %>%
      mutate(
        # Set test and site as factors
        Test = droplevels(factor(Test, levels = test_names, ordered = TRUE)),
        Site = factor(Site, levels = all_sites, ordered = TRUE),
        AddOnVolume = ifelse(is.na(AddOnVolume), 0, AddOnVolume))
    
    add_on_table <- dcast(add_on_volume, Test ~ Site, value.var = "AddOnVolume")
    
    # Create kable of add on orders
    add_on_table %>%
      kable(format = "html", escape = FALSE, align = "c",
            col.names = c("Test",
                          "MSH", "MSQ", "MSBI", "MSB", "MSW",
                          "MSM", "MSSN", "RTC"),
            color = "gray") %>%
      kable_styling(
        bootstrap = "hover",
        position = "center",
        font_size = 11,
        full_width = FALSE) %>%
      add_header_above(
        c(" " = 1,
          "Volume of Add On Labs" = ncol(add_on_table) - 1),
        background = c("white", "#00AEEF"),
        color = "white",
        line = FALSE,
        font_size = 13) %>%
      column_spec(
        column = c(1, ncol(add_on_table)),
        border_right = "thin solid lightgray") %>%
      column_spec(
        column = c(2:ncol(add_on_table)),
        background = "#E6F8FF", color = "black") %>%
      # column_spec(column = c(2:ncol(add_on_table)),
      #             background = "inherit",
      #             color = "inherit") %>%
      row_spec(row = 0, font_size = 13)
  }
}
