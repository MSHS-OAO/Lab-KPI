# Custom functions for processing raw SCC and Sunquest reports

# SCC Processing ----------------
preprocess_scc <- function(raw_scc)  {
  # Preprocess SCC data -------------------------------
  # Remove any duplicates
  raw_scc <- unique(raw_scc)
  # Correct and format any columns that were not imported correctly; usually have a message saying "*failed to decode... *"
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
      MISSING_COLLECT = COLLECT_TO_RECEIVE_TAT %in% 0, #COLLECTION_DATE %in% RECEIVE_DATE,
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

# Sunquest Processing -------------
preprocess_sun <- function(raw_sun) {
  
  # Preprocess Sunquest data --------------------------------
  # Remove any duplicates
  raw_sun <- unique(raw_sun)
  # Correct and format any columns that were not imported correctly; usually have a message saying "*failed to decode... *"  
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
      MISSING_COLLECT = (CollectDateTime - OrderDateTime) %in% 0,
        # CollectDateTime %in% OrderDateTime,
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