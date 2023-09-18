# Code for preprocessing CP data prior to receipt of Ops &
# Quality Indicators Form

# Preprocess raw data using pre-defined custom functions ------------
if (is.null(scc_data_raw) | nrow(scc_data_raw) == 0){
  scc_processed <- NULL
} else {
  scc_processed <- preprocess_scc(raw_scc = scc_data_raw)
}

if (is.null(sq_data_raw) | nrow(sq_data_raw) == 0){
  sun_processed <- NULL
} else {
  sun_processed <- preprocess_sun(raw_sun = sq_data_raw)
}

cp_processed <- rbind(scc_processed, sun_processed)


#
# Summarize  data by site, date, test, setting, priority, etc.-------
if (is.null(cp_processed)) {
  cp_summary <- NULL
} else {
  cp_summary <- cp_processed %>%
    group_by(SITE,
             RESULT_DATE,
             TEST,
             DIVISION,
             SETTING_ROLL_UP,
             DETAILED_SETTING,
             DASHBOARD_SETTING,
             ADJ_PRIORITY,
             DASHBOARD_PRIORITY,
             RECEIVE_RESULT_TARGET,
             COLLECT_RESULT_TARGET) %>%
    summarize(
      # Calculate total number of labs resulted
      TOTAL_RESULTED = n(),
      # Calculate number of labs with valid receive times and collection times
      RECEIVE_TIME_VOL_INCL = sum(RECEIVE_TIME_TAT_INCL, na.rm = TRUE),
      COLLECT_TIME_VOL_INCL = sum(COLLECT_TIME_TAT_INCL, na.rm = TRUE),
      # Calculate number of labs within target TAT
      TOTAL_RECEIVE_RESULT_IN_TARGET =
        sum(RECEIVE_RESULT_IN_TARGET[RECEIVE_TIME_TAT_INCL], na.rm = TRUE),
      TOTAL_COLLECT_RESULT_IN_TARGET =
        sum(COLLECT_RESULT_IN_TARGET[COLLECT_TIME_TAT_INCL], na.rm = TRUE),
      TOTAL_ADD_ON_ORDER = sum(ADD_ON_FINAL %in% c("AddOn"), na.rm = TRUE),
      TOTAL_MISSING_COLLECTIONS = sum(MISSING_COLLECT),
      .groups = "keep") %>%
    arrange(SITE, RESULT_DATE) %>%
    ungroup()
}