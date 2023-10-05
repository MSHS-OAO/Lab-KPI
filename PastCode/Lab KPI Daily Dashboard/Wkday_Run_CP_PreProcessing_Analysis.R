# Code for preprocessing CP data prior to receipt of Ops &
# Quality Indicators Form

# Preprocess raw data using pre-defined custom functions ------------
scc_sun_wday_list <- preprocess_cp(raw_scc = scc_weekday,
                                   raw_sun = sq_weekday)
scc_wday <- scc_sun_wday_list[[1]]
sun_wday <- scc_sun_wday_list[[2]]
scc_sun_wday_master <- scc_sun_wday_list[[3]]

if (is.null(sq_not_weekday) & is.null(scc_not_weekday)) {
  include_not_wday <- FALSE
  scc_sun_not_wday_list <- NULL
  scc_not_wday <- NULL
  sun_not_wday <- NULL
  scc_sun_not_wday_master <- NULL
} else {
  include_not_wday <- TRUE
  scc_sun_not_wday_list <- preprocess_cp(raw_scc = scc_not_weekday,
                                         raw_sun = sq_not_weekday)
  scc_not_wday <- scc_sun_not_wday_list[[1]]
  sun_not_wday <- scc_sun_not_wday_list[[2]]
  scc_sun_not_wday_master <- scc_sun_not_wday_list[[3]]
}

# Update preprocessed data to only include correct dates ----------------------
scc_sun_wday_master <- correct_scc_result_dates(scc_sun_wday_master, 1)

if (scenario == 1) {
  scc_sun_not_wday_master <- correct_scc_result_dates(scc_sun_not_wday_master,
                                                      3)
} else if (scenario == 2) {
  scc_sun_not_wday_master <- correct_scc_result_dates(scc_sun_not_wday_master,
                                                      2)
} else if (scenario == 3) {
  scc_sun_not_wday_master <- correct_scc_result_dates(scc_sun_not_wday_master,
                                                      1)
}


# Determine resulted date for weekday labs
wday_result_date_as_date <- unique(scc_sun_wday_master$ResultDate)
wday_result_date <- format(wday_result_date_as_date,
                           format = "%a %m/%d/%y")


if (!is.null(scc_sun_not_wday_master)) {
  not_wday_result_date <- sort(unique(scc_sun_not_wday_master$ResultDate))
  wkend_holiday_result_date <- ifelse(
    length(not_wday_result_date) == 1,
    format(not_wday_result_date, format = "%a %m/%d/%y"),
    paste0(format(not_wday_result_date[1],
                  format = "%a %m/%d/%y"),
           "-",
           format(not_wday_result_date[length(not_wday_result_date)],
                  format = "%a %m/%d/%y")))
  date_range <- c(wday_result_date_as_date, not_wday_result_date)
} else {
  wkend_holiday_result_date <- NULL
  date_range <- c(wday_result_date_as_date)
}

#
# Summarize weekday data by site, date, test, setting, priority, etc.-------
cp_wday_summary <- scc_sun_wday_master %>%
  group_by(Site,
           ResultDate,
           Test,
           Division,
           Setting,
           SettingRollUp,
           MasterSetting,
           DashboardSetting,
           OrderPriority,
           AdjPriority,
           DashboardPriority,
           ReceiveResultTarget,
           CollectResultTarget) %>%
  summarize(TotalResulted = n(),
            ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
            CollectTime_VolIncl = sum(CollectTime_TATInclude),
            TotalReceiveResultInTarget =
              sum(ReceiveResultInTarget[ReceiveTime_TATInclude]),
            TotalCollectResultInTarget =
              sum(CollectResultInTarget[CollectTime_TATInclude]),
            TotalAddOnOrder = sum(AddOnMaster == "AddOn"),
            TotalMissingCollections = sum(MissingCollect),
            .groups = "keep") %>%
  arrange(Site, ResultDate) %>%
  ungroup()


# Format and summarize data to update repository ----------------
# Summarize and bind weekday and non-weekday data, if any exists
if (!is.null(scc_sun_not_wday_master)) {
  # Summarize weekend/holiday data by site, date, test, setting, priority, etc.
  cp_not_wday_summary <- scc_sun_not_wday_master %>%
    group_by(Site,
             ResultDate,
             Test,
             Division,
             Setting,
             SettingRollUp,
             MasterSetting,
             DashboardSetting,
             OrderPriority,
             AdjPriority,
             DashboardPriority,
             ReceiveResultTarget,
             CollectResultTarget) %>%
    summarize(TotalResulted = n(),
              ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
              CollectTime_VolIncl = sum(CollectTime_TATInclude),
              TotalReceiveResultInTarget =
                sum(ReceiveResultInTarget[ReceiveTime_TATInclude]),
              TotalCollectResultInTarget =
                sum(CollectResultInTarget[CollectTime_TATInclude]),
              TotalAddOnOrder = sum(AddOnMaster == "AddOn"),
              TotalMissingCollections = sum(MissingCollect),
              .groups = "keep") %>%
    arrange(Site, ResultDate) %>%
    ungroup()
  #
  # Combine weekday and weekend data into dataframe for repository
  cp_all_days <- rbind(cp_wday_summary, cp_not_wday_summary)
} else {
  #
  # Create dataframe for repository
  cp_all_days <- cp_wday_summary
}

# # Open existing repository
# existing_repo <-
#   readRDS(file =
#             choose.files(
#               default =
#                 paste0(user_directory,
#                        "/CP Historical Repo",
#                        "/*.*"),
#               caption = "Select SCC and Sunquest Historical Repository"))
# 
# # Convert ResultDate from date-time to date
# existing_repo <- existing_repo %>%
#   mutate(ResultDate  = date(ResultDate)) %>%
#   filter(!(ResultDate %in% date_range))
# 
# # Bind new data with existing repository
# cp_repo <- rbind(existing_repo, cp_all_days)
# 
# # Remove any duplicates
# cp_repo <- unique(cp_repo)
# 
# # Determine earliest and latest date in repository for use in file name
# start_date <- format(min(cp_repo$ResultDate), "%m%d%y")
# end_date <- format(max(cp_repo$ResultDate), "%m%d%y")
# 
# saveRDS(cp_repo, file = paste0(user_directory,
#                                "/CP Historical Repo",
#                                "/CP Repo ", start_date, " to ",
#                                end_date, " Created ",
#                                format(Sys.Date(), "%Y-%m-%d"), ".RDS"))
