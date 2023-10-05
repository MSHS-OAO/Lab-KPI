# Code for preprocessing CP data prior to receipt of Ops &
# Quality Indicators Form

# Preprocess raw data using pre-defined custom functions ------------
scc_sun_list <- preprocess_cp(raw_scc = scc_data_raw,
                              raw_sun = sq_data_raw)

scc_df <- scc_sun_list[[1]]
sun_df <- scc_sun_list[[2]]
scc_sun_df_master <- scc_sun_list[[3]]

#
# Summarize  data by site, date, test, setting, priority, etc.-------
if (is.null(scc_sun_df_master)) {
  cp_summary <- NULL
} else {
  cp_summary <- scc_sun_df_master %>%
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
}