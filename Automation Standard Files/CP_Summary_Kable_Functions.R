# Code for preprocessing, analyzing, and displaying Clinical Pathology KPI -----
# CP includes Chemistry, Hematology, and Microbiology RRL divisions

# Custom function to subset and summarize data for each lab division ----------
summarize_cp_tat <- function(x, lab_division) {
  if (is.null(x) || nrow(x) == 0) {
    lab_summary <- NULL
    lab_dashboard_cast <- NULL
  } else {
    # Subset data to be included based on lab division, whether or not TAT
    # meets inclusion criteria, and site location
    lab_summary <- x %>%
      # filter(DIVISION == lab_division) %>%
      
      
      
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
                                lab_division %in% c("Infusion","Blood Bank")),
                           "green",
                           ifelse(
                             (ReceiveResultPercent >= 0.8 &
                                lab_division %in%
                                c("Chemistry", "Hematology", "Infusion","Blood Bank")) |
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
                                lab_division %in% c("Infusion","Blood Bank")),
                           "green",
                           ifelse(
                             (CollectResultPercent >= 0.8 &
                                lab_division %in%
                                c("Chemistry", "Hematology", "Infusion","Blood Bank")) |
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
  lab_sub_output <- list(lab_summary,
                         lab_dashboard_cast)
  # #
  return(lab_sub_output)
  # return(lab_dashboard_cast)
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
      # filter(Division == lab_division) %>%
      group_by(SITE,
               TEST,
               DASHBOARD_PRIORITY,
               DETAILED_SETTING) %>%
      summarize(ResultedLabs = sum(TOTAL_RESULTED),
                .groups = "keep") %>%
      rename(Site = SITE,
             Test = TEST,
             DashboardPriority = DASHBOARD_PRIORITY,
             DetailedSetting = DETAILED_SETTING)
    #
    # Subset volume dataframe template for this division
    lab_div_vol_templ <- vol_dashboard_templ %>%
      filter(Division == lab_division) %>%
      select(-Incl)
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
            "No data available.",
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
                    font_size = 11,
                    full_width = FALSE) %>%
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
      group_by(SITE) %>%
      summarize(ResultedVolume = sum(TOTAL_RESULTED),
                MissingCollection = sum(TOTAL_MISSING_COLLECTIONS, na.rm = TRUE),
                Percent = formattable::percent(MissingCollection / ResultedVolume,
                                               digits = 0),
                .groups = "keep") %>%
      ungroup() %>%
      rename(Site = SITE) %>%
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
      group_by(TEST, SITE) %>%
      summarize(AddOnVolume = sum(TOTAL_ADD_ON_ORDER, na.rm = TRUE),
                .groups = "keep") %>%
      ungroup() %>%
      rename(Test = TEST,
             Site = SITE)
    
    test_site_templ <- tat_dashboard_templ %>%
      select(Test, Site) %>%
      distinct()
    
    add_on_volume <- left_join(test_site_templ, add_on_volume,
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
