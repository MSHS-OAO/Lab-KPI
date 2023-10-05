#######
# Code for preprocessing, analyzing, and displaying Anatomic Pathology KPI -----
# Anatomic Pathology (AP) includes Cytology and Surgical Pathology divisions
#######

# Function to preprate prepare cytology data for pre-processing by crosswalking
# Epic and PowerPath data
cyto_prep <- function(epic_data, pp_data) {
  if (is.null(epic_data) || is.null(pp_data) || 
      nrow(epic_data) == 0 || nrow(pp_data) == 0) {
    cyto_final <- NULL
  } else {
    # Preprocess Epic data
    # Select specimens that were finalized in Epic based on Lab Status
    epic_data_final <- epic_data %>%
      filter(LAB_STATUS %in% c("Final result", "Edited Result - FINAL"))
    
    # Create dataframe of unique specimen ID for crosswalking with PowerPath data
    # cross-walking with PowerPath data
    epic_data_spec <- epic_data_final %>%
      distinct(SPECIMEN_ID)
    
    # Update names for MSH and MSM
    pp_data <- pp_data %>%
      mutate(Facility = ifelse(Facility == "MSS", "MSH",
                               ifelse(Facility == "STL", "SL",
                                      Facility)))
    
    # Subset PowerPath data to keep Cyto Gyn and Cyto NonGyn and primary
    # specimens only
    cyto_raw <- pp_data %>%
      filter(spec_sort_order == "A" &
               spec_group %in% c("CYTO NONGYN", "CYTO GYN"))
    
    cyto_final <- merge(x = cyto_raw, y = epic_data_spec,
                        by.x = "Case_no",
                        by.y = "SPECIMEN_ID")
    
  }
  
  return(cyto_final)
  
}

#create a function to prepare pathology data for pre-processing
patho_prep <- function(raw_data, gi_codes) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    sp_data <- NULL
  } else {
    
    #------------Extract the All Breast and GI specs Data Only--------------#
    # Merge the inclusion/exclusion criteria with PowerPath data to determine
    # which GI cases to include in the analysis
    
    raw_data <- merge(x = raw_data, y = gi_codes, all.x = TRUE)
    
    raw_data <- raw_data %>%
      mutate(
        # Update names for MSH and MSM
        Facility = ifelse(Facility == "MSS", "MSH",
                          ifelse(Facility == "STL", "SL",
                                 Facility)),
        # Change BREAST specimens to proper case
        spec_group = ifelse(spec_group == "BREAST", "Breast", spec_group))
    
    #Create dataframe with cases that should be excluded based on GI code
    exclude_gi_codes_df <- raw_data %>%
      filter(spec_group %in% c("GI") &
               !(GI_Code_InclExcl %in% c("Include")))
    
    # Create vector of case numbers to exclude
    exclude_case_num <- unique(exclude_gi_codes_df$Case_no)
    
    # Subset surgical pathology data based on inclusion criteria
    sp_data <- raw_data %>%
      filter(# Select primary specimens only
        spec_sort_order == "A" &
          # Select GI specimens with codes that are included and any breast specimens
          ((spec_group == "GI" & !(Case_no %in% exclude_case_num)) |
             (spec_group == "Breast" )) &
          # Exclude NYEE
          Facility != "NYEE")
  }
  
  return(sp_data)
  
}

#------------------------------Data Pre-Processing-----------------------------#
############Create a function for preprocessing AP data ############
pre_processing_pp <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    raw_data_new <- NULL
    summarized_table <- NULL
  } else {
    # Crosswalk Rev_ctr and patient setting for PowerPath data
    raw_data_ps <- merge(x = raw_data, y = patient_setting, all.x = TRUE)
    
    # Update MSB patient setting based on patient type column
    raw_data_ps$Patient.Setting[raw_data_ps$Rev_ctr == "MSBK" &
                                  (raw_data_ps$patient_type == "A" |
                                     raw_data_ps$patient_type == "O")] <- "Amb"
    
    raw_data_ps$Patient.Setting[raw_data_ps$Rev_ctr == "MSBK" &
                                  raw_data_ps$patient_type == "IN"] <- "IP"
    
    # Crosswalk TAT targets based on spec_group and patient setting
    raw_data_new <- merge(x = raw_data_ps, y = tat_targets_ap,
                          all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    
    # check if any of the dates were imported as characters
    if (is.character(raw_data_new$Collection_Date)) {
      raw_data_new <- raw_data_new %>%
        mutate(Collection_Date = as.numeric(Collection_Date)) %>%
        mutate(Collection_Date = as.Date(Collection_Date,
                                         origin = "1899-12-30"))
    } else {
      raw_data_new <- raw_data_new %>%
        mutate(Collection_Date = Collection_Date)
    }
    #Change all Dates into POSIXct format to start the calculations
    raw_data_new[c("Case_created_date",
                   "Collection_Date",
                   "Received_Date",
                   "signed_out_date")] <-
      lapply(raw_data_new[c("Case_created_date",
                            "Collection_Date",
                            "Received_Date",
                            "signed_out_date")],
             as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p")
    
    # Add columns for turnaround time calculations:
    # Collection to signed out (in calendar days) and
    # received to signed out (in business days)
    raw_data_new <- raw_data_new %>%
      mutate(
        # Add column for collected to signed out turnaround time in calendar days
        Collection_to_signed_out =
          as.numeric(difftime(signed_out_date, Collection_Date,
                              units = "days")),
        # Add column for received to signed out turnaround time in business days
        Received_to_signed_out = bizdays(Received_Date, signed_out_date),
        # Prepare data for accessioned volume analysis
        # First find the date of the last weekday and add 1 for report date
        report_date_only = as.Date(signed_out_date) + 1,
        #  Find the accessioned date and use this for determining accessioned volume
        acc_date_only = as.Date(Received_Date)) %>%
      # Filter out anything with a sign out date other than result date of interest
      filter(date(signed_out_date) %in% resulted_date)
    
    # raw_data_new <- raw_data_new %>%
    #   mutate(Collection_to_signed_out =
    #            as.numeric(difftime(signed_out_date, Collection_Date,
    #                                units = "days")))
    # #recieve to signed out
    # #without weekends and holidays
    # raw_data_new <- raw_data_new %>%
    #   mutate(Received_to_signed_out = bizdays(Received_Date, signed_out_date))
    # 
    # #prepare data for first part accessioned volume analysis
    # #1. Find the date that we need to report --> the date of the last weekday
    # raw_data_new$report_date_only <- as.Date(raw_data_new$signed_out_date) + 1
    # 
    # #2. count the accessioned volume that was accessioned on that date
    # #from the cyto report
    # raw_data_new$acc_date_only <- as.Date(raw_data_new$Received_Date)
    
    #summarize the data to be used for analysis and to be stored as historical
    #repo
    summarized_table <-
      summarise(
        group_by(raw_data_new,
                 Spec_code,
                 spec_group,
                 Facility,
                 Patient.Setting,
                 Rev_ctr,
                 as.Date(signed_out_date),
                 weekdays(as.Date(signed_out_date)),
                 Received.to.signed.out.target..Days.,
                 Collected.to.signed.out.target..Days.,
                 acc_date_only,
                 weekdays(acc_date_only),
                 report_date_only,
                 weekdays(report_date_only)),
        no_cases_signed = n(),
        lab_metric_tat_avg = round(mean(Received_to_signed_out,
                                        na.rm = TRUE), 0),
        lab_metric_tat_med = round(median(Received_to_signed_out,
                                          na.rm = TRUE), 0),
        lab_metric_tat_sd = round(sd(Received_to_signed_out, na.rm = TRUE), 1),
        lab_metric_within_target = as.numeric(format(
          round(
            sum(Received_to_signed_out <= Received.to.signed.out.target..Days.,
                na.rm = TRUE) / sum(
                  Received_to_signed_out >= 0, na.rm = TRUE), 2))),
        patient_metric_tat_avg = as.numeric(format(
          ceiling(mean(Collection_to_signed_out, na.rm = TRUE)))),
        patient_metric_tat_med = round(median(Collection_to_signed_out,
                                              na.rm = TRUE), 0),
        patient_metric_tat_sd = round(sd(Collection_to_signed_out,
                                         na.rm = TRUE), 1),
        cyto_acc_vol = as.numeric(sum((report_date_only - 1) == acc_date_only,
                                      na.rm = TRUE)))
    
    colnames(summarized_table) <-
      c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
        "Signed_out_date_only", "Signed_out_day_only", "Lab_metric_target",
        "Patient_metric_target", "acc_date_only", "acc_day_only",
        "report_date_only", "report_day_only", "No_cases_signed_out",
        "Lab_metric_avg", "Lab_metric_med", "Lab_metric_std",
        "Lab_metric_within_target", "Patient_metric_avg", "Patient_metric_med",
        "Patient_metric_std", "cyto_acc_vol")
    
    # # Filter out any specimens signed out on other dates
    # summarize_table <- summarized_table %>%
    #   filter(Signed_out_date_only %in% dates)
    
  }
  
  return_tables <- list(summarized_table,
                        raw_data_new)
  
  return(return_tables)
}

##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
analyze_pp <- function(summarized_table) {
  if (is.null(summarized_table) || nrow(summarized_table) == 0) {
    processed_data_table <- NULL
    processed_data_table_v2 <- NULL
    vol_cases_signed_strat <- NULL
    cyto_acc_vol1 <- NULL
  } else {
    #Calculate total number of cases signed per spec group
    vol_cases_signed <- summarise(group_by(summarized_table,
                                           Spec_group,
                                           Patient_setting),
                                  no_cases_signed = sum(No_cases_signed_out,
                                                        na.rm = TRUE))
    
    #Calculate total number of cases signed out per spec_group per facility
    vol_cases_signed_strat <- summarise(group_by(summarized_table,
                                                 Spec_group,
                                                 Facility,
                                                 Patient_setting),
                                        no_cases_signed =
                                          sum(No_cases_signed_out,
                                              na.rm = TRUE))
    
    vol_cases_signed_strat <- dcast(
      vol_cases_signed_strat,
      Spec_group + Patient_setting ~ Facility, value.var = "no_cases_signed")
    
    vol_cases_signed_strat[is.na(vol_cases_signed_strat)] <- 0
    
    #Calculate average collection to signed out
    patient_metric <- summarise(group_by(summarized_table,
                                         Spec_group,
                                         Facility,
                                         Patient_setting),
                                avg_collection_to_signed_out =
                                  format(
                                    round(
                                      sum(
                                        (Patient_metric_avg *
                                           No_cases_signed_out) /
                                          sum(No_cases_signed_out),
                                        na.rm = TRUE), 0)))
    
    patient_metric <- dcast(patient_metric,
                            Spec_group + Patient_setting ~ Facility,
                            value.var = "avg_collection_to_signed_out")
    
    #Calculate % Receive to result TAT within target
    
    #this part of the code creates the table for the received to result TAT
    #within target with an assumption that the receive to result is not
    #centralized which means it is stratified by facility
    lab_metric <- summarise(group_by(summarized_table,
                                     Spec_group,
                                     Facility,
                                     Patient_setting),
                            received_to_signed_out_within_target =
                              format(
                                round(
                                  sum(
                                    (Lab_metric_within_target *
                                       No_cases_signed_out) /
                                      sum(No_cases_signed_out),
                                    na.rm = TRUE), 2)))
    
    lab_metric <- dcast(lab_metric,
                        Spec_group + Patient_setting ~ Facility,
                        value.var = "received_to_signed_out_within_target")
    
    #this part of the code creates the table for the received to result TAT
    #within target with an assumption that the receive to result is centralized
    lab_metric_v2 <- summarise(group_by(summarized_table,
                                        Spec_group,
                                        Patient_setting),
                               received_to_signed_out_within_target =
                                 format(
                                   round(
                                     sum(
                                       (Lab_metric_within_target *
                                          No_cases_signed_out) /
                                         sum(No_cases_signed_out),
                                       na.rm = TRUE), 2)))
    #here I will merge number of cases signed, received to result TAT,
    #and acollect to result TAT calcs into one table
    processed_data_table <-
      left_join(full_join(vol_cases_signed, lab_metric),
                patient_metric,
                by = c("Spec_group", "Patient_setting"))
    processed_data_table <-
      processed_data_table[!(processed_data_table$Patient_setting == "Other"), ]
    
    processed_data_table_v2 <-
      left_join(full_join(vol_cases_signed, lab_metric_v2),
                patient_metric,
                by = c("Spec_group", "Patient_setting"))
    processed_data_table_v2 <-
      processed_data_table_v2[!(processed_data_table_v2$Patient_setting ==
                                  "Other"), ]
    
    cyto_acc_vol1 <- summarise(group_by(summarized_table, Spec_group),
                               cyto_acc_vol1 =
                                 as.numeric(sum(cyto_acc_vol,
                                                na.rm = TRUE)))
  }
  return_tables <- list(processed_data_table,
                        processed_data_table_v2,
                        vol_cases_signed_strat,
                        cyto_acc_vol1)
  
  return(return_tables)
}


tat_targets_ap_kn <- tat_targets_ap %>%
  rename(ReceiveToResultTarget = Received.to.signed.out.target..Days.,
         CollectToResultTarget = Collected.to.signed.out.target..Days.)


preprocessing_ap <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    ap_div_df <- NULL
    ap_summary_by_code <- NULL
    vol_signed_out_site <- NULL
  } else {
    # Crosswalk Rev_ctr and patient setting for PowerPath data
    ap_div_df <- merge(x = raw_data, y = patient_setting, all.x = TRUE)
    
    # Update MSB patient setting based on patient type column
    ap_div_df <- ap_div_df %>%
      mutate(Patient.Setting = ifelse(Rev_ctr %in% c("MSBK") &
                                        patient_type %in% c("A", "O"), "Amb",
                                      ifelse(Rev_ctr %in% c("MSBK") &
                                               patient_type %in% c("IN"), "IP",
                                             Patient.Setting)))
    
    # Crosswalk TAT targets based on spec_group and patient setting
    ap_div_df <- merge(x = ap_div_df, y = tat_targets_ap_kn,
                       all.x = TRUE,
                       by = c("spec_group", "Patient.Setting"))
    
    # check if any of the dates were imported as characters
    if (is.character(ap_div_df$Collection_Date)) {
      ap_div_df <- ap_div_df %>%
        mutate(Collection_Date = as.numeric(Collection_Date)) %>%
        mutate(Collection_Date = as.Date(Collection_Date,
                                         origin = "1899-12-30"))
    }
    
    # Change all Dates into POSIXct format to start the calculations
    ap_div_df[c("Case_created_date",
                "Collection_Date",
                "Received_Date",
                "signed_out_date")] <-
      lapply(ap_div_df[c("Case_created_date",
                         "Collection_Date",
                         "Received_Date",
                         "signed_out_date")],
             as.POSIXct, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    
    # Add columns for turnaround time calculations:
    # Collection to signed out (in calendar days) and
    # received to signed out (in business days)
    ap_div_df <- ap_div_df %>%
      mutate(
        # Add column for collected to signed out turnaround time in calendar days
        Collection_to_signed_out =
          as.numeric(difftime(signed_out_date, Collection_Date,
                              units = "days")),
        # Add column for received to signed out turnaround time in business days
        Received_to_signed_out = bizdays(Received_Date, signed_out_date),
        # Prepare data for accessioned volume analysis
        # First find the date of the last weekday and add 1 for report date
        report_date_only = as.Date(signed_out_date) + 1,
        #  Find the accessioned date and use this for determining accessioned volume
        acc_date_only = as.Date(Received_Date)) %>%
      # Filter out anything with a sign out date other than result date of interest
      filter(date(signed_out_date) %in% resulted_date)
    
    # Summarize the data to be by spec code, spec group, facility, patient setting, etc.
    ap_summary_by_code <-
      summarise(
        group_by(ap_div_df,
                 Spec_code,
                 spec_group,
                 Facility,
                 Patient.Setting,
                 Rev_ctr,
                 as.Date(signed_out_date),
                 weekdays(as.Date(signed_out_date)),
                 ReceiveToResultTarget,
                 CollectToResultTarget,
                 acc_date_only,
                 weekdays(acc_date_only),
                 report_date_only,
                 weekdays(report_date_only)),
        # Calculate volume of cases signed out
        no_cases_signed = n(),
        # Calculate key statistics for received to signed out (lab ops metric)
        ReceiveResultAvg = mean(Received_to_signed_out, na.rm = TRUE),
        ReceiveResultMed = median(Received_to_signed_out, na.rm = TRUE),
        ReceiveResultSD = sd(Received_to_signed_out, na.rm = TRUE),
        ReceiveResultInTarget =
          sum(Received_to_signed_out <= ReceiveToResultTarget, na.rm = TRUE) / 
          sum(Received_to_signed_out >= 0, na.rm = TRUE),
        # Calculate key statistics for collected to singed out (patient-centric metric)
        CollectResultAvg = mean(Collection_to_signed_out, na.rm = TRUE),
        CollectResultMed = median(Collection_to_signed_out, na.rm = TRUE),
        CollectResultSD = sd(Collection_to_signed_out, na.rm = TRUE),
        # Calculate number of specimens signed out and accessioned yesterday
        AccVol = sum((report_date_only - 1) == acc_date_only,
                      na.rm = TRUE))
    
    # Rename columns
    colnames(ap_summary_by_code) <-
      c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
        "Signed_out_date_only", "Signed_out_day_only", "Lab_metric_target",
        "Patient_metric_target", "acc_date_only", "acc_day_only",
        "report_date_only", "report_day_only", "No_cases_signed_out",
        "Lab_metric_avg", "Lab_metric_med", "Lab_metric_std",
        "Lab_metric_within_target", "Patient_metric_avg", "Patient_metric_med",
        "Patient_metric_std", "acc_vol")
    
    # Calculate volume of cases signed out across the system stratified by spec group
    vol_signed_out_system <- ap_summary_by_code %>%
      group_by(Spec_group,
               Patient_setting) %>%
      summarize(No_cases_signed = sum(No_cases_signed_out, na.rm = TRUE))
    
    # Calculate volume of cases signed out from each originating site stratified by spec group
    vol_signed_out_site <- ap_summary_by_code %>%
      group_by(Spec_group,
               Facility,
               Patient_setting) %>%
      summarize(No_cases_signed = sum(No_cases_signed_out, na.rm = TRUE))
    
    vol_signed_out_site <- dcast(vol_signed_out_site,
                                 Spec_group + Patient_setting ~ Facility,
                                 value.var = "No_cases_signed")
    
    vol_signed_out_site[is.na(vol_signed_out_site)] <- 0
    
    # Calculate average collection to sign out time
    patient_metric <- ap_summary_by_code %>%
      group_by(Spec_group,
               Facility,
               Patient_setting) %>%
      summarize(avg_collection_to_signed_out =
                  format(
                    round(
                      sum((Patient_metric_avg * No_cases_signed_out) /
                            sum(No_cases_signed_out), na.rm = TRUE), 0)))
    
    # Calculate average collection to sign out time using
    patient_metric2 <- ap_div_df %>%
      group_by(spec_group,
               Facility,
               Patient.Setting) %>%
      summarize(CollectToResultAvg = (mean(Collection_to_signed_out,
                                           na.rm = TRUE)))
    
    patient_metric <- dcast(patient_metric,
                            Spec_group + Patient_setting ~ Facility,
                            value.var = "avg_collection_to_signed_out")
    
    patient_metric2 <- dcast(patient_metric2,
                             spec_group + Patient.Setting ~ Facility,
                             value.var = "CollectToResultAvg")
    
  }
  
  return_tables <- list(ap_summary_by_code,
                        ap_div_df,
                        vol_signed_out_site,)
  
  return(return_tables)
}

##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
analyze_ap <- function(summarized_table) {
  if (is.null(summarized_table) || nrow(summarized_table) == 0) {
    processed_data_table <- NULL
    processed_data_table_v2 <- NULL
    vol_cases_signed_strat <- NULL
    cyto_acc_vol1 <- NULL
  } else {
    # Calculate total number of cases signed out per spec group across system
    vol_signed_out_system <- summarise(
      group_by(summarized_table,
               Spec_group,
               Patient_setting),
      No_cases_signed = sum(No_cases_signed_out, na.rm = TRUE))
    
    # Calculate total number of cases signed out per spec_group by facility
    vol_signed_out_site <-
      summarise(
        group_by(summarized_table,
                 Spec_group,
                 Facility,
                 Patient_setting),
        No_cases_signed = sum(No_cases_signed_out, na.rm = TRUE))
    
    vol_signed_out_site <- dcast(vol_signed_out_site,
                                 Spec_group + Patient_setting ~ Facility,
                                 value.var = "No_cases_signed")
    
    vol_cases_signed_strat[is.na(vol_cases_signed_strat)] <- 0
    
    # Calculate average collection to sign out time
    patient_metric <- summarise(
      group_by(summarized_table,
               Spec_group,
               Facility,
               Patient_setting),
      avg_collection_to_signed_out =format(round(sum(
        (Patient_metric_avg *
           No_cases_signed_out) /
          sum(No_cases_signed_out),
        na.rm = TRUE), 0)))
    # Code added by Kate
    patient_metric2 <- raw_data_new %>%
      group_by(spec_group,
               Facility,
               Patient.Setting) %>%
      summarize(Avg_Collect_to_Result = (mean(Collection_to_signed_out,
                                              na.rm = TRUE)))
    
    patient_metric <- dcast(patient_metric,
                            Spec_group + Patient_setting ~ Facility,
                            value.var = "avg_collection_to_signed_out")
    
    #Calculate % Receive to result TAT within target
    
    # This part of the code creates the table for the received to result TAT
    # within target with an assumption that the receive to result is not
    #centralized which means it is stratified by facility
    lab_metric <- summarise(group_by(summarized_table,
                                     Spec_group,
                                     Facility,
                                     Patient_setting),
                            received_to_signed_out_within_target =
                              format(
                                round(
                                  sum(
                                    (Lab_metric_within_target *
                                       No_cases_signed_out) /
                                      sum(No_cases_signed_out),
                                    na.rm = TRUE), 2)))
    
    lab_metric <- dcast(lab_metric,
                        Spec_group + Patient_setting ~ Facility,
                        value.var = "received_to_signed_out_within_target")
    
    # This part of the code creates the table for the received to result TAT
    # within target with an assumption that the receive to result is centralized
    lab_metric_v2 <- summarise(group_by(summarized_table,
                                        Spec_group,
                                        Patient_setting),
                               received_to_signed_out_within_target =
                                 format(
                                   round(
                                     sum(
                                       (Lab_metric_within_target *
                                          No_cases_signed_out) /
                                         sum(No_cases_signed_out),
                                       na.rm = TRUE), 2)))
    # Merge number of cases signed, received to result TAT,and collect to result
    # TAT calcs into one table
    processed_data_table <-
      left_join(full_join(vol_cases_signed, lab_metric),
                patient_metric,
                by = c("Spec_group", "Patient_setting"))
    processed_data_table <-
      processed_data_table[!(processed_data_table$Patient_setting == "Other"), ]
    
    processed_data_table_v2 <-
      left_join(full_join(vol_cases_signed, lab_metric_v2),
                patient_metric,
                by = c("Spec_group", "Patient_setting"))
    processed_data_table_v2 <-
      processed_data_table_v2[!(processed_data_table_v2$Patient_setting ==
                                  "Other"), ]
    
    cyto_acc_vol1 <- summarise(group_by(summarized_table, Spec_group),
                               cyto_acc_vol1 =
                                 as.numeric(sum(cyto_acc_vol,
                                                na.rm = TRUE)))
  }
  return_tables <- list(processed_data_table,
                        processed_data_table_v2,
                        vol_cases_signed_strat,
                        cyto_acc_vol1)
  
  return(return_tables)
}





##### This function helps in preprocessing the raw backlog data.
# Will be used in first run
pre_processing_backlog <- function(cyto_backlog_raw) {
  #cyto backlog Calculation
  if (is.null(cyto_backlog_raw) || nrow(cyto_backlog_raw) == 0) {
    summarized_table <- NULL
  } else {
    #vlookup the Rev_Center and its corresponding patient setting for the
    #PowerPath Data
    
    cyto_backlog_ps <- merge(x = cyto_backlog_raw, y = patient_setting,
                             all.x = TRUE)
    
    #vlookup targets based on spec_group and patient setting
    cyto_backlog_ps_target <- merge(x = cyto_backlog_ps, y = tat_targets_ap,
                                    all.x = TRUE,
                                    by = c("spec_group", "Patient.Setting"))
    
    #Keep the cyto gyn and cyto non-gyn
    cyto_backlog <-
      cyto_backlog_ps_target[which(
        cyto_backlog_ps_target$spec_group == "CYTO NONGYN" |
          cyto_backlog_ps_target$spec_group == "CYTO GYN"), ]
    
    #Change all Dates into POSIXct format to start the calculations
    cyto_backlog[c("Case_created_date", "Collection_Date", "Received_Date",
                   "signed_out_date")] <-
      lapply(cyto_backlog[c("Case_created_date", "Collection_Date",
                            "Received_Date", "signed_out_date")],
             as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p")
    
    #Backlog Calculations: Date now - case created date
    #without weekends and holidays, subtract one so we don't include today's date
    
    cyto_backlog$backlog <-
      bizdays(cyto_backlog$Case_created_date, today) - 1
    
    cyto_backlog$acc_date_only <- as.Date(cyto_backlog$Received_Date)
    
    # acc_date <- cyto_table_weekday_summarized$Signed_out_date_only[1]
    acc_date <- resulted_date
    cyto_backlog$Report_Date <- resulted_date + 1
    
    #summarize the data to be used for analysis and to be stored as historical
    #repo
    summarized_table <-
      summarise(
        group_by(cyto_backlog,
                 Spec_code,
                 spec_group,
                 Facility,
                 Patient.Setting,
                 Rev_ctr,
                 acc_date_only,
                 weekdays(acc_date_only),
                 Report_Date),
        cyto_backlog = format(
          round(
            sum(
              backlog > Received.to.signed.out.target..Days.,
              na.rm = TRUE), 0)),
        
        percentile_25th =
          format(
            ceiling(
              quantile(
                backlog[backlog > Received.to.signed.out.target..Days.],
                prob = 0.25, na.rm = TRUE))),
        
        percentile_50th =
          format(
            ceiling(
              quantile(
                backlog[backlog > Received.to.signed.out.target..Days.],
                prob = 0.5, na.rm = TRUE))),
        
        maximum = format(
          ceiling(
            max(
              backlog[backlog > Received.to.signed.out.target..Days.],
              na.rm = TRUE))),
        
        cyto_acc_vol = as.numeric(sum(acc_date == acc_date_only,
                                      na.rm = TRUE)))
    
    summarized_table$maximum[summarized_table$maximum == "-Inf"] <- 0
    
    #standardize the name for the current summary to match the historical repo
    colnames(summarized_table) <-
      c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
        "acc_date_only", "acc_day_only", "Report_Date", "cyto_backlog",
        "percentile_25th", "percentile_50th", "maximum", "cyto_acc_vol")
  }
  return(summarized_table)
  
}

##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
analyze_backlog <- function(summarized_table) {
  if (is.null(summarized_table)) {
    backlog_acc_table_new2 <- NULL
  } else {
    cyto_backlog_vol <- summarise(group_by(summarized_table, Spec_group),
                                  cyto_backlog = sum(as.numeric(cyto_backlog),
                                                     na.rm = TRUE),
                                  percentile_25th =
                                    ceiling(quantile(as.numeric(percentile_25th),
                                                     prob = 0.25,
                                                     na.rm = TRUE)),
                                  percentile_50th =
                                    ceiling(quantile(as.numeric(percentile_50th),
                                                     prob = 0.5,
                                                     na.rm = TRUE)),
                                  maximum =
                                    ceiling(max(as.numeric(maximum),
                                                na.rm = TRUE)))
    
    cyto_backlog_vol$maximum[cyto_backlog_vol$maximum == "-Inf"] <- 0
    #Days of work
    cyto_case_vol_dow <- as.numeric(cyto_backlog_vol$cyto_backlog[1]) / 80
    
    #count the accessioned volume that was accessioned on that date
    #from the backlog report
    cyto_acc_vol2 <-
      summarise(
        group_by(
          summarized_table,
          Spec_group),
        cyto_acc_vol2 = as.numeric(sum(cyto_acc_vol,
                                       na.rm = TRUE)))
    #sum the two counts
    cyto_acc_vol3 <- merge(x = cyto_acc_vol1, y = cyto_acc_vol2)
    
    cyto_acc_vol3$total_acc_vol <-
      cyto_acc_vol3$cyto_acc_vol1 + cyto_acc_vol3$cyto_acc_vol2
    
    cyto_acc_vol3$cyto_acc_vol1 <- NULL
    cyto_acc_vol3$cyto_acc_vol2 <- NULL
    
    backlog_acc_table <- merge(x = cyto_acc_vol3,
                               y = cyto_backlog_vol,
                               all = TRUE)
    
    table_temp_backlog_acc <- data.frame(matrix(ncol = 6, nrow = 2))
    colnames(table_temp_backlog_acc) <- c("Spec_group",
                                          "total_accessioned_volume",
                                          "cyto_backlog",
                                          "percentile_25th",
                                          "percentile_50th",
                                          "maximum")
    
    table_temp_backlog_acc[1] <- c("CYTO GYN", "CYTO NONGYN")
    
    backlog_acc_table_new2 <- merge(x = table_temp_backlog_acc[1],
                                    y = backlog_acc_table,
                                    all.x = TRUE, by = c("Spec_group"))
    
    backlog_acc_table_new2[is.na(backlog_acc_table_new2)] <- 0
    
    #added this line to delete the cyto gyn from the table until we get
    #correct data. Currently not in use
    backlog_acc_table_new3 <- backlog_acc_table_new2[-c(1), ]
  }
  return(backlog_acc_table_new2)
}

######Create a function for Table standardization for cyto and patho########
#To add all the missing rows and columns
#cyto table
table_merging_cyto <- function(cyto_table) {
  if (is.null(cyto_table)) {
    cyto_table_new2 <- NULL
  } else {
    #first step is merging the table template with the cyto table and this
    #will include all of the missing columns.
    
    cyto_table_new <- merge(x = table_temp_cyto, y = cyto_table, all.y = TRUE)
    #second step is merging the table with all of the columns with only the
    #first two columns of the template to include all the missing rows
    cyto_table_new2 <- merge(x = table_temp_cyto[c(1, 2)],
                             y = cyto_table_new, all.x = TRUE,
                             by = c("Spec_group", "Patient_setting"))
    
    rows_order_cyto <- factor(rownames(cyto_table_new2), levels = c(2, 1, 4, 3))
    
    cyto_table_new2 <-
      cyto_table_new2[
        order(rows_order_cyto),
        c("Spec_group",
          "Patient_setting",
          "no_cases_signed",
          "MSH.x", "MSQ.x", "BIMC.x", "PACC.x", "KH.x", "R.x", "SL.x", "NYEE.x",
          "MSH.y", "MSQ.y", "BIMC.y", "PACC.y", "KH.y", "R.y", "SL.y",
          "NYEE.y")]
  }
  return(cyto_table_new2)
}

table_merging_cyto_v2 <- function(cyto_table_v2) {
  if (is.null(cyto_table_v2)) {
    cyto_table_new2_v2 <- NULL
  } else {
    #first step is merging the table template with the cyto table and this
    #will include all of the missing columns.
    
    cyto_table_new_v2 <- merge(x = table_temp_cyto_v2, y = cyto_table_v2,
                               all.y = TRUE)
    
    #second step is merging the table with all of the columns with only the
    #first two columns of the template to include all the missing rows
    
    cyto_table_new2_v2 <- merge(x = table_temp_cyto_v2[c(1, 2)],
                                y = cyto_table_new_v2,
                                all.x = TRUE,
                                by = c("Spec_group", "Patient_setting"))
    
    rows_order_cyto_v2 <- factor(rownames(cyto_table_new2_v2),
                                 levels = c(2, 1, 4, 3))
    
    cyto_table_new2_v2 <-
      cyto_table_new2_v2[order(rows_order_cyto_v2),
                         c("Spec_group",
                           "Patient_setting",
                           "no_cases_signed",
                           "received_to_signed_out_within_target",
                           "MSH", "MSQ", "BIMC", "PACC", "KH", "R",
                           "SL", "NYEE")]
  }
  
  return(cyto_table_new2_v2)
}

#Pathology table
table_merging_patho <- function(sp_table) {
  if (is.null(sp_table)) {
    sp_table_new2 <- NULL
  } else {
    #first step is merging the table template with the cyto table
    #and this will include all of the missing columns.
    
    sp_table_new <- merge(x = table_temp_patho, y = sp_table, all.y = TRUE)
    
    #second step is merging the table with all of the columns with
    #only the first two columns of the template to include all the missing rows
    sp_table_new2 <- merge(x = table_temp_patho[c(1, 2)], y = sp_table_new,
                           all.x = TRUE,
                           by = c("Spec_group", "Patient_setting"))
    
    rows_order_patho <- factor(rownames(sp_table_new2), levels = c(2, 1, 4, 3))
    
    sp_table_new2 <-
      sp_table_new2[
        order(rows_order_patho),
        c("Spec_group",
          "Patient_setting",
          "no_cases_signed",
          "MSH.x", "MSQ.x", "BIMC.x", "PACC.x", "KH.x", "R.x", "SL.x",
          "MSH.y", "MSQ.y", "BIMC.y", "PACC.y", "KH.y", "R.y", "SL.y")]
    
  }
  return(sp_table_new2)
}

table_merging_volume <-
  function(table_temp_vol, vol_table, columns_order_v3) {
    if (is.null(vol_table)) {
      vol_table_new2 <- NULL
    } else {
      vol_table_new <- merge(x = table_temp_vol, y = vol_table, all.y = TRUE)
      
      vol_table_new2 <- merge(x = table_temp_vol[c(1, 2)],
                              y = vol_table_new, all.x = TRUE,
                              by = c("Spec_group", "Patient_setting"))
      
      rows_order_vol <- factor(rownames(vol_table_new2), levels = c(2, 1, 4, 3))
      vol_table_new2 <- vol_table_new2[order(rows_order_vol), columns_order_v3]
      row.names(vol_table_new2) <- NULL
      vol_table_new2[is.na(vol_table_new2)] <- 0
      
      vol_table_new2 <- vol_table_new2 %>%
        mutate(Spec_group = ifelse(Spec_group == "GI", "GI Biopsies",
                                   ifelse(Spec_group == "Breast",
                                          "All Breast Specimens", Spec_group)))
    }
    return(vol_table_new2)
  }

############Create a function for conditional formatting############
conditional_formatting_cyto <- function(table_new2) {
  if (is.null(table_new2)) {
    table_new5 <- NULL
  } else {
    table_new2[, 4:19] <- lapply(table_new2[, 4:19], as.numeric)
    
    table_new2[, 4:11] <- lapply(table_new2[, 4:11], formattable::percent)
    
    #steps for conditional formatting:
    
    table_new3 <-
      melt(
        table_new2,
        id = c("Spec_group",
               "Patient_setting",
               "no_cases_signed",
               "MSH.y", "MSQ.y", "BIMC.y", "PACC.y", "KH.y",
               "R.y", "SL.y", "NYEE.y"))
    
    table_new3 <- table_new3 %>%
      mutate(value = ifelse(is.na(value),
                            cell_spec(value, "html",
                                      color = "lightgray"),
                            ifelse(value > 0.9,
                                   cell_spec(value, "html",
                                             color = "green"),
                                   ifelse(value < 0.8,
                                          cell_spec(value, "html",
                                                    color = "red"),
                                          cell_spec(value, "html",
                                                    color = "orange")))))
    
    table_new3 <-
      dcast(table_new3,
            Spec_group +
              Patient_setting +
              no_cases_signed +
              BIMC.y + MSH.y + MSQ.y + NYEE.y + PACC.y + R.y + SL.y +
              KH.y ~ variable)
    
    table_new4 <-
      melt(
        table_new3,
        id = c("Spec_group",
               "Patient_setting",
               "no_cases_signed",
               "MSH.x", "MSQ.x", "BIMC.x", "PACC.x", "KH.x", "R.x", "SL.x",
               "NYEE.x"))
    
    table_new4 <-
      table_new4 %>%
      mutate(value = ifelse(is.na(value),
                            cell_spec(value, "html", color = "lightgray"),
                            cell_spec(value, "html", color = "black")))
    
    table_new4 <-
      dcast(table_new4,
            Spec_group +
              Patient_setting +
              no_cases_signed +
              BIMC.x + MSH.x + MSQ.x + NYEE.x + PACC.x + R.x + SL.x +
              KH.x ~ variable)
    
    table_new5 <- merge(x = table_new4, y = tat_targets_ap[1:3],
                        by.x = c("Spec_group", "Patient_setting"),
                        by.y = c("spec_group", "Patient.Setting"))
    
    rows_order <- factor(rownames(table_new5), levels = c(2, 1, 4, 3))
    
    table_new5 <-
      table_new5[
        order(rows_order),
        c("Spec_group",
          "Received.to.signed.out.target..Days.",
          "Patient_setting",
          "no_cases_signed",
          "MSH.x", "MSQ.x", "BIMC.x", "PACC.x", "KH.x", "R.x", "SL.x", "NYEE.x",
          "MSH.y", "MSQ.y", "BIMC.y", "PACC.y", "KH.y", "R.y", "SL.y",
          "NYEE.y")]
    
    table_new5 <-
      table_new5 %>%
      mutate(Received.to.signed.out.target..Days. =
               paste("<= ", Received.to.signed.out.target..Days., "days"))
    
    row.names(table_new5) <- NULL
    
    table_new5 <- table_new5 %>%
      mutate(no_cases_signed = coalesce(no_cases_signed, 0))
    
  }
  return(table_new5)
}

conditional_formatting_cyto2 <- function(table_new2_v2) {
  if (is.null(table_new2_v2)) {
    table_new4_v2 <- NULL
  } else {
    table_new2_v2[4] <- lapply(table_new2_v2[4], as.numeric)
    table_new2_v2[4] <- lapply(table_new2_v2[4], formattable::percent, d = 0)
    
    #steps for conditional formatting:
    table_new3_v2 <-
      table_new2_v2 %>%
      mutate(
        received_to_signed_out_within_target =
          ifelse(
            is.na(received_to_signed_out_within_target),
            cell_spec(received_to_signed_out_within_target, "html",
                      color = "lightgray"),
            ifelse(
              received_to_signed_out_within_target > 0.9,
              cell_spec(received_to_signed_out_within_target, "html",
                        color  = "green"),
              ifelse(
                received_to_signed_out_within_target < 0.8,
                cell_spec(received_to_signed_out_within_target, "html",
                          color = "red"),
                cell_spec(received_to_signed_out_within_target, "html",
                          color = "orange")))))
    
    table_new4_v2 <-
      melt(table_new3_v2,
           id = c("Spec_group",
                  "Patient_setting",
                  "no_cases_signed",
                  "received_to_signed_out_within_target"))
    
    table_new4_v2 <-
      table_new4_v2 %>%
      mutate(value = ifelse(is.na(value),
                            cell_spec(value, "html", color = "lightgray"),
                            cell_spec(value, "html", color = "black")))
    
    table_new4_v2 <-
      dcast(table_new4_v2,
            Spec_group +
              Patient_setting +
              no_cases_signed +
              received_to_signed_out_within_target ~ variable)
    
    table_new4_v2 <- merge(x = table_new4_v2, y = tat_targets_ap[1:3],
                           by.x = c("Spec_group", "Patient_setting"),
                           by.y = c("spec_group", "Patient.Setting"))
    
    rows_order_v2 <- factor(rownames(table_new4_v2), levels = c(2, 1, 4, 3))
    
    table_new4_v2 <-
      table_new4_v2[order(rows_order_v2),
                    c("Spec_group",
                      "Received.to.signed.out.target..Days.",
                      "Patient_setting", "no_cases_signed",
                      "received_to_signed_out_within_target",
                      "MSH", "MSQ", "BIMC", "PACC", "KH", "R", "SL", "NYEE")]
    
    table_new4_v2 <- table_new4_v2 %>%
      mutate(Received.to.signed.out.target..Days. =
               paste("<= ", Received.to.signed.out.target..Days., "days"))
    
    row.names(table_new4_v2) <- NULL
    
    table_new4_v2 <- table_new4_v2 %>%
      mutate(no_cases_signed = coalesce(no_cases_signed, 0))
  }
  return(table_new4_v2)
}

conditional_formatting_patho <- function(table_new2) {
  if (is.null(table_new2)) {
    table_new5 <- NULL
  } else {
    
    table_new2[, 4:17] <- lapply(table_new2[, 4:17], as.numeric)
    table_new2[, 4:10] <- lapply(table_new2[, 4:10], formattable::percent,
                                 d = 0)
    
    #steps for conditional formatting:
    table_new3 <-
      melt(table_new2,
           id = c("Spec_group",
                  "Patient_setting",
                  "no_cases_signed",
                  "MSH.y", "MSQ.y", "BIMC.y", "PACC.y", "KH.y", "R.y", "SL.y"))
    
    table_new3 <-
      table_new3 %>%
      mutate(value = ifelse(is.na(value),
                            cell_spec(value, "html",
                                      color = "lightgray"),
                            ifelse(value > 0.9,
                                   cell_spec(value, "html",
                                             color  = "green"),
                                   ifelse(value < 0.8,
                                          cell_spec(value, "html",
                                                    color = "red"),
                                          cell_spec(value, "html",
                                                    color = "orange")))))
    
    table_new3 <-
      dcast(table_new3,
            Spec_group +
              Patient_setting +
              no_cases_signed +
              BIMC.y + MSH.y + MSQ.y + PACC.y + R.y + SL.y + KH.y ~ variable)
    
    table_new4 <-
      melt(table_new3,
           id = c("Spec_group",
                  "Patient_setting",
                  "no_cases_signed",
                  "MSH.x", "MSQ.x", "BIMC.x", "PACC.x", "KH.x", "R.x", "SL.x"))
    table_new4 <-
      table_new4 %>%
      mutate(value =
               ifelse(is.na(value),
                      cell_spec(value, "html", color = "lightgray"),
                      cell_spec(value, "html", color = "black")))
    
    table_new4 <-
      dcast(table_new4,
            Spec_group +
              Patient_setting +
              no_cases_signed +
              BIMC.x + MSH.x + MSQ.x + PACC.x + R.x + SL.x + KH.x ~ variable)
    
    table_new5 <- merge(x = table_new4, y = tat_targets_ap[1:3],
                        by.x = c("Spec_group", "Patient_setting"),
                        by.y = c("spec_group", "Patient.Setting"))
    
    rows_order <- factor(rownames(table_new5), levels = c(2, 1, 4, 3))
    
    table_new5 <-
      table_new5[order(rows_order),
                 c("Spec_group",
                   "Received.to.signed.out.target..Days.",
                   "Patient_setting",
                   "no_cases_signed",
                   "MSH.x", "MSQ.x", "BIMC.x", "PACC.x", "KH.x", "R.x", "SL.x",
                   "MSH.y", "MSQ.y", "BIMC.y", "PACC.y", "KH.y", "R.y", "SL.y")]
    
    table_new5 <-
      table_new5 %>%
      mutate(Received.to.signed.out.target..Days. =
               paste("<= ", Received.to.signed.out.target..Days., "days"))
    
    row.names(table_new5) <- NULL
    
    table_new5 <- table_new5 %>%
      mutate(no_cases_signed = coalesce(no_cases_signed, 0),
             # Rename spec groups
             Spec_group = ifelse(Spec_group == "GI", "GI Biopsies",
                                 ifelse(Spec_group == "Breast",
                                        "All Breast Specimens", Spec_group)))
  }
  return(table_new5)
}

table_formatting <- function(table_new3, column_names) {
  if (is.null(table_new3)) {
    table_new3 <- NULL
    asis_output(
      paste("<i>",
            "No data available for efficiency indicator reporting.",
            "</i>")
    )
  } else {
    table_new3 %>%
      select(everything()) %>%
      kable(escape = F, align = "c", col.names = column_names) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      add_header_above(
        c(" " = 1,
          "Receive to Result within Target (Business Days)" = 10,
          "Average Collection to Result TAT (Calendar Days)" = 7),
        background = c("white", "#00B9F2", "#221F72"),
        color = "white", font_size = 13) %>%
      column_spec(2:11, background = "#E6F8FF", color = "black") %>%
      column_spec(12:18, background = "#EBEBF9", color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = c(1, 2))
  }
}


table_formatting2 <- function(table_new3_v2) {
  if (is.null(table_new3_v2)) {
    table_new3_v2 <- NULL
    asis_output(
      paste("<i>",
            "No data available for efficiency indicator reporting.",
            "</i>")
    )
  } else {
    table_new3_v2 %>%
      select(everything()) %>%
      kable(escape = F, align = "c",
            col.names = c("Case Type", "Target", "Setting",
                          "No. Cases Signed Out", "Centralized Lab",
                          "MSH", "MSQ", "MSBI", "PACC", "MSB", "MSW", "MSSL",
                          "NYEE")) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      add_header_above(
        c(" " = 1,
          "Receive to Result within Target (Business Days)" = 4,
          "Average Collection to Result TAT (Calendar Days)" = 8),
        background = c("white", "#00B9F2", "#221F72"),
        color = "white", font_size = 13) %>%
      column_spec(2:5, background = "#E6F8FF", color = "black") %>%
      column_spec(6:13, background = "#EBEBF9", color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = c(1, 2))
  }
}

#Volume table formatting
table_formatting_volume <- function(vol_table, column_names) {
  if (is.null(vol_table)) {
    vol_table <- NULL
    asis_output(
      paste("<i>",
            "No data available for volume reporting.",
            "</i>")
    )
  } else {
    vol_table %>%
      select(everything()) %>%
      kable(escape = F, align = "c", col.names = column_names) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      add_header_above(c(" " = 2,
                         "Resulted Lab Volume" = length(column_names) - 2),
                       background = c("white", "#00B9F2"), color = "white",
                       font_size = 13) %>%
      column_spec(3:length(column_names), background = "#E6F8FF",
                  color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = 1)
    
  }
}
