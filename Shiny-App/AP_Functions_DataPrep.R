site_mapping <- function(data){
  
  
  data <- data %>%
    mutate(Facility = case_when(Facility == "KH" ~ "MSB",
                                Facility == "R" ~ "MSW",
                                Facility == "STL" ~ "MSM",
                                Facility == "BIMC" ~ "MSBI",
                                Facility == "SNCH" ~ "MSSN",
                                Facility == "MSS" ~ "MSH",
                                Facility == "SL" ~ "MSM",
                                #Facility == "PACC" ~ "MS USQ",
                                TRUE ~ Facility ))
  
  
  
}


format_dates <- function(data){
  
  data[c("Case_created_date",
                 "Collection_Date",
                 "Received_Date",
                 "signed_out_date")] <-
    lapply(data[c("Case_created_date",
                          "Collection_Date",
                          "Received_Date",
                          "signed_out_date")],
           as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p" ,origin = "1970-01-01")
  
  data
  
}


# Function to preprate prepare cytology data for pre-processing by crosswalking
# Epic and PowerPath data
# combines orginal cyto_prep, pre_processing_pp in Daily_Run_AP_Custom_Functions

cyto_prep <- function(epic_data, pp_data,resulted_date) {
  if (is.null(epic_data) || is.null(pp_data) || 
      nrow(epic_data) == 0 || nrow(pp_data) == 0) {
    summarized_table <- NULL
    return(summarized_table)
  } else {
    
    # Preprocess Epic data
    # Select specimens that were finalized in Epic based on Lab Status
    epic_data_finalized_results <- epic_data %>%
      filter(LAB_STATUS %in% c("Final result", "Edited Result - FINAL"))
    
    # Create dataframe of unique specimen ID for crosswalking with PowerPath data
    # cross-walking with PowerPath data
    epic_data_specimens <- epic_data_finalized_results %>%
      distinct(SPECIMEN_ID)
    
    # Update names for MSH and MSM
    # pp_data <- pp_data %>%
    #   mutate(Facility = case_when(Facility == "MSS" ~ "MSH",
    #                               Facility == "STL"~ "SL",
    #                               TRUE ~ Facility))
    
    # Subset PowerPath data to keep Cyto Gyn and Cyto NonGyn and primary
    # specimens only
    cyto_data <- pp_data %>%
      filter(spec_sort_order == "A" &
               spec_group %in% c("CYTO NONGYN", "CYTO GYN"))
    
    cyto_data <- merge(x = cyto_data, y = epic_data_specimens,
                        by.x = "Case_no",
                        by.y = "SPECIMEN_ID")
    
    
    
    # Crosswalk Rev_ctr and patient setting for PowerPath data
    cyto_data <- merge(x = cyto_data, y = patient_setting, all.x = TRUE)
    
    # update sites/facilities
    cyto_data <- site_mapping(cyto_data)
    
    # Update MSB patient setting based on patient type column
    cyto_data <- cyto_data %>%
      mutate(Patient.Setting = case_when(Rev_ctr == "MSBK" & (patient_type == "A" | patient_type == "O") ~ "Amb",
                                         Rev_ctr == "MSBK" &  patient_type == "IN" ~ "IP",
                                         TRUE ~ Patient.Setting))
    
    # Crosswalk TAT targets based on spec_group and patient setting
    cyto_data <- merge(x = cyto_data, y = tat_targets_ap,
                          all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    

    #Change all Dates into POSIXct format to start the calculations
    cyto_data <- format_dates(cyto_data)
    
    # Add columns for turnaround time calculations:
    # Collection to signed out (in calendar days) and
    # received to signed out (in business days)
    cyto_data <- cyto_data %>%
      mutate(
        # Add column for collected to signed out turnaround time in calendar days
        Collection_to_signed_out =
          as.numeric(difftime(signed_out_date,Collection_Date,units="days")),
        # Add column for received to signed out turnaround time in business days
        Received_to_signed_out = bizdays(Received_Date, signed_out_date),
        # Prepare data for accessioned volume analysis
        # First find the date of the last weekday and add 1 for report date
        report_date_only = as.Date(signed_out_date) + 1,
        #  Find the accessioned date and use this for determining accessioned volume
        acc_date_only = as.Date(Received_Date)) %>%
      # Filter out anything with a sign out date other than result date of interest
      filter(date(signed_out_date) %in% resulted_date)
    

    #summarize the data to be used for analysis and to be stored as historical
    #repo
    summarized_table <- cyto_data %>%
      group_by(Spec_code,
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
               weekdays(report_date_only)) %>%
      summarise(no_cases_signed = n(),
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
    
    summarized_table <- summarized_table %>%
      rename(SPECIMEN_CODE = Spec_code,
             SPECIMEN_GROUP = Spec_group,
             SITE = Facility,
             PATIENT_SETTING = Patient_setting,
             REVENUE_CENTER = Rev_ctr,
             SIGNED_OUT_DATE = Signed_out_date_only,
             SIGNED_OUT_DAY = Signed_out_day_only,
             REC_TO_SIGNED_OUT_TARGET = Lab_metric_target,
             COL_TO_SIGNED_OUT_TARGET = Patient_metric_target,
             ACCESSION_DATE = acc_date_only,
             ACCESION_DAY = acc_day_only,
             REPORT_DATE = report_date_only,
             REPORT_DAY = report_day_only,
             NO_CASES_SIGNED_OUT = No_cases_signed_out,
             REC_TO_SIGNED_OUT_AVG = Lab_metric_avg,
             REC_TO_SIGNED_OUT_MEDIAN = Lab_metric_med,
             REC_TO_SIGNED_OUT_STDDEV = Lab_metric_std,
             REC_TO_SIGNED_OUT_WITHIN_TARGET = Lab_metric_within_target,
             COL_TO_SIGNED_OUT_AVG = Patient_metric_avg,
             COL_TO_SIGNED_OUT_MEDIAN = Patient_metric_med,
             COL_TO_SIGNED_OUT_STDDEV = Patient_metric_std,
             CYTO_ACCESSION_VOLUME = cyto_acc_vol) #%>%
      #mutate(TAB = "CYTOLOGY") 
    # # Filter out any specimens signed out on other dates
    # summarize_table <- summarized_table %>%
    #   filter(Signed_out_date_only %in% dates)
    return(summarized_table)
  }
}




#create a function to prepare pathology data for pre-processing
# combines orginal patho_prep, pre_processing_pp in Daily_Run_AP_Custom_Functions

patho_prep <- function(raw_data,resulted_date) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    summarized_table <- NULL
    return(summarized_table)
  } else {
    
    # resulted_date <- as.Date(max(raw_data$signed_out_date), format("%m/%d/%y"))
    #------------Extract the All Breast and GI specs Data Only--------------#
    # Merge the inclusion/exclusion criteria with PowerPath data to determine
    # which GI cases to include in the analysis
    
    pp_data_gi_codes <- merge(x = raw_data, y = gi_codes, all.x = TRUE)
    
    pp_data_gi_codes <- pp_data_gi_codes %>%
      mutate(spec_group = case_when(spec_group == "BREAST" ~ "Breast", 
                                    TRUE ~ spec_group))
    
    pp_data_gi_codes <- site_mapping(pp_data_gi_codes)
    
    
    #Create dataframe with cases that should be excluded based on GI code
    exclude_gi_codes <- pp_data_gi_codes %>%
      filter(spec_group %in% c("GI") &
               !(GI_Code_InclExcl %in% c("Include")))
    
    # Create vector of case numbers to exclude
    exclude_case_numbers <- unique(exclude_gi_codes$Case_no)
    
    # Subset surgical pathology data based on inclusion criteria
    sp_data <- pp_data_gi_codes %>%
      filter(# Select primary specimens only
        spec_sort_order == "A" &
          # Select GI specimens with codes that are included and any breast specimens
          ((spec_group == "GI" & !(Case_no %in% exclude_case_numbers)) |
             (spec_group == "Breast" )) &
          # Exclude NYEE
          Facility != "NYEE")
    # Crosswalk Rev_ctr and patient setting for PowerPath data
    sp_data_patient_setting <- merge(x = sp_data, y = patient_setting, all.x = TRUE)
    
    
    # Update MSB patient setting based on patient type column
    sp_data_patient_setting <- sp_data_patient_setting %>%
      mutate(Patient.Setting = case_when(Rev_ctr == "MSBK" & (patient_type == "A" | patient_type == "O") ~ "Amb",
                                         Rev_ctr == "MSBK" &  patient_type == "IN" ~ "IP",
                                         TRUE ~ Patient.Setting))
    
    # Crosswalk TAT targets based on spec_group and patient setting
    patient_setting_with_targets <- merge(x = sp_data_patient_setting, y = tat_targets_ap,
                                          all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    
    #Change all Dates into POSIXct format to start the calculations
    patient_setting_with_targets <- format_dates(patient_setting_with_targets)
    
    
    # Add columns for turnaround time calculations:
    # Collection to signed out (in calendar days) and
    # received to signed out (in business days)
    patient_setting_with_targets <- patient_setting_with_targets %>%
      mutate(
        # Add column for collected to signed out turnaround time in calendar days
        Collection_to_signed_out =
          as.numeric(difftime(signed_out_date,Collection_Date,units="days")),
        # Add column for received to signed out turnaround time in business days
        Received_to_signed_out = bizdays(Received_Date, signed_out_date),
        # Prepare data for accessioned volume analysis
        # First find the date of the last weekday and add 1 for report date
        report_date_only = as.Date(signed_out_date) + 1,
        #  Find the accessioned date and use this for determining accessioned volume
        acc_date_only = as.Date(Received_Date)) %>%
      # Filter out anything with a sign out date other than result date of interest
      filter(date(signed_out_date) %in% resulted_date)
    

    #summarize the data to be used for analysis and to be stored as historical
    #repo
    summarized_table <- patient_setting_with_targets %>%
      group_by(Spec_code,
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
               weekdays(report_date_only)) %>%
      summarise(no_cases_signed = n(),
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
    
    summarized_table <- summarized_table %>%
      rename(SPECIMEN_CODE = Spec_code,
             SPECIMEN_GROUP = Spec_group,
             SITE = Facility,
             PATIENT_SETTING = Patient_setting,
             REVENUE_CENTER = Rev_ctr,
             SIGNED_OUT_DATE = Signed_out_date_only,
             SIGNED_OUT_DAY = Signed_out_day_only,
             REC_TO_SIGNED_OUT_TARGET = Lab_metric_target,
             COL_TO_SIGNED_OUT_TARGET = Patient_metric_target,
             ACCESSION_DATE = acc_date_only,
             ACCESION_DAY = acc_day_only,
             REPORT_DATE = report_date_only,
             REPORT_DAY = report_day_only,
             NO_CASES_SIGNED_OUT = No_cases_signed_out,
             REC_TO_SIGNED_OUT_AVG = Lab_metric_avg,
             REC_TO_SIGNED_OUT_MEDIAN = Lab_metric_med,
             REC_TO_SIGNED_OUT_STDDEV = Lab_metric_std,
             REC_TO_SIGNED_OUT_WITHIN_TARGET = Lab_metric_within_target,
             COL_TO_SIGNED_OUT_AVG = Patient_metric_avg,
             COL_TO_SIGNED_OUT_MEDIAN = Patient_metric_med,
             COL_TO_SIGNED_OUT_STDDEV = Patient_metric_std,
             CYTO_ACCESSION_VOLUME = cyto_acc_vol) #%>%
      #mutate(TAB = "SURGICAL PATHOLOGY")
    
    # # Filter out any specimens signed out on other dates
    # summarize_table <- summarized_table %>%
    #   filter(Signed_out_date_only %in% dates)
    return(summarized_table)
  }
  
}



##### This function helps in preprocessing the raw backlog data.
# Will be used in first run
pre_processing_backlog <- function(cyto_backlog_raw,resulted_date) {
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
    
    cyto_backlog_ps_target <- site_mapping(cyto_backlog_ps_target)
    
    #Keep the cyto gyn and cyto non-gyn
    cyto_backlog <- cyto_backlog_ps_target %>%
      filter(spec_group %in% c("CYTO NONGYN","CYTO GYN"))
    
    #Change all Dates into POSIXct format to start the calculations
    cyto_backlog <- format_dates(cyto_backlog)
    
    #Backlog Calculations: Date now - case created date
    #without weekends and holidays, subtract one so we don't include today's date

    cyto_backlog <- cyto_backlog %>%
      mutate(backlog = bizdays(cyto_backlog$Case_created_date, today) - 1,
             acc_date_only = as.Date(cyto_backlog$Received_Date),
             Report_Date = resulted_date + 1 )
    
    #summarize the data to be used for analysis and to be stored as historical
    #repo
    summarized_table <- cyto_backlog %>%
      group_by(Spec_code,
               spec_group,
               Facility,
               Patient.Setting,
               Rev_ctr,
               acc_date_only,
               weekdays(acc_date_only),
               Report_Date) %>%
      summarise(cyto_backlog = format(
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
        
        percentile_75th =
          format(
            ceiling(
              quantile(
                backlog[backlog > Received.to.signed.out.target..Days.],
                prob = 0.75, na.rm = TRUE))),
        
        maximum = format(
          ceiling(
            max(
              backlog[backlog > Received.to.signed.out.target..Days.],
              na.rm = TRUE))),
        
        average = format(
          ceiling(
            mean(
              backlog[backlog > Received.to.signed.out.target..Days.],
              na.rm = TRUE))),
        
        minimum = format(
          ceiling(
            min(
              backlog[backlog > Received.to.signed.out.target..Days.],
              na.rm = TRUE))),
        
        cyto_acc_vol = as.numeric(sum(resulted_date == acc_date_only,
                                      na.rm = TRUE))) %>%
      mutate(maximum = if_else(maximum %in% c("-Inf" ,"Inf") ,"0",
                               maximum),
             maximum = as.numeric(maximum))
    
    #standardize the name for the current summary to match the historical repo
    colnames(summarized_table) <-
      c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
        "acc_date_only", "acc_day_only", "Report_Date", "cyto_backlog",
        "percentile_25th", "percentile_50th", "percentile_75th","minimum","average","maximum", "cyto_acc_vol")
  }
  return(summarized_table)
  
}
