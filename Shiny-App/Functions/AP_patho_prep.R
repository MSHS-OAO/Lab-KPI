#create a function to prepare pathology data for pre-processing
# combines orginal patho_prep, pre_processing_pp in Daily_Run_AP_Custom_Functions

patho_prep <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    summarized_table <- NULL
    return(summarized_table)
    } else {
    
    #------------Extract the All Breast and GI specs Data Only--------------#
    # Merge the inclusion/exclusion criteria with PowerPath data to determine
    # which GI cases to include in the analysis
    
    raw_data <- merge(x = raw_data, y = gi_codes, all.x = TRUE)
    
    raw_data <- raw_data %>%
      mutate(Facility = case_when(Facility == "MSS" ~ "MSH",
                                  Facility == "STL"~ "SL",
                                  TRUE ~ Facility),
             spec_group = case_when(spec_group == "BREAST" ~ "Breast", 
                                    TRUE ~ spec_group))
    
    
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
    # Crosswalk Rev_ctr and patient setting for PowerPath data
    raw_data_ps <- merge(x = sp_data, y = patient_setting, all.x = TRUE)
    
    # Update MSB patient setting based on patient type column
    raw_data_ps <- raw_data_ps %>%
      mutate(Patient.Setting = case_when(Rev_ctr == "MSBK" & (patient_type == "A" | patient_type == "O") ~ "Amb",
                                         Rev_ctr == "MSBK" &  patient_type == "IN" ~ "IP",
                                         TRUE ~ Patient.Setting))
    
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
    summarized_table <- raw_data_new %>%
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
    
    # # Filter out any specimens signed out on other dates
    # summarize_table <- summarized_table %>%
    #   filter(Signed_out_date_only %in% dates)
    return(summarized_table)
  }
  
}