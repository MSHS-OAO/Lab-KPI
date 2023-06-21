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
    cyto_backlog <- cyto_backlog_ps_target %>%
      filter(spec_group %in% c("CYTO NONGYN","CYTO GYN"))
    
    #Change all Dates into POSIXct format to start the calculations
    cyto_backlog <- cyto_backlog %>%
      mutate(Case_created_date = as.POSIXct(Case_created_date,tz = "",format = "%m/%d/%y %I:%M %p"),
             Collection_Date = as.POSIXct(Collection_Date,tz = "",format = "%m/%d/%y %I:%M %p"),
             Received_Date = as.POSIXct(Received_Date,tz = "",format = "%m/%d/%y %I:%M %p"),
             signed_out_date = as.POSIXct(signed_out_date,tz = "",format = "%m/%d/%y %I:%M %p"))

    #Backlog Calculations: Date now - case created date
    #without weekends and holidays, subtract one so we don't include today's date
    acc_date <- resulted_date
    
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
        
        maximum = format(
          ceiling(
            max(
              backlog[backlog > Received.to.signed.out.target..Days.],
              na.rm = TRUE))),
        
        cyto_acc_vol = as.numeric(sum(acc_date == acc_date_only,
                                      na.rm = TRUE))) %>%
      mutate(maximum = if_else(maximum == "-Inf","0",
                               maximum),
             maximum = as.numeric(maximum))
    
    #standardize the name for the current summary to match the historical repo
    colnames(summarized_table) <-
      c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
        "acc_date_only", "acc_day_only", "Report_Date", "cyto_backlog",
        "percentile_25th", "percentile_50th", "maximum", "cyto_acc_vol")
  }
  return(summarized_table)
  
}