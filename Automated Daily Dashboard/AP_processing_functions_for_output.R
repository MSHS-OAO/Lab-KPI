# For 24 Hour Volume Look Back Surgical Pathology/Cytology Tab ----
get_stratified_volume <- function(summarized_data, division){
  
  vol_cases_signed_strat <- summarized_data %>%
    group_by(SPECIMEN_GROUP,
             SITE,
             PATIENT_SETTING) %>%
    summarise(no_cases_signed =
                sum(NO_CASES_SIGNED_OUT,
                    na.rm = TRUE))%>%
    pivot_longer(cols = c(no_cases_signed),
                 names_to  = "METRIC",
                 values_to = "VALUE")
  
  if(division == "SURGICAL PATHOLOGY"){
    volume_24_template <- table_ap_template_surgical_pathology_24 
  }else{
    volume_24_template <- table_ap_template_cytology_24
  }
  
  vol_cases_signed_strat <- left_join(volume_24_template,vol_cases_signed_strat) %>%
    mutate(VALUE = cell_spec(VALUE, "html",
                             color = ifelse(is.na(VALUE), 
                                            "lightgray",
                                            "black"))) %>%
    pivot_wider(id_cols = c(SPECIMEN_GROUP,PATIENT_SETTING),
                names_from = c(SITE),
                values_from = VALUE)
  
  return(vol_cases_signed_strat)
}



# For Efficiency Indicators Cytology Data ---- 
get_efficiency_indicators_cytology <- function(summarized_data){
  
  # Volume
  vol_cases_signed <- summarized_data %>%
    group_by(SPECIMEN_GROUP,
             PATIENT_SETTING) %>%
    summarise(no_cases_signed = sum(NO_CASES_SIGNED_OUT,
                                    na.rm = TRUE))
  
  # Received to Signed
  received_to_signed <- summarized_data %>%
    group_by(SPECIMEN_GROUP,
             PATIENT_SETTING) %>%
    summarise(received_to_signed_out_within_target =
                format(
                  round(
                    sum(
                      (REC_TO_SIGNED_OUT_WITHIN_TARGET *
                         NO_CASES_SIGNED_OUT) /
                        sum(NO_CASES_SIGNED_OUT),
                      na.rm = TRUE), 2)))
  
  #Calculate average collection to signed out
  efficiency_indicator_cytology_tab <- summarized_data %>%
    group_by(SPECIMEN_GROUP,
             SITE,
             PATIENT_SETTING) %>%
    summarise(avg_collection_to_signed_out =
                format(
                  round(
                    sum(
                      (COL_TO_SIGNED_OUT_AVG *
                         NO_CASES_SIGNED_OUT) /
                        sum(NO_CASES_SIGNED_OUT),
                      na.rm = TRUE), 0))) %>%
    pivot_longer(cols = c(avg_collection_to_signed_out),
                 names_to  = "METRIC",
                 values_to = "VALUE") %>%
    mutate(VALUE = as.numeric(VALUE))
  
  
  
  efficiency_indicator_cytology_tab <- left_join(table_ap_template_cytology,
                                                 efficiency_indicator_cytology_tab) %>%
    mutate(VALUE = cell_spec(VALUE, "html",
                             color = ifelse(is.na(VALUE), 
                                            "lightgray",
                                            "black"))) %>%
    pivot_wider(id_cols = c(SPECIMEN_GROUP,PATIENT_SETTING),
                names_from = c(METRIC,SITE),
                values_from = VALUE)
  
  
  efficiency_indicator_cytology_tab <- left_join(efficiency_indicator_cytology_tab,
                                                 tat_targets_ap %>%
                                                   select(1,2,3),
                                                 by = c("SPECIMEN_GROUP" = "spec_group",
                                                        "PATIENT_SETTING" = "Patient.Setting"))
  # set the order
  efficiency_indicator_cytology_tab <- left_join(left_join(efficiency_indicator_cytology_tab,
                                                           vol_cases_signed),
                                                 received_to_signed) %>%
    mutate(Target = paste0("<= ",as.character(Received.to.signed.out.target..Days.)," Days")) %>%
    arrange(SPECIMEN_GROUP,PATIENT_SETTING) %>%
    select(SPECIMEN_GROUP,
           Target,
           PATIENT_SETTING,
           no_cases_signed,
           received_to_signed_out_within_target,
           avg_collection_to_signed_out_MSH,
           avg_collection_to_signed_out_MSQ,
           avg_collection_to_signed_out_MSBI,
           avg_collection_to_signed_out_PACC,
           avg_collection_to_signed_out_MSB,
           avg_collection_to_signed_out_MSW,
           avg_collection_to_signed_out_MSM,
           avg_collection_to_signed_out_NYEE) %>%
    mutate(received_to_signed_out_within_target = percent(received_to_signed_out_within_target, digits = 0),
           no_cases_signed = cell_spec(no_cases_signed, "html",
                                       color = ifelse(is.na(no_cases_signed), 
                                                      "lightgray",
                                                      "black")),
           received_to_signed_out_within_target = cell_spec(
             received_to_signed_out_within_target, "html",
             color = ifelse(is.na(received_to_signed_out_within_target), "lightgray",
                            ifelse((received_to_signed_out_within_target >= 0.90),
                                   "green",
                                   ifelse(
                                     (received_to_signed_out_within_target >= 0.8) |
                                       (received_to_signed_out_within_target > 0.9),
                                     "orange", "red")))))
  
  
  return(efficiency_indicator_cytology_tab)
  
}

#Tests Surgical Cytology Efficiency Data ----
#cytology_eff_data <- get_efficiency_indicators_cytology(tab_data_cytology)



# For Efficiency Indicators Surgical Pathology Tab ----
get_efficiency_indicators_surgical_pathology <- function(summarized_data){
  # get the volume by patient setting
  vol_cases_signed <- summarized_data %>%
    group_by(SPECIMEN_GROUP,
             PATIENT_SETTING) %>%
    summarise(no_cases_signed = sum(NO_CASES_SIGNED_OUT,
                                    na.rm = TRUE))
  
  #Calculate average collection to signed out
  #Calculate % Receive to result TAT within target
  #this part of the code creates the table for the received to result TAT
  #within target with an assumption that the receive to result is not
  #centralized which means it is stratified by facility
  
  efficiency_indicator_pathology_tab <- summarized_data %>%
    group_by(SPECIMEN_GROUP,
             SITE,
             PATIENT_SETTING) %>%
    summarise(avg_collection_to_signed_out =
                format(
                  round(
                    sum(
                      (COL_TO_SIGNED_OUT_AVG *
                         NO_CASES_SIGNED_OUT) /
                        sum(NO_CASES_SIGNED_OUT),
                      na.rm = TRUE), 0)),
              received_to_signed_out_within_target =
                format(
                  round(
                    sum(
                      (REC_TO_SIGNED_OUT_WITHIN_TARGET *
                         NO_CASES_SIGNED_OUT) /
                        sum(NO_CASES_SIGNED_OUT),
                      na.rm = TRUE), 2))) %>%
    pivot_longer(cols = c(avg_collection_to_signed_out,received_to_signed_out_within_target),
                 names_to  = "METRIC",
                 values_to = "VALUE") %>%
    mutate(VALUE = as.numeric(VALUE))
  
  
  
  efficiency_indicator_pathology_tab <- left_join(table_ap_template_surgical_pathology,
                                                  efficiency_indicator_pathology_tab) %>%
    pivot_wider(id_cols = c(SPECIMEN_GROUP,PATIENT_SETTING,SITE),
                names_from = c(METRIC),
                values_from = VALUE) %>%
    mutate(received_to_signed_out_within_target = percent(received_to_signed_out_within_target, digits = 0),
           received_to_signed_out_within_target = cell_spec(
             received_to_signed_out_within_target, "html",
             color = ifelse(is.na(received_to_signed_out_within_target), "lightgray",
                            ifelse((received_to_signed_out_within_target >= 0.90),
                                   "green",
                                   ifelse(
                                     (received_to_signed_out_within_target >= 0.8) |
                                       (received_to_signed_out_within_target > 0.9),
                                     "orange", "red")))),
           avg_collection_to_signed_out = cell_spec(
             avg_collection_to_signed_out, "html",
             color = ifelse(is.na(avg_collection_to_signed_out), "lightgray","black")),
           avg_collection_to_signed_out = as.character(avg_collection_to_signed_out)) %>%
    pivot_longer(cols = c(avg_collection_to_signed_out,received_to_signed_out_within_target),
                 names_to  = "METRIC",
                 values_to = "VALUE") %>%
    pivot_wider(id_cols = c(SPECIMEN_GROUP,PATIENT_SETTING),
                names_from = c(METRIC,SITE),
                values_from = VALUE)
  
  
  efficiency_indicator_pathology_tab <- left_join(efficiency_indicator_pathology_tab,
                                                  tat_targets_ap %>%
                                                    select(1,2,3),
                                                  by = c("SPECIMEN_GROUP" = "spec_group",
                                                         "PATIENT_SETTING" = "Patient.Setting"))
  # set the order
  efficiency_indicator_pathology_tab <- left_join(efficiency_indicator_pathology_tab,vol_cases_signed) %>%
    mutate(Target = paste0("<= ",as.character(Received.to.signed.out.target..Days.)," Days"),
           no_cases_signed = cell_spec(no_cases_signed, "html",
                                       color = ifelse(is.na(no_cases_signed), 
                                                      "lightgray",
                                                      "black"))) %>%
    arrange(SPECIMEN_GROUP,PATIENT_SETTING) %>%
    select(SPECIMEN_GROUP,
           Target,
           PATIENT_SETTING,
           no_cases_signed,
           received_to_signed_out_within_target_MSH,
           received_to_signed_out_within_target_MSQ,
           received_to_signed_out_within_target_MSBI,
           received_to_signed_out_within_target_PACC,
           received_to_signed_out_within_target_MSB,
           received_to_signed_out_within_target_MSW,
           received_to_signed_out_within_target_MSM,
           avg_collection_to_signed_out_MSH,
           avg_collection_to_signed_out_MSQ,
           avg_collection_to_signed_out_MSBI,
           avg_collection_to_signed_out_PACC,
           avg_collection_to_signed_out_MSB,
           avg_collection_to_signed_out_MSW,
           avg_collection_to_signed_out_MSM) %>%
    mutate(SPECIMEN_GROUP = case_when(SPECIMEN_GROUP == "Breast" ~ paste0("All ",
                                                                          SPECIMEN_GROUP,
                                                                          " Specimens"),
                                      SPECIMEN_GROUP == "GI" ~ paste0(SPECIMEN_GROUP,
                                                                      " Biopsies")))
  
  return(efficiency_indicator_pathology_tab)
  
}


##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
analyze_backlog <- function(summarized_table_backlog, summarized_table_cytology) {
  if (is.null(summarized_table_backlog)) {
    backlog_acc_table <- NULL
  } else {
    
    cyto_backlog_volume <- summarized_table_backlog %>%
      group_by(Spec_group) %>%
      summarise(cyto_backlog = sum(as.numeric(cyto_backlog),
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
    
    cyto_backlog_volume  <- cyto_backlog_volume  %>%
      mutate(maximum = ifelse(maximum %in% c("-Inf","Inf"), 
                              0,
                              maximum))
    #Days of work
    cyto_case_vol_dow <- as.numeric(cyto_backlog_volume$cyto_backlog[1]) / 80
    
    
    cyto_cases_accesioned <- summarized_table_cytology %>%
      group_by(SPECIMEN_GROUP) %>%
      summarise(cyto_acc_recieved = as.numeric(sum(CYTO_ACCESSION_VOLUME,
                                                   na.rm = TRUE))) %>%
      rename(Spec_group = SPECIMEN_GROUP )
    
    #count the accessioned volume that was accessioned on that date
    #from the backlog report
    cyto_acc_spec_group <- summarized_table_backlog %>%
      group_by(Spec_group) %>%
      summarise(cyto_acc_spec_group_vol = as.numeric(sum(cyto_acc_vol,
                                                         na.rm = TRUE)))
    #sum the two counts
    cyto_acc_totals <- merge(x = cyto_cases_accesioned, y = cyto_acc_spec_group)
    
    cyto_acc_totals  <- cyto_acc_totals%>%
      mutate(total_accessioned_volume = cyto_acc_recieved + cyto_acc_spec_group_vol,
             cyto_acc_recieved = NULL,
             cyto_acc_spec_group_vol = NULL)
    
    
    backlog_acc_table <- merge(x = cyto_acc_totals,
                               y = cyto_backlog_volume,
                               all = TRUE,
    ) %>%
      pivot_longer(cols = c("total_accessioned_volume",
                            "cyto_backlog",
                            "percentile_25th",
                            "percentile_50th",
                            "maximum"),
                   names_to = "METRIC",
                   values_to = "VALUE") %>%
      drop_na(VALUE)
    
    
    backlog_acc_table <- merge(x = table_backlog_template,
                               y = backlog_acc_table,
                               all.x = TRUE, by = c("Spec_group","METRIC")) %>%
      pivot_wider(names_from = "METRIC", 
                  values_from = "VALUE")
    
    backlog_acc_table <- backlog_acc_table %>% 
      mutate(across(everything(), ~replace_na(.x, 0)))
    
    #added this line to delete the cyto gyn from the table until we get
    #correct data. Currently not in use
    #backlog_acc_table <- backlog_acc_table[-c(1), ]
  }
  return(backlog_acc_table)
}