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
    # Do we really need this?
    vol_cases_signed <- summarized_table %>%
      group_by(Spec_group,
               Patient_setting) %>%
      summarise(no_cases_signed = sum(No_cases_signed_out,
                                      na.rm = TRUE))
    
    #Calculate total number of cases signed out per spec_group per facility
    vol_cases_signed_strat <- summarized_table %>%
      group_by(Spec_group,
               Facility,
               Patient_setting) %>%
      summarise(no_cases_signed =
                  sum(No_cases_signed_out,
                      na.rm = TRUE))%>%
      pivot_wider(id_cols = c(Spec_group,Patient_setting),
                  names_from = Facility,
                  values_from = no_cases_signed,
                  values_fill = 0)
    
  
    #Calculate average collection to signed out
    patient_metric <- summarized_table %>%
      group_by(Spec_group,
               Facility,
               Patient_setting) %>%
      summarise(avg_collection_to_signed_out =
                  format(
                    round(
                      sum(
                        (Patient_metric_avg *
                           No_cases_signed_out) /
                          sum(No_cases_signed_out),
                        na.rm = TRUE), 0)))
    
    patient_metric <- patient_metric %>%
      pivot_wider(id_cols = c(Spec_group,Patient_setting),
                  names_from = Facility,
                  values_from = avg_collection_to_signed_out)
    
    #Calculate % Receive to result TAT within target
    
    #this part of the code creates the table for the received to result TAT
    #within target with an assumption that the receive to result is not
    #centralized which means it is stratified by facility
    lab_metric <- summarized_table %>%
      group_by(Spec_group,
               Facility,
               Patient_setting) %>%
      summarise(received_to_signed_out_within_target =
                  format(
                    round(
                      sum(
                        (Lab_metric_within_target *
                           No_cases_signed_out) /
                          sum(No_cases_signed_out),
                        na.rm = TRUE), 2)))
    
    lab_metric <- lab_metric %>%
      pivot_wider(id_cols = c(Spec_group,Patient_setting),
                  names_from = Facility,
                  values_from = received_to_signed_out_within_target)

    #this part of the code creates the table for the received to result TAT
    #within target with an assumption that the receive to result is centralized
    lab_metric_v2 <- summarized_table %>%
      group_by(Spec_group,
               Patient_setting) %>%
      summarise(received_to_signed_out_within_target =
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
                by = c("Spec_group", "Patient_setting")) %>%
      filter(!Patient_setting %in% c("Other"))

    processed_data_table_v2 <-
      left_join(full_join(vol_cases_signed, lab_metric_v2),
                patient_metric,
                by = c("Spec_group", "Patient_setting"))%>%
      filter(!Patient_setting %in% c("Other"))
    
    cyto_acc_vol1 <- summarized_table %>%
      group_by(Spec_group) %>%
      summarise(cyto_acc_vol1 =
                  as.numeric(sum(cyto_acc_vol,
                                 na.rm = TRUE)))

  }
  return_tables <- list(processed_data_table,
                        processed_data_table_v2,
                        vol_cases_signed_strat,
                        cyto_acc_vol1)
  
  return(return_tables)
}