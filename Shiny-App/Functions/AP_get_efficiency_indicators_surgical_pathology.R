# ap_summary_07_12 <- ap_summary %>%
#   filter(REPORT_DATE == as.Date("2023-07-12"))
# 
# 
# tab_data_pathology <- ap_summary_07_12 %>%
#   filter(TAB == "SURGICAL PATHOLOGY")
# 
# 
# tab_data_cytology <- ap_summary_07_12 %>%
#   filter(TAB == "CYTOLOGY")



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
    mutate(Target = paste0("<= ",as.character(Received.to.signed.out.target..Days.)," Days")) %>%
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

#Tests Surgical Pathology Efficiency Data ----
#pathology_eff_data <- get_efficiency_indicators_surgical_pathology(tab_data_pathology)
