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
