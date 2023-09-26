ap_update_date_choices <- function(session){
  
  # SP Efficiancy Indicators ----
  picker_choice_surgical_pathology_min <- min(ap_summary %>%
    ungroup() %>%
    filter(TAB == "SURGICAL PATHOLOGY") %>%
    select(SIGNED_OUT_DATE) %>%
    pull())
  
  picker_choice_surgical_pathology_max <- max(ap_summary %>%
                                                ungroup() %>%
                                                filter(TAB == "SURGICAL PATHOLOGY") %>%
                                                select(SIGNED_OUT_DATE) %>%
                                                pull())
  
  updateDateInput(session, "ap_report_date_eff_indicators_sp",
                  value = picker_choice_surgical_pathology_max, 
                  min = picker_choice_surgical_pathology_min,
                  max = picker_choice_surgical_pathology_max)
  
  
  # Cyto Efficiancy Indicators ----
  picker_choice_cytology_min <- min(ap_summary %>%
                                                ungroup() %>%
                                                filter(TAB == "CYTOLOGY") %>%
                                                select(SIGNED_OUT_DATE) %>%
                                                pull())
  
  picker_choice_cytology_max <- max(ap_summary %>%
                                                ungroup() %>%
                                                filter(TAB == "CYTOLOGY") %>%
                                                select(SIGNED_OUT_DATE) %>%
                                                pull())
  
  updateDateInput(session, "ap_report_date_eff_indicators_cyto",
                  value = picker_choice_cytology_max, 
                  min = picker_choice_cytology_min,
                  max = picker_choice_cytology_max)
  
}