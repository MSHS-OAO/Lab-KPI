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
    pivot_wider(id_cols = c(SPECIMEN_GROUP,PATIENT_SETTING),
                names_from = c(SITE),
                values_from = VALUE,
                values_fill = 0)
  
  return(vol_cases_signed_strat)
}

# Test 24 Volume LookBack ---- 

cyto_strat <- get_stratified_volume(tab_data_cytology,"CYTOLOGY")
patho_strat <- get_stratified_volume(tab_data_pathology,"SURGICAL PATHOLOGY")
