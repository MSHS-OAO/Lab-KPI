
# Process the data ----
processed_cytology_data <-  cyto_prep(epic_data_raw ,pp_data_raw,resulted_date)

processed_surgical_pathology_data <- patho_prep(pp_data_raw,resulted_date)

processed_backlog_data <- pre_processing_backlog(cyto_backlog_data_raw,resulted_date)

# Get the data to output form ----
stratified_volume_cytology_24 <- get_stratified_volume(processed_cytology_data, "CYTOLOGY")
stratified_volume_surgical_pathology_24 <- get_stratified_volume(processed_surgical_pathology_data, "SURGICAL PATHOLOGY")

backlog_cytlogy <- analyze_backlog(processed_backlog_data,processed_cytology_data)

ei_cytlogy <- get_efficiency_indicators_cytology(processed_cytology_data)
ei_surgical_pathology <- get_efficiency_indicators_surgical_pathology(processed_surgical_pathology_data)


#Formatting ----
ei_cytlogy <- ap_ei_kable_cytology(ei_cytlogy)
ei_surgical_pathology <- ap_ei_kable_surgical_pathology(ei_surgical_pathology)


backlog_cytlogy <- table_formatting_volume_backlog(backlog_cytlogy)

stratified_volume_cytology_24 <- table_formatting_volume(stratified_volume_cytology_24, "CYTOLOGY")
stratified_volume_surgical_pathology_24 <- table_formatting_volume(stratified_volume_surgical_pathology_24, "SURGICAL PATHOLOGY")