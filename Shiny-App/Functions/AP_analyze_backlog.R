##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
analyze_backlog <- function(summarized_table_backlog, summarized_table_cytology) {
  if (is.null(summarized_table)) {
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