##### This function helps in creating the analysis and tables from the
# summarized table. Will be used in first run and second run as well.
analyze_backlog <- function(summarized_table,cyto_acc_vol1) {
  if (is.null(summarized_table)) {
    backlog_acc_table_new2 <- NULL
  } else {
    cyto_backlog_vol <- summarized_table%>%
      group_by(Spec_group)%>%
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
                              na.rm = TRUE))) %>%
      mutate(maximum = if_else(maximum == "-Inf",0,maximum))
    
    #Days of work
    cyto_case_vol_dow <- as.numeric(cyto_backlog_vol$cyto_backlog[1]) / 80
    
    #count the accessioned volume that was accessioned on that date
    #from the backlog report
    cyto_acc_vol2 <-
      summarise(
        group_by(
          summarized_table,
          Spec_group),
        cyto_acc_vol2 = as.numeric(sum(cyto_acc_vol,
                                       na.rm = TRUE)))
    #sum the two counts
    cyto_acc_vol3 <- merge(x = cyto_acc_vol1, y = cyto_acc_vol2)
    
    cyto_acc_vol3 <- cyto_acc_vol3%>%
      mutate(total_acc_vol = cyto_acc_vol1+cyto_acc_vol2,
             cyto_acc_vol1 = NULL,
             cyto_acc_vol2 = NULL)
    

    backlog_acc_table <- merge(x = cyto_acc_vol3,
                               y = cyto_backlog_vol,
                               all = TRUE)
    
    
    Spec_group <- c("CYTO GYN", "CYTO NONGYN")
    table_temp_backlog_acc <- expand.grid(Spec_group)
    
    colnames(table_temp_backlog_acc) <- c("Spec_group")
    
    
    other_cols_table_temp_backlog_acc <- c("total_accessioned_volume",
                                       "cyto_backlog",
                                       "percentile_25th",
                                       "percentile_50th",
                                       "maximum")
    
    table_temp_backlog_acc <- table_temp_backlog_acc %>% 
      mutate(!!!setNames(rep(NA, length(other_cols_table_temp_backlog_acc)), other_cols_table_temp_backlog_acc))
    
    
    backlog_acc_table_new2 <- merge(x = table_temp_backlog_acc[1],
                                    y = backlog_acc_table,
                                    all.x = TRUE, by = c("Spec_group")) %>%
      replace(is.na(.), 0)
    
    #added this line to delete the cyto gyn from the table until we get
    #correct data. Currently not in use
    #backlog_acc_table_new3 <- backlog_acc_table_new2[-c(1), ]
  }
  return(backlog_acc_table_new2)
}