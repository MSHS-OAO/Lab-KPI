# Surgical Pathology - Efficiency Indicators ----
ap_ei_kable_surgical_pathology <- function(surgical_pathology_ei_data){
  
  if(is.null(surgical_pathology_ei_data)){
    surgical_pathology_ei_data <- NULL
    asis_output(
      paste("<i>",
            "No data available for efficiency indicator reporting.",
            "</i>"))
  }else{
    colnames <- c("Case Type",
                  "Target",
                  "Setting",
                  "No. Cases Signed Out",
                  "MSH",
                  "MSQ",
                  "MSBI",
                  "PACC",
                  "MSB",
                  "MSW",
                  "MSM",
                  "MSH",
                  "MSQ",
                  "MSBI",
                  "PACC",
                  "MSB",
                  "MSW",
                  "MSM")
    
    num_col <- length(colnames)
    # Format kable
    kable(surgical_pathology_ei_data, format = "html", escape = FALSE, align = "c",
          col.names = colnames) %>%
      kable_styling(bootstrap_options = "hover", position = "center",
                    font_size = 11,
                    full_width = FALSE) %>%
      column_spec(column = c(1, 11, 18),
                  border_right = "thin solid lightgray") %>%
      add_header_above(c(" " = 1,
                         "Receive to Result Within Target (Business Days)" =
                           10,
                         "Average Collect to Result TAT (Calendar Days)" =
                           7),
                       background = c("white", "#00AEEF", "#221f72"),
                       color = "white", line = FALSE, font_size = 13) %>%
      column_spec(column = 2:11, background = "#E6F8FF",color = "black") %>%
      column_spec(column = 12:num_col, background = "#EBEBF9",color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = c(1, 2))
    
  }
}


# Cytology - Efficiency Indicators ----
ap_ei_kable_cytology <- function(cytology_ei_data){
  
  if(is.null(cytology_ei_data)){
    cytology_ei_data <- NULL
    asis_output(
      paste("<i>",
            "No data available for efficiency indicator reporting.",
            "</i>"))
  }else{
    colnames <- c("Case Type",
                  "Target",
                  "Setting",
                  "No. Cases Signed Out",
                  "Centralized Lab",
                  "MSH",
                  "MSQ",
                  "MSBI",
                  "PACC",
                  "MSB",
                  "MSW",
                  "MSM",
                  "NYEE")
    
    num_col <- length(colnames)
    # Format kable
    kable(cytology_ei_data, format = "html", escape = FALSE, align = "c",
          col.names = colnames) %>%
      kable_styling(bootstrap_options = "hover", position = "center",
                    font_size = 11,
                    full_width = FALSE) %>%
      column_spec(column = c(1, 5, num_col),
                  border_right = "thin solid lightgray") %>%
      add_header_above(c(" " = 1,
                         "Receive to Result Within Target (Business Days)" =
                           4,
                         "Average Collect to Result TAT (Calendar Days)" =
                           8),
                       background = c("white", "#00AEEF", "#221f72"),
                       color = "white", line = FALSE, font_size = 13) %>%
      column_spec(column = 2:5, background = "#E6F8FF",color = "black") %>%
      column_spec(column = 6:num_col, background = "#EBEBF9",color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = c(1, 2))
    
  }
}


# 24 Hours Volume Backlog ----
table_formatting_volume_backlog <- function(backlog_table) {
  if (holiday_det) {
    asis_output(
      paste("<i>",
            "No cases accessioned on weekends and holidays.",
            "</i>")
    )
  } else if (is.null(backlog_acc_table_new2)) {
    asis_output(
      paste("<i>",
            "No data available for backlog and accessioned volume reporting.",
            "</i>")
    )
  } else {
    backlog_table %>%
      kable(escape = F, align = "c",
            col.names = c("Case Type", "Cases Accessioned", "Backlog Volume",
                          "25th Percentile", "50th Percentile", "Maximum")) %>%
      kable_styling(bootstrap_options = "hover", full_width = TRUE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      column_spec(2, background = "#E6F8FF", color = "black") %>%
      column_spec(3:6, background = "#EBEBF9", color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      add_header_above(c(
        " " = 2,
        "Elapsed Turn Around Time for Backlogged Cases From the Received Time" = 4),
        background = c("white", "#221F72"), color = "white", font_size = 13) %>%
      collapse_rows(columns = 1)
  }
}


# 24 Hours Volume SP and Cytology ----

table_formatting_volume <- function(vol_table, tab) {
  if (is.null(vol_table)) {
    vol_table <- NULL
    asis_output(
      paste("<i>",
            "No data available for volume reporting.",
            "</i>")
    )
  } else {
    
    column_names <- NA
    
    if(tab == "CYTOLOGY"){
      column_names <- cyto_vol_column_names
    }else{
      column_names <- sp_vol_column_names
    }
    
    vol_table %>%
      select(everything()) %>%
      kable(escape = F, align = "c", col.names = column_names) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      add_header_above(c(" " = 2,
                         "Resulted Lab Volume" = length(column_names) - 2),
                       background = c("white", "#00B9F2"), color = "white",
                       font_size = 13) %>%
      column_spec(3:length(column_names), background = "#E6F8FF",
                  color = "black") %>%
      row_spec(row = 0, font_size = 13) %>%
      collapse_rows(columns = 1)
    
  }
}


