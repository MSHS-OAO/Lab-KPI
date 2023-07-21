source("AP30DayHistoryGen/Package_Ref.R")
source("AP30DayHistoryGen/AP_cyto_prep.R")
source("AP30DayHistoryGen/AP_patho_prep.R")
source("AP30DayHistoryGen/AP_pre_processing_backlog.R")
library("lubridate")

# defining end date - Today
end_date <- Sys.Date()

# defining start date - 30 days back
start_date <- end_date - 180

# generating range of dates
days <- seq(start_date, end_date,"days")
print(days)


ap_summary <- NULL
backlog_daily_repo <- NULL

for (date in days) {
  today <- as.Date(date,origin = "1970-01-01")
  yesterday <- today - 1
  source("AP30DayHistoryGen/Daily_Run_Raw_Data_Import.R")
  summary_cyto <- cyto_prep(epic_data_raw,pp_data_raw)
  summary_patho <- patho_prep(pp_data_raw)
  processed_backlog_data <- pre_processing_backlog(cyto_backlog_data_raw)
  
  if (date == as.Date("2023-01-22")){
    ap_summary <<- rbind(summary_cyto,
                         summary_patho)
    backlog_daily_repo <<- processed_backlog_data
  }
    
  
  

  if(!is.null(summary_cyto)){
    remove_dupl_dates_test_level <- anti_join(ap_summary,
                                              summary_cyto)
    
    ap_summary <- rbind(remove_dupl_dates_test_level, summary_cyto)
    
    ap_summary <- ap_summary %>%
      arrange(SITE, REPORT_DATE)
    
    saveRDS(ap_summary,
            paste0(user_directory,
                   "/Shiny App Repo/APDailySummary",
                   "/APRepo180Days.rds"))
    
  }
  
  
  if(!is.null(processed_backlog_data)){
    remove_dupl_dates_test_level <- anti_join(backlog_daily_repo,
                                              processed_backlog_data,
                                              by = "Report_Date")
    
    backlog_daily_repo <- rbind(remove_dupl_dates_test_level, processed_backlog_data)
    
    backlog_daily_repo <- backlog_daily_repo %>%
      arrange(Facility, Report_Date)
    
    saveRDS(backlog_daily_repo,
            paste0(user_directory,
                   "/Shiny App Repo/APDailySummary",
                   "/BacklogRepo30Days.rds"))
    
  }
  
  
  if(!is.null(summary_patho)){
    remove_dupl_dates_test_level <- anti_join(ap_summary,
                                              summary_patho)
    
    ap_summary <- rbind(remove_dupl_dates_test_level, summary_patho)
    
    ap_summary <- ap_summary %>%
      arrange(SITE, REPORT_DATE)
    
    saveRDS(ap_summary,
            paste0(user_directory,
                   "/Shiny App Repo/APDailySummary",
                   "/APRepo180Days.rds"))
    
  }
  
}