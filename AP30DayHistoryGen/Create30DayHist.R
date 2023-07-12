source("AP30DayHistoryGen/Package_Ref.R")
source("AP30DayHistoryGen/AP_cyto_prep.R")
source("AP30DayHistoryGen/AP_patho_prep.R")
library("lubridate")

# defining end date - Today
end_date <- Sys.Date()

# defining start date - 30 days back
start_date <- end_date - 60

# generating range of dates
days <- seq(start_date, end_date,"days")
print(days)

for (date in days) {
  today <- as.Date(date,origin = "1970-01-01")
  yesterday <- today - 1
  source("AP30DayHistoryGen/Daily_Run_Raw_Data_Import.R")
  summary_cyto <- cyto_prep(epic_data_raw,pp_data_raw)
  summary_patho <- patho_prep(pp_data_raw)
  
  

  if(!is.null(summary_cyto)){
    remove_dupl_dates_test_level <- anti_join(cyto_daily_repo,
                                              summary_cyto,
                                              by = "report_date_only")
    
    cyto_daily_repo <- rbind(remove_dupl_dates_test_level, summary_cyto)
    
    cyto_daily_repo <- cyto_daily_repo %>%
      arrange(Facility, report_date_only)
    
    saveRDS(cyto_daily_repo,
            paste0(user_directory,
                   "/Shiny App Repo/APDailySummary",
                   "/APCytologyRepo30Days.rds"))
    
  }
  
  
  if(!is.null(summary_patho)){
    remove_dupl_dates_test_level <- anti_join(patho_daily_repo,
                                              summary_patho,
                                              by = "report_date_only")
    
    patho_daily_repo <- rbind(remove_dupl_dates_test_level, summary_patho)
    
    patho_daily_repo <- patho_daily_repo %>%
      arrange(Facility, report_date_only)
    
    saveRDS(patho_daily_repo,
            paste0(user_directory,
                   "/Shiny App Repo/APDailySummary",
                   "/APPathologyRepo30Days.rds"))
    
  }
  
}