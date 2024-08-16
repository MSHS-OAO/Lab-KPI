library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(readr)
library(zip)

todays_date <- Sys.Date()
current_year <- format(todays_date, "%Y")
previous_year <- as.numeric(current_year)-1
previous_year_start <- paste0(current_year,'-01','-01')
# todays_date <- as.Date('2024-08-05') #comment later
this_week_no <- format(todays_date, "%U")
this_week_start <- todays_date - (wday(todays_date) - 1)

lookback_period <- 12
dashboard_start <- this_week_start - lookback_period*7
dashboard_start_sql <- format(dashboard_start, '%Y-%m-%d')
dashboard_end <- dashboard_start  + 84



report_path <- paste0("/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Lab KPI/MSHS Lab OI Dashboards/", 
                      "System Lab ED PI Weekly Dashboard ", 
                      format(Sys.Date(),"%m-%d-%y"),
                      ".html")