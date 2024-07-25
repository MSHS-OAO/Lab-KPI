# This code is used to create and maintain a data repository with:
# 1. Raw data for in scope analytes for 60 days
# 2. Daily summary based on dashboard stratifications
# 3. Weekly summary based on dashboard stratifications
# 4. Monthly summary based on dashboard stratifications

#Install packages only the first time you run the code
#install.packages("timeDate")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("formattable")
#install.packages("bizdays")
#install.packages("rmarkdown")
#install.packages("stringr")
#install.packages("writexl")
#install.packages("tidyr")

#-------------------------------Required packages-------------------------------#

#Required packages: run these every time you run the code
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
library(kableExtra)
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(tidyr)

rm(list = ls())

source(here::here("CPDataProcessing/CONSTANTS.R"))
source(here::here("CPDataProcessing/processing.R"))
source(here::here("CPDataProcessing/write_temporary_table_to_database_and_merge_updated.R"))



# Determine date range for reports to include in repository
if (initial_run == TRUE) {
  # Provide start date for new data repository
  # repo_start_date <- as.Date(paste0(this_month, "/",
  #                                   1, "/",
  #                                   year(Sys.Date())), format = "%m/%d/%Y")
  # repo_start_date <- todays_date - 60
  repo_start_date <- as.Date("7/1/2023", format = "%m/%d/%Y")
  # Create vector with date range for new data repository
  repo_date_range <- seq(from = repo_start_date + 1,
                         to = todays_date,
                         by = "day")
  scc_date_range <- repo_date_range
  sun_date_range <- repo_date_range
  
} else {
  # Import existing historical repositories
  
  # Import raw data repository by finding file with the latest creation time
  raw_data_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/RawDataRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  raw_data_repo_files <- raw_data_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_raw_data_repo <- raw_data_repo_files[1, "FileName"]
  
  raw_data_repo <- readRDS(latest_raw_data_repo)
  
  # Import daily summary repository by finding file with the latest creation time
  daily_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/DailyRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  daily_repo_files <- daily_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_daily_repo <- daily_repo_files[1, "FileName"]
  
  daily_repo <- readRDS(latest_daily_repo)
  
  # Import weekly summary repository by finding file with the latest creation time
  weekly_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/WeeklyRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  weekly_repo_files <- weekly_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_weekly_repo <- weekly_repo_files[1, "FileName"]
  
  weekly_repo <- readRDS(latest_weekly_repo)
  
  # Import monthly summary repository by finding file with the latest creation time
  monthly_repo_files <- data.frame(
    "FileName" = list.files(path = paste0(user_directory,
                                          "/CP Repositories",
                                          "/MonthlyRepo"),
                            pattern = "*.RDS",
                            full.names = TRUE))
  
  monthly_repo_files <- monthly_repo_files %>%
    mutate(CreatedTime = file.info(FileName)$ctime) %>%
    arrange(desc(CreatedTime))
  
  latest_monthly_repo <- monthly_repo_files[1, "FileName"]
  
  monthly_repo <- readRDS(latest_monthly_repo)

  #
  # Find last date of resulted lab data in historical repositories for
  # SCC and Sunquest sites
  last_dates <- data.frame(
    "SCCSites" = as.Date(
      max(raw_data_repo[
        which(raw_data_repo$Site %in% c("MSH", "MSQ")), ]$ResultDate),
      format = "%Y-%m-%d"),
    "SunSites" = as.Date(
      max(raw_data_repo[
        which(!(raw_data_repo$Site %in% c("MSH", "MSQ"))), ]$ResultDate),
      format = "%Y-%m-%d"))
  # Create vector with possible data report dates for SCC and Sunquest sites
  if (last_dates$SCCSites + 2 > todays_date) {
    scc_date_range <- seq(from = todays_date,
                          to = todays_date,
                          by = "day")
  } else {
    scc_date_range <- seq(from = last_dates$SCCSites + 2,
                          to = todays_date,
                          by = "day")
  }
  
  if (last_dates$SunSites + 2 > todays_date) {
    sun_date_range <- seq(from = todays_date,
                          to = todays_date,
                          by = "day")
  } else {
    sun_date_range <- seq(from = last_dates$SunSites + 2,
                          to = todays_date,
                          by = "day")
  }
}



if(length(scc_date_range)>900){
  scc_date_ranges <- split(scc_date_range, ceiling(seq_along(scc_date_range)/500))
  pattern_scc = lapply(scc_date_ranges, 
                       function(x){
                         paste0("^(Doc){1}.+",
                                x,
                                ".xlsx",
                                collapse = "|")})
  # Find list of SCC data reports within date range
  file_list_scc <- lapply(pattern_scc,
                          function(x){
                            list.files(
                              path = paste0(user_directory, "/SCC CP Reports"),
                              pattern = x )})
  file_list_scc <- as.vector(unlist(file_list_scc))
  
  
}else{
  pattern_scc = paste0("^(Doc){1}.+",
                       scc_date_range,
                       ".xlsx",
                       collapse = "|")
  file_list_scc <- list.files(
    path = paste0(user_directory, "/SCC CP Reports"),
    pattern = pattern_scc)
  
}


if(length(sun_date_range)>900){
  sun_date_ranges <- split(sun_date_range, ceiling(seq_along(sun_date_range)/500))
  # Find list of SCC data reports within date range
  pattern_sun = lapply(sun_date_ranges, 
                       function(x){
                         paste0("^(KPI_Daily_TAT_Report){1}.*",
                                x,
                                ".xls", collapse = "|")})
  file_list_sun <- lapply(pattern_sun,
                          function(x){
                            list.files(
                              path = paste0(user_directory, "/SUN CP Reports"),
                              pattern = x )})
  
  file_list_sun <- as.vector(unlist(file_list_sun))
  
  
}else{
  pattern_sun <-  paste0("^(KPI_Daily_TAT_Report){1}.*",
                         sun_date_range,
                         ".xls", collapse = "|")
  file_list_sun <- list.files(
    path = paste0(user_directory, "/SUN CP Reports"),
    pattern = pattern_sun)
  
  
}



# Read daily SCC reports from date range, if any exist --------
if (length(file_list_scc) > 0) {
  scc_raw_data_list <- lapply(
    file_list_scc, function(x) read_excel(
      path = paste0(user_directory, "/SCC CP Reports/", x)))
} else {
  scc_raw_data_list <- NULL
}

# Read daily Sunquest reports, if any exist --------
if (length(file_list_sun) > 0) {
  sun_daily_raw_data_list <- lapply(
    file_list_sun, function(x) (read_excel(
      path = paste0(user_directory, "/SUN CP Reports/", x),
      col_types = c("text", "text", "text", "text", "text",
                    "text", "text", "text", "text",
                    "numeric", "numeric", "numeric", "numeric", "numeric",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text", "text"))))
} else {
  sun_daily_raw_data_list <- NULL
}



# # Custom function to determine resulted lab date from preprocessed data -------
# # SCC data often has a few labs with incorrect result date
# correct_result_dates <- function(data, number_days) {
#   all_resulted_dates_vol <- data %>%
#     group_by(ResultDate) %>%
#     summarize(VolLabs = n()) %>%
#     arrange(desc(VolLabs)) %>%
#     ungroup()
#   
#   
#   correct_dates <- all_resulted_dates_vol$ResultDate[1:number_days]
#   
#   new_data <- data %>%
#     filter(ResultDate %in% correct_dates)
#   return(new_data)
# }
for (file in file_list_scc){
  scc_data <- read_excel(path = paste0(user_directory, "/SCC CP Reports/", file))
  scc_daily_preprocessed <- preprocess_scc(scc_data)
  key_cols <- c("ORDERID","TEST")
  update_cols <- names(scc_daily_preprocessed)
  update_cols <- update_cols[! update_cols %in% key_cols]
  write_temporary_table_to_database_and_merge_updated(scc_daily_preprocessed,
                                                      key_columns = key_cols,
                                                      update_columns = update_cols,
                                                      source_table_name = "LAB_KPI_PROCESSED_DAILY_CP_DATA_ST",
                                                      destination_table_name = "LAB_KPI_PROCESSED_DAILY_CP_DATA")
  
}

for (file in file_list_sun){
  sun_data <- read_excel((read_excel(
    path = paste0(user_directory, "/SUN CP Reports/", file),
    col_types = c("text", "text", "text", "text", "text",
                  "text", "text", "text", "text",
                  "numeric", "numeric", "numeric", "numeric", "numeric",
                  "text", "text", "text", "text", "text",
                  "text", "text", "text", "text", "text",
                  "text", "text", "text", "text", "text",
                  "text", "text", "text", "text", "text", "text"))))
  sun_daily_preprocessed <- preprocess_scc(sun_data)
  key_cols <- c("ORDERID","TEST")
  update_cols <-  names(sun_daily_preprocessed)
  update_cols <- update_cols[! update_cols %in% key_cols]
  write_temporary_table_to_database_and_merge_updated(sun_daily_preprocessed,
                                                      key_columns = key_cols,
                                                      update_columns = update_cols,
                                                      source_table_name = "LAB_KPI_PROCESSED_DAILY_CP_DATA_ST",
                                                      destination_table_name = "LAB_KPI_PROCESSED_DAILY_CP_DATA")
  
}

