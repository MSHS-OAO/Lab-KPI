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


# Compile processed SCC daily data ------------------------------
if (!is.null(scc_raw_data_list)) {
  # Preprocess SCC daily raw data using custom function
  scc_daily_preprocessed <- lapply(scc_raw_data_list,
                                   preprocess_scc)
  # Select the second element of the list of lists
  # scc_preprocessed_data <- lapply(scc_daily_preprocessed, function(x) x[[2]])
  
  # # Remove any labs with incorrect dates then bind daily reports into one data frame
  # scc_preprocessed_data <- lapply(scc_preprocessed_data, 
  #                                 function(x) correct_result_dates(x, number_days = 1))
  
  # Bind all data together
  scc_daily_bind <- bind_rows(scc_daily_preprocessed)
  
  } else {
  scc_daily_bind <- NULL
}


# Compile preprocessed Sunquest daily data, if any exists ---------------
if (!is.null(sun_daily_raw_data_list)) {
  # Preprocess Sunquest daily raw data using custom function
  sun_daily_preprocessed <- lapply(sun_daily_raw_data_list,
                                   preprocess_daily_sun)
  # Select the second element of the list of lists
  # sun_preprocessed_data <- lapply(sun_daily_preprocessed, function(x) x[[2]])
  
  # Bind daily reports into one data frame
  sun_daily_bind <- bind_rows(sun_daily_preprocessed)
  
} else {
  sun_daily_bind <- NULL
}

# Bind together preprocessed SCC and Sunquest data --------------------------------------
bind_all_data <- rbind(sun_daily_bind, scc_daily_bind)

# Combine raw data repo with new data
if (initial_run == TRUE) {
  latest_raw_data_repo <- bind_all_data
} else {
  # Remove dates more than 60 days back and remove any dates that are in the
  # latest data imports
  mod_raw_data_repo <- raw_data_repo %>%
    filter(ResultDate >= (todays_date - 60) &
             !(ResultDate %in% unique(bind_all_data$ResultDate))) %>%
    mutate(CompleteWeek = NULL,
           CompleteMonth = NULL)
  
  latest_raw_data_repo <- rbind(mod_raw_data_repo, bind_all_data)
  
}

# Create data frame of start and end dates of weeks and months
# This will be used to determine if the data for a complete week or month is present
# If the week or month is not complete, that data will not be included in the repositories
week_dates <- unique(latest_raw_data_repo[, c("WEEKSTART", "WEEKEND")])

week_dates <- week_dates %>%
  mutate(StartInData = WeekStart %in% unique(latest_raw_data_repo$ResultDate),
         EndInData = WeekEnd %in% unique(latest_raw_data_repo$ResultDate),
         CompleteWeek = StartInData & EndInData)

month_dates <- unique(latest_raw_data_repo[, c("MonthNo", "Year")])

month_dates <- month_dates %>%
  mutate(MonthRollUp = as.Date(paste0(MonthNo, "/",
                                      1, "/",
                                      Year),
                               format = "%m/%d/%Y"),
         MonthStart = floor_date(MonthRollUp, unit = "month"),
         MonthEnd = ceiling_date(MonthRollUp, unit = "month") - 1,
         StartInData = MonthStart %in% unique(latest_raw_data_repo$ResultDate),
         EndInData = MonthEnd %in% unique(latest_raw_data_repo$ResultDate),
         CompleteMonth = StartInData & EndInData)

latest_raw_data_repo <- left_join(latest_raw_data_repo,
                           week_dates[, c("WeekStart",
                                          "WeekEnd",
                                          "CompleteWeek")],
                           by = c("WeekStart" = "WeekStart",
                                  "WeekEnd" = "WeekEnd"))

latest_raw_data_repo <- left_join(latest_raw_data_repo,
                           month_dates[, c("MonthRollUp", "CompleteMonth")],
                           by = c("MonthRollUp" = "MonthRollUp"))

# Summarize data for each day
cp_daily_summary <- latest_raw_data_repo %>%
  group_by(
    Site,
    ResultDate,
    WeekStart,
    WeekEnd,
    WeekOf,
    MonthNo,
    MonthName,
    Year,
    Test,
    Division,
    SettingRollUp,
    MasterSetting,
    DashboardSetting,
    AdjPriority,
    DashboardPriority,
    ReceiveResultTarget,
    CollectResultTarget) %>%
  summarize(
    # Calculate total number of labs resulted
    TotalResulted = n(),
    # Calculate number of labs with valid receive times and collection times
    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
    CollectTime_VolIncl = sum(CollectTime_TATInclude),
    # Calculate number of labs within target TAT
    TotalReceiveResultInTarget =
      sum(ReceiveResultInTarget[ReceiveTime_TATInclude], na.rm = TRUE),
    TotalCollectResultInTarget =
      sum(CollectResultInTarget[CollectTime_TATInclude], na.rm = TRUE),
    TotalAddOnOrder = sum(AddOnMaster %in% c("AddOn"), na.rm = TRUE),
    TotalMissingCollections = sum(MissingCollect),
    # Calculate key statistics for collect-to-receive TAT
    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                 na.rm = TRUE),
    # Calculate key statistics for receive-to-result TAT
    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    # Calculate key statistics for collect-to-result TAT
    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    .groups = "keep") %>%
  ungroup()

# Summarize data for each week
# Filter out data for incomplete weeks
cp_weekly_summary <- latest_raw_data_repo %>%
  filter(CompleteWeek) %>%
  group_by(
    Site,
    WeekStart,
    WeekEnd,
    WeekOf,
    Test,
    Division,
    MasterSetting,
    DashboardSetting,
    DashboardPriority,
    ReceiveResultTarget,
    CollectResultTarget) %>%
  summarize(
    # Calculate total number of labs resulted
    TotalResulted = n(),
    # Calculate number of labs with valid receive times and collection times
    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
    CollectTime_VolIncl = sum(CollectTime_TATInclude),
    # Calculate number of labs within target TAT
    TotalReceiveResultInTarget =
      sum(ReceiveResultInTarget[ReceiveTime_TATInclude], na.rm = TRUE),
    TotalCollectResultInTarget =
      sum(CollectResultInTarget[CollectTime_TATInclude], na.rm = TRUE),
    TotalAddOnOrder = sum(AddOnMaster %in% c("AddOn"), na.rm = TRUE),
    TotalMissingCollections = sum(MissingCollect),
    # Calculate key statistics for collect-to-receive TAT
    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                 na.rm = TRUE),
    # Calculate key statistics for receive-to-result TAT
    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    # Calculate key statistics for collect-to-result TAT
    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    .groups = "keep") %>%
  ungroup()

# Summarize data for each month
# Filter out data for incomplete months
cp_monthly_summary <- latest_raw_data_repo %>%
  filter(CompleteMonth) %>%
  group_by(
    Site,
    MonthRollUp,
    MonthNo,
    MonthName,
    Year,
    Test,
    Division,
    MasterSetting,
    DashboardSetting,
    DashboardPriority,
    ReceiveResultTarget,
    CollectResultTarget) %>%
  summarize(
    # Calculate total number of labs resulted and those with valid TAT
    TotalResulted = n(),
    # Calculate number of labs with valid receive times and collection times
    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
    CollectTime_VolIncl = sum(CollectTime_TATInclude),
    # Calculate number of labs within target TAT
    TotalReceiveResultInTarget =
      sum(ReceiveResultInTarget[ReceiveTime_TATInclude], na.rm = TRUE),
    TotalCollectResultInTarget =
      sum(CollectResultInTarget[CollectTime_TATInclude], na.rm = TRUE),
    TotalAddOnOrder = sum(AddOnMaster %in% c("AddOn"), na.rm = TRUE),
    TotalMissingCollections = sum(MissingCollect),
    # Calculate key statistics for collect-to-receive TAT
    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                 na.rm = TRUE),
    # Calculate key statistics for receive-to-result TAT
    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude], na.rm = TRUE),
    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    # Calculate key statistics for collect-to-result TAT
    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude], na.rm = TRUE),
    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude], probs = c(0.95),
                                na.rm = TRUE),
    .groups = "keep") %>%
  ungroup()

# Update repositories with latest data
if (initial_run == TRUE) {
  latest_daily_repo <- cp_daily_summary
  latest_weekly_repo <- cp_weekly_summary
  latest_monthly_repo <- cp_monthly_summary
  
} else {
  # Remove data from daily repo for dates that are in current daily summary
  mod_daily_repo <- daily_repo %>%
    filter(!(ResultDate %in% cp_daily_summary$ResultDate))
  # Bind repository with current daily summary
  latest_daily_repo <- rbind(mod_daily_repo, cp_daily_summary)
  
  # Remove data from weekly repo for weeks that are in current weekly summary
  mod_weekly_repo <- weekly_repo %>%
    filter(!(WeekStart %in% cp_weekly_summary$WeekStart))
  # Bind repository with current weekly summary
  latest_weekly_repo <- rbind(mod_weekly_repo, cp_weekly_summary)
  
  # Remove data from monthly_repo for months that are in current monthly summary
  mod_monthly_repo <- monthly_repo %>%
    filter(!(MonthRollUp %in% cp_monthly_summary$MonthRollUp))
  # Bind repository with current monthly summary
  latest_monthly_repo <- rbind(mod_monthly_repo, cp_monthly_summary)
}

# Save repositories in appropriate folder
# First ensure that raw data repository only includes 60 days of data
if (initial_run == TRUE) {
  raw_data_repo_export <- latest_raw_data_repo %>%
    filter(ResultDate >= todays_date - 60)
} else {
  raw_data_repo_export <- latest_raw_data_repo
}

# Test code for comparing existing repos to new repos; want to make sure historical data remains unchanged
# old_monthly_repo_months <- unique(monthly_repo$MonthRollUp)
# 
# check_new_monthly_repo <- latest_monthly_repo %>%
#   filter(MonthRollUp %in% old_monthly_repo_months)
# 
# new_monthly_repo_data <- latest_monthly_repo %>%
#   filter(!(MonthRollUp %in% old_monthly_repo_months))
# 
# old_weekly_repo_weeks <- unique(weekly_repo$WeekOf)
# 
# check_new_weekly_repo <- latest_weekly_repo %>%
#   filter(WeekOf %in% old_weekly_repo_weeks)
# 
# weekly_repo <- weekly_repo %>%
#   arrange(Site,
#           WeekStart,                 
#           WeekEnd,                  
#           WeekOf,                    
#           Test,                      
#           Division,                 
#           MasterSetting,             
#           DashboardSetting,          
#           DashboardPriority,        
#           ReceiveResultTarget,       
#           CollectResultTarget,       
#           TotalResulted,            
#           ReceiveTime_VolIncl,       
#           CollectTime_VolIncl,       
#           TotalReceiveResultInTarget,
#           TotalCollectResultInTarget,
#           TotalAddOnOrder,           
#           TotalMissingCollections,  
#           CollectReceive_Avg,        
#           CollectReceive_Median,     
#           CollectReceive_95,        
#           ReceiveResult_Avg,         
#           ReceiveResult_Median,      
#           ReceiveResult_95,         
#           CollectResult_Avg,         
#           CollectResult_Median,      
#           CollectResult_95)
# 
# check_new_weekly_repo <- check_new_weekly_repo %>%
#   arrange(Site,
#           WeekStart,                 
#           WeekEnd,                  
#           WeekOf,                    
#           Test,                      
#           Division,                 
#           MasterSetting,             
#           DashboardSetting,          
#           DashboardPriority,        
#           ReceiveResultTarget,       
#           CollectResultTarget,       
#           TotalResulted,            
#           ReceiveTime_VolIncl,       
#           CollectTime_VolIncl,       
#           TotalReceiveResultInTarget,
#           TotalCollectResultInTarget,
#           TotalAddOnOrder,           
#           TotalMissingCollections,  
#           CollectReceive_Avg,        
#           CollectReceive_Median,     
#           CollectReceive_95,        
#           ReceiveResult_Avg,         
#           ReceiveResult_Median,      
#           ReceiveResult_95,         
#           CollectResult_Avg,         
#           CollectResult_Median,      
#           CollectResult_95)

saveRDS(raw_data_repo_export,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/RawDataRepo",
                      "/Labs Resulted ",
                      format(min(latest_raw_data_repo$ResultDate), "%m-%d-%y"),
                      " to ",
                      format(max(latest_raw_data_repo$ResultDate), "%m-%d-%y"),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))

saveRDS(latest_daily_repo,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/DailyRepo",
                      "/Daily Repo ",
                      format(min(latest_daily_repo$ResultDate), "%m-%d-%y"),
                      " to ",
                      format(max(latest_daily_repo$ResultDate), "%m-%d-%y"),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))

saveRDS(latest_weekly_repo,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/WeeklyRepo",
                      "/Weekly Repo ",
                      format(min(latest_weekly_repo$WeekStart), "%m-%d-%y"),
                      " to ",
                      format(max(latest_weekly_repo$WeekEnd), "%m-%d-%y"),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))

saveRDS(latest_monthly_repo,
        file = paste0(user_directory,
                      "/CP Repositories",
                      "/MonthlyRepo",
                      "/Monthly Repo ",
                      paste0(month(min(latest_monthly_repo$MonthRollUp), 
                                   label = TRUE, abbr = TRUE), 
                             year(min(latest_monthly_repo$MonthRollUp))),
                      " to ",
                      paste0(month(max(latest_monthly_repo$MonthRollUp), 
                                   label = TRUE, abbr = TRUE), 
                             year(max(latest_monthly_repo$MonthRollUp))),
                      " as of ",
                      format(Sys.Date(), "%m-%d-%y"),
                      ".RDS"))
