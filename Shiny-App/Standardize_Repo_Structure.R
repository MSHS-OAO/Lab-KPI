# Code to restructure and rename existing repositories

#Required packages: run these every time you run the code
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
# library(gdtools)
# library(kableExtra)
library(kableExtra,  "~/R/x86_64-pc-linux-gnu-library/4.2")
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)

# Determine root directory using custom function
# Function to determine path to share drive on R Workbench or R Studio
define_root_path <- function(){
  #Check if directory is from R Workbench; starts with '/home'
  if(grepl("^/home", dirname(getwd()))){
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files("/SharedDrive/") == "Presidents",
           #Define prefix of path to share drive with R Workbench format
           output <- "/SharedDrive/Presidents/", 
           output <- "/SharedDrive/deans/Presidents/")
  }#Check if directory is from R Studio; starts with an uppercase letter than ':'
  else if(grepl("^[[:upper:]]+:", dirname(getwd()))){
    #Determine which drive is mapped to Sharedrive (x)
    for(i in LETTERS){
      if(any(grepl("deans|Presidents", list.files(paste0(i, "://"))))){x <- i}
    }
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files(paste0(x, "://")) == "Presidents",
           #Define prefix of path to share drive with R Studio format
           output <- paste0(x, ":/Presidents/"),
           output <- paste0(x, ":/deans/Presidents/"))
    
  }
  return(output)
}

user_directory <- paste0(
  define_root_path(),
  "HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/",
  "Lab KPI/Data/Shiny App Repo"
)

raw_data <- readRDS(paste0(user_directory,
                           "/Shiny App Repo/",
                           "/CPTestLevelData/CPTestData60Days.rds"))

daily_repo <- readRDS(paste0(user_directory,
                             "/Shiny App Repo/",
                             "/CPDailySummary/CPDailySummary.rds"))

weekly_repo <- readRDS(paste0(user_directory,
                           "/WeeklyRepo/WeeklySummary.rds"))

monthly_repo <- readRDS(paste0(user_directory,
                           "/MonthlyRepo/MonthlySummary.rds"))

# raw_data <- raw_data %>%
#   rename("DetailedSetting" = "MasterSetting",
#          "AddOnFinal" = "AddOnMaster")

raw_data <- raw_data %>%
  select(-Year,
         -MonthNo,
         -MonthName,
         -MonthRollUp,
         -WeekStart,
         -WeekEnd,
         -WeekOf,
         -CompleteWeek,
         -CompleteMonth)

daily_repo <- daily_repo %>%
  mutate(WeekStart = NULL,
         WeekEnd = NULL,
         WeekOf = NULL,
         MonthNo = NULL,
         MonthName = NULL,
         Year = NULL) %>%
  rename("DetailedSetting" = "MasterSetting")

weekly_repo <- weekly_repo %>%
  rename("DetailedSetting" = "MasterSetting")

monthly_repo <- monthly_repo %>%
  rename("DetailedSetting" = "MasterSetting")

saveRDS(raw_data,
        paste0(user_directory,
               "/Shiny App Repo/",
               "/CPTestLevelData/CPTestData60Days.rds"))

saveRDS(daily_repo,
        paste0(user_directory,
               "/Shiny App Repo/",
               "/CPDailySummary/CPDailySummary.rds"))

saveRDS(weekly_repo,
        paste0(user_directory,
               "/WeeklyRepo/WeeklySummary.rds"))

saveRDS(monthly_repo,
        paste0(user_directory,
               "/MonthlyRepo/MonthlySummary.rds"))
