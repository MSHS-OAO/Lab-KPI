# Code for saving monthly repo as an excel file for Daya

# Clear environment
rm(list = ls())

# Load libraries
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
library(ggplot2)
library(gridExtra)
library(scales)
library(ggQC)
library(ggplot2)
library(janitor)
library(tidyr)
library(purrr)

# Determine directory
if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}

monthly_repo_file <- choose.files(default = paste0(user_directory,
                                                   "/CP Repositories",
                                                   "/MonthlyRepo",
                                                   "/*.*"))

monthly_repo <- readRDS(monthly_repo_file)

data_definitions = data.frame(
  "Field" = c("Site",
              "MonthRollUp",
              "MonthNo",
              "MonthName",
              "Year",
              "Test",
              "Division",
              "MasterSetting",
              "DashboardSetting",
              "DashboardPriority",
              "ReceiveResultTarget",
              "CollectResultTarget",
              "TotalResulted",
              "ReceiveTime_VolIncl",
              "CollectTime_VolIncl",
              "TotalReceiveResultInTarget",
              "TotalCollectResultInTarget",
              "TotalAddOnOrder",
              "TotalMissingCollections",
              "CollectReceive_Avg",
              "CollectReceive_Median",
              "CollectReceive_95",
              "ReceiveResult_Avg",
              "ReceiveResult_Median",
              "ReceiveResult_95",
              "CollectResult_Avg",
              "CollectResult_Median",
              "CollectResult_95"
              ),
  "Definition" = c("MSHS Site",
                   "Month specimens were resulted",
                   "2 digit month specimens were resulted",
                   "Name of month specimens were resulted",
                   "Year specimens were resulted",
                   "Test name",
                   "Lab division",
                   "Patient setting based on documented clinic type and ICU mapping",
                   "Roll up of patient setting as displayed on Daily Lab KPI dashboard (ie, ED & ICU grouped together)",
                   "Order priority as displayed on Daily Lab KPI Dashboard (ie, all ED & ICU labs treated as stat, all troponin and lactate WB labs treated as stat)",
                   "Receive-to-Result turnaround time target in minutes",
                   "Collect-to-Result turnaround time target in minutes",
                   "Total resulted volume (ie, all resulted labs, even those with missing timestamps, negative timestamps, etc. Not all of these labs should be used in further analysis for % of labs within target.)",
                   "Volume of resulted labs with valid receive-to-result times. This is what should be used when calculating % of labs meeting receive-to-result turnaround time targets.",
                   "Volume of resulted labs with valid collect-to-result times. This is what should be used when calculating % of labs meeting collect-to-result turnaround time targets.",
                   "Total number of labs that meet the receive-to-result turnaround time target",
                   "Total number of labs that meet the collect-to-result turnaround time target",
                   "Total number of add-on orders",
                   "Total number of labs with missing collections",
                   "Average collect-to-receive turnaround time in minutes",
                   "Median collect-to-receive turnaround time in minutes",
                   "95th percentile collect-to-receive turnaround time in minutes",
                   "Average receive-to-result turnaround time in minutes",
                   "Median receive-to-result turnaround time in minutes",
                   "95th percentile receive-to-result turnaround time in minutes",
                   "Average collect-to-result turnaround time in minutes",
                   "Median collect-to-result turnaround time in minutes",
                   "95th percentile collect-to-result turnaround time in minutes"
                   )
)

export_list = list("Data_Definitions" = data_definitions,
                   "Monthly_Data" = monthly_repo)

write_xlsx(export_list,
           path = paste0(user_directory,
                         "/AdHoc",
                         "/Lab KPI Monthly Data ",
                         format(Sys.Date(), "%Y-%m-%d"),
                         ".xlsx"))
