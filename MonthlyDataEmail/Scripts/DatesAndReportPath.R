library(DBI)
library(odbc)
library(lubridate)
library(glue)
library(readr)
library(zip)




data_path <- paste0("/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Lab KPI/Data/", 
                    "/AdHoc",
                    "/Lab KPI Monthly Data ",
                    format(Sys.Date(), "%Y-%m-%d"),
                    ".xlsx")