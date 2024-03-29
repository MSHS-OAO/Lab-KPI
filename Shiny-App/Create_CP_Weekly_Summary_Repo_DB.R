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
library(here)
library(odbc)
library(dbplyr)
library(DBI)
library(glue)
library(foreach)
library(doParallel)
library(parallel)

#Clear existing history
rm(list = ls())

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

user_directory <- paste0(define_root_path(),
                         "HSPI-PM/",
                         "Operations Analytics and Optimization/Projects/",
                         "Service Lines/Lab Kpi/Data")

# Daily Summary Repo ----------------------

weekly_repo <- readRDS(
  paste0(user_directory,
         "/CP Repositories/WeeklyRepo",
         "/Weekly Repo 12-06-20 to 08-26-23 as of 08-30-23.RDS")
)

weekly_repo_db <- weekly_repo %>%
  select(-WeekOf) %>%
  rename(SITE = Site,
         WEEK_START = WeekStart,
         WEEK_END = WeekEnd,
         TEST = Test,
         DIVISION = Division,
         DETAILED_SETTING = MasterSetting,
         DASHBOARD_SETTING = DashboardSetting,
         DASHBOARD_PRIORITY = DashboardPriority,
         RECEIVE_RESULT_TARGET = ReceiveResultTarget,
         COLLECT_RESULT_TARGET = CollectResultTarget,
         TOTAL_RESULTED = TotalResulted,
         RECEIVE_TIME_VOL_INCL = ReceiveTime_VolIncl,
         COLLECT_TIME_VOL_INCL = CollectTime_VolIncl,
         TOTAL_RECEIVE_RESULT_IN_TARGET = TotalReceiveResultInTarget,
         TOTAL_COLLECT_RESULT_IN_TARGET = TotalCollectResultInTarget,
         TOTAL_ADD_ON_ORDER = TotalAddOnOrder,
         TOTAL_MISSING_COLLECTIONS = TotalMissingCollections,
         COLLECT_RECEIVE_AVG = CollectReceive_Avg,
         COLLECT_RECEIVE_MEDIAN = CollectReceive_Median,
         COLLECT_RECEIVE_95 = CollectReceive_95,
         RECEIVE_RESULT_AVG = ReceiveResult_Avg,
         RECEIVE_RESULT_MEDIAN = ReceiveResult_Median,
         RECEIVE_RESULT_95 = ReceiveResult_95,
         COLLECT_RESULT_AVG = CollectResult_Avg,
         COLLECT_RESULT_MEDIAN = CollectResult_Median,
         COLLECT_RESULT_95 = CollectResult_95)


id_primary_keys_1 <- weekly_repo_db %>%
  select(SITE, WEEK_START, TEST, DETAILED_SETTING, DASHBOARD_PRIORITY) %>%
  distinct()


get_values_weekly_summary <- function(x, table_name){

  site <- x[1]
  week_start <- x[2]
  week_end <- x[3]
  test <- x[4]
  division <- x[5]
  detailed_setting <- x[6]
  dashboard_setting <- x[7]
  dashboard_priority <- x[8]
  receive_result_target <- x[9]
  collect_result_target <- x[10]
  total_resulted <- x[11]
  receive_time_vol_incl <- x[12]
  collect_time_vol_incl <- x[13]
  total_receive_result_in_target <- x[14]
  total_collect_result_in_target <- x[15]
  total_add_on <- x[16]
  total_missing_collections <- x[17]
  collect_receive_avg <- x[18]
  collect_receive_median <- x[19]
  collect_receive_95 <- x[20]
  receive_result_avg <- x[21]
  receive_result_median <- x[22]
  receive_result_95 <- x[23]
  collect_result_avg <- x[24]
  collect_result_median <- x[25]
  collect_result_95 <- x[26]


  values <- glue(
    "INTO \"{table_name}\"
    (SITE,
    WEEK_START,
    WEEK_END,
    TEST,
    DIVISION,
    DETAILED_SETTING,
    DASHBOARD_SETTING,
    DASHBOARD_PRIORITY,
    RECEIVE_RESULT_TARGET,
    COLLECT_RESULT_TARGET,
    TOTAL_RESULTED,
    RECEIVE_TIME_VOL_INCL,
    COLLECT_TIME_VOL_INCL,
    TOTAL_RECEIVE_RESULT_IN_TARGET,
    TOTAL_COLLECT_RESULT_IN_TARGET,
    TOTAL_ADD_ON_ORDER,
    TOTAL_MISSING_COLLECTIONS,
    COLLECT_RECEIVE_AVG,
    COLLECT_RECEIVE_MEDIAN,
    COLLECT_RECEIVE_95,
    RECEIVE_RESULT_AVG,
    RECEIVE_RESULT_MEDIAN,
    RECEIVE_RESULT_95,
    COLLECT_RESULT_AVG,
    COLLECT_RESULT_MEDIAN,
    COLLECT_RESULT_95)

    VALUES (
    '{site}',
    TO_DATE('{week_start}', 'YYYY-MM-DD'),
    TO_DATE('{week_end}', 'YYYY-MM-DD'),
    '{test}',
    '{division}',
    '{detailed_setting}',
    '{dashboard_setting}',
    '{dashboard_priority}',
    '{receive_result_target}',
    '{collect_result_target}',
    '{total_resulted}',
    '{receive_time_vol_incl}',
    '{collect_time_vol_incl}',
    '{total_receive_result_in_target}',
    '{total_collect_result_in_target}',
    '{total_add_on}',
    '{total_missing_collections}',
    '{collect_receive_avg}',
    '{collect_receive_median}',
    '{collect_receive_95}',
    '{receive_result_avg}',
    '{receive_result_median}',
    '{receive_result_95}',
    '{collect_result_avg}',
    '{collect_result_median}',
    '{collect_result_95}')"
  )

  return(values)
}

weekly_summary_temp_table <- "CP_WEEKLY_REPO_TEMP"
weekly_summary_repo_table <- "CP_WEEKLY_REPO"

# test_data <- weekly_repo_db[1:10000, ]
# 
# processed_input_data <- test_data
processed_input_data <- weekly_repo_db

# Ensure all the fields are correct data type
processed_input_data <- processed_input_data %>%
  mutate(SITE = as.character(SITE),
         WEEK_START = format(WEEK_START, "%Y-%m-%d"),
         WEEK_END = format(WEEK_END, "%Y-%m-%d"),
         TEST = as.character(TEST),
         DIVISION = as.character(DIVISION),
         DETAILED_SETTING = as.character(DETAILED_SETTING),
         DASHBOARD_SETTING = as.character(DASHBOARD_SETTING),
         DASHBOARD_PRIORITY = as.character(DASHBOARD_PRIORITY),
         RECEIVE_RESULT_TARGET = as.integer(RECEIVE_RESULT_TARGET),
         COLLECT_RESULT_TARGET = as.integer(COLLECT_RESULT_TARGET),
         TOTAL_RESULTED = as.integer(TOTAL_RESULTED),
         RECEIVE_TIME_VOL_INCL = as.integer(RECEIVE_TIME_VOL_INCL),
         COLLECT_TIME_VOL_INCL = as.integer(COLLECT_TIME_VOL_INCL),
         TOTAL_RECEIVE_RESULT_IN_TARGET = as.integer(TOTAL_RECEIVE_RESULT_IN_TARGET),
         TOTAL_COLLECT_RESULT_IN_TARGET = as.integer(TOTAL_COLLECT_RESULT_IN_TARGET),
         TOTAL_ADD_ON_ORDER = as.integer(TOTAL_ADD_ON_ORDER),
         TOTAL_MISSING_COLLECTIONS = as.integer(TOTAL_MISSING_COLLECTIONS),
         COLLECT_RECEIVE_AVG = round(as.numeric(COLLECT_RECEIVE_AVG), digits = 6),
         COLLECT_RECEIVE_MEDIAN = round(as.numeric(COLLECT_RECEIVE_MEDIAN), digits = 6),
         COLLECT_RECEIVE_95 = round(as.numeric(COLLECT_RECEIVE_95), digits = 6),
         RECEIVE_RESULT_AVG = round(as.numeric(RECEIVE_RESULT_AVG), digits = 6),
         RECEIVE_RESULT_MEDIAN = round(as.numeric(RECEIVE_RESULT_MEDIAN), digits = 6),
         RECEIVE_RESULT_95 = round(as.numeric(RECEIVE_RESULT_95), digits = 6),
         COLLECT_RESULT_AVG = round(as.numeric(COLLECT_RESULT_AVG), digits = 6),
         COLLECT_RESULT_MEDIAN = round(as.numeric(COLLECT_RESULT_MEDIAN), digits = 6),
         COLLECT_RESULT_95 = round(as.numeric(COLLECT_RESULT_95), digits = 6)
  ) %>%
  mutate(across(where(is.numeric), replace_na, NA)) %>%
  mutate(across(everything(),
                gsub, pattern = "\'", replacement = "''")) %>%
  # function(x) {gsub("\'", "''", x)})) %>%
  # mutate(across(where(is.character), replace_na, replace = '')) %>%
  # mutate(across(where(is.numeric)), replace_na, pattern = paste0(c(NA, NaN), collapse = '|'),
  #        replace = '') %>%
  mutate(across(everything(), replace_na, '')) %>%
  mutate(across(everything(), gsub, pattern = "&",
                replacement = "'||chr(38)||'"))

# Convert the each record/row of tibble to INTO clause of insert statement
system.time(
  inserts <- mclapply(
    mclapply(
      mclapply(split(processed_input_data,
                     1:nrow(processed_input_data)),
               as.list),
      as.character),
    FUN = get_values_weekly_summary, weekly_summary_temp_table)
)

values <- glue_collapse(inserts,sep = "\n\n")

# Combine into statements from get_values() function and combine with insert statements
all_data <- glue('INSERT ALL
                  {values}
                 SELECT 1 from DUAL;')

# glue() query to merge data from temporary table to repository table
merge_query <- glue('MERGE INTO "{weekly_summary_repo_table}" RT
                    USING "{weekly_summary_temp_table}" SOURCE_TABLE
                    ON (RT."SITE" = SOURCE_TABLE."SITE" AND
                        RT."WEEK_START" = SOURCE_TABLE."WEEK_START" AND
                        RT."TEST" = SOURCE_TABLE."TEST" AND
                        RT."DETAILED_SETTING" = SOURCE_TABLE."DETAILED_SETTING" AND
                        RT."DASHBOARD_PRIORITY" = SOURCE_TABLE."DASHBOARD_PRIORITY")
                    WHEN MATCHED THEN
                    UPDATE SET RT."WEEK_END" = SOURCE_TABLE."WEEK_END",
                               RT."DIVISION" = SOURCE_TABLE."DIVISION",
                               RT."DASHBOARD_SETTING" = SOURCE_TABLE."DASHBOARD_SETTING",
                               RT."RECEIVE_RESULT_TARGET" = SOURCE_TABLE."RECEIVE_RESULT_TARGET",
                               RT."COLLECT_RESULT_TARGET" = SOURCE_TABLE."COLLECT_RESULT_TARGET",
                               RT."TOTAL_RESULTED" = SOURCE_TABLE."TOTAL_RESULTED",
                               RT."RECEIVE_TIME_VOL_INCL" = SOURCE_TABLE."RECEIVE_TIME_VOL_INCL",
                               RT."COLLECT_TIME_VOL_INCL" = SOURCE_TABLE."COLLECT_TIME_VOL_INCL",
                               RT."TOTAL_RECEIVE_RESULT_IN_TARGET" = SOURCE_TABLE."TOTAL_RECEIVE_RESULT_IN_TARGET",
                               RT."TOTAL_COLLECT_RESULT_IN_TARGET" = SOURCE_TABLE."TOTAL_COLLECT_RESULT_IN_TARGET",
                               RT."TOTAL_ADD_ON_ORDER" = SOURCE_TABLE."TOTAL_ADD_ON_ORDER",
                               RT."TOTAL_MISSING_COLLECTIONS" = SOURCE_TABLE."TOTAL_MISSING_COLLECTIONS",
                               RT."COLLECT_RECEIVE_AVG" = SOURCE_TABLE."COLLECT_RECEIVE_AVG",
                               RT."COLLECT_RECEIVE_MEDIAN" = SOURCE_TABLE."COLLECT_RECEIVE_MEDIAN",
                               RT."COLLECT_RECEIVE_95" = SOURCE_TABLE."COLLECT_RECEIVE_95",
                               RT."RECEIVE_RESULT_AVG" = SOURCE_TABLE."RECEIVE_RESULT_AVG",
                               RT."RECEIVE_RESULT_MEDIAN" = SOURCE_TABLE."RECEIVE_RESULT_MEDIAN",
                               RT."RECEIVE_RESULT_95" = SOURCE_TABLE."RECEIVE_RESULT_95",
                               RT."COLLECT_RESULT_AVG" = SOURCE_TABLE."COLLECT_RESULT_AVG",
                               RT."COLLECT_RESULT_MEDIAN" = SOURCE_TABLE."COLLECT_RESULT_MEDIAN",
                               RT."COLLECT_RESULT_95" = SOURCE_TABLE."COLLECT_RESULT_95"
                    WHEN NOT MATCHED THEN
                    INSERT(RT."SITE",
                           RT."WEEK_START",
                           RT."WEEK_END",
                           RT."TEST",
                           RT."DIVISION",
                           RT."DETAILED_SETTING",
                           RT."DASHBOARD_SETTING",
                           RT."DASHBOARD_PRIORITY",
                           RT."RECEIVE_RESULT_TARGET",
                           RT."COLLECT_RESULT_TARGET",
                           RT."TOTAL_RESULTED",
                           RT."RECEIVE_TIME_VOL_INCL",
                           RT."COLLECT_TIME_VOL_INCL",
                           RT."TOTAL_RECEIVE_RESULT_IN_TARGET",
                           RT."TOTAL_COLLECT_RESULT_IN_TARGET",
                           RT."TOTAL_ADD_ON_ORDER",
                           RT."TOTAL_MISSING_COLLECTIONS",
                           RT."COLLECT_RECEIVE_AVG",
                           RT."COLLECT_RECEIVE_MEDIAN",
                           RT."COLLECT_RECEIVE_95",
                           RT."RECEIVE_RESULT_AVG",
                           RT."RECEIVE_RESULT_MEDIAN",
                           RT."RECEIVE_RESULT_95",
                           RT."COLLECT_RESULT_AVG",
                           RT."COLLECT_RESULT_MEDIAN",
                           RT."COLLECT_RESULT_95"
                    )
                    VALUES(SOURCE_TABLE."SITE",
                           SOURCE_TABLE."WEEK_START",
                           SOURCE_TABLE."WEEK_END",
                           SOURCE_TABLE."TEST",
                           SOURCE_TABLE."DIVISION",
                           SOURCE_TABLE."DETAILED_SETTING",
                           SOURCE_TABLE."DASHBOARD_SETTING",
                           SOURCE_TABLE."DASHBOARD_PRIORITY",
                           SOURCE_TABLE."RECEIVE_RESULT_TARGET",
                           SOURCE_TABLE."COLLECT_RESULT_TARGET",
                           SOURCE_TABLE."TOTAL_RESULTED",
                           SOURCE_TABLE."RECEIVE_TIME_VOL_INCL",
                           SOURCE_TABLE."COLLECT_TIME_VOL_INCL",
                           SOURCE_TABLE."TOTAL_RECEIVE_RESULT_IN_TARGET",
                           SOURCE_TABLE."TOTAL_COLLECT_RESULT_IN_TARGET",
                           SOURCE_TABLE."TOTAL_ADD_ON_ORDER",
                           SOURCE_TABLE."TOTAL_MISSING_COLLECTIONS",
                           SOURCE_TABLE."COLLECT_RECEIVE_AVG",
                           SOURCE_TABLE."COLLECT_RECEIVE_MEDIAN",
                           SOURCE_TABLE."COLLECT_RECEIVE_95",
                           SOURCE_TABLE."RECEIVE_RESULT_AVG",
                           SOURCE_TABLE."RECEIVE_RESULT_MEDIAN",
                           SOURCE_TABLE."RECEIVE_RESULT_95",
                           SOURCE_TABLE."COLLECT_RESULT_AVG",
                           SOURCE_TABLE."COLLECT_RESULT_MEDIAN",
                           SOURCE_TABLE."COLLECT_RESULT_95"
                    );')

# Glue query for truncating temporary table
truncate_query <- glue('TRUNCATE TABLE "{weekly_summary_temp_table}";')

chunk_length <- 200

split_insert_queries <- split(inserts,
                              ceiling(seq_along(inserts) / chunk_length))

split_queries_final <- list()

for (i in 1:length(split_insert_queries)) {
  row <- glue_collapse(split_insert_queries[[i]], sep = "\n\n")
  # row <- gsub("\'", "''", row)
  # row <- gsub("&", " ' || chr(38) || ' ", row)
  sql <- glue('INSERT ALL {row} SELECT 1 FROM DUAL;')
  split_queries_final <- append(split_queries_final, sql)
}

write_to_temp_table <- function(x) {
  oao_personal_conn <- dbConnect(odbc(), "OAO Cloud DB Kate")
  dbBegin(oao_personal_conn)
  dbExecute(oao_personal_conn, x)
  dbCommit(oao_personal_conn)
  # dbDisconnect(oao_personal_conn)

}

print("Before OAO Cloud DB Connection")

oao_personal_dsn <- "OAO Cloud DB Kate"

oao_personal_conn <- dbConnect(odbc(),
                               oao_personal_dsn)

dbBegin(oao_personal_conn)
# ## Execute staments and if there is an error  with one of them rollback changes
tryCatch({
  print("Before first truncate")
  dbExecute(oao_personal_conn, truncate_query)
  print("After first truncate")

  system.time(
    mclapply(split_queries_final, write_to_temp_table)
  )

  # dbExecute(oao_personal_conn, all_data)

  print("After all data glue statement")
  dbExecute(oao_personal_conn, merge_query)
  print("After merge")
  dbExecute(oao_personal_conn, truncate_query)
  print("After second truncate")
  dbCommit(oao_personal_conn)
  dbDisconnect(oao_personal_conn)
  print("Success!")
},
error = function(err){
  #print(err)
  dbRollback(oao_personal_conn)
  dbDisconnect(oao_personal_conn)
  dbExecute(oao_personal_conn, truncate_query)
  dbCommit(oao_personal_conn)
  print("Error")
}
)
