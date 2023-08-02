# Script to begin creating Clinical Pathology database tables

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

# 60 Days Raw Data ---------------------------------
raw_data_60days <- readRDS(
  paste0(user_directory,
         "/CP Repositories/RawDataRepo",
         "/Labs Resulted 05-25-23 to 07-23-23 as of 07-24-23.RDS")
)

raw_data_60days_db <- raw_data_60days %>%
  rename(DetailedSetting = MasterSetting) %>%
  select(-Year, -MonthNo, -MonthName, -MonthRollUp,
         -WeekStart, -WeekEnd, -WeekOf,
         -CompleteWeek, -CompleteMonth) %>%
  rename(LOC_CODE = LocCode,                
         LOC_NAME = LocName,                
         ORDER_ID = OrderID,               
         REQUEST_MD = RequestMD,              
         MSMRN = MSMRN,                  
         WORK_SHIFT = WorkShift,             
         TEST_NAME = TestName,               
         TEST = Test,                   
         DIVISION = Division,              
         ORDER_PRIORITY = OrderPriority,          
         SITE = Site,                   
         ICU = ICU,                   
         LOC_TYPE = LocType,                
         SETTING = Setting,                
         SETTING_ROLL_UP = SettingRollUp,         
         DETAILED_SETTING = DetailedSetting,        
         DASHBOARD_SETTING = DashboardSetting,       
         ADJ_PRIORITY = AdjPriority,           
         DASHBOARD_PRIORITY = DashboardPriority,      
         ORDER_TIME = OrderTime,              
         COLLECT_TIME = CollectTime,           
         RECEIVE_TIME = ReceiveTime,            
         RESULT_TIME = ResultTime,             
         RESULT_DATE = ResultDate,            
         COLLECT_TO_RECEIVE_TAT = CollectToReceiveTAT,    
         RECEIVE_TO_RESULT_TAT = ReceiveToResultTAT,     
         COLLECT_TO_RESULT_TAT = CollectToResultTAT,    
         ADD_ON_FINAL = AddOnMaster,             
         MISSING_COLLECT = MissingCollect,         
         RECEIVE_RESULT_TARGET = ReceiveResultTarget,   
         COLLECT_RESULT_TARGET = CollectResultTarget,    
         RECEIVE_RESULT_IN_TARGET = ReceiveResultInTarget,  
         COLLECT_RESULT_IN_TARGET = CollectResultInTarget,
         RECEIVE_TIME_TAT_INCL = ReceiveTime_TATInclude, 
         COLLECT_TIME_TAT_INCL = CollectTime_TATInclude
         ) %>%
  mutate(across(where(is.logical), as.numeric))

test_data <- raw_data_60days_db[1:5, ]



get_values_raw_cp_data <- function(x, table_name){
  
  loc_code <- x[1]
  loc_name <- x[2]
  order_id <- x[3]
  request_md <- x[4]
  msmrn <- x[5]
  work_shift <- x[6]
  test_name <- x[7]
  test <- x[8]
  division <- x[9]
  order_priority <- x[10]
  site <- x[11]
  icu <- x[12]
  loc_type <- x[13]
  setting <- x[14]
  setting_roll_up <- x[15]
  detailed_setting <- x[16]
  dashboard_setting <- x[17]
  adj_priority <- x[18]
  dashboard_priority <- x[19]
  order_time <- x[20]
  collect_time <- x[21]
  receive_time <- x[22]
  result_time <- x[23]
  result_date <- x[24]
  collect_to_receive_tat <- x[25]
  receive_to_result_tat <- x[26]
  collect_to_result_tat <- x[27]
  add_on_final <- x[28]
  missing_collect <- x[29]
  receive_result_target <- x[30]
  collect_result_target <- x[31]
  receive_result_in_target <- x[32]
  collect_result_in_target <- x[33]
  receive_time_tat_incl <- x[34]
  collect_time_tat_incl <- x[35]
  
  
  values <- glue(
    "INTO \"{table_name}\" 
    (LOC_CODE,
    LOC_NAME,    
    ORDER_ID,
    REQUEST_MD,
    MSMRN,
    WORK_SHIFT,
    TEST_NAME,
    TEST,
    DIVISION,
    ORDER_PRIORITY,
    SITE,
    ICU,
    LOC_TYPE,
    SETTING,
    SETTING_ROLL_UP,
    DETAILED_SETTING,
    DASHBOARD_SETTING,
    ADJ_PRIORITY,
    DASHBOARD_PRIORITY,
    ORDER_TIME,
    COLLECT_TIME,
    RECEIVE_TIME,
    RESULT_TIME,
    RESULT_DATE,
    COLLECT_TO_RECEIVE_TAT,
    RECEIVE_TO_RESULT_TAT,
    COLLECT_TO_RESULT_TAT,
    ADD_ON_FINAL,
    MISSING_COLLECT,
    RECEIVE_RESULT_TARGET,
    COLLECT_RESULT_TARGET,
    RECEIVE_RESULT_IN_TARGET,
    COLLECT_RESULT_IN_TARGET,
    RECEIVE_TIME_TAT_INCL,
    COLLECT_TIME_TAT_INCL) 
    
    VALUES (
    '{loc_code}',
    '{loc_name}',         
    '{order_id}',
    '{request_md}',
    '{msmrn}',
    '{work_shift}',
    '{test_name}',
    '{test}',
    '{division}',
    '{order_priority}',
    '{site}',
    '{icu}',
    '{loc_type}',
    '{setting}',
    '{setting_roll_up}',
    '{detailed_setting}',
    '{dashboard_setting}',
    '{adj_priority}',
    '{dashboard_priority}',
    TO_TIMESTAMP('{order_time}', 'YYYY-MM-DD HH24:MI:SS'),
    TO_TIMESTAMP('{collect_time}', 'YYYY-MM-DD HH24:MI:SS'),
    TO_TIMESTAMP('{receive_time}', 'YYYY-MM-DD HH24:MI:SS'),
    TO_TIMESTAMP('{result_time}', 'YYYY-MM-DD HH24:MI:SS'),
    TO_DATE('{result_date}', 'YYYY-MM-DD'),
    '{collect_to_receive_tat}',
    '{receive_to_result_tat}',
    '{collect_to_result_tat}',
    '{add_on_final}',
    '{missing_collect}',
    '{receive_result_target}',
    '{collect_result_target}',
    '{receive_result_in_target}',
    '{collect_result_in_target}',
    '{receive_time_tat_incl}',
    '{collect_time_tat_incl}')"
  )
  
  return(values)
}

write_temp_raw_data_to_db_and_merge <- function(processed_data, temp_table, repo_table) {
  if(nrow(processed_data) == 0) {
    print("No new data")
  } else {
    
    # temp_table_name <- "CP_RAW_DATA_60DAYS_TEMP"
    
    # Ensure all columns are correct data types
    processed_data <- processed_data %>%
      mutate(LOC_CODE = as.character(LOC_CODE),
             LOC_NAME = as.character(LOC_NAME),    
             ORDER_ID = as.character(ORDER_ID),
             REQUEST_MD = as.character(REQUEST_MD),
             MSMRN = as.character(MSMRN),
             WORK_SHIFT = as.character(WORK_SHIFT),
             TEST_NAME = as.character(TEST_NAME),
             TEST = as.character(TEST),
             DIVISION = as.character(DIVISION),
             ORDER_PRIORITY = as.character(ORDER_PRIORITY),
             SITE = as.character(SITE),
             ICU = as.numeric(ICU),
             LOC_TYPE = as.character(LOC_TYPE),
             SETTING = as.character(SETTING),
             SETTING_ROLL_UP = as.character(SETTING_ROLL_UP),
             DETAILED_SETTING = as.character(DETAILED_SETTING),
             DASHBOARD_SETTING = as.character(DASHBOARD_SETTING),
             ADJ_PRIORITY = as.character(ADJ_PRIORITY),
             DASHBOARD_PRIORITY = as.character(DASHBOARD_PRIORITY),
             ORDER_TIME = format(ORDER_TIME, "%Y-%m-%d %H:%M:%S"),
             COLLECT_TIME = format(COLLECT_TIME, "%Y-%m-%d %H:%M:%S"),
             RECEIVE_TIME = format(RECEIVE_TIME, "%Y-%m-%d %H:%M:%S"),
             RESULT_TIME = format(RESULT_TIME, "%Y-%m-%d %H:%M:%S"),
             RESULT_DATE = format(RESULT_DATE, "%Y-%m-%d"),
             COLLECT_TO_RECEIVE_TAT = as.numeric(COLLECT_TO_RECEIVE_TAT),
             RECEIVE_TO_RESULT_TAT = as.numeric(RECEIVE_TO_RESULT_TAT),
             COLLECT_TO_RESULT_TAT = as.numeric(COLLECT_TO_RESULT_TAT),
             ADD_ON_FINAL = as.character(ADD_ON_FINAL),
             MISSING_COLLECT = as.numeric(MISSING_COLLECT),
             RECEIVE_RESULT_TARGET = as.numeric(RECEIVE_RESULT_TARGET),
             COLLECT_RESULT_TARGET = as.numeric(COLLECT_RESULT_TARGET),
             RECEIVE_RESULT_IN_TARGET = as.numeric(RECEIVE_RESULT_IN_TARGET),
             COLLECT_RESULT_IN_TARGET = as.numeric(COLLECT_RESULT_IN_TARGET),
             RECEIVE_TIME_TAT_INCL = as.numeric(RECEIVE_TIME_TAT_INCL),
             COLLECT_TIME_TAT_INCL = as.numeric(COLLECT_TIME_TAT_INCL)
      )
    
    # Convert the each row of tibble to INTO clause of insert statement on temporary table
    inserts <- lapply(
      lapply(
        lapply(split(processed_data , 
                     1:nrow(processed_data)),
               as.list), 
        as.character),
      FUN = get_values_raw_cp_data, temp_table)
    
    values <- glue_collapse(inserts, sep = "\n\n")
    
    # Combine into statements from get_values() function and combine with
    # insert statements
    all_data <- glue('INSERT ALL
                        {values}
                      SELECT 1 from DUAL;')
    
    # glue() query to merge data from temporary table to repository table
    query <- glue('MERGE INTO "{repo_table}" RT
                    USING "{temp_table}" SOURCE_TABLE
                    ON (  RT."ORDER_ID" = SOURCE_TABLE."ORDER_ID" AND
                          RT."MSMRN" = SOURCE_TABLE."MSMRN" AND
                          RT."TEST_NAME" = SOURCE_TABLE."TEST_NAME" AND
                          RT."RESULT_TIME" = SOURCE_TABLE."RESULT_TIME")
                    WHEN MATCHED THEN
                    UPDATE SET RT."LOC_CODE" = SOURCE_TABLE."LOC_CODE",
                               RT."LOC_NAME" = SOURCE_TABLE."LOC_NAME",
                               RT."REQUEST_MD" = SOURCE_TABLE."REQUEST_MD",
                               RT."WORK_SHIFT" = SOURCE_TABLE."WORK_SHIFT",
                               RT."TEST" = SOURCE_TABLE."TEST",
                               RT."DIVISION" = SOURCE_TABLE."DIVISION",
                               RT."ORDER_PRIORITY" = SOURCE_TABLE."ORDER_PRIORITY",
                               RT."SITE" = SOURCE_TABLE."SITE",
                               RT."ICU" = SOURCE_TABLE."ICU",
                               RT."LOC_TYPE" = SOURCE_TABLE."LOC_TYPE",
                               RT."SETTING" = SOURCE_TABLE."SETTING",
                               RT."SETTING_ROLL_UP" = SOURCE_TABLE."SETTING_ROLL_UP",
                               RT."DETAILED_SETTING" = SOURCE_TABLE."DETAILED_SETTING",
                               RT."DASHBOARD_SETTING" = SOURCE_TABLE."DASHBOARD_SETTING",
                               RT."ADJ_PRIORITY" = SOURCE_TABLE."ADJ_PRIORITY",
                               RT."DASHBOARD_PRIORITY" = SOURCE_TABLE."DASHBOARD_PRIORITY",
                               RT."ORDER_TIME" = SOURCE_TABLE."ORDER_TIME",
                               RT."COLLECT_TIME" = SOURCE_TABLE."COLLECT_TIME",
                               RT."RECEIVE_TIME" = SOURCE_TABLE."RECEIVE_TIME",
                               RT."RESULT_DATE" = SOURCE_TABLE."RESULT_DATE",
                               RT."COLLECT_TO_RECEIVE_TAT" = SOURCE_TABLE."COLLECT_TO_RECEIVE_TAT",
                               RT."RECEIVE_TO_RESULT_TAT" = SOURCE_TABLE."RECEIVE_TO_RESULT_TAT",
                               RT."COLLECT_TO_RESULT_TAT" = SOURCE_TABLE."COLLECT_TO_RESULT_TAT",
                               RT."ADD_ON_FINAL" = SOURCE_TABLE."ADD_ON_FINAL",
                               RT."MISSING_COLLECT" = SOURCE_TABLE."MISSING_COLLECT",
                               RT."RECEIVE_RESULT_TARGET" = SOURCE_TABLE."RECEIVE_RESULT_TARGET",
                               RT."COLLECT_RESULT_TARGET" = SOURCE_TABLE."COLLECT_RESULT_TARGET",
                               RT."RECEIVE_RESULT_IN_TARGET" = SOURCE_TABLE."RECEIVE_RESULT_IN_TARGET",
                               RT."COLLECT_RESULT_IN_TARGET" = SOURCE_TABLE."COLLECT_RESULT_IN_TARGET",
                               RT."RECEIVE_TIME_TAT_INCL" = SOURCE_TABLE."RECEIVE_TIME_TAT_INCL",
                               RT."COLLECT_TIME_TAT_INCL" = SOURCE_TABLE."COLLECT_TIME_TAT_INCL"
                    WHEN NOT MATCHED THEN
                    INSERT(RT."LOC_CODE",
                           RT."LOC_NAME",
                           RT."ORDER_ID",
                           RT."REQUEST_MD",
                           RT."MSMRN",
                           RT."WORK_SHIFT",
                           RT."TEST_NAME",
                           RT."TEST",
                           RT."DIVISION",
                           RT."ORDER_PRIORITY",
                           RT."SITE",
                           RT."ICU",
                           RT."LOC_TYPE",
                           RT."SETTING",
                           RT."SETTING_ROLL_UP",
                           RT."DETAILED_SETTING",
                           RT."DASHBOARD_SETTING",
                           RT."ADJ_PRIORITY",
                           RT."DASHBOARD_PRIORITY",
                           RT."ORDER_TIME",
                           RT."COLLECT_TIME",
                           RT."RECEIVE_TIME",
                           RT."RESULT_TIME",
                           RT."RESULT_DATE",
                           RT."COLLECT_TO_RECEIVE_TAT",
                           RT."RECEIVE_TO_RESULT_TAT",
                           RT."COLLECT_TO_RESULT_TAT",
                           RT."ADD_ON_FINAL",
                           RT."MISSING_COLLECT",
                           RT."RECEIVE_RESULT_TARGET",
                           RT."COLLECT_RESULT_TARGET",
                           RT."RECEIVE_RESULT_IN_TARGET",
                           RT."COLLECT_RESULT_IN_TARGET",
                           RT."RECEIVE_TIME_TAT_INCL"
                    )
                    VALUES(SOURCE_TABLE."LOC_CODE",
                           SOURCE_TABLE."LOC_NAME",
                           SOURCE_TABLE."ORDER_ID",
                           SOURCE_TABLE."REQUEST_MD",
                           SOURCE_TABLE."MSMRN",
                           SOURCE_TABLE."WORK_SHIFT",
                           SOURCE_TABLE."TEST_NAME",
                           SOURCE_TABLE."TEST",
                           SOURCE_TABLE."DIVISION",
                           SOURCE_TABLE."ORDER_PRIORITY",
                           SOURCE_TABLE."SITE",
                           SOURCE_TABLE."ICU",
                           SOURCE_TABLE."LOC_TYPE",
                           SOURCE_TABLE."SETTING",
                           SOURCE_TABLE."SETTING_ROLL_UP",
                           SOURCE_TABLE."DETAILED_SETTING",
                           SOURCE_TABLE."DASHBOARD_SETTING",
                           SOURCE_TABLE."ADJ_PRIORITY",
                           SOURCE_TABLE."DASHBOARD_PRIORITY",
                           SOURCE_TABLE."ORDER_TIME",
                           SOURCE_TABLE."COLLECT_TIME",
                           SOURCE_TABLE."RECEIVE_TIME",
                           SOURCE_TABLE."RESULT_TIME",
                           SOURCE_TABLE."RESULT_DATE",
                           SOURCE_TABLE."COLLECT_TO_RECEIVE_TAT",
                           SOURCE_TABLE."RECEIVE_TO_RESULT_TAT",
                           SOURCE_TABLE."COLLECT_TO_RESULT_TAT",
                           SOURCE_TABLE."ADD_ON_FINAL",
                           SOURCE_TABLE."MISSING_COLLECT",
                           SOURCE_TABLE."RECEIVE_RESULT_TARGET",
                           SOURCE_TABLE."COLLECT_RESULT_TARGET",
                           SOURCE_TABLE."RECEIVE_RESULT_IN_TARGET",
                           SOURCE_TABLE."COLLECT_RESULT_IN_TARGET",
                           SOURCE_TABLE."RECEIVE_TIME_TAT_INCL",
                           SOURCE_TABLE."COLLECT_TIME_TAT_INCL"
                    );')
    # query = glue('MERGE INTO "{repo_table}" RT
    #                 USING "{temp_table}" SOURCE_TABLE
    #                 ON (  RT."ORDER_ID" = SOURCE_TABLE."ORDER_ID" AND
    #                       RT."MSMRN" = SOURCE_TABLE."MSMRN" AND
    #                       RT."TEST_NAME" = SOURCE_TABLE."TEST_NAME" AND
    #                       RT."RESULT_TIME" = SOURCE_TABLE."RESULT_TIME")
    #                 WHEN MATCHED THEN 
    #                 UPDATE SET RT."LOC_CODE" = SOURCE_TABLE."LOC_CODE",
    #                            RT."LOC_NAME" = SOURCE_TABLE."LOC_NAME",    
    #                            RT."REQUEST_MD" = SOURCE_TABLE."REQUEST_MD",
    #                            RT."WORK_SHIFT" = SOURCE_TABLE."WORK_SHIFT",
    #                            RT."TEST" = SOURCE_TABLE."TEST",
    #                            RT."DIVISION" = SOURCE_TABLE."DIVISION",
    #                            RT."ORDER_PRIORITY" = SOURCE_TABLE."ORDER_PRIORITY",
    #                            RT."SITE" = SOURCE_TABLE."SITE",
    #                            RT."ICU" = SOURCE_TABLE."ICU",
    #                            RT."LOC_TYPE" = SOURCE_TABLE."LOC_TYPE",
    #                            RT."SETTING" = SOURCE_TABLE."SETTING",
    #                            RT."SETTING_ROLL_UP" = SOURCE_TABLE."SETTING_ROLL_UP",
    #                            RT."DETAILED_SETTING" = SOURCE_TABLE."DETAILED_SETTING",
    #                            RT."DASHBOARD_SETTING" = SOURCE_TABLE."DASHBOARD_SETTING",
    #                            RT."ADJ_PRIORITY" = SOURCE_TABLE."ADJ_PRIORITY",
    #                            RT."DASHBOARD_PRIORITY" = SOURCE_TABLE."DASHBOARD_PRIORITY",
    #                            RT."ORDER_TIME" = SOURCE_TABLE."ORDER_TIME",
    #                            RT."COLLECT_TIME" = SOURCE_TABLE."COLLECT_TIME",
    #                            RT."RECEIVE_TIME" = SOURCE_TABLE."RECEIVE_TIME" 
    #                 WHEN NOT MATCHED THEN
    #                 INSERT(RT."LOC_CODE",
    #                        RT."LOC_NAME",
    #                        RT."ORDER_ID",
    #                        RT."REQUEST_MD",
    #                        RT."MSMRN",
    #                        RT."WORK_SHIFT",
    #                        RT."TEST_NAME",
    #                        RT."TEST",
    #                        RT."DIVISION",
    #                        RT."ORDER_PRIORITY",
    #                        RT."SITE",
    #                        RT."ICU",
    #                        RT."LOC_TYPE",
    #                        RT."SETTING",
    #                        RT."SETTING_ROLL_UP",
    #                        RT."DETAILED_SETTING",
    #                        RT."DASHBOARD_SETTING",
    #                        RT."ADJ_PRIORITY",
    #                        RT."DASHBOARD_PRIORITY",
    #                        RT."ORDER_TIME",
    #                        # RT."COLLECT_TIME",
    #                        # RT."RECEIVE_TIME",
    #                        # RT."RESULT_TIME"
    #                 )
    #                 VALUES(SOURCE_TABLE."LOC_CODE",
    #                        SOURCE_TABLE."LOC_NAME",    
    #                        SOURCE_TABLE."ORDER_ID",
    #                        SOURCE_TABLE."REQUEST_MD",
    #                        SOURCE_TABLE."MSMRN",
    #                        SOURCE_TABLE."WORK_SHIFT",
    #                        SOURCE_TABLE."TEST_NAME",
    #                        SOURCE_TABLE."TEST",
    #                        SOURCE_TABLE."DIVISION",
    #                        SOURCE_TABLE."ORDER_PRIORITY",
    #                        SOURCE_TABLE."SITE",
    #                        SOURCE_TABLE."ICU",
    #                        SOURCE_TABLE."LOC_TYPE",
    #                        SOURCE_TABLE."SETTING",
    #                        SOURCE_TABLE."SETTING_ROLL_UP",
    #                        SOURCE_TABLE."DETAILED_SETTING",
    #                        SOURCE_TABLE."DASHBOARD_SETTING",
    #                        SOURCE_TABLE."ADJ_PRIORITY",
    #                        SOURCE_TABLE."DASHBOARD_PRIORITY",
    #                        SOURCE_TABLE."ORDER_TIME",
    #                        SOURCE_TABLE."COLLECT_TIME",
    #                        SOURCE_TABLE."RECEIVE_TIME",
    #                        SOURCE_TABLE."RESULT_TIME"
    #                 );')
    
    # glue query for dropping the table
    truncate_query <- glue('TRUNCATE TABLE "{temp_table}";')
    
    print("Before OAO Cloud DB Connection")

    oao_personal_dsn <- "OAO Cloud DB Kate"
    
    oao_personal_conn <- dbConnect(odbc(),
                                   oao_personal_dsn)
    
    dbBegin(oao_personal_conn)
    # ## Execute statements and if there is an error  with one of them rollback changes
    tryCatch({
      print("Before first truncate")
      dbExecute(oao_personal_conn, truncate_query)
      print("After first truncate")
      dbExecute(oao_personal_conn, all_data)
      print("After all data glue statement")
      dbExecute(oao_personal_conn, query)
      print("After merge")
      dbExecute(oao_personal_conn, truncate_query)
      print("After second truncate")
      dbCommit(oao_personal_conn)
      dbDisconnect(oao_personal_conn)
      print("Success!")
    },
    error = function(err){
      dbRollback(oao_personal_conn)
      dbExecute(oao_personal_conn, truncate_query)
      dbDisconnect(oao_personal_conn)
      print("Error!")
    })
  }
}

write_temp_raw_data_to_db_and_merge(processed_data = test_data,
                                    temp_table = "CP_RAW_DATA_60DAYS_TEMP",
                                    repo_table = "CP_RAW_DATA_60DAYS")

# Daily Summary Repo ----------------------

daily_summary_repo <- readRDS(
  paste0(user_directory,
         "/CP Repositories/DailyRepo",
         "/Daily Repo 12-01-20 to 07-23-23 as of 07-24-23.RDS")
)

daily_repo_db <- daily_summary_repo %>%
  select(-WeekStart, -WeekEnd, -WeekOf,
         -MonthNo, -MonthName, -Year) %>%
  rename(SITE = Site,
         RESULT_DATE = ResultDate,
         TEST = Test,
         DIVISION = Division,
         SETTING_ROLL_UP = SettingRollUp,
         DETAILED_SETTING = MasterSetting,
         DASHBOARD_SETTING = DashboardSetting,
         ADJ_PRIORITY = AdjPriority,
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

get_values_daily_summary <- function(x, table_name){
  
  site <- x[1]
  result_date <- x[2]
  test <- x[3]
  division <- x[4]
  setting_roll_up <- x[5]
  detailed_setting <- x[6]
  dashboard_setting <- x[7]
  adj_priority <- x[8]
  dashboard_priority <- x[9]
  receive_result_target <- x[10]
  collect_result_target <- x[11]
  total_resulted <- x[12]
  receive_time_vol_incl <- x[13]
  collect_time_vol_incl <- x[14]
  total_receive_result_in_target <- x[15]
  total_collect_result_in_target <- x[16]
  total_add_on <- x[17]
  total_missing_collections <- x[18]
  collect_receive_avg <- x[19]
  collect_receive_median <- x[20]
  collect_receive_95 <- x[21]
  receive_result_avg <- x[22]
  receive_result_median <- x[23]
  receive_result_95 <- x[24]
  collect_result_avg <- x[25]
  collect_result_median <- x[26]
  collect_result_95 <- x[27]
  
  
  values <- glue(
    "INTO \"{table_name}\" 
    (SITE,
    RESULT_DATE,
    TEST,
    DIVISION,
    SETTING_ROLL_UP,
    DETAILED_SETTING,
    DASHBOARD_SETTING,
    ADJ_PRIORITY,
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
    TO_DATE('{result_date}', 'YYYY-MM-DD'),
    '{test}',  
    '{division}',
    '{setting_roll_up}',
    '{detailed_setting}',
    '{dashboard_setting}',
    '{adj_priority}',
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

TABLE_NAME <- "CP_DAILY_REPO"

processed_input_data <- daily_repo_db

# Ensure all the fields are correct data type
processed_input_data <- processed_input_data %>%
  mutate(SITE = as.character(SITE),
         RESULT_DATE = format(RESULT_DATE, "%Y-%m-%d"),
         TEST = as.character(TEST),
         DIVISION = as.character(DIVISION),
         SETTING_ROLL_UP = as.character(SETTING_ROLL_UP),
         DETAILED_SETTING = as.character(DETAILED_SETTING),
         DASHBOARD_SETTING = as.character(DASHBOARD_SETTING),
         ADJ_PRIORITY = as.character(ADJ_PRIORITY),
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
         COLLECT_RECEIVE_AVG = as.numeric(COLLECT_RECEIVE_AVG),
         COLLECT_RECEIVE_MEDIAN = as.numeric(COLLECT_RECEIVE_MEDIAN),
         COLLECT_RECEIVE_95 = as.numeric(COLLECT_RECEIVE_95),
         RECEIVE_RESULT_AVG = as.numeric(RECEIVE_RESULT_AVG),
         RECEIVE_RESULT_MEDIAN = as.numeric(RECEIVE_RESULT_MEDIAN),
         RECEIVE_RESULT_95 = as.numeric(RECEIVE_RESULT_95),
         COLLECT_RESULT_AVG = as.numeric(COLLECT_RESULT_AVG),
         COLLECT_RESULT_MEDIAN = as.numeric(COLLECT_RESULT_MEDIAN),
         COLLECT_RESULT_95 = as.numeric(COLLECT_RESULT_95)
         )
    
# Convert the each record/row of tibble to INTO clause of insert statement
inserts <- lapply(
  lapply(
    lapply(split(processed_input_data, 
                 1:nrow(processed_input_data)),
           as.list), 
    as.character),
  FUN = get_values ,TABLE_NAME)

values <- glue_collapse(inserts,sep = "\n\n")
    
# Combine into statements from get_values() function and combine with insert statements
all_data <- glue('INSERT ALL 
                  {values}
                 SELECT 1 from DUAL;')
    
# # glue() query to merge data from temporary table to summary_repo table
# query = glue('MERGE INTO MSHS_CENSUS_REPO CR
#                     USING "{TABLE_NAME}" SOURCE_TABLE
#                     ON (  CR."SITE" = SOURCE_TABLE."SITE" AND
#                           CR."DEPARTMENT" = SOURCE_TABLE."DEPARTMENT" AND
#                           CR."REFRESH_TIME" = SOURCE_TABLE."REFRESH_TIME")
#                     WHEN MATCHED THEN 
#                     UPDATE  SET CR."CENSUS" = SOURCE_TABLE."CENSUS"
#                     WHEN NOT MATCHED THEN
#                     INSERT( CR."SITE",
#                             CR."DEPARTMENT",
#                             CR."CENSUS",
#                             CR."REFRESH_TIME"
#                             )  
#                     VALUES( SOURCE_TABLE."SITE",
#                             SOURCE_TABLE."DEPARTMENT",
#                             SOURCE_TABLE."CENSUS",
#                             SOURCE_TABLE."REFRESH_TIME");')
#     
#     # glue query for dropping the table
#     truncate_query <- glue('TRUNCATE TABLE "{TABLE_NAME}";')
#     
#     print("Before OAO Cloud DB Connection")
#     # conn <- dbConnect(drv = odbc::odbc(),  ## Create connection for updating picker choices
#     #                   dsn = dsn)
    
oao_personal_dsn <- "OAO Cloud DB Kate"
    
oao_personal_conn <- dbConnect(odbc(),
                                   oao_personal_dsn)
    
dbBegin(oao_personal_conn)
# ## Execute staments and if there is an error  with one of them rollback changes
tryCatch({
  # print("Before first truncate")
  # dbExecute(oao_personal_conn, truncate_query)
  print("After first truncate")
  dbExecute(oao_personal_conn, all_data)
  print("After all data glue statement")
  # dbExecute(oao_personal_conn, query)
  # print("After merge")
  # dbExecute(oao_personal_conn, truncate_query)
  # print("After second truncate")
  dbCommit(oao_personal_conn)
  dbDisconnect(oao_personal_conn)
  print("Success!")
  },
  error = function(err){
    #print(err)
    dbRollback(oao_personal_conn)
    dbDisconnect(oao_personal_conn)
    dbExecute(oao_personal_conn, truncate_query)
    print("Error")
  }
)



id_primary_keys_1 <- daily_repo_db %>%
  select(SITE, RESULT_DATE, TEST, DETAILED_SETTING, ADJ_PRIORITY) %>%
  distinct()
