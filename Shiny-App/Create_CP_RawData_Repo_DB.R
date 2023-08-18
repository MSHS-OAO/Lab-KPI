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

# 60 Days Raw Data ---------------------------------
raw_data_60days <- readRDS(
  paste0(user_directory,
         "/CP Repositories/RawDataRepo",
         "/Labs Resulted 06-01-23 to 07-30-23 as of 07-31-23.RDS")
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
  mutate(across(where(is.logical), as.numeric)) %>%
  mutate(MSMRN = ifelse(is.na(MSMRN), "NoMRN", MSMRN),
         ORDER_ID = ifelse(is.na(ORDER_ID), "NoOrderID", ORDER_ID),
         RECEIVE_RESULT_IN_TARGET = ifelse(is.na(RECEIVE_RESULT_IN_TARGET), FALSE,
                                           RECEIVE_RESULT_IN_TARGET),  
         COLLECT_RESULT_IN_TARGET = ifelse(is.na(COLLECT_RESULT_IN_TARGET), FALSE,
                                           COLLECT_RESULT_IN_TARGET),
         MISSING_COLLECT = ifelse(is.na(MISSING_COLLECT), FALSE, MISSING_COLLECT)) #%>%
  # filter(!is.na(RECEIVE_TIME))

# test_data <- raw_data_60days_db[1:100000, ]
# test_data <- raw_data_60days_db

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


# processed_data <- test_data
processed_data <- raw_data_60days_db

# processed_data <- rbind(error_row, typical_row)

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
  ) %>%
  mutate(across(everything(), 
         function(x) {gsub("\'", "''", x)})) %>%
  mutate(across(where(is.character), replace_na, replace = '')) %>%
  mutate(across(everything(), gsub, pattern = "&",
                replacement = "'||chr(38)||'"))

processed_data <- processed_data %>%
  filter(RESULT_DATE >= Sys.Date() - 60)

processed_data <- processed_data[350001:nrow(processed_data), ]

# processed_data <- processed_data[61001:61200, ]

temp_table <- "CP_RAW_DATA_60DAYS_TEMP"
repo_table <- "CP_RAW_DATA_60DAYS"

# Convert the each row of tibble to INTO clause of insert statement on temporary table
# inserts <- lapply(
#   lapply(
#     lapply(split(processed_data , 
#                  1:nrow(processed_data)),
#            as.list), 
#     as.character),
#   FUN = get_values_raw_cp_data, temp_table) # 2.5 min
# 
# # Convert the each row of tibble to INTO clause of insert statement on temporary table
# inserts2 <- lapply(
#   lapply(split(processed_data , 
#                  1:nrow(processed_data)),
#            as.list),
#   as.character)

system.time(
  inserts <- mclapply(
  mclapply(
    mclapply(split(processed_data , 
                 1:nrow(processed_data)),
           as.list), 
    as.character),
  FUN = get_values_raw_cp_data, temp_table)
)

values <- glue_collapse(inserts,sep = "\n\n")

# Combine into statements from get_values() function and combine with insert statements
all_data <- glue('INSERT ALL 
                  {values}
                 SELECT 1 from DUAL;')

# glue query for dropping the table
truncate_query <- glue('TRUNCATE TABLE "{temp_table}";')

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
                           RT."RECEIVE_TIME_TAT_INCL",
                           RT."COLLECT_TIME_TAT_INCL"
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

# oao_personal_dsn <- "OAO Cloud DB Kate"
# 
# oao_personal_conn <- dbConnect(odbc(),
#                                oao_personal_dsn)
# 
# dbBegin(oao_personal_conn)
# # ## Execute staments and if there is an error  with one of them rollback changes
# tryCatch({
#   print("Before first truncate")
#   dbExecute(oao_personal_conn, truncate_query)
#   print("After first truncate")
#   dbExecute(oao_personal_conn, all_data)
#   print("After all data glue statement")
#   dbExecute(oao_personal_conn, query)
#   print("After merge")
#   dbExecute(oao_personal_conn, truncate_query)
#   print("After second truncate")
#   dbCommit(oao_personal_conn)
#   dbDisconnect(oao_personal_conn)
#   print("Success!")
# },
# error = function(err){
#   #print(err)
#   dbRollback(oao_personal_conn)
#   dbDisconnect(oao_personal_conn)
#   dbExecute(oao_personal_conn, truncate_query)
#   print("Error")
# }
# )


# Try parallel processing ----------------------
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




#DO NOT RUN BELOW IF TABLE DOES NOT EXIST
#Parallelize the inserting of split_queires_final to the database

# begin connection
# truncate temp table
# 
# then do the for each to populate the temporary w all the rows
# then end the for each
# then merge into the repo table
# then truncate



write_to_temp_table <- function(x) {
  oao_personal_conn <- dbConnect(odbc(), "OAO Cloud DB Kate")
  dbBegin(oao_personal_conn)
  dbExecute(oao_personal_conn, x)
  dbCommit(oao_personal_conn)
  # dbDisconnect(oao_personal_conn)
  
}

# mclapply(split_queries_final, test_function)

oao_personal_conn <- dbConnect(odbc(), "OAO Cloud DB Kate")
dbBegin(oao_personal_conn)

system.time(
  
  tryCatch({
    print("Before first truncate")
    dbExecute(oao_personal_conn, truncate_query)
    print("After first truncate")

    # mclapply(split_queries_final, write_to_temp_table)
    
    # 8.3 min
    registerDoParallel()

    foreach(i = 1:length(split_queries_final),
            .packages = c("DBI", "odbc"))%dopar%{
              oao_personal_conn <- dbConnect(odbc(), "OAO Cloud DB Kate")
              dbBegin(oao_personal_conn)
              dbExecute(oao_personal_conn, split_queries_final[[i]])
              dbCommit(oao_personal_conn)

            }

    registerDoSEQ()
    
    # dbCommit(oao_personal_conn)
    
    print("After all rows added to temporary table")
    dbExecute(oao_personal_conn, query)
    print("After merge into repo table")
    dbExecute(oao_personal_conn, truncate_query)
    print("After second truncate")
    dbCommit(oao_personal_conn)
    print("After repo table commit")
    dbDisconnect(oao_personal_conn)
    print("Success!")
  },
  error = function(err){
    print("error")
    dbRollback(oao_personal_conn)
    dbExecute(oao_personal_conn, truncate_query)
    dbDisconnect(oao_personal_conn)
    
  })
  
  
)

# # Test connection to database and check dates ----------
# oao_personal_conn <- dbConnect(odbc(), "OAO Cloud DB Kate")
# 
# raw_data_repo <- "CP_R_DATE_TEST"
# 
# date_filter_query <- glue('DELETE FROM {raw_data_repo}
#                           WHERE RESULT_DATE < (TO_DATE(SYSDATE) - 60);')
# 
# dbBegin(oao_personal_conn)
# dbExecute(oao_personal_conn, date_filter_query)
# dbCommit(oao_personal_conn)
# dbDisconnect(oao_personal_conn)





