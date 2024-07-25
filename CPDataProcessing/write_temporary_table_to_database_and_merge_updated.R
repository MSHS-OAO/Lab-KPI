get_values_updated <- function(x, columns,table_name){
  
  values <- glue("INTO \"{table_name}\" ({columns}) 
                 VALUES{x}")
  
  return(values)
}

write_temporary_table_to_database_and_merge_updated <- function(data, key_columns, destination_table_name, source_table_name, update_columns) {
  
  process_data <- data %>% mutate_if(is.factor, as.character) %>%
                  mutate_if(is.character, function(x) gsub("\'", "''", x)) %>%
                  mutate_if(is.character, function(x) gsub("&", "' || chr(38) || '", x)) %>%
                  mutate_if(is.character, function(x) paste0("'", x, "'")) %>%
                  mutate_if(is.Date, function(x) paste0("TO_DATE('", x, "', 'YYYY-MM-DD')")) %>%
                  mutate(ORDERTIME = paste0("TO_TIMESTAMP('", ORDERTIME, "', 'YYYY-MM-DD HH24:MI:SS')"),
                         COLLECTTIME = paste0("TO_TIMESTAMP('", COLLECTTIME, "', 'YYYY-MM-DD HH24:MI:SS')"),
                         RECEIVETIME = paste0("TO_TIMESTAMP('", RECEIVETIME, "', 'YYYY-MM-DD HH24:MI:SS')"),
                         RESULTTIME = paste0("TO_TIMESTAMP('", RESULTTIME, "', 'YYYY-MM-DD HH24:MI:SS')")) %>%
                  mutate(across(where(is.numeric),~ replace_na(as.character(.x), "''")))
  
  columns <- paste(colnames(process_data), collapse = ",")
  
  values <- process_data %>% mutate(values = paste0("(", col_concat(., sep = ","), ")")) %>%
    select(values)
  
  values$values <- gsub('NA', "", values$values)
  
  data_records <- split(values ,1:nrow(values))
  
  
  registerDoParallel()
  inserts <- foreach(record = data_records) %dopar% {
    tmp <- as.list(record)
    tmp <- as.character(tmp)
    tmp <- get_values_updated(tmp,columns = columns, table_name = source_table_name)
  }
  registerDoSEQ()
  
  chunk_length <- 500
  split_queries <- split(inserts, ceiling(seq_along(inserts)/chunk_length))
  
  
  split_queries_sql_statements <- list()
  for (i in 1:length(split_queries)) {
    row <- glue_collapse(split_queries[[i]], sep = "\n\n")
    # row <- gsub('NA', "", row)
    # row <- gsub("&", " ' || chr(38) || ' ", row)
    sql <- glue('INSERT ALL {row} SELECT 1 FROM DUAL;')
    split_queries_sql_statements <- append(split_queries_sql_statements, gsub("\\n", "", sql))
  }
  

  
  # glue statement for dropping table
  truncate_query <- glue('TRUNCATE TABLE "{source_table_name}";')
  
  #glue statement to copy empty table
  copy_table_query <- glue('CREATE TABLE {source_table_name}
                      AS
                      SELECT *
                      FROM {destination_table_name} WHERE 1=0;')
  
  #glue statement to drop table
  drop_query <- glue('DROP TABLE {source_table_name};')
  
  
  # Clear the staging data
  tryCatch({
    ch = dbConnect(odbc(), dsn)
    dbBegin(ch)
    if(dbExistsTable(ch,source_table_name)){
      dbExecute(ch,drop_query)
    }
    dbExecute(ch,copy_table_query)
    dbExecute(ch,truncate_query)
    
    dbCommit(ch)
    dbDisconnect(ch)
  },
  error = function(err){
    print(err)
    print("Table Copy Failed")
    dbRollback(ch)
    dbDisconnect(ch)
    
  })
  
  
  registerDoParallel()
  system.time(
    outputPar <- foreach(i = 1:length(split_queries_sql_statements), .packages = c("DBI", "odbc"))%dopar%{
      #Connecting to database through DBI
      ch = dbConnect(odbc(), dsn)
      #Test connection
      tryCatch({
        dbBegin(ch)
        dbExecute(ch, split_queries_sql_statements[[i]])
        dbCommit(ch)
      },
      error = function(err){
        #print("error2")split_queries_sql_statements[[1]]
        dbRollback(ch)
        dbDisconnect(ch)
        print("Staging Data Write Error")
      })
    }
  )
  registerDoSEQ()
  
  
  ###MErge Statement
  # key_columns <- c("FUNCTION", "CATEGORY", "SITE", "CC", "NAME", "EXPTYPE", "SUB_ACCOUNT", "SUB_ACCOUNT_DESCRIPTION", "SUPPLY_MAPPING_FILE_CATEGORY", "MONTH")
  merge_on_cols <- paste(paste0("DT.",key_columns, " = ST.",key_columns), sep="", collapse = ",")
  merge_on_cols <- gsub(',', " AND ", merge_on_cols)
  
  # update_columns <- c("SUM_OF_MONTH_BUDGET", "SUM_OF_MONTH_ACTUAL", "SUM_OF_YTD_BUDGET", "SUM_OF_YTD_ACTUAL", "SUM_OF_ANNUAL_BUDGET")
  update_on_cols <- paste(paste0("DT.",update_columns, " = ST.",update_columns), sep="", collapse = ",")

  
  get_dest_table_cols <-  paste(paste0("DT.",unique(c(key_columns,update_columns))), sep="", collapse = ",")
  get_source_table_values <- paste(paste0("ST.",unique(c(key_columns,update_columns))), sep="", collapse = ",")
    
  # destination_table_name <- "BSC_FINANCE_TABLE"
  # source_table_name <- "BSC_FINANCE_TABLE_TESTING"
  
  merge_query <- glue("MERGE INTO \"{destination_table_name}\" DT 
                        USING \"{source_table_name}\" ST 
                        ON ({merge_on_cols})
                      WHEN MATCHED THEN UPDATE SET
                      {update_on_cols}
                      WHEN NOT MATCHED THEN
                      INSERT ({get_dest_table_cols})
                      VALUES({get_source_table_values})
                      ")
  
  
  ch = dbConnect(odbc(), dsn_oracle)
  #Test connection
  tryCatch({
    dbBegin(ch)
    dbExecute(ch, merge_query)
    dbExecute(ch,drop_query)
    dbCommit(ch)
    dbDisconnect(ch)
    # print("success")
    print("Merge Succesfull")

  },
  error = function(err){
    #print("error")
    dbRollback(ch)
    dbDisconnect(ch)
    print("Merge Failed")
    
  })
  
}
