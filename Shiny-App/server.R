if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)


server <- function(input, output, session) {
  
  
  
  # Chemistry Efficiency Indicators -------
  # Header based on date
  output$chemistry_tat_header <- renderUI({
    
    input$chem_tat_date
    
    h4(paste0("Chemistry KPI (Labs Resulted on ",
              format(input$chem_tat_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Table
  output$chemistry_kpi <- function() {
    
    input$chem_tat_date
    
    chem_result_date <- input$chem_tat_date
    
    oao_conn <- dbConnect(odbc(), oao_cloud_db)
    
    cp_daily_repo <- tbl(oao_conn, "CP_DAILY_REPO") %>%
      filter(RESULT_DATE == to_date(chem_result_date, 'YYYY-MM-DD')) %>%
      # filter(RESULT_DATE == to_date(chemistry_default_date, 'YYYY-MM-DD')) %>%
      filter(DIVISION == "Chemistry") %>%
      collect()
    
    dbDisconnect(oao_conn)
    
      
    chem_tat_output <- summarize_cp_tat(x = cp_daily_repo,
                                        lab_division = "Chemistry")

    kable_cp_tat(x = chem_tat_output[[2]])
    
  }
  
  # Hematology Efficiency Indicators ---------
  # Header based on date
  output$hematology_tat_header <- renderUI({
    
    input$hematology_tat_date
    
    h4(paste0("Hematology KPI (Labs Resulted on ",
              format(input$hematology_tat_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Table
  output$hematology_kpi <- function() {
    
    input$hematology_tat_date
    
    hematology_result_date <- input$hematology_tat_date
    
    oao_conn <- dbConnect(odbc(), oao_cloud_db)
    
    cp_daily_repo <- tbl(oao_conn, "CP_DAILY_REPO") %>%
      filter(RESULT_DATE == to_date(hematology_result_date, 'YYYY-MM-DD')) %>%
      # filter(RESULT_DATE == to_date(hematology_default_date, 'YYYY-MM-DD')) %>%
      filter(DIVISION == "Hematology") %>%
      collect()
    
    dbDisconnect(oao_conn)
    
    
    hematology_tat_output <- summarize_cp_tat(x = cp_daily_repo,
                                        lab_division = "Hematology")
    
    kable_cp_tat(x = hematology_tat_output[[2]])
    
    
    
  }
  
  # Microbiology RRL Efficiency Indicators ---------
  # Header based on date
  output$micro_tat_header <- renderUI({
    
    input$micro_tat_date
    
    h4(paste0("Microbiology RRL KPI (Labs Resulted on ",
              format(input$micro_tat_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Tables
  output$micro_kpi <- function() {
    
    input$micro_tat_date
    
    micro_result_date <- input$micro_tat_date
    
    oao_conn <- dbConnect(odbc(), oao_cloud_db)
    
    cp_daily_repo <- tbl(oao_conn, "CP_DAILY_REPO") %>%
      filter(RESULT_DATE == to_date(micro_result_date, 'YYYY-MM-DD')) %>%
      # filter(RESULT_DATE == to_date(micro_default_date, 'YYYY-MM-DD')) %>%
      filter(DIVISION == "Microbiology RRL") %>%
      collect()
    
    dbDisconnect(oao_conn)
    
    micro_tat_output <- summarize_cp_tat(x = cp_daily_repo,
                                         lab_division = "Microbiology RRL")
    
    micro_summary <- micro_tat_output[[1]]
    micro_dashboard_cast <- micro_tat_output[[2]]
    
    # Create volume table for Microbiology RRL that mimics TAT table layout
    if (is.null(micro_summary)) {
      micro_tat_vol_cast <- NULL
      } else {
        
        # Ensure all Microbiology RRL test, site, and priority combinations are included
        micro_vol_templ <- tat_dashboard_templ %>%
          filter(Division == "Microbiology RRL") %>%
          mutate(Incl = NULL,
                 Division = NULL)
        
        micro_vol_df <- micro_summary %>%
          mutate(Metric = "ResultedVolume") %>%
          rename(Value = ResultedVolume) %>%
          select(-Division, -ResultedVol_ReceiveTAT, -ResultedVol_CollectTAT,
                 -ReceiveResultInTarget, -CollectResultInTarget,
                 -ReceiveResultPercent, -CollectResultPercent) %>%
          left_join(micro_vol_templ,
                    by = c("Test" = "Test",
                           "Site" = "Site",
                           "DashboardPriority" = "DashboardPriority",
                           "DashboardSetting" = "DashboardSetting")) %>%
          mutate(Value = replace_na(Value, 0),
                 ReceiveResultTarget = "ResultedVolume",
                 CollectResultTarget = "",
                 #
                 # Set test, site, priority, and setting as factors
                 Test = droplevels(factor(Test, levels = test_names,
                                          ordered = TRUE)),
                 Site = droplevels(factor(Site, levels = all_sites,
                                          ordered = TRUE)),
                 DashboardPriority = droplevels(
                   factor(DashboardPriority,
                          levels = dashboard_priority_order,
                          ordered = TRUE)),
                 DashboardSetting = droplevels(
                   factor(DashboardSetting,
                          levels = dashboard_pt_setting,
                          ordered = TRUE))) %>%
          arrange(Test, DashboardPriority, DashboardSetting, Site) %>%
          pivot_wider(names_from = c(Metric, Site),
                      names_sep = "_",
                      values_from = Value) %>%
          relocate(TestAndPriority, .after = DashboardPriority) %>%
          relocate(ReceiveResultTarget, .after = TestAndPriority) %>%
          relocate(CollectResultTarget, .after = last_col())
          
        # 
        # micro_volume_melt <- melt(micro_summary,
        #                           id.var = c("Test",
        #                                      "Site",
        #                                      "DashboardPriority",
        #                                      "TestAndPriority",
        #                                      "DashboardSetting",
        #                                      "ReceiveResultTarget",
        #                                      "CollectResultTarget"),
        #                           measure.vars = "ResultedVolume")
        # 
        # micro_volume_cast <- dcast(micro_volume_melt,
        #                            Test + DashboardPriority + TestAndPriority +
        #                              DashboardSetting + ReceiveResultTarget +
        #                              CollectResultTarget ~
        #                              variable + Site,
        #                            value.var = "value")
        original_length <- ncol(micro_vol_df)
        
        missing_cols <- colnames(micro_dashboard_cast)[
          seq(original_length + 1, ncol(micro_dashboard_cast))]
        
        micro_vol_df[missing_cols] <- ""
        
        colnames(micro_vol_df) <- colnames(micro_dashboard_cast)
        
       
        micro_tat_vol_cast <- rbind(micro_dashboard_cast, micro_vol_df)
        
        micro_tat_vol_cast <- micro_tat_vol_cast %>%
          arrange(Test, ReceiveResultTarget)
      }
    
    kable_cp_tat(x = micro_tat_vol_cast)
  }
  
  # Infusion Efficiency Indicators ---------
  # Header based on date
  output$infusion_tat_header <- renderUI({
    
    input$infusion_tat_date
    
    h4(paste0("Infusion KPI (Labs Resulted on ",
              format(input$infusion_tat_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Table
  output$infusion_kpi <- function() {
    
    input$infusion_tat_date
    
    infusion_result_date <- input$infusion_tat_date
    
    oao_conn <- dbConnect(odbc(), oao_cloud_db)
    
    cp_daily_repo <- tbl(oao_conn, "CP_DAILY_REPO") %>%
      filter(RESULT_DATE == to_date(infusion_result_date, 'YYYY-MM-DD')) %>%
      # filter(RESULT_DATE == to_date(infusion_default_date, 'YYYY-MM-DD')) %>%
      filter(DIVISION == "Infusion") %>%
      collect()
    
    dbDisconnect(oao_conn)
    
    
    infusion_tat_output <- summarize_cp_tat(x = cp_daily_repo,
                                            lab_division = "Infusion")
    
    kable_cp_tat(x = infusion_tat_output[[2]])
    
  }
  
  # Missing Collections and Add On Orders ---------
  # Header based on date
  output$missing_collect_tat_header <- renderUI({
    
    input$missing_collect_date
    
    h4(paste0("Missing Collection Times and Add On Order Volume ",
              "(Labs Resulted on ",
              format(input$missing_collect_date, "%a %m/%d/%y"),
              ")"
              )
       )
  }
  )
  
  # KPI Tables
  output$missing_collections <- function() {
    
    input$missing_collect_date
    
    missing_collect_result_date <- input$missing_collect_date
    
    oao_conn <- dbConnect(odbc(), oao_cloud_db)
    
    cp_daily_repo <- tbl(oao_conn, "CP_DAILY_REPO") %>%
      filter(RESULT_DATE == to_date(missing_collect_result_date, 'YYYY-MM-DD')) %>%
      # filter(RESULT_DATE == to_date(missing_collect_default_date, 'YYYY-MM-DD')) %>%
      collect()
    
    dbDisconnect(oao_conn)
    
    kable_missing_collections(x = cp_daily_repo)
    
  }
  
  output$add_on_volume <- function() {
    
    input$missing_collect_date
    
    missing_collect_result_date <- input$missing_collect_date
    
    oao_conn <- dbConnect(odbc(), oao_cloud_db)
    
    cp_daily_repo <- tbl(oao_conn, "CP_DAILY_REPO") %>%
      filter(RESULT_DATE == to_date(missing_collect_result_date, 'YYYY-MM-DD')) %>%
      # filter(RESULT_DATE == to_date(missing_collect_default_date, 'YYYY-MM-DD')) %>%
      collect()
    
    dbDisconnect(oao_conn)
    
    kable_add_on_volume(x = cp_daily_repo)
    
  }
  
  # Surgical Pathology - Efficiency Indicators ----
  output$surg_path_kpi <- function(){
    
    input$submit_ap_eff_data
    
    
    signed_out_date <- input$ap_report_date_eff_indicators
    
    
    ap_summary_07_12 <- ap_summary %>%
      filter(SIGNED_OUT_DATE == as.Date(signed_out_date))
    tab_data_pathology <- ap_summary_07_12 %>%
      filter(TAB == "SURGICAL PATHOLOGY")
    
    pathology_eff_data <- get_efficiency_indicators_surgical_pathology(tab_data_pathology)
    ap_ei_kable_surgical_pathology(pathology_eff_data)
    
  }
  
  # Cytology - Efficiency Indicators ----
  output$cyto_kpi <- function(){
    
    input$submit_ap_eff_data
    
    ap_summary_07_12 <- ap_summary %>%
      filter(REPORT_DATE == as.Date("2023-07-12"))
    tab_data_cytology <- ap_summary_07_12 %>%
      filter(TAB == "CYTOLOGY")
    
    cytlogy_eff_data <- get_efficiency_indicators_cytology(tab_data_cytology)
    ap_ei_kable_cytology(cytlogy_eff_data)
    
  }
  
  # Chemistry Volume ---------
  # Header based on date
  output$chemistry_vol_header <- renderUI({
    
    input$chem_vol_date
    
    h4(paste0("Chemistry Resulted Lab Volume ",
              "(Labs Resulted on ",
              format(input$chem_vol_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI Volume Table
  output$chem_volume <- function() {
    
    input$chem_vol_date
    
    chem_vol_result_date <- input$chem_vol_date
    
    
    
    chem_vol_table <- summarize_cp_vol(x = cp_submitted_daily_summary,
                                       lab_division = "Chemistry")
    
    kable_cp_vol(chem_vol_table)
    
  }
  
  # Hematology Volume ---------
  # Header based on date
  output$hematology_vol_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Hematology Resulted Lab Volume ",
              "(Labs Resulted on ",
              format(cp_resulted_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI Volume Table
  output$hem_volume <- function() {
    
    input$submit_cp_eff_data
    
    hem_vol_table <- summarize_cp_vol(x = cp_submitted_daily_summary,
                                      lab_division = "Hematology")
    
    kable_cp_vol(hem_vol_table)
    
  }
  
  # Infusion Volume ---------
  # Header based on date
  output$infusion_vol_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Infusion Resulted Lab Volume ",
              "(Labs Resulted on ",
              format(cp_resulted_date, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI Volume Table
  output$inf_volume <- function() {
    
    input$submit_cp_eff_data
    
    inf_vol_table <- summarize_cp_vol(x = cp_submitted_daily_summary,
                                      lab_division = "Infusion")
    
    kable_cp_vol(inf_vol_table)
    
  }
  
  # Operations Indicators -------
  output$ops_indicators_header <- renderUI({

    input$submit_ops_qlty_data
    
    h4(paste0("Operational Indicators - Updated on ",
       format(ops_qlty_date, "%m/%d/%y")))
    
  }
  )
  
  output$ops_indicators_status <- function() {
    
    input$submit_ops_qlty_data
    
    ops_indicators_responses <- ops_qlty_responses_today %>%
      select(-ID, -StartTime, -CompletionTime, -Email, -Name,
             -NeverEvents, -NeverEventsComments, -GoodCatch,
             -CompletionDate, -CompletionHour)
    
    ops_indicators_responses <- ops_indicators_responses %>%
      pivot_longer(cols = !c(Facility, Comments),
                   names_to = "Metric",
                   values_to = "Status") %>%
      mutate(Status = ifelse(str_detect(Status, "Green"),
                             "Safe",
                             ifelse(str_detect(Status, "Yellow"),
                                    "Under Stress",
                                    ifelse(str_detect(Status, "Red"),
                                           "Not Safe", NA))))
    
    ops_indicators_responses <- ops_qlty_formatting(ops_indicators_responses)
    
    ops_indicators_responses <- ops_indicators_responses %>%
      pivot_wider(names_from = "Metric",
                  values_from = "Status") %>%
      relocate(Comments, .after = last_col())
    
    ops_indicators_status <- ops_indicators_responses %>%
      select(-Comments)
    
    # kable(ops_indicators_status)
    
    kable(ops_indicators_status, escape = FALSE, align = "c") %>%
          # col.names = c("Facility", "Lab Corp Consumables",
          #               "Vendor Services", "Environment", "Equipment",
          #               "IT", "Service Change", "Acute Volume Increase",
          #               "Staffing")) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      row_spec(row = 0, font_size = 13)
    
  }
  
  output$ops_indicators_comments <- function() {

    input$submit_ops_qlty_data
    
    ops_indicators_responses <- ops_qlty_responses_today %>%
      select(-ID, -StartTime, -CompletionTime, -Email, -Name,
             -NeverEvents, -NeverEventsComments, -GoodCatch,
             -CompletionDate, -CompletionHour)
    
    ops_indicators_responses <- ops_indicators_responses %>%
      pivot_longer(cols = !c(Facility, Comments),
                   names_to = "Metric",
                   values_to = "Status") %>%
      mutate(Status = ifelse(str_detect(Status, "Green"),
                             "Safe",
                             ifelse(str_detect(Status, "Yellow"),
                                    "Under Stress",
                                    ifelse(str_detect(Status, "Red"),
                                           "Not Safe", NA))))
    
    ops_indicators_responses <- ops_qlty_formatting(ops_indicators_responses)
    
    ops_indicators_responses <- ops_indicators_responses %>%
      pivot_wider(names_from = "Metric",
                  values_from = "Status") %>%
      relocate(Comments, .after = last_col())

    ops_indicators_comments <- ops_indicators_responses %>%
      select(Facility, Comments) %>%
      mutate(Comments = ifelse(is.na(Comments) |
                                 toupper(Comments) %in% c("NONE", "N/A", "NA"),
                               "No Issues Reported (Safe)", Comments)
      )

    kable(ops_indicators_comments, escape = F, align = "c",
          col.names = c("Facility", "Comments if At Risk or Not Safe")) %>%
      kable_styling(bootstrap_options = "hover", full_width = TRUE,
                    position = "center", row_label_position = "c",
                    font_size = 11) %>%
      row_spec(row = 0, font_size = 13)

  }


  
  # Observe event for Clinical Pathology data -------
  observeEvent(input$submit_cp_eff_data, {
    button_name <- "submit_cp_eff_data"
    shinyjs::disable(button_name)
    
    flag <- 0
    
    scc_file <- input$scc

    sun_file <- input$sunquest
    
    # scc_file <- paste0(user_directory,
    #                    "/SCC CP Reports/Doc05-09_0003_20054042 2023-05-09.xlsx")
    # 
    # sun_file <- paste0(user_directory,
    #                    "/SUN CP Reports/KPI_Daily_TAT_Report_Updated 2023-05-09.xls")

    if(is.null(scc_file) |
       is.null(sun_file))
       {
      showModal(modalDialog(
        title = "Error",
        "Please submit both SCC and Sunquest files.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      
      tryCatch({
        
        # Read in SCC file
        scc_filename <- scc_file$datapath
        scc_data_raw <- read_excel(scc_filename)
        # scc_data_raw <- read_excel(scc_file)
        
        flag <- 1

      },
      
      error = function(err){
        showModal(modalDialog(
          title = "Read Error",
          paste0("There seems to be an issue reading this SCC file."),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        shinyjs::enable(button_name)
      }
      )
      
    }
    
    if(flag == 1) {
      
      tryCatch({
        # Read in Sunquest file
        sun_filename <- sun_file$datapath
        sun_data_raw <- read_excel(sun_filename)
        # sun_data_raw <- read_excel(sun_file)
        
        flag <- 2
        
        },
        
        error = function(err){
          showModal(modalDialog(
            title = "Read Error",
            paste0("There seems to be an issue reading this Sunquest file."),
            easyClose = TRUE,
            footer = modalButton("Dismiss")
            ))
          shinyjs::enable(button_name)
          }
      )
      
    }
    
    if(flag == 2) {
      
      tryCatch({
        # Process SCC data
        scc_processed <- preprocess_scc(scc_data_raw)[[1]]
        scc_date <- preprocess_scc(scc_data_raw)[[2]]
        
        flag <- 3
      },
      error = function(err){
        showModal(modalDialog(
          title = "Processing Error",
          paste0("There seems to be an issue processing this SCC file.",
                 " Please check that the correct file was selected."),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        shinyjs::enable(button_name)
      }
      )
    }
    
    if(flag == 3){
      
      tryCatch({
        # Process Sunquest data
        sun_processed <- preprocess_sun(sun_data_raw)[[1]]
        sun_date <- preprocess_sun(sun_data_raw)[[2]]
        
        flag <- 4
      },
      error = function(err){
        showModal(modalDialog(
          title = "Processing Error",
          paste0("There seems to be an issue processing this Sunquest file.",
                 " Please check that the correct file was selected."),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        shinyjs::enable(button_name)
      }
      )
    }
    
    if(flag == 4){
      if(scc_date != sun_date) {
        showModal(modalDialog(
          title = "Date Error",
          "Please check that Sunquest and SCC data are for the same dates.",
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      } else{
        
        cp_resulted_date <<- scc_date
        
        # Bind preprocessed SCC and Sunquest data
        scc_sun_processed <- rbind(scc_processed, sun_processed)
        
        remove_dupl_dates_test_level <- anti_join(cp_test_repo,
                                        scc_sun_processed,
                                        by = "ResultDate")
        
        cp_test_repo <- rbind(remove_dupl_dates_test_level, scc_sun_processed)
        
        cp_test_repo <- cp_test_repo %>%
          arrange(Site, ResultDate)
        
        # saveRDS(cp_test_repo,
        #         paste0(user_directory,
        #                "/Shiny App Repo/CPTestLevelData",
        #                "/CPTestData60Days DummyTest.rds"))

        # Summarize data for kables
        cp_daily_summary <- scc_sun_processed %>%
          group_by(Site,
                   ResultDate,
                   Test,
                   Division,
                   SettingRollUp,
                   DetailedSetting,
                   DashboardSetting,
                   AdjPriority,
                   DashboardPriority,
                   ReceiveResultTarget,
                   CollectResultTarget) %>%
          summarize(TotalResulted = n(),
                    ReceiveTime_VolIncl = sum(ReceiveTime_TATInclude),
                    CollectTime_VolIncl = sum(CollectTime_TATInclude),
                    TotalReceiveResultInTarget =
                      sum(ReceiveResultInTarget[ReceiveTime_TATInclude]),
                    TotalCollectResultInTarget =
                      sum(CollectResultInTarget[CollectTime_TATInclude]),
                    TotalAddOnOrder = sum(AddOnFinal == "AddOn"),
                    TotalMissingCollections = sum(MissingCollect),
                    # Calculate key statistics for collect-to-receive TAT
                    CollectReceive_Avg = mean(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
                    CollectReceive_Median = median(CollectToReceiveTAT[CollectTime_TATInclude], na.rm = TRUE),
                    CollectReceive_95 = quantile(CollectToReceiveTAT[CollectTime_TATInclude], probs = c(0.95),
                                                 na.rm = TRUE),
                    # Calculate key statistics for receive-to-result TAT
                    ReceiveResult_Avg = mean(ReceiveToResultTAT[ReceiveTime_TATInclude],
                                             na.rm = TRUE),
                    ReceiveResult_Median = median(ReceiveToResultTAT[ReceiveTime_TATInclude],
                                                  na.rm = TRUE),
                    ReceiveResult_95 = quantile(ReceiveToResultTAT[ReceiveTime_TATInclude],
                                                probs = c(0.95), na.rm = TRUE),
                    # Calculate key statistics for collect-to-result TAT
                    CollectResult_Avg = mean(CollectToResultTAT[CollectTime_TATInclude],
                                             na.rm = TRUE),
                    CollectResult_Median = median(CollectToResultTAT[CollectTime_TATInclude],
                                                  na.rm = TRUE),
                    CollectResult_95 = quantile(CollectToResultTAT[CollectTime_TATInclude],
                                                probs = c(0.95), na.rm = TRUE),
                    .groups = "keep") %>%
          arrange(Site, ResultDate) %>%
          ungroup()
        
        remove_dupl_dates_daily_summary <- anti_join(cp_daily_repo,
                                                     cp_daily_summary,
                                                     by = "ResultDate")
        
        cp_daily_repo <- rbind(remove_dupl_dates_daily_summary,
                               cp_daily_summary)
        
        cp_daily_repo <- cp_daily_repo %>%
          arrange(Site, ResultDate)
        
        cp_submitted_daily_summary <<- cp_daily_repo %>%
          filter(ResultDate == cp_resulted_date)
        
        # saveRDS(cp_daily_repo,
        #         paste0(user_directory,
        #                "/Shiny App Repo/CPDailySummary",
        #                "/CPDailySummary DummyTest.rds"))
        
        showModal(modalDialog(
          title = "Success",
          "SCC and Sunquest files successfully submitted and processed.",
          easyClose = TRUE,
          footer = modalButton("Dismiss"))
        )
        
      }
    }
  }
  )
  
  # # Observe event for Ops & Quality Indicators -------
  # observeEvent(input$submit_ops_qlty_data, {
  #   button_name <- "submit_ops_qlty_data"
  #   shinyjs::disable(button_name)
  #   
  #   flag <- 0
  #   
  #   ops_qlty_file <- input$ops_qlty
  #   
  #   
  #   if(is.null(ops_qlty_file))
  #   {
  #     showModal(modalDialog(
  #       title = "Error",
  #       "Please submit the latest Operations and Quality Indicators file.",
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   } else {
  #     
  #     tryCatch({
  #       
  #       # Read in SCC file
  #       ops_qlty_filename <- ops_qlty_file$datapath
  #       ops_qlty_data_raw <- read_excel(ops_qlty_filename)
  #       
  #       flag <- 1
  #       
  #     },
  #     
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Read Error",
  #         paste0("There seems to be an issue reading this file."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #   }
  #   
  #   if (flag == 1) {
  #     
  #     # Try processing the data
  #     
  #     
  #     tryCatch({
  #       # Process Sunquest data
  #       sun_processed <- preprocess_sun(sun_data_raw)[[1]]
  #       sun_date <- preprocess_sun(sun_data_raw)[[2]]
  #       
  #       flag <- 4
  #     },
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Processing Error",
  #         paste0("There seems to be an issue processing this Sunquest file.",
  #                " Please check that the correct file was selected."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #   }
  #   
  # 
  #   
  #   
  #   
  # }
  # 
  # )
  # # Observe event for AP Processing  -------
  # observeEvent(input$submit_ap_eff_data, {
  #   button_name <- "submit_ap_eff_data"
  #   shinyjs::disable(button_name)
  #   
  #   flag <- 0
  #   
  #   report_date <- input$ap_report_date
  #   epic_cyto_file <- input$epic_cyto
  #   ap_cyto_signed_file <- input$ap_cyto_signed
  #   cyto_backlog_file <- input$cyto_backlog
  #   resulted_date <<- as.Date(report_date-1)
  #   print(typeof(resulted_date))
  #   print(resulted_date)
  #   
  #   if(is.null(report_date)){
  #     showModal(modalDialog(
  #       title = "Error",
  #       "Please select report date",
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }
  #   
  #   if(is.null(epic_cyto_file) | is.null(ap_cyto_signed_file) | is.null(cyto_backlog_file) )
  #   {
  #     showModal(modalDialog(
  #       title = "Error",
  #       "Please submit all the three latest AP files.",
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   } else {
  #     
  #     tryCatch({
  #       
  #       # Read in epic cyto file
  #       epic_cyto_filename <- epic_cyto_file$datapath
  #       epic_cyto_data_raw <- read_excel(epic_cyto_filename)
  #       
  #     },
  #     
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Read Error",
  #         paste0("There seems to be an issue reading Epic Cytology file."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #     tryCatch({
  #       
  #       # Read in signed cases report
  #       ap_cyto_signed_filename <- ap_cyto_signed_file$datapath
  #       ap_cyto_signed_data_raw <- read_excel(ap_cyto_signed_filename,skip = 1, 1)
  #       ap_cyto_signed_data_raw <- ap_cyto_signed_data_raw %>% 
  #         filter(row_number() <= n()-1)
  #       
  #     },
  #     
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Read Error",
  #         paste0("There seems to be an issue reading AP & Cytology Signed Cases file."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #     tryCatch({
  #       
  #       # Read in epic cyto file
  #       cyto_backlog_filename <- cyto_backlog_file$datapath
  #       cyto_backlog_data_raw <- read_excel(cyto_backlog_filename,skip = 1, 1)
  #       
  #       
  #     },
  #     
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Read Error",
  #         paste0("There seems to be an issue reading Cytology Backlog file."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #     
  #     flag <- 1
  #     
  #   }
  #   
  #   if (flag == 1) {
  #     
  #     # Try processing the data
  #     
  #     
  #     tryCatch({
  #       # Process Epic Cytology and AP Signed cases data
  #       summarized_data_cyto <- cyto_prep(epic_cyto_data_raw,ap_cyto_signed_data_raw,resulted_date)
  #       View(summarized_data_cyto)
  #       print(1)
  #       # Process  AP Signed cases data
  #       summarized_data_patho <- patho_prep(ap_cyto_signed_data_raw,resulted_date)
  #       View(summarized_data_patho)
  #       print(2)
  #       # Process  backlog data
  #       processed_backlog_data <- pre_processing_backlog(cyto_backlog_data_raw)
  #       View(processed_backlog_data)
  #       
  # 
  #       flag <- 2
  #     },
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Processing Error",
  #         paste0("There seems to be an issue processing this Epic Cytology/ AP Signed Cases Report/ Backlog file.",
  #                "Please check that the correct file was selected."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #   }
  #   
  #   if (flag == 2) {
  #     
  #     # save the data
  #     
  #     tryCatch({
  #       if(!is.null(summarized_data_cyto)){
  #         remove_dupl_dates_test_level <- anti_join(ap_summary,
  #                                                   summarized_data_cyto)
  #         
  #         ap_summary <- rbind(remove_dupl_dates_test_level, summarized_data_cyto)
  #         
  #         ap_summary <- ap_summary %>%
  #           arrange(SITE, REPORT_DATE)
  #         
  #         saveRDS(ap_summary,
  #                 paste0(user_directory,
  #                        "/Shiny App Repo/APDailySummary",
  #                        "/APRepo60Days.rds"))
  #         print("CYTO")
  #         
  #       }
  #       
  #       
  #       if(!is.null(summarized_data_patho)){
  #         remove_dupl_dates_test_level <- anti_join(ap_summary,
  #                                                   summarized_data_patho)
  #         
  #         ap_summary <- rbind(remove_dupl_dates_test_level, summarized_data_patho)
  #         
  #         ap_summary <- ap_summary %>%
  #           arrange(SITE, REPORT_DATE)
  #         
  #         saveRDS(ap_summary,
  #                 paste0(user_directory,
  #                        "/Shiny App Repo/APDailySummary",
  #                        "/APRepo60Days.rds"))
  #         print("PATHO")
  #       }
  #       
  #       if(!is.null(processed_backlog_data)){
  #         remove_dupl_dates_test_level <- anti_join(backlog_daily_repo,
  #                                                   processed_backlog_data,
  #                                                   by = "Report_Date")
  #         
  #         backlog_daily_repo <- rbind(remove_dupl_dates_test_level, processed_backlog_data)
  #         
  #         backlog_daily_repo <- backlog_daily_repo %>%
  #           arrange(Facility, Report_Date)
  #         
  #         saveRDS(backlog_daily_repo,
  #                 paste0(user_directory,
  #                        "/Shiny App Repo/APDailySummary",
  #                        "/BacklogRepo60Days.rds"))
  #         
  #       }
  #     
  #       
  #       flag <- 3
  #     },
  #     error = function(err){
  #       showModal(modalDialog(
  #         title = "Processing Error",
  #         paste0("There seems to be an issue storing the data.",
  #                "Please submit again."),
  #         easyClose = TRUE,
  #         footer = modalButton("Dismiss")
  #       ))
  #       shinyjs::enable(button_name)
  #     }
  #     )
  #     
  #   }
  #   
  #   if (flag == 3) {
  #     showModal(modalDialog(
  #       title = "Success",
  #       paste0("The data has been submitted successfully!"),
  #       easyClose = TRUE,
  #       footer = modalButton("Dismiss")
  #     ))
  #     shinyjs::enable(button_name)
  #   }
  #   
  #   
  #   
  # }
  # 
  # )
  # 
  # 
}



 


