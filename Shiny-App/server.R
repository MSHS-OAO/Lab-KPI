if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)


server <- function(input, output, session) {
  
  # Chemistry Efficiency Indicators -------
  # Header based on date
  output$chemistry_tat_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Chemistry KPI (Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Table
  output$chemistry_kpi <- function() {
    
    input$submit_cp_eff_data
    
    chem_sub_output <- summarize_cp_tat(x = cp_summary,
                                        lab_division = "Chemistry")
    chem_subset <- chem_sub_output[[1]]
    chem_summary <- chem_sub_output[[2]]
    chem_dashboard_melt <- chem_sub_output[[3]]
    chem_dashboard_cast <- chem_sub_output[[4]]
    
    kable_cp_tat(x = chem_dashboard_cast)
    
  }
  
  # Hematology Efficiency Indicators ---------
  # Header based on date
  output$hematology_tat_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Hematology KPI (Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Table
  output$hematology_kpi <- function() {
    
    input$submit_cp_eff_data
    
    hem_sub_output <- summarize_cp_tat(x = cp_summary,
                                       lab_division = "Hematology")
    hem_subset <- hem_sub_output[[1]]
    hem_summary <- hem_sub_output[[2]]
    hem_dashboard_melt <- hem_sub_output[[3]]
    hem_dashboard_cast <- hem_sub_output[[4]]
    
    kable_cp_tat(x = hem_dashboard_cast)
    
  }
  
  # Microbiology RRL Efficiency Indicators ---------
  # Header based on date
  output$micro_tat_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Microbiology RRL KPI (Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Tables
  output$micro_kpi <- function() {
    
    input$submit_cp_eff_data
    
    micro_sub_output <- summarize_cp_tat(x = cp_summary,
                                         lab_division = "Microbiology RRL")
    
    micro_subset <- micro_sub_output[[1]]
    micro_summary <- micro_sub_output[[2]]
    micro_dashboard_melt <- micro_sub_output[[3]]
    micro_dashboard_cast <- micro_sub_output[[4]]
    
    # Create volume table for Microbiology RRL that mimics TAT table layout
    if (is.null(micro_summary)) {
      micro_tat_vol_cast <- NULL
      } else {
        micro_volume_melt <- melt(micro_summary,
                                  id.var = c("Test",
                                             "Site",
                                             "DashboardPriority",
                                             "TestAndPriority",
                                             "DashboardSetting",
                                             "ReceiveResultTarget",
                                             "CollectResultTarget"),
                                  measure.vars = "ResultedVolume")
        # Ensure all Microbiology RRL test, site, and priority combinations are included
        micro_vol_templ <- tat_dashboard_templ %>%
          filter(Division == "Microbiology RRL") %>%
          mutate(Incl = NULL,
                 Division = NULL)
        
        micro_volume_melt <- left_join(
          micro_vol_templ,
          micro_volume_melt,
          by = c("Test" = "Test",
                 "Site" = "Site",
                 "DashboardPriority" = "DashboardPriority",
                 "DashboardSetting" = "DashboardSetting"))
        
        # Replace NA with 0 and format site, tests, priority, and settings as factors
        micro_volume_melt <- micro_volume_melt %>%
          mutate(
            #
            # Replace NA with 0
            value = ifelse(is.na(value), 0, value),
            #
            # Set test, site, priority, and setting as factors
            Test = droplevels(factor(Test, levels = test_names, ordered = TRUE)),
            Site = droplevels(factor(Site, levels = all_sites, ordered = TRUE)),
            DashboardPriority = droplevels(factor(DashboardPriority,
                                                  levels = dashboard_priority_order,
                                                  ordered = TRUE)),
            DashboardSetting = droplevels(factor(DashboardSetting,
                                                 levels = dashboard_pt_setting,
                                                 ordered = TRUE)))
        
        micro_volume_cast <- dcast(micro_volume_melt,
                                   Test + DashboardPriority + TestAndPriority +
                                     DashboardSetting + ReceiveResultTarget +
                                     CollectResultTarget ~
                                     variable + Site,
                                   value.var = "value")
        
        original_length <- ncol(micro_volume_cast)
        
        micro_volume_cast <- micro_volume_cast %>%
          mutate(
            #
            # Replace TAT targets with "Resulted Volume"
            ReceiveResultTarget = "Resulted Volume",
            CollectResultTarget = "Resulted Volume",
            #
            # Duplicate dashboard setting column
            DashboardSetting2 = DashboardSetting,
            #
            # Duplicate resulted volume columns
            ResultedVolume_MSH2 = ResultedVolume_MSH,
            ResultedVolume_MSQ2 = ResultedVolume_MSQ,
            ResultedVolume_MSBI2 = ResultedVolume_MSBI,
            ResultedVolume_MSB2 = ResultedVolume_MSB,
            ResultedVolume_MSW2 = ResultedVolume_MSW,
            ResultedVolume_MSM2 = ResultedVolume_MSM,
            ResultedVolume_MSSN2 = ResultedVolume_MSSN) %>%
          select(Test,
                 DashboardPriority,
                 TestAndPriority,
                 ReceiveResultTarget,
                 DashboardSetting,
                 ResultedVolume_MSH,
                 ResultedVolume_MSQ,
                 ResultedVolume_MSBI,
                 ResultedVolume_MSB,
                 ResultedVolume_MSW,
                 ResultedVolume_MSM,
                 ResultedVolume_MSSN,
                 CollectResultTarget,
                 DashboardSetting2,
                 ResultedVolume_MSH2,
                 ResultedVolume_MSQ2,
                 ResultedVolume_MSBI2,
                 ResultedVolume_MSB2,
                 ResultedVolume_MSW2,
                 ResultedVolume_MSM2,
                 ResultedVolume_MSSN2)
        
        # Rename columns to match TAT table for binding
        colnames(micro_volume_cast) <- colnames(micro_dashboard_cast)
        
        micro_volume_cast[, c(original_length:ncol(micro_volume_cast))] <- ""
        
        micro_tat_vol_cast <- rbind(micro_dashboard_cast, micro_volume_cast)
        
        micro_tat_vol_cast <- micro_tat_vol_cast %>%
          arrange(Test, ReceiveResultTarget)
      }
    
    kable_cp_tat(x = micro_tat_vol_cast)
  }
  
  # Infusion Efficiency Indicators ---------
  # Header based on date
  output$infusion_tat_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Infusion KPI (Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI TAT Table
  output$infusion_kpi <- function() {
    
    input$submit_cp_eff_data
    
    inf_sub_output <- summarize_cp_tat(x = cp_summary,
                                       lab_division = "Infusion")
    inf_subset <- inf_sub_output[[1]]
    inf_summary <- inf_sub_output[[2]]
    inf_dashboard_melt <- inf_sub_output[[3]]
    inf_dashboard_cast <- inf_sub_output[[4]]
    
    kable_cp_tat(x = inf_dashboard_cast)
    
  }
  
  # Missing Collections and Add On Orders ---------
  # Header based on date
  output$missing_collect_tat_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Missing Collection Times and Add On Order Volume ",
              "(Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
              )
       )
  }
  )
  
  # KPI Tables
  output$missing_collections <- function() {
    
    input$submit_cp_eff_data
    
    kable_missing_collections(x = cp_summary)
      
  }
  
  output$add_on_volume <- function() {
    
    input$submit_cp_eff_data
    
    kable_add_on_volume(x = cp_summary)
    
  }
  
  # Chemistry Volume ---------
  # Header based on date
  output$chemistry_vol_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Chemistry Resulted Lab Volume ",
              "(Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI Volume Table
  output$chem_volume <- function() {
    
    input$submit_cp_eff_data
    
    chem_vol_table <- summarize_cp_vol(x = cp_summary,
                                       lab_division = "Chemistry")
    
    kable_cp_vol(chem_vol_table)
    
  }
  
  # Hematology Volume ---------
  # Header based on date
  output$hematology_vol_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Hematology Resulted Lab Volume ",
              "(Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI Volume Table
  output$hem_volume <- function() {
    
    input$submit_cp_eff_data
    
    hem_vol_table <- summarize_cp_vol(x = cp_summary,
                                      lab_division = "Hematology")
    
    kable_cp_vol(hem_vol_table)
    
  }
  
  # Infusion Volume ---------
  # Header based on date
  output$infusion_vol_header <- renderUI({
    
    input$submit_cp_eff_data
    
    h4(paste0("Infusion Resulted Lab Volume ",
              "(Labs Resulted on ",
              format(cp_latest_day, "%a %m/%d/%y"),
              ")"
    )
    )
  }
  )
  
  # KPI Volume Table
  output$inf_volume <- function() {
    
    input$submit_cp_eff_data
    
    inf_vol_table <- summarize_cp_vol(x = cp_summary,
                                      lab_division = "Infusion")
    
    kable_cp_vol(inf_vol_table)
    
  }
  
  # Observe event for SCC data
  observeEvent(input$submit_cp_eff_data, {
    button_name <- "submit_cp_eff_data"
    shinyjs::disable(button_name)
    
    flag <- 0
    
    scc_file <<- input$scc
    
    sun_file <<- input$sunquest
    
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
        scc_filename <<- scc_file$datapath
        scc_data_raw <<- read_excel(scc_filename)
        
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
        sun_filename <<- sun_file$datapath
        sun_data_raw <<- read_excel(sun_filename)
        
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
        
        cp_latest_day <<- scc_date
        
        # Bind preprocessed SCC and Sunquest data
        scc_sun_processed <- rbind(scc_processed, sun_processed)
        
        # Add columns for year, month, week, etc.
        scc_sun_processed <<- scc_sun_processed %>%
          mutate(
            # Find month name from result date
            Year = year(ResultDate),
            MonthNo = month(ResultDate),
            MonthName = month(ResultDate, label = TRUE, abbr = TRUE),
            MonthRollUp = as.Date(paste0(MonthNo, "/",
                                         1, "/",
                                         Year),
                                  format = "%m/%d/%Y"),
            WeekStart = ResultDate - (wday(ResultDate) - 1),
            WeekEnd = ResultDate + (7 - wday(ResultDate)),
            WeekOf = paste0(format(WeekStart, "%m/%d/%y"),
                            "-",
                            format(WeekEnd, "%m/%d/%y")))
        
        remove_dupl_dates <<- anti_join(cp_test_repo,
                                       scc_sun_processed,
                                       by = "ResultDate")
        
        week_dates <<- scc_sun_processed %>%
          distinct(WeekStart, WeekEnd)
          
        
        # Summarize data for kables
        cp_summary <<- scc_sun_processed %>%
          group_by(Site,
                   ResultDate,
                   Test,
                   Division,
                   Setting,
                   SettingRollUp,
                   DetailedSetting,
                   DashboardSetting,
                   OrderPriority,
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
                    .groups = "keep") %>%
          arrange(Site, ResultDate) %>%
          ungroup()
        
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
}
 


