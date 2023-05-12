if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)


server <- function(input, output, session) {
  
  output$chemistry_kpi <- function() {
    
    input$submit_eff_data
    
    chem_sub_output <- summarize_cp_tat(x = cp_summary,
                                        lab_division = "Chemistry")
    chem_subset <- chem_sub_output[[1]]
    chem_summary <- chem_sub_output[[2]]
    chem_dashboard_melt <- chem_sub_output[[3]]
    chem_dashboard_cast <- chem_sub_output[[4]]
    
    kable_cp_tat(x = chem_dashboard_cast)
    
  }
  
  # Observe event for SCC data
  observeEvent(input$submit_eff_data, {
    button_name <- "submit_eff_data"
    shinyjs::disable(button_name)
    
    flag <- 0
    
    scc_file <<- input$scc
    
    sun_file <<- input$sunquest
    
    epic_cyto_file <<- input$epic_cyto
    
    ap_cyto_signed_file <<- input$ap_cyto_signed
    
    cyto_backlog_file <<- input$cyto_backlog
    
    if(is.null(scc_file) |
       is.null(sun_file)) #|
       # is.null(epic_cyto_file) |
       # is.null(ap_cyto_signed_file) |
       # is.null(cyto_backlog_file)) 
       {
      showModal(modalDialog(
        title = "Error",
        "Please submit all files.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Success",
        "All files submitted.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    scc_filename <<- scc_file$datapath
    
    sun_filename <<- sun_file$datapath
    
    scc_data_raw <<- read_excel(scc_filename)
    
    sun_data_raw <<- read_excel(sun_filename)
    
    scc_processed <- preprocess_scc(scc_data_raw)
    
    sun_processed <- preprocess_sun(sun_data_raw)
    
    # Bind preprocessed SCC and Sunquest data
    scc_sun_processed <- rbind(scc_processed, sun_processed)
    
    # Summarize  data by site, date, test, setting, priority, etc.-------
    if (is.null(scc_sun_processed)) {
      cp_summary <<- NULL
    } else {
      cp_summary <<- scc_sun_processed %>%
        group_by(Site,
                 ResultDate,
                 Test,
                 Division,
                 Setting,
                 SettingRollUp,
                 SettingFinal,
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
    }
    
  })
  
}
  


