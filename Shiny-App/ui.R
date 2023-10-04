

ui <- 
  # fluidPage(
  #   title = "MSHS Lab KPI Daily Dashboard",
  #   shinyjs::useShinyjs(),
  #   
  #   tags$style(type = 'text/css', 
  #              '.navbar { background-color: #dddedd; color: black; font-size: 24px; font-weight: bold;}',
  #              '.navbar-default .navbar-brand{color: black; font-size: 24px;}'
  #   ),
  #   
  #   tags$style(HTML("
  #   .box.box-solid.box-primary>.box-header {
  #   color:#fff;
  #   background:#221f72
  #   }
  #   .box.box-solid.box-primary{
  #   border-bottom-color:#ffffff;
  #   border-left-color:#ffffff;
  #   border-right-color:#ffffff;
  #   border-top-color:#ffffff;
  #   }
  #                   ")),
  #   
  #   tags$style(
  #     "h3{
  #     margin-top: -0.7em;
  #     }"
  #   ),
  #   
  #   tags$style(
  #     "h4{
  #     font-weight: bold;
  #     margin-top: -0.7em;
  #     }"
  #   ),
    navbarPage("MSHS Lab KPI Dashboard",
               fluid = TRUE,
               tabPanel("Efficiency Indicators",
                        tabsetPanel(
                          # Chemistry Efficiency Indicators --------
                          tabPanel("Chemistry",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("chem_tat_date",
                                                      "Resulted Lab Date:",
                                                      value = chemistry_default_date,
                                                      max = chemistry_default_date)
                                     )
                                   ),
                                   uiOutput("chemistry_tat_header"),
                                   tags$style("#chemistry_tat_header
                                              {font-style: italic}"),
                                   p("Status Definitions:",
                                     span("Red: ", style = "color:red"),
                                     "<80%,",
                                     span("Yellow: ", style = "color:orange"),
                                     ">=80% & <95%",
                                     span("Green: ", style = "color:green"),
                                     ">=95%"),
                                   fluidRow(
                                     tableOutput("chemistry_kpi")
                                   )
                          ),
                          # Hematology Efficiency Indicators --------
                          tabPanel("Hematology",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("hematology_tat_date",
                                                      "Resulted Lab Date:",
                                                      value = hematology_default_date,
                                                      max = hematology_default_date)
                                     )
                                   ),
                                   uiOutput("hematology_tat_header"),
                                   tags$style("#hematology_tat_header
                                              {font-style: italic}"),
                                   p("Status Definitions:",
                                     span("Red: ", style = "color:red"),
                                     "<80%,",
                                     span("Yellow: ", style = "color:orange"),
                                     ">=80% & <95%",
                                     span("Green: ", style = "color:green"),
                                     ">=95%"),
                                   fluidRow(
                                     tableOutput("hematology_kpi")
                                   )
                          ),
                          # Microbiology Efficiency Indicators --------
                          tabPanel("Microbiology RRL",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("micro_tat_date",
                                                      "Resulted Lab Date:",
                                                      value = micro_default_date,
                                                      max = micro_default_date)
                                     )
                                   ),
                                   uiOutput("micro_tat_header"),
                                   tags$style("#micro_tat_header
                                              {font-style: italic}"),
                                   p("Status Definitions:",
                                     span("Red: ", style = "color:red"),
                                     "<90%,",
                                     span("Yellow: ", style = "color:orange"),
                                     ">=90% & <100%",
                                     span("Green: ", style = "color:green"),
                                     ">=100%"),
                                   fluidRow(
                                     tableOutput("micro_kpi")
                                     )
                                   ),
                          # Infusion Efficiency Indicators --------
                          tabPanel("Infusion",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("infusion_tat_date",
                                                      "Resulted Lab Date:",
                                                      value = infusion_default_date,
                                                      max = infusion_default_date)
                                     )
                                   ),
                                   uiOutput("infusion_tat_header"),
                                   tags$style("#infusion_tat_header
                                              {font-style: italic}"),
                                   p("Status Definitions:",
                                     span("Red: ", style = "color:red"),
                                     "<80%,",
                                     span("Yellow: ", style = "color:orange"),
                                     ">=80% & <90%",
                                     span("Green: ", style = "color:green"),
                                     ">=90%"),
                                   fluidRow(
                                     tableOutput("infusion_kpi")
                                     )
                                   ),
                          # Clinical Pathology Missing Collections and Add Ons --------
                          tabPanel("Missing Collections & Add Ons",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("missing_collect_date",
                                                      "Resulted Lab Date:",
                                                      value = missing_collect_default_date,
                                                      max = missing_collect_default_date)
                                     )
                                   ),
                                   uiOutput("missing_collect_tat_header"),
                                   tags$style("#missing_collect_tat_header
                                              {font-style: italic}"),
                                   p("Status Definitions:",
                                     span("Red: ", style = "color:red"),
                                     ">15%,",
                                     span("Yellow: ", style = "color:orange"),
                                     "<=15% & >5%",
                                     span("Green: ", style = "color:green"),
                                     "<=5%"),
                                   fluidRow(
                                     tableOutput("missing_collections"),
                                     tableOutput("add_on_volume")
                                     )
                                   ),
                          # Surgical Pathology - Efficiency Indicators ----
                          tabPanel("Surgical Pathology",
                                   br(),
                                   br(),
                                   column(4,
                                          box(title = NULL, solidHeader = FALSE, width = 12,
                                            fluidRow(dateInput("ap_report_date_eff_indicators",
                                                      label = "Select the signed out date",
                                                      value = Sys.Date()-1,
                                                      max = Sys.Date(),
                                                      min = Sys.Date() -60)
                                   )),
                                   fluidRow(
                                     HTML(
                                       paste0("<h4><em>",
                                              "Surgical Pathology KPI ",
                                              "(Specimens Signed Out on ",
                                              format(Sys.Date()-1, "%m/%d/%y"),
                                              ")",
                                              "</h4></em>")
                                     )),
                                   fluidRow(
                                     HTML(
                                       paste0("<h5>Status Definitions: ",
                                              "<span style = 'color:red'>",
                                              "Red: </span>",
                                              "<80%, ",
                                              "<span style = 'color:orange'>",
                                              "Yellow: </span>",
                                              ">=80% & <90%, ",
                                              "<span style = 'color:green'>",
                                              "Green: </span>",
                                              ">=90%",
                                              "</h5>")
                                   ))),
                                   
                                   fluidRow(
                                     tableOutput("surg_path_kpi")
                                     )
                                   ),
                          # Cytology - Efficiency Indicators ----
                          tabPanel("Cytology",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Cytology KPI ",
                                            "(Specimens Signed Out on ",
                                            format(yesterday, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   HTML(
                                     paste0("<h5>Status Definitions: ",
                                            "<span style = 'color:red'>",
                                            "Red: </span>",
                                            "<80%, ",
                                            "<span style = 'color:orange'>",
                                            "Yellow: </span>",
                                            ">=80% & <90%, ",
                                            "<span style = 'color:green'>",
                                            "Green: </span>",
                                            ">=90%",
                                            "</h5>")
                                   ),
                                   fluidRow(
                                     tableOutput("cyto_kpi")
                                     )
                                   )
                        )
               ),
               # Ops & Quality Indicators --------
               tabPanel("Ops & Quality Indicators",
                        tabsetPanel(
                          tabPanel("Operational Indicators",
                                   uiOutput("ops_indicators_header"),
                                   tags$style("#ops_indicators_header
                                              {font-style: italic}"),
                                   tableOutput("ops_indicators_status"),
                                   hr(),
                                   tableOutput("ops_indicators_comments")
                          ),
                          tabPanel("Quality Indicators",
                                   uiOutput("qlty_indicators_header"),
                                   tags$style("#qlty_indicators_header
                                              {font-style: italic"),
                                   tableOutput("qlty_indicators_status"),
                                   hr(),
                                   tableOutput("qlty_indicators_comments")
                          ),
                        ),
               ),
               # Clinical Pathology Volume Lookback --------
               tabPanel("24 Hour Volume Lookback",
                        tabsetPanel(
                          tabPanel("Chemistry",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("chem_vol_date",
                                                      "Resulted Lab Date:",
                                                      value = chemistry_default_date,
                                                      max = chemistry_default_date)
                                     )
                                   ),
                                   uiOutput("chemistry_vol_header"),
                                   tags$style("#chemistry_vol_header
                                              {font-style: italic}"),
                                   fluidRow(
                                     tableOutput("chem_volume")
                                   )
                          ),
                          tabPanel("Hematology",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("hematology_vol_date",
                                                      "Resulted Lab Date:",
                                                      value = hematology_default_date,
                                                      max = hematology_default_date)
                                     )
                                   ),
                                   uiOutput("hematology_vol_header"),
                                   tags$style("#hematology_vol_header
                                              {font-style: italic}"),
                                   fluidRow(
                                     tableOutput("hem_volume")
                                   )
                          ),
                          tabPanel("Infusion",
                                   fluidRow(
                                     column(width = 3,
                                            dateInput("infusion_vol_date",
                                                      "Resulted Lab Date:",
                                                      value = infusion_default_date,
                                                      max = infusion_default_date)
                                     )
                                   ),
                                   uiOutput("infusion_vol_header"),
                                   tags$style("#infusion_vol_header
                                              {font-style: italic}"),
                                   fluidRow(
                                     tableOutput("inf_volume")
                                   )
                          ),
                          tabPanel("Surgical Pathology",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Surgical Pathology Signed Out ",
                                            "Case Volume ",
                                            "(Signed Out on ",
                                            format(yesterday, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   fluidRow(
                                     tableOutput("surg_path_volume")
                                   )
                          ),
                          tabPanel("Cytology",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Cytology Accessioned Cases and ",
                                            "Backlog Volume ",
                                            "(As Of ",
                                            format(yesterday, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   fluidRow(
                                     tableOutput("cyto_acc_backlog_volume")
                                   ),
                                   HTML(
                                     paste0("<h4><em>",
                                            "Cytology Signed Out ",
                                            "Case Volume ",
                                            "(As Of ",
                                            format(yesterday, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   fluidRow(
                                     tableOutput("cyto_signed_out_volume")
                                   )
                          )
                        )
               ),
               # # Data Submissions ----
               # tabPanel("Data Submission",
               #          tabsetPanel(
               #            tabPanel("Clinical Pathology Efficiency Indicators",
               #                     fluidPage(
               #                       fluidRow(h4("SCC"),
               #                                fileInput("scc",
               #                                          label = "Submit SCC file"
               #                                          )
               #                                ),
               #                       fluidRow(h4("Sunquest"),
               #                                fileInput("sunquest",
               #                                          label = "Submit Sunquest file"
               #                                          )
               #                                ),
               #                       fluidRow(actionButton(
               #                         inputId = "submit_cp_eff_data",
               #                         label = "Submit",
               #                         class = "btn-primary"
               #                       )
               #                       )
               #                     )
               #            ),
               #            tabPanel("Anatomic Pathology Efficiency Indicators",
               #                     fluidPage(
               #                       fluidRow(h4("Report Date"),
               #                                dateInput("ap_report_date",
               #                                          label = "Select the date the reports are generated:",
               #                                          value = Sys.Date(),
               #                                          max = Sys.Date(),
               #                                          min = Sys.Date() -7)
               #                       ),
               #                       fluidRow(h4("Epic Cytology"),
               #                                fileInput("epic_cyto",
               #                                          label = "Submit Epic Cytology file"
               #                                          )
               #                                ),
               #                       fluidRow(h4("AP & Cytology Signed Cases"),
               #                                fileInput("ap_cyto_signed",
               #                                          label = "Submit AP & Cytology signed cases file"
               #                                          )
               #                                ),
               #                       fluidRow(h4("Cytology Backlog"),
               #                                fileInput("cyto_backlog",
               #                                          label = "Submit Cytology backlog file"
               #                                          )
               #                                ),
               #                       fluidRow(actionButton(
               #                         inputId = "submit_ap_eff_data",
               #                         label = "Submit",
               #                         class = "btn-primary"
               #                       )
               #                       )
               #                     )
               #            )
               tabPanel("Assumptions & Methodology")
    )

               

               
  
