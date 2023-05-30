library(shiny)
library(shinyWidgets)
library(shinydashboard)


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
               tabPanel("Efficiency Indicators",
                        h4("Select Appropriate Tab"),
                        tabsetPanel(
                          tabPanel("Chemistry",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Chemistry KPI (Labs Resulted on ",
                                            format(cp_latest_day, "%a %m/%d/%y"),
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
                                            ">=80% & <95%, ",
                                            "<span style = 'color:green'>",
                                            "Green: </span>",
                                            ">=95%",
                                            "</h5>")
                                   ),
                                   fluidRow(
                                     tableOutput("chemistry_kpi")
                                     )
                                   ),
                          tabPanel("Hematology",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Hematology KPI (Labs Resulted on ",
                                            format(cp_latest_day, "%a %m/%d/%y"),
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
                                            ">=80% & <95%, ",
                                            "<span style = 'color:green'>",
                                            "Green: </span>",
                                            ">=95%",
                                            "</h5>")
                                   ),
                                   fluidRow(
                                     tableOutput("hematology_kpi")
                                     )
                                   ),
                          tabPanel("Microbiology RRL",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Microbiology RRL KPI (Labs Resulted on ",
                                            format(cp_latest_day, "%a %m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   HTML(
                                     paste0("<h5>Status Definitions: ",
                                            "<span style = 'color:red'>",
                                            "Red: </span>",
                                            "<90%, ",
                                            "<span style = 'color:orange'>",
                                            "Yellow: </span>",
                                            ">=90% & <100%, ",
                                            "<span style = 'color:green'>",
                                            "Green: </span>",
                                            "=100%",
                                            "</h5>")
                                   ),
                                   fluidRow(
                                     tableOutput("micro_kpi")
                                     )
                                   ),
                          tabPanel("Infusion",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Infusion KPI (Labs Resulted on ",
                                            format(cp_latest_day, "%a %m/%d/%y"),
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
                                     tableOutput("infusion_kpi")
                                     )
                                   ),
                          tabPanel("Missing Collections & Add Ons",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Missing Collection Times and ",
                                            "Add On Order Volume ",
                                            "(Labs Resulted on ",
                                            format(cp_latest_day, "%a %m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   HTML(
                                     paste0("<h5>Status Definitions: ",
                                            "<span style = 'color:red'>",
                                            "Red: </span>",
                                            ">15%, ",
                                            "<span style = 'color:orange'>",
                                            "Yellow: </span>",
                                            "<=15% & >5%, ",
                                            "<span style = 'color:green'>",
                                            "Green: </span>",
                                            "<=5%",
                                            "</h5>")
                                   ),
                                   fluidRow(
                                     tableOutput("missing_collections"),
                                     tableOutput("add_on_volume")
                                     )
                                   ),
                          tabPanel("Surgical Pathology",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Surgical Pathology KPI ",
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
                                     tableOutput("surg_path_kpi")
                                     )
                                   ),
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
               tabPanel("Ops & Quality Indicators"),
               tabPanel("24 Hour Volume Lookback",
                        h4("Select appropriate tab."),
                        tabsetPanel(
                          tabPanel("Chemistry",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Chemistry Resulted Lab Volume ",
                                            "(Labs Resulted on ",
                                            format(cp_latest_day, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   fluidRow(
                                     tableOutput("chem_volume")
                                   )
                          ),
                          tabPanel("Hematology",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Hematology Resulted Lab Volume ",
                                            "(Labs Resulted on ",
                                            format(cp_latest_day, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
                                   fluidRow(
                                     tableOutput("hem_volume")
                                   )
                          ),
                          tabPanel("Infusion",
                                   HTML(
                                     paste0("<h4><em>",
                                            "Infusion Resulted Lab Volume ",
                                            "(Labs Resulted on ",
                                            format(cp_latest_day, "%m/%d/%y"),
                                            ")",
                                            "</h4></em>")
                                   ),
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
               tabPanel("Data Submission",
                        tabsetPanel(
                          tabPanel("Efficiency Indicators",
                                   fluidPage(
                                     fluidRow(h4("SCC"),
                                              fileInput("scc",
                                                        label = "Submit SCC file"
                                                        )
                                              ),
                                     fluidRow(h4("Sunquest"),
                                              fileInput("sunquest",
                                                        label = "Submit Sunquest file"
                                                        )
                                              ),
                                     fluidRow(h4("Epic Cytology"),
                                              fileInput("epic_cyto",
                                                        label = "Submit Epic Cytology file"
                                                        )
                                              ),
                                     fluidRow(h4("AP & Cytology Signed Cases"),
                                              fileInput("ap_cyto_signed",
                                                        label = "Submit AP & Cytology signed cases file"
                                                        )
                                              ),
                                     fluidRow(h4("Cytology Backlog"),
                                              fileInput("cyto_backlog",
                                                        label = "Submit Cytology backlog file"
                                                        )
                                              ),
                                     fluidRow(actionButton(
                                       inputId = "submit_eff_data",
                                       label = "Submit",
                                       class = "btn-primary"
                                       )
                                     )
                                   )
                          ),
                          tabPanel("Ops & Quality Indicators",
                                   fluidPage(
                                     fluidRow(h4("Ops & Quality Indicators"),
                                              fileInput("ops_qlty",
                                                        label = "Submit form responses for Ops & Quality Indicators",
                                                        width = "50%"
                                                        )
                                              ),
                                     fluidRow(
                                       actionButton(
                                         inputId = "submit_ops_qlty",
                                         label = "Submit"
                                       )
                                     )
                                   )
                          )
                        )
               ),
               tabPanel("Assumptions & Methodology")
    )

               

               
  
