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
                                   h4("Chemistry KPI"),
                                   fluidRow(
                                     tableOutput("chemistry_kpi")
                                     )
                                   ),
                          tabPanel("Hematology",
                                   h4("Hematology KPI"),
                                   fluidRow(
                                     tableOutput("hematology_kpi")
                                     )
                                   ),
                          tabPanel("Microbiology RRL",
                                   h4("Microbiology RRL KPI"),
                                   fluidRow(
                                     tableOutput("micro_kpi")
                                     )
                                   ),
                          tabPanel("Infusion",
                                   h4("Infusion KPI"),
                                   fluidRow(
                                     tableOutput("infusion_kpi")
                                     )
                                   ),
                          tabPanel("Missing Collections & Add Ons",
                                   h4("Missing Collection Times and Ad On Order Volume"),
                                   fluidRow(
                                     tableOutput("missing_collections"),
                                     tableOutput("add_on_volume")
                                     )
                                   ),
                          tabPanel("Surgical Pathology",
                                   h4("Surgical Pathology KPI"),
                                   fluidRow(
                                     tableOutput("surg_path_kpi")
                                     )
                                   ),
                          tabPanel("Cytology",
                                   h4("Cytology KPI"),
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
                                   h4("Chemistry Resulted Lab Volume"),
                                   fluidRow(
                                     tableOutput("chem_volume")
                                   )
                          ),
                          tabPanel("Hematology",
                                   h4("Hematology Resulted Lab Volume"),
                                   fluidRow(
                                     tableOutput("hem_volume")
                                   )
                          ),
                          tabPanel("Infusion",
                                   h4("Infusion Resulted Lab Volume"),
                                   fluidRow(
                                     tableOutput("inf_volume")
                                   )
                          ),
                          tabPanel("Surgical Pathology",
                                   h4("Surgical Pathology Signed Out Case Volume"),
                                   fluidRow(
                                     tableOutput("surg_path_volume")
                                   )
                          ),
                          tabPanel("Cytology",
                                   h4("Cytology Accessioned Cases and Backlog Volume"),
                                   fluidRow(
                                     tableOutput("cyto_acc_backlog_volume")
                                   ),
                                   h4("Cytology Signed Out Cases Volume"),
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

               

               
  