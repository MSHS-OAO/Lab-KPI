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
                                     column(12, tableOutput("chemistry_kpi"))
                                   )
                                   ),
                          tabPanel("Hematology",
                                   h4("Hematology KPI"),
                                   fluidRow(
                                     column(12, tableOutput("hematology_kpi"))
                                   )
                                   ),
                          tabPanel("Microbiology RRL"),
                          tabPanel("Infusion"),
                          tabPanel("Missing Collections & Add Ons"),
                          tabPanel("Surgical Pathology"),
                          tabPanel("Cytology")
                        )
                        ),
               tabPanel("Ops & Quality Indicators"),
               tabPanel("24 Hour Volume Lookback"),
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

               

               
  