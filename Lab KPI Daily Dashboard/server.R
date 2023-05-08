# Increase allowable file size (Sunquest monthly files are too large for default)
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=100*1024^2)


server <- function(input, output, session) {

  # 1. Summary Tab Output ---------------------------------------------------------------------------------
  output$kpitable <- renderText({
    
    "Test"
  })
    

  # 4. Data Tab Output ---------------------------------------------------------------------------------
  observeEvent(input$submit_scc,{
    button_name <- "submit_scc"
    shinyjs::disable(button_name)
  #   inFile_budget <- input$finance_budget
  #   flag <- 0
  #   
  #   if(input$name_finance == ""){
  #     showModal(modalDialog(
  #       title = "Error",
  #       paste0("Please fill in the required fields"),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #   }else{
  #     updated_user <- input$name_finance
  #     file_path <- inFile_budget$datapath
  #     tryCatch({data <- read_excel(file_path, sheet = "5-BSC Cost Center Detail", skip = 3,
  #                                  col_types = c("guess", "text", "text", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess"))
  #     flag <- 1
  #     },
  #     error = function(err){  showModal(modalDialog(
  #       title = "Error",
  #       paste0("There seems to be an issue with the budget file."),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #       shinyjs::enable(button_name)
  #     })
  #   }
  #   
  #   if(flag == 1){
  #     # Process the data into standar Summary Repo format
  #     tryCatch({budget_process <- budget_raw_file_process(data, updated_user)
  #     flag <- 2
  #     
  #     },
  #     error = function(err){  showModal(modalDialog(
  #       title = "Error",
  #       paste0("There seems to be an issue with the budget file."),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #       shinyjs::enable(button_name)
  #     })
  #   }
  #   
  #   
  #   if(flag == 2){
  #     ##Compare submitted results to what is in the Summary Repo in db and return only updated rows
  #     budget_data <- file_return_updated_rows(budget_process)
  #     
  #     #wirte the updated data to the Summary Repo in the server
  #     write_temporary_table_to_database_and_merge(budget_data,
  #                                                 "TEMP_BUDGET", button_name)
  #     
  #     update_picker_choices_sql(session, input$selectedService, input$selectedService2, 
  #                               input$selectedService3)
  #   }
  #   shinyjs::enable(button_name)
  #   
  })

} # Close Server



