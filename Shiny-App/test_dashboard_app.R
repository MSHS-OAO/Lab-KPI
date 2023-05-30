# Source packages and reference information for shiny app
source("Shiny_Package_Ref.R")

# Source custom functions for clinical pathology
source("Shiny_CP_Custom_Functions.R")

# Import historical data
source("Import_Historical_Data.R")

# Source ui
source("ui.R")

# Source server
source("server.R")

shinyApp(ui, server)
