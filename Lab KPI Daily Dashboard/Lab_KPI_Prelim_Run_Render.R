# This code renders the first run markdown for the daily dashboard

# Clear environment
rm(list = ls())

# Determine directory
if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}
# user_directory <- "/Pathology/"

# Render markdown file with dashboard code and save with today's date
rmarkdown::render(paste0("Lab KPI Daily Dashboard/",
                         "Lab_KPI_Daily_Run_Prelim_Dashboard.Rmd"), 
                  output_file = paste0(
                    paste0(user_directory, "Prelim Dashboard Output Tests KN",
                    # substr(user_directory, 1,
                    #        nchar(user_directory) - nchar("/Data")),
                    # "/Dashboard Drafts",
                    "/Daily Run Lab KPI Dashboard Pre KPI Form ",
                    format(Sys.Date(), "%m-%d-%y")))
)


