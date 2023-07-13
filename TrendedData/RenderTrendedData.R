# This code renders the trended troponin turnaround time data

# Clear environment
rm(list = ls())

define_root_path <- function(){
  #Check if directory is from R Workbench; starts with '/home'
  if(grepl("^/home", dirname(getwd()))){
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files("/SharedDrive/") == "Presidents",
           #Define prefix of path to share drive with R Workbench format
           output <- "/SharedDrive/Presidents/", 
           output <- "/SharedDrive/deans/Presidents/")
  }#Check if directory is from R Studio; starts with an uppercase letter than ':'
  else if(grepl("^[[:upper:]]+:", dirname(getwd()))){
    #Determine which drive is mapped to Sharedrive (x)
    for(i in LETTERS){
      if(any(grepl("deans|Presidents", list.files(paste0(i, "://"))))){x <- i}
    }
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files(paste0(x, "://")) == "Presidents",
           #Define prefix of path to share drive with R Studio format
           output <- paste0(x, ":/Presidents/"),
           output <- paste0(x, ":/deans/Presidents/"))
    
  }
  return(output)
}

# Select file/folder path for easier file selection and navigation
user_directory <- define_root_path()

# Render markdown file with dashboard code and save with today's date
rmarkdown::render(paste0("TrendedData/",
                         "TrendedPerformance.RMD"), 
                  output_file = paste0(
                    user_directory,
                    "HSPI-PM/Operations Analytics and Optimization/",
                    "Projects/Service Lines/Lab KPI/TrendedData2023/",
                    "MSHS Lab KPI Trended Performance ",
                    format(Sys.Date(), "%m-%d-%y")))
