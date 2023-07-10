#######
# Code for importing the needed packages, constants, reference files, and
# data templates for the lab KPI dashboard pre-processing -----
#######

#Install packages only the first time you run the code
# install.packages("timeDate")
# install.packages("lubridate")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("knitr")
# install.packages("gdtools")
# install.packages("kableExtra")
# install.packages("formattable")
# install.packages("bizdays")
# install.packages("rmarkdown")
# install.packages("stringr")
# install.packages("writexl")
# install.packages("gsubfn")
# install.packages("tidyr")
#-------------------------------Required packages------------------------------#

#Required packages: run these every time you run the code
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
# library(gdtools)
library(kableExtra)
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)
library(ggplot2)
library(scales)
library(plotly)
library(flexdashboard)
library(bslib)
# library(plyr)
# library(data.table)


#Clear existing history
rm(list = ls())
#-------------------------------holiday/weekend-------------------------------#
# Get today and yesterday's date
today <- Sys.Date()

#Determine if yesterday was a holiday/weekend
#get yesterday's DOW
yesterday <- today - 1

#Get yesterday's DOW
yesterday_day <- wday(yesterday, label = TRUE, abbr = TRUE)

#Remove Good Friday from MSHS Holidays
nyse_holidays <- as.Date(holidayNYSE(year = 1990:2100))
good_friday <- as.Date(GoodFriday())
mshs_holiday <- nyse_holidays[good_friday != nyse_holidays]

#Determine whether yesterday was a holiday/weekend
#holiday_det <- isHoliday(yesterday, holidays = mshs_holiday)
holiday_det <- isHoliday(as.timeDate(yesterday), holidays = mshs_holiday)

#Set up a calendar for collect to received TAT calculations for Path & Cyto
create.calendar("MSHS_working_days", mshs_holiday,
                weekdays = c("saturday", "sunday"))
bizdays.options$set(default.calendar = "MSHS_working_days")

#Function to determine path to share drive on R Workbench or R Studio

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

# Import monthly repository
monthly_repo <- readRDS(
  file = paste0(user_directory,
                "HSPI-PM/Operations Analytics and Optimization/Projects/",
                "Service Lines/Lab KPI/Data/CP Repositories/MonthlyRepo/",
                "Monthly Repo Dec2020 to May2023 as of 06-20-23.RDS")
)

weekly_repo <- readRDS(
  file = paste0(user_directory,
                "HSPI-PM/Operations Analytics and Optimization/Projects/",
                "Service Lines/Lab KPI/Data/CP Repositories/WeeklyRepo/",
                "Weekly Repo 12-06-20 to 06-17-23 as of 06-20-23.rds")
)

all_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN", "RTC")
hosp_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN")
infusion_sites <- c("RTC")

pt_setting_order <- c("ED", "ICU", "IP Non-ICU", "Amb", "Other")
pt_setting_order2 <- c("ED & ICU", "IP Non-ICU", "Amb", "Other")

cp_division_order <- c("Chemistry", "Hematology", "Microbiology RRL", "Infusion")

# Color Palettes -----------
# Mount Sinai corporate colors
# Update color palette based on brand manual and set medium and light to 50% and 25% tint
MountSinai_colors <- c(
  `dark purple`  = "#221F72",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#58595B",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#918FB9",
  `med pink`     = "#EC85C6",
  `med blue`     = "#80D7F7",
  `med grey`     = "#ACACAD",
  `light purple` = "#C8C7DC",
  `light pink`   = "#F5C2E2",
  `light blue`   = "#BFEBFB",
  `light grey`   = "#D5D6D6"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Use in ggplot 

#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `main`  = MountSinai_cols("dark purple","dark pink","dark blue","dark grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)

# MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order

MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot instead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
