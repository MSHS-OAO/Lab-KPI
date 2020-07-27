---
title: "MSHS Laboratory KPI Dashboard"
output:
  html_document:
    toc: true
    toc_depth: 2
    toc_float: true
---


```{r include = FALSE}
#Install packages only the first time you run the code
#install.packages("timeDate")
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("formattable")
#install.packages("bizdays")
#install.packages("rmarkdown")
#install.packages("stringr")
#install.packages("writexl")

#-------------------------------Required packages-------------------------------#

#Required packages: run these everytime you run the code
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
library(kableExtra)
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)


```


<h4><span style = "color:red">Draft - Not For Distribution</h4></span style = "color:red">


```{r global_options, echo=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
```

```{r Determine date, warning = FALSE, message = FALSE, echo = FALSE}
#Clear existing history
rm(list = ls())
#-------------------------------holiday/weekend-------------------------------#
# Get today and yesterday's date

Today <- as.timeDate(format(Sys.Date(),"%m/%d/%Y"))
# Today <- as.timeDate(as.Date("07/20/2020", format = "%m/%d/%Y"))


#Determine if yesterday was a holiday/weekend 
#get yesterday's DOW
Yesterday <- as.timeDate(format(Sys.Date()-1,"%m/%d/%Y"))
# Yesterday <- as.timeDate(as.Date("07/19/2020", format = "%m/%d/%Y"))

#Get yesterday's DOW
Yesterday_Day <- dayOfWeek(Yesterday)

#Remove Good Friday from MSHS Holidays
NYSE_Holidays <- as.Date(holidayNYSE(year = 1990:2100))
GoodFriday <- as.Date(GoodFriday())
MSHS_Holiday <- NYSE_Holidays[GoodFriday != NYSE_Holidays]

#Determine whether yesterday was a holiday/weekend
Holiday_Det <- isHoliday(Yesterday, holidays = MSHS_Holiday)

#Set up a default calendar for collect to received TAT calculations for Pathology and Cytology
create.calendar("MSHS_working_days", MSHS_Holiday, weekdays=c("saturday","sunday"))
bizdays.options$set(default.calendar="MSHS_working_days")
```

#### *Dashboard Creation Date: `r format(Today, "%m/%d/%Y")`*

```{r Import all data, warning = FALSE, message = FALSE, echo = FALSE}
# Select file/folder path for easier file selection and navigation
# user_wd <- choose.dir(caption = "Select your working directory")
#user_wd <- "J:\\deans\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Lab KPI\\Data"
#user_path <- paste0(user_wd, "\\*.*")

if (list.files("J://") == "Presidents") {
  user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Lab Kpi/Data"
} else {
  user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/Service Lines/Lab Kpi/Data"
}
user_path <- paste0(user_directory, "/*.*")


#------------------------------Read Excel sheets------------------------------#

# The if-statement below determines the number of raw data files to import based on the day of week and holidays.
# The user is then prompted to select each file from the relevant folder.

if(((Holiday_Det) & (Yesterday_Day =="Mon"))|((Yesterday_Day =="Sun") & (isHoliday(Yesterday-(86400*2))))){ # Scenario 1: Mon Holiday or Friday Holiday (Need to select 4 files)
  # Save scenario
  scenario <- 1
  # Import SCC data
  SCC_Holiday_Monday_or_Friday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Recent Holiday"), sheet = 1, col_names = TRUE)
  SCC_Sunday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Sunday"), sheet = 1, col_names = TRUE)
  SCC_Saturday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Saturday"), sheet = 1, col_names = TRUE)
  #Merge the weekend data with the holiday data in one data frame
  SCC_Not_Weekday <- rbind(SCC_Holiday_Monday_or_Friday, SCC_Sunday, SCC_Saturday)
  SCC_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE)
  # 
  # Import Sunquest data
  SQ_Holiday_Monday_or_Friday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Recent Holiday"), sheet = 1, col_names = TRUE))
  SQ_Sunday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Sunday"), sheet = 1, col_names = TRUE))
  SQ_Saturday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Saturday"), sheet = 1, col_names = TRUE))
  #Merge the weekend data with the holiday data in one data frame
  SQ_Not_Weekday <- rbind(SQ_Holiday_Monday_or_Friday, SQ_Sunday, SQ_Saturday)
  SQ_Weekday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE))
  #
  # Import Powerpath data
  PP_Holiday_Monday_or_Friday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Recent Holiday"), skip = 1, 1)
  PP_Holiday_Monday_or_Friday  <- PP_Holiday_Monday_or_Friday[-nrow(PP_Holiday_Monday_or_Friday),]
  PP_Sunday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Sunday"), skip = 1, 1)
  PP_Sunday <- PP_Sunday[-nrow(PP_Sunday),]
  PP_Saturday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Saturday"), skip = 1, 1)
  PP_Saturday <- PP_Saturday[-nrow(PP_Saturday),]
  #Merge the weekend data with the holiday data in one data frame
  PP_Not_Weekday <- data.frame(rbind(PP_Holiday_Monday_or_Friday ,PP_Sunday,PP_Saturday),stringsAsFactors = FALSE)
  PP_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Most Recent Weekday"), skip = 1, 1)
  PP_Weekday <- data.frame(PP_Weekday[-nrow(PP_Weekday),],stringsAsFactors = FALSE)
} else if ((Holiday_Det) & (Yesterday_Day =="Sun")){ # Scenario 2: Regular Monday (Need to select 3 files)
  # Save scenario
  scenario <- 2
  #
  # Import SCC data
  SCC_Sunday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Sunday"), sheet = 1, col_names = TRUE)
  SCC_Saturday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Saturday"), sheet = 1, col_names = TRUE)
  #Merge the weekend data with the holiday data in one data frame
  SCC_Not_Weekday <- rbind(SCC_Sunday,SCC_Saturday)
  SCC_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE)
  #
  # Import Sunquest data
  SQ_Sunday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Sunday"), sheet = 1, col_names = TRUE))
  SQ_Saturday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Saturday"), sheet = 1, col_names = TRUE))
  #Merge the weekend data with the holiday data in one data frame
  SQ_Not_Weekday <- rbind(SQ_Sunday,SQ_Saturday)
  SQ_Weekday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE))
  #
  # Import Powerpath data
  PP_Sunday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Sunday"), skip = 1, 1)
  PP_Sunday <- PP_Sunday[-nrow(PP_Sunday),]
  PP_Saturday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Saturday"), skip = 1, 1)
  PP_Saturday <- PP_Saturday[-nrow(PP_Saturday),]
  #Merge the weekend data with the holiday data in one data frame
  PP_Not_Weekday <- data.frame(rbind(PP_Sunday,PP_Saturday),stringsAsFactors = FALSE)
  PP_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Most Recent Weekday"), skip = 1, 1)
  PP_Weekday <- data.frame(PP_Weekday[-nrow(PP_Weekday),], stringsAsFactors = FALSE)
} else if ((Holiday_Det) & ((Yesterday_Day !="Mon")|(Yesterday_Day !="Sun"))){ #Scenario 3: Midweek holiday (Need to select 2 files)
  # Save scenario
  scenario <- 3
  #
  # Import SCC data
  SCC_Holiday_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Recent Holiday"), sheet = 1, col_names = TRUE)
  SCC_Not_Weekday <- SCC_Holiday_Weekday
  SCC_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE)
  #
  # Import Sunquest data
  SQ_Holiday_Weekday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Recent Holiday"), sheet = 1, col_names = TRUE))
  SQ_Not_Weekday <- SQ_Holiday_Weekday
  SQ_Weekday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select Sunquest Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE))
  #
  # Import Powerpath data
  PP_Holiday_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Recent Holiday"), skip = 1, 1)
  PP_Holiday_Weekday <- PP_Holiday_Weekday[-nrow(PP_Holiday_Weekday),]
  PP_Not_Weekday <- data.frame(PP_Holiday_Weekday, stringsAsFactors = FALSE)
  PP_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Most Recent Weekday"), skip = 1, 1)
  PP_Weekday <- data.frame(PP_Weekday[-nrow(PP_Weekday),], stringsAsFactors = FALSE)
} else { #Scenario 4: Tue-Fri without holidays (Need to select 1 file)
  # Save scenario
  scenario <- 4
  #
  # Import SCC data
  SCC_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/SCC CP Reports/*.*"), caption = "Select SCC Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE)
  SCC_Not_Weekday <- NULL
  #
  # Import Sunquest data
  SQ_Weekday <- suppressWarnings(read_excel(choose.files(default = paste0(user_directory, "/SUN CP Reports/*.*"), caption = "Select SunQuest Report for Labs Resulted on Most Recent Weekday"), sheet = 1, col_names = TRUE))
  SQ_Not_Weekday <- NULL
  #
  # Import Powerpath data
  PP_Weekday <- read_excel(choose.files(default = paste0(user_directory, "/AP & Cytology Signed Cases Reports/*.*"), caption = "Select PowerPath Report for Specimens Signed Out on Most Recent Weekday"), skip = 1, 1)
  PP_Weekday <- data.frame(PP_Weekday[-nrow(PP_Weekday),], stringsAsFactors = FALSE)
  PP_Not_Weekday <- NULL
}

#-----------Cytology Backlog Excel Files-----------#
#For the backlog files the read excel is starting from the second row and remove last row
Cytology_Backlog_Raw <- read_excel(choose.files(default = paste0(user_directory, "/Cytology Backlog Reports/*.*"), caption = "Select Cytology Backlog Report"),skip =1 , 1)
Cytology_Backlog_Raw <- data.frame(Cytology_Backlog_Raw[-nrow(Cytology_Backlog_Raw),], stringsAsFactors = FALSE)
```

```{r Reference Data, warning = FALSE, message = FALSE, echo = FALSE}
# CP and Micro --------------------------------
# Import analysis reference data starting with test codes for SCC and Sunquest
# reference_file <- "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Lab KPI\\Data\\Code Reference\\Analysis Reference 2020-01-22.xlsx"

reference_file <- choose.files(default = paste0(user_directory, "/Code Reference/*.*"), caption = "Select analysis reference file")
test_code <- read_excel(reference_file, sheet = "TestNames")

tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")
tat_targets$Concate <- ifelse(tat_targets$Priority == "All" & tat_targets$`Pt Setting` == "All", tat_targets$Test,
                              ifelse(tat_targets$Priority != "All" & tat_targets$`Pt Setting` == "All", paste(tat_targets$Test, tat_targets$Priority),
                                     paste(tat_targets$Test, tat_targets$Priority, tat_targets$`Pt Setting`)))

scc_icu <- read_excel(reference_file, sheet = "SCC_ICU")
scc_setting <- read_excel(reference_file, sheet = "SCC_ClinicType")
sun_icu <- read_excel(reference_file, sheet = "SUN_ICU")
sun_setting <- read_excel(reference_file, sheet = "SUN_LocType")

mshs_site <- read_excel(reference_file, sheet = "SiteNames")

scc_wday <- SCC_Weekday
sun_wday <- SQ_Weekday

cp_micro_lab_order <- c("Troponin", "Lactate WB", "BUN", "HGB", "PT", "Rapid Flu", "C. diff")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM")

pt_setting_order <- c("ED", "ICU", "IP Non-ICU", "Amb", "Other")
pt_setting_order2 <- c("ED & ICU", "IP Non-ICU", "Amb", "Other")
dashboard_pt_setting <- c("ED & ICU", "IP Non-ICU", "Amb")

dashboard_priority_order <- c("All", "Stat", "Routine")

# To Be Updated in Analysis reference file
#-----------Patient Setting Excel File-----------#
#Using Rev Center to determine patient setting
Patient_Setting <- data.frame(read_excel(reference_file, sheet = "AP_Patient Setting"), stringsAsFactors = FALSE)

#-----------Anatomic Pathology Targets Excel File-----------#
TAT_Targets <- data.frame(read_excel(reference_file, sheet = "AP_TAT Targets"), stringsAsFactors = FALSE)

#-----------GI Codes Excel File-----------#
#Upload the exclusion vs inclusion criteria associated with the GI codes
GI_Codes <- data.frame(read_excel(reference_file, sheet = "GI_Codes"), stringsAsFactors = FALSE)

#-----------Create table template for Cyto/Path-----------#

#Cyto
Table_Template_Cyto <- data.frame(matrix(ncol=19, nrow = 4)) 
colnames(Table_Template_Cyto) <- c("spec_group","Patient.Setting","No_Cases_Signed","MSH.x", "BIMC.x","MSQ.x","NYEE.x","PACC.x","R.x","SL.x", "KH.x","BIMC.y", "MSH.y", "MSQ.y","NYEE.y","PACC.y","R.y","SL.y", "KH.y")
Table_Template_Cyto[1] <- c('CYTO GYN','CYTO GYN','CYTO NONGYN', 'CYTO NONGYN')
Table_Template_Cyto[2] <- c('IP', 'Amb')

Table_Template_Cyto_V2 <- data.frame(matrix(ncol=12, nrow = 4)) 
colnames(Table_Template_Cyto_V2) <- c("spec_group","Patient.Setting","No_Cases_Signed","Received_to_Signed_Out_within_target","BIMC", "MSH", "MSQ","NYEE","PACC","R","SL", "KH")
Table_Template_Cyto_V2[1] <- c('CYTO GYN','CYTO GYN','CYTO NONGYN', 'CYTO NONGYN')
Table_Template_Cyto_V2[2] <- c('IP', 'Amb')

Table_Template_Cyto_Vol <- data.frame(matrix(ncol=10, nrow = 4)) 
colnames(Table_Template_Cyto_Vol) <- c("spec_group","Patient.Setting","BIMC", "MSH", "MSQ","NYEE","PACC","R","SL", "KH")
Table_Template_Cyto_Vol[1] <- c('CYTO GYN','CYTO GYN','CYTO NONGYN', 'CYTO NONGYN')
Table_Template_Cyto_Vol[2] <- c('IP', 'Amb')

#Patho
Table_Template_Patho <- data.frame(matrix(ncol=17, nrow = 4)) 
colnames(Table_Template_Patho) <- c("spec_group","Patient.Setting","No_Cases_Signed","MSH.x", "BIMC.x","MSQ.x","PACC.x","R.x","SL.x", "KH.x","BIMC.y", "MSH.y", "MSQ.y","PACC.y","R.y","SL.y", "KH.y")
Table_Template_Patho[1] <- c('Breast','Breast','GI', 'GI')
Table_Template_Patho[2] <- c('IP', 'Amb')

Table_Template_Patho_Vol <- data.frame(matrix(ncol=9, nrow = 4)) 
colnames(Table_Template_Patho_Vol) <- c("spec_group","Patient.Setting","BIMC", "MSH", "MSQ", "PACC","R","SL", "KH")
Table_Template_Patho_Vol[1] <- c('Breast','Breast','GI', 'GI')
Table_Template_Patho_Vol[2] <- c('IP', 'Amb')
```


```{r Custom Function for preprocessing raw SCC and Sunquest data, warning = FALSE, message = FALSE, echo = FALSE}
preprocess_scc_sun <- function(raw_scc, raw_sun)  {
 
# SCC DATA PROCESSING --------------------------
  # SCC lookup references ----------------------------------------------
  # Crosswalk labs included and remove out of scope labs
  raw_scc <- left_join(raw_scc, test_code[ , c("Test", "SCC_TestID", "Division")], by = c("TEST_ID" = "SCC_TestID"))
  raw_scc$TEST_ID <- as.factor(raw_scc$TEST_ID)
  raw_scc$Division <- as.factor(raw_scc$Division)

  raw_scc <- raw_scc %>%
    mutate(TestIncl = ifelse(is.na(raw_scc$Test), FALSE, TRUE))
  
  raw_scc <- raw_scc %>%
    filter(TestIncl)
  
  # Crosswalk units and identify ICUs
  raw_scc <- raw_scc %>%
    mutate(WardandName = paste(Ward, WARD_NAME))
  
  raw_scc <- left_join(raw_scc, scc_icu[ , c("Concatenate", "ICU")], by = c("WardandName" = "Concatenate"))
  raw_scc[is.na(raw_scc$ICU), "ICU"] <- FALSE
  
  # Crosswalk unit type
  raw_scc <- left_join(raw_scc, scc_setting, by = c("CLINIC_TYPE" = "Clinic_Type"))
  # Crosswalk site name
  raw_scc <- left_join(raw_scc, mshs_site, by = c("SITE" = "DataSite"))
  
  # SCC data formatting ----------------------------------------------
  raw_scc[c("Ward", "WARD_NAME", 
             "REQUESTING_DOC", 
             "GROUP_TEST_ID", "TEST_ID", "TEST_NAME", "PRIORITY", "Test",
             "COLLECT_CENTER_ID", "SITE", "Site",
             "CLINIC_TYPE", "Setting", "SettingRollUp")] <- lapply(raw_scc[c("Ward", "WARD_NAME", 
                                                                              "REQUESTING_DOC", 
                                                                              "GROUP_TEST_ID", "TEST_ID", "TEST_NAME", "PRIORITY", "Test",
                                                                              "COLLECT_CENTER_ID", "SITE", "Site",
                                                                              "CLINIC_TYPE", "Setting", "SettingRollUp")], as.factor)
  
  # Fix any timestamps that weren't imported correctly and then format as date/time
  raw_scc[c("ORDERING_DATE", "COLLECTION_DATE", "RECEIVE_DATE", "VERIFIED_DATE")] <- 
    lapply(raw_scc[c("ORDERING_DATE", "COLLECTION_DATE", "RECEIVE_DATE", "VERIFIED_DATE")], 
           function(x) ifelse(!is.na(x) & str_detect(x, "\\*.*\\*")  == TRUE, str_replace(x, "\\*.*\\*", ""), x))
  
  raw_scc[c("ORDERING_DATE", "COLLECTION_DATE", "RECEIVE_DATE", "VERIFIED_DATE")] <- 
    lapply(raw_scc[c("ORDERING_DATE", "COLLECTION_DATE", "RECEIVE_DATE", "VERIFIED_DATE")], 
           as.POSIXct, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS", options(digits.sec = 1))
  
  # Add a column for Resulted date for later use in repository
  raw_scc <- raw_scc %>%
    mutate(ResultedDate = as.Date(VERIFIED_DATE, format = "%m/%d/%Y"))
  
  # Update patient setting to reflect ICU/Non-ICU and update priorities for ED and ICU labs
  raw_scc <- raw_scc %>%
    mutate(MasterSetting = ifelse(CLINIC_TYPE == "E", "ED", 
                                  ifelse(CLINIC_TYPE == "O", "Amb", 
                                         ifelse(CLINIC_TYPE == "I" & ICU == TRUE, "ICU",
                                                ifelse(CLINIC_TYPE == "I" & ICU != TRUE, "IP Non-ICU", "Other")))),
           DashboardSetting = ifelse(MasterSetting == "ED" | MasterSetting == "ICU", "ED & ICU", 
                                     MasterSetting))
  
  # Update priority to reflect ED/ICU as stat and create Master Priority for labs where all specimens are treated as stat
  raw_scc <- raw_scc %>%
    mutate(AdjPriority = ifelse(MasterSetting == "ED" | MasterSetting == "ICU" | PRIORITY == "S", 
                                "Stat", "Routine"),
           DashboardPriority = ifelse(tat_targets$Priority[match(Test, tat_targets$Test)] == "All", 
                                      "All", AdjPriority))

  # Calculate turnaround times
  raw_scc <- raw_scc %>%
    mutate(CollectToReceive = RECEIVE_DATE - COLLECTION_DATE,
           ReceiveToResult = VERIFIED_DATE - RECEIVE_DATE,
           CollectToResult = VERIFIED_DATE - COLLECTION_DATE)
  
  raw_scc[c("CollectToReceive", "ReceiveToResult", "CollectToResult")] <- 
    lapply(raw_scc[c("CollectToReceive", "ReceiveToResult", "CollectToResult")], 
           as.numeric, units = "mins")
  
  # Identify add on orders as orders placed more than 5 min after specimen received
  # Identify specimens with missing collections times as those with collection time defaulted to receive time
  raw_scc <- raw_scc %>%
    mutate(AddOnMaster = ifelse(difftime(ORDERING_DATE, RECEIVE_DATE, units = "mins") > 5, 
                                "AddOn", "Original"),
           MissingCollect = ifelse(CollectToReceive == 0, TRUE, FALSE))
  
  # Determine target TAT based on test, priority, and patient setting
  raw_scc <- raw_scc %>%
    mutate(Concate1 = paste(Test, DashboardPriority),
           Concate2 = paste(Test, DashboardPriority, MasterSetting),
           ReceiveResultTarget = ifelse(!is.na(match(Concate2, tat_targets$Concate)),
                                        tat_targets$ReceiveToResultTarget[match(Concate2, tat_targets$Concate)],
                                        ifelse(!is.na(match(Concate1, tat_targets$Concate)), 
                                               tat_targets$ReceiveToResultTarget[match(Concate1, tat_targets$Concate)], 
                                               tat_targets$ReceiveToResultTarget[match(Test, tat_targets$Concate)])),
           CollectResultTarget = ifelse(!is.na(match(Concate2, tat_targets$Concate)), 
                                         tat_targets$CollectToResultTarget[match(Concate2, tat_targets$Concate)], 
                                         ifelse(!is.na(match(Concate1, tat_targets$Concate)), 
                                                tat_targets$CollectToResultTarget[match(Concate1, tat_targets$Concate)],
                                                tat_targets$CollectToResultTarget[match(Test, tat_targets$Concate)])),
           ReceiveResultInTarget = ReceiveToResult <= ReceiveResultTarget, 
           CollectResultInTarget = CollectToResult <= CollectResultTarget)
  
  # Identify and remove duplicate tests
  raw_scc <- raw_scc %>%
    mutate(Concate3 = paste(LAST_NAME, FIRST_NAME,
                            ORDER_ID, TEST_NAME, 
                            COLLECTION_DATE, RECEIVE_DATE, VERIFIED_DATE))
  
  raw_scc <- raw_scc[!duplicated(raw_scc$Concate3), ]
  
  # Identify which labs to include in TAT analysis
  # Exclude add on orders, orders from "other" settings, orders with collect or receive times after result, or orders with missing collect, receive, or result timestamps
  raw_scc <- raw_scc %>% 
    mutate(TATInclude = ifelse(AddOnMaster != "Original" | MasterSetting == "Other" | 
                                 CollectToResult < 0 | ReceiveToResult < 0 | 
                                 is.na(CollectToResult) | is.na(ReceiveToResult), FALSE, TRUE))

  scc_master <- raw_scc[ , c("Ward", "WARD_NAME", "WardandName",
                             "ORDER_ID", "REQUESTING_DOC NAME", "MPI", "WORK SHIFT",
                             "TEST_NAME", "Test", "Division", "PRIORITY", 
                             "Site", "ICU", "CLINIC_TYPE", 
                             "Setting", "SettingRollUp", "MasterSetting", "DashboardSetting",
                             "AdjPriority", "DashboardPriority",
                             "ORDERING_DATE", "COLLECTION_DATE", "RECEIVE_DATE", "VERIFIED_DATE",
                             "ResultedDate", 
                             "CollectToReceive", "ReceiveToResult", "CollectToResult", 
                             "AddOnMaster", "MissingCollect", 
                             "ReceiveResultTarget", "CollectResultTarget", 
                             "ReceiveResultInTarget", "CollectResultInTarget", 
                             "TATInclude")]
  
  colnames(scc_master) <- c("LocCode", "LocName", "LocConcat",
                            "OrderID", "RequestMD", "MSMRN", "WorkShift", 
                            "TestName", "Test", "Division", "OrderPriority", 
                            "Site", "ICU", "LocType", 
                            "Setting", "SettingRollUp", "MasterSetting", "DashboardSetting",
                            "AdjPriority", "DashboardPriority",
                            "OrderTime", "CollectTime", "ReceiveTime", "ResultTime", 
                            "ResultDate", 
                            "CollectToReceiveTAT", "ReceiveToResultTAT", "CollectToResultTAT", 
                            "AddOnMaster", "MissingCollect", 
                            "ReceiveResultTarget", "CollectResultTarget", 
                            "ReceiveResultInTarget", "CollectResultInTarget", 
                            "TATInclude")

  
  ## SUNQUEST DATA PROCESSING ----------
  # Sunquest lookup references ----------------------------------------------
  # Crosswalk labs included and remove out of scope labs
  raw_sun <- left_join(raw_sun, test_code[ , c("Test", "SUN_TestCode", "Division")], by = c("TestCode" = "SUN_TestCode"))
  raw_sun$TestCode <- as.factor(raw_sun$TestCode)
  raw_sun$Division <- as.factor(raw_sun$Division)
  raw_sun <- raw_sun %>%
    mutate(TestIncl = ifelse(is.na(Test), FALSE, TRUE))
  
  raw_sun <- raw_sun[raw_sun$TestIncl == TRUE, ]
  
  # Crosswalk units and identify ICUs
  raw_sun <- raw_sun %>%
    mutate(LocandName = paste(LocCode, LocName))
  
  raw_sun <- left_join(raw_sun, sun_icu[ , c("Concatenate", "ICU")], by = c("LocandName" = "Concatenate"))
  raw_sun[is.na(raw_sun$ICU), "ICU"] <- FALSE
  
  # Crosswalk unit type
  raw_sun <- left_join(raw_sun, sun_setting, by = c("LocType" = "LocType"))
  
  # Crosswalk site name
  raw_sun <- left_join(raw_sun, mshs_site, by = c("HospCode" = "DataSite"))
  
  # Sunquest data formatting --------------------------------------------
  raw_sun[c("HospCode", "BatTstCode", "BATName", "TestCode", "TSTName",
             "LocType", "LocCode", "LocName", 
             "SpecimenPriority", "PhysName", "SHIFT",
             "ReceiveTech", "ResultTech", "PerformingLabCode",
             "Test", "LocandName", "Setting", "SettingRollUp", "Site")] <- 
    lapply(raw_sun[c("HospCode", "BatTstCode", "BATName", "TestCode", "TSTName",
                     "LocType", "LocCode", "LocName","SpecimenPriority", "PhysName", 
                     "SHIFT", "ReceiveTech", "ResultTech", "PerformingLabCode", "Test", 
                     "LocandName", "Setting", "SettingRollUp", "Site")], as.factor)
  
  # Fix any timestamps that weren't imported correctly and then format as date/time
  raw_sun[c("OrderDateTime", "CollectDateTime", "ReceiveDateTime", "ResultDateTime")] <- 
    lapply(raw_sun[c("OrderDateTime", "CollectDateTime", "ReceiveDateTime", "ResultDateTime")], 
           function(x) ifelse(!is.na(x) & str_detect(x, "\\*.*\\*")  == TRUE, str_replace(x, "\\*.*\\*", ""), x))

  raw_sun[c("OrderDateTime", "CollectDateTime", "ReceiveDateTime", "ResultDateTime")] <- 
    lapply(raw_sun[c("OrderDateTime", "CollectDateTime", "ReceiveDateTime", "ResultDateTime")], 
           as.POSIXct, tz = "UTC", format = "%m/%d/%Y %H:%M:%S")
  
  # Add a column for Resulted date for later use in repository
  raw_sun <- raw_sun %>%
    mutate(ResultedDate = as.Date(ResultDateTime, format = "%m/%d/%Y"))

  # Sunquest data preprocessing --------------------------------------------------
  # Update patient setting to reflect ICU/Non-ICU and update priorities for ED and ICU labs
  raw_sun <- raw_sun %>%
    mutate(MasterSetting = ifelse(SettingRollUp == "ED", "ED", 
                                  ifelse(SettingRollUp == "Amb", "Amb", 
                                         ifelse(SettingRollUp == "IP" & ICU == TRUE, "ICU",
                                                ifelse(SettingRollUp == "IP" & ICU == FALSE, "IP Non-ICU", "Other")))), 
           DashboardSetting = ifelse(MasterSetting == "ED" | MasterSetting == "ICU", "ED & ICU", MasterSetting))
  
  # Update priority to reflect ED/ICU as stat and create Master Priority for labs where all specimens are treated as stat
  raw_sun <- raw_sun %>%
    mutate(AdjPriority = ifelse(MasterSetting != "ED" & MasterSetting != "ICU" & is.na(SpecimenPriority), "Routine",
                                ifelse(MasterSetting == "ED" | MasterSetting == "ICU" | SpecimenPriority == "S", 
                                       "Stat", "Routine")),
           DashboardPriority = ifelse(tat_targets$Priority[match(Test, tat_targets$Test)] == "All", 
                                      "All", AdjPriority))

  # Calculate turnaround times
  raw_sun <- raw_sun %>%
    mutate(CollectToReceive = ReceiveDateTime - CollectDateTime,
           ReceiveToResult = ResultDateTime - ReceiveDateTime,
           CollectToResult = ResultDateTime - CollectDateTime)
  
  raw_sun[c("CollectToReceive", "ReceiveToResult", "CollectToResult")] <- 
    lapply(raw_sun[c("CollectToReceive", "ReceiveToResult", "CollectToResult")], 
           as.numeric, units = "mins")
  
  # Identify add on orders as orders placed more than 5 min after specimen received
  # Identify specimens with missing collections times as those with collection time defaulted to order time
  raw_sun <- raw_sun %>%
    mutate(AddOnMaster = ifelse(difftime(OrderDateTime, ReceiveDateTime, units = "mins") > 5, "AddOn", "Original"),
           MissingCollect = ifelse(CollectDateTime == OrderDateTime, TRUE, FALSE))
  
  # Determine target TAT based on test, priority, and patient setting
  raw_sun <- raw_sun %>%
    mutate(Concate1 = paste(Test, DashboardPriority), 
           Concate2 = paste(Test, DashboardPriority, MasterSetting),
           ReceiveResultTarget = ifelse(!is.na(match(Concate2, tat_targets$Concate)), 
                                        tat_targets$ReceiveToResultTarget[match(Concate2, tat_targets$Concate)], 
                                        ifelse(!is.na(match(Concate1, tat_targets$Concate)), 
                                               tat_targets$ReceiveToResultTarget[match(Concate1, tat_targets$Concate)],
                                               tat_targets$ReceiveToResultTarget[match(Test, tat_targets$Concate)])),
           CollectResultTarget = ifelse(!is.na(match(Concate2, tat_targets$Concate)), 
                                        tat_targets$CollectToResultTarget[match(Concate2, tat_targets$Concate)], 
                                        ifelse(!is.na(match(Concate1, tat_targets$Concate)), 
                                               tat_targets$CollectToResultTarget[match(Concate1, tat_targets$Concate)], 
                                               tat_targets$CollectToResultTarget[match(Test, tat_targets$Concate)])),
           ReceiveResultInTarget = ReceiveToResult <= ReceiveResultTarget,
           CollectResultInTarget = CollectToResult <= CollectResultTarget)
  
  # Identify and remove duplicate tests
  raw_sun <- raw_sun %>%
    mutate(Concate3 = paste(PtNumber, HISOrderNumber, TSTName, 
                            CollectDateTime, ReceiveDateTime, ResultDateTime))
  
  raw_sun <- raw_sun[!duplicated(raw_sun$Concate3), ]
  
  # Identify which labs to include in TAT analysis
  # Exclude add on orders, orders from "other" settings, orders with collect or receive times after result, or orders with missing collect, receive, or result timestamps
  raw_sun <- raw_sun %>%
    mutate(TATInclude = ifelse(AddOnMaster != "Original" | MasterSetting == "Other" | 
                                 CollectToResult < 0 | ReceiveToResult < 0 | 
                                 is.na(CollectToResult) | is.na(ReceiveToResult), FALSE, TRUE))
  
  sun_master <- raw_sun[ ,c("LocCode", "LocName", "LocandName", 
                            "HISOrderNumber", "PhysName", "PtNumber", "SHIFT",            
                            "TSTName", "Test", "Division", "SpecimenPriority", 
                            "Site", "ICU", "LocType",               
                            "Setting", "SettingRollUp", "MasterSetting", "DashboardSetting", 
                            "AdjPriority", "DashboardPriority", 
                            "OrderDateTime", "CollectDateTime", "ReceiveDateTime", "ResultDateTime", 
                            "ResultedDate", 
                            "CollecttoReceive", "ReceivetoResult", "CollecttoResult", 
                            "AddOnMaster", "MissingCollect", 
                            "ReceiveResultTarget", "CollectResultTarget", 
                            "ReceiveResultInTarget", "CollectResultInTarget", 
                            "TATInclude")]
  
  colnames(sun_master) <- c("LocCode", "LocName", "LocConcat", 
                            "OrderID", "RequestMD", "MSMRN", "WorkShift", 
                            "TestName", "Test", "Division", "OrderPriority", 
                            "Site", "ICU", "LocType", 
                            "Setting", "SettingRollUp", "MasterSetting", "DashboardSetting", 
                            "AdjPriority", "DashboardPriority",
                            "OrderTime", "CollectTime", "ReceiveTime", "ResultTime", 
                            "ResultDate", 
                            "CollectToReceiveTAT", "ReceiveToResultTAT", "CollectToResultTAT", 
                            "AddOnMaster", "MissingCollect", 
                            "ReceiveResultTarget", "CollectResultTarget", 
                            "ReceiveResultInTarget", "CollectResultInTarget", 
                            "TATInclude")
  
  
  scc_sun_master <- rbind(scc_master, sun_master)
  
  scc_sun_master[c("LocConcat", "RequestMD", "WorkShift", "AdjPriority", "AddOnMaster")] <-
    lapply(scc_sun_master[c("LocConcat", "RequestMD", "WorkShift", "AdjPriority", "AddOnMaster")], as.factor)
  
  scc_sun_master$Site <- factor(scc_sun_master$Site, levels = site_order)
  scc_sun_master$Test <- factor(scc_sun_master$Test, levels = cp_micro_lab_order)
  scc_sun_master$MasterSetting <- factor(scc_sun_master$MasterSetting, levels = pt_setting_order)
  scc_sun_master$DashboardSetting <- factor(scc_sun_master$DashboardSetting, levels = pt_setting_order2)
  scc_sun_master$DashboardPriority <- factor(scc_sun_master$DashboardPriority, levels = dashboard_priority_order)
  
  scc_sun_list <- list(raw_scc, raw_sun, scc_sun_master)
  
}
```

```{r Data Preprocessing and Merging for SCC and Sunquest data, message = FALSE, warning = FALSE, echo = FALSE}
scc_sun_wday_list <- preprocess_scc_sun(SCC_Weekday, SQ_Weekday)
scc_wday <- scc_sun_wday_list[[1]]
sun_wday <- scc_sun_wday_list[[2]]
scc_sun_wday_master <- scc_sun_wday_list[[3]]

if (is.null(SQ_Not_Weekday) & is.null(SQ_Not_Weekday)) {
  include_not_wday <- FALSE
  scc_sun_not_wday_list <- NULL
  scc_not_wday <- NULL
  sun_not_wday <- NULL
  scc_sun_not_wday_master <- NULL
} else {
  include_not_wday <- TRUE
  scc_sun_not_wday_list <- preprocess_scc_sun(SCC_Not_Weekday, SQ_Not_Weekday)
  scc_not_wday <- scc_sun_not_wday_list[[1]]
  sun_not_wday <- scc_sun_not_wday_list[[2]]
  scc_sun_not_wday_master <- scc_sun_not_wday_list[[3]]
}

# Remove labs from master data frame that were resulted at exactly midnight on next morning or prior day
# Custom function to determine correct number of dates
correct_scc_result_dates <- function(data, number_days) {
  all_resulted_dates_vol <- data %>%
    select(MSMRN, ResultDate) %>%
    group_by(ResultDate) %>%
    summarize(VolLabs = n()) %>%
    arrange(desc(VolLabs)) %>%
    ungroup()
  
  correct_dates <- all_resulted_dates_vol$ResultDate[1:number_days]
  
  new_data <- data[data$ResultDate %in% correct_dates, ]
  
  return(new_data)
}

# Update preprocessed data to only include correct dates ----------------------
scc_sun_wday_master <- correct_scc_result_dates(scc_sun_wday_master, 1)

if (scenario == 1) {
  scc_sun_not_wday_master <- correct_scc_result_dates(scc_sun_not_wday_master, 3)
} else if (scenario == 2) {
  scc_sun_not_wday_master <- correct_scc_result_dates(scc_sun_not_wday_master, 2)
} else if (scenario == 3) {
  scc_sun_not_wday_master <- correct_scc_result_dates(scc_sun_not_wday_master, 1)
}


# Determine resulted date for weekday labs
wday_dates_df <- scc_sun_wday_master[ , "ResultDate"]
wday_result_date <- format(unique(wday_dates_df$ResultDate), format = "%a %m/%d/%y")


if (is.null(scc_sun_not_wday_master) == FALSE) {
  # not_wday_dates_df <- scc_sun_not_wday_master[(hour(scc_sun_not_wday_master$ResultTime) !=0 & minute(scc_sun_not_wday_master$ResultTime) !=0) & is.na(scc_sun_not_wday_master$ResultTime) == FALSE, "ResultTime"]
  # not_wday_dates_df$dates <- date(not_wday_dates_df$ResultTime)
  # not_wday_result_date <- unique(date(not_wday_dates_df$ResultTime))
  # wkend_holiday_result_date <- ifelse(length(not_wday_result_date) == 1, format(not_wday_result_date, format = "%a %m/%d/%y"), paste0(format(not_wday_result_date[length(not_wday_result_date)], format = "%a %m/%d/%y"), "-", format(not_wday_result_date[1], format = "%a %m/%d/%y")))
  not_wday_dates_df <- scc_sun_not_wday_master[ , "ResultDate"]
  not_wday_result_date <- unique(date(not_wday_dates_df$ResultDate))
  wkend_holiday_result_date <- ifelse(length(not_wday_result_date) == 1, format(not_wday_result_date, format = "%a %m/%d/%y"), paste0(format(not_wday_result_date[length(not_wday_result_date)], format = "%a %m/%d/%y"), "-", format(not_wday_result_date[1], format = "%a %m/%d/%y")))
} else {
  wkend_holiday_result_date <- NULL
}
```

```{r Begin combining and summarizing data for historical repository, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE}
# Bind weekday and non-weekday data, if any exists
if (is.null(scc_sun_not_wday_master) == FALSE) {
  scc_sun_all_days <- rbind(scc_sun_wday_master, scc_sun_not_wday_master)
} else {
  scc_sun_all_days <- scc_sun_wday_master
}

# Summarize data for export to historical repo
scc_sun_all_days_subset <- scc_sun_all_days %>%
  group_by(Site, ResultDate, Test, Division, 
           Setting, SettingRollUp, MasterSetting, DashboardSetting,
           OrderPriority, AdjPriority, DashboardPriority,
           ReceiveResultTarget, CollectResultTarget) %>%
  summarize(TotalResulted = n(), TotalResultedTAT = sum(TATInclude), 
            TotalReceiveResultInTarget = sum(ReceiveResultInTarget[TATInclude == TRUE]), 
            TotalCollectResultInTarget = sum(CollectResultInTarget[TATInclude == TRUE]), 
            TotalAddOnOrder = sum(AddOnMaster == "AddOn"), 
            TotalMissingCollections = sum(MissingCollect)) %>%
  arrange(Site, ResultDate) %>%
  ungroup()

existing_repo <- read_excel(choose.files(default = user_path, caption = "Select SCC and Sunquest Historical Repository"),
                              sheet = 1, col_names = TRUE)
  
# Convert ResultDate from date-time to date
existing_repo <- existing_repo %>%
   mutate(ResultDate  = date(ResultDate))
  
# Bind new data with existing repository
scc_sun_repo <- rbind(existing_repo, scc_sun_all_days_subset)
  
# Remove any duplicates
scc_sun_repo <- unique(scc_sun_repo)


# Determine earliest and latest date in repository for use in file name
start_date <- format(min(scc_sun_repo$ResultDate), "%m-%d-%y")
end_date <- format(max(scc_sun_repo$ResultDate), "%m-%d-%y")

# Add data to historical repository
write_xlsx(scc_sun_repo, path = paste0(user_directory, "\\SCC Sunquest Script Repo", "\\Hist Repo Test ", start_date, " to ", end_date, " Created ", Sys.Date(), ".xlsx"))


test_results <- scc_sun_all_days_subset %>%
  group_by(Site, Test, DashboardPriority, DashboardSetting) %>%
  summarize(ReceiveResultPercent = sum(TotalReceiveResultInTarget)/sum(TotalResultedTAT)*100, CollectResultPercent = sum(TotalCollectResultInTarget)/sum(TotalResultedTAT)*100)

```


```{r Custom function for preprocessing raw Powerpath data and calling function, warning = FALSE, message = FALSE, echo = FALSE}
#standardize column names
#GI_codes_column_name <- c("Specimen_code", "GI.Codes.Must.Include.in.Analysis..All.GI.Biopsies.")
#colnames(GI_Codes) <- GI_codes_column_name
# create an extra column for old Fcaility and change the facility from MSSM to MSH 
PP_Weekday$Facility_Old <- PP_Weekday$Facility
PP_Weekday$Facility[PP_Weekday$Facility_Old == "MSS"] <- "MSH" 
PP_Weekday$Facility[PP_Weekday$Facility_Old == "STL"] <- "SL"
PP_Weekday$spec_group[PP_Weekday$spec_group == "BREAST"] <- "Breast"
#------------------------------Extract the Cytology GYN and NON-GYN Data Only------------------------------#
#Cytology
#Keep the cyto gyn and cyto non-gyn

Cytology_Weekday_Raw <- PP_Weekday[which((PP_Weekday$spec_group=="CYTO NONGYN" | PP_Weekday$spec_group=="CYTO GYN") & PP_Weekday$spec_sort_order=="A"),]

Cytology_Not_Weekday_Raw <- PP_Not_Weekday[which((PP_Not_Weekday$spec_group=="CYTO NONGYN" | PP_Not_Weekday$spec_group=="CYTO GYN") & PP_Weekday$spec_sort_order=="A"),]

#------------------------------Extract the All Breast and GI Specimens Data Only------------------------------#

#Merge the exclusion/inclusion cloumn into the modified powerpath Dataset for weekdays and not weekdays

PP_Weekday <- merge(x = PP_Weekday, y= GI_Codes, all.x = TRUE)

#find case numbers with GI_Code = exclude
Exclude_GI_Codes_DF <- PP_Weekday[which((PP_Weekday$spec_group=="GI") & (PP_Weekday$GI.Codes.Must.Include.in.Analysis..All.GI.Biopsies.=="Exclude")),]

Must_Exclude_CNum <- unique(Exclude_GI_Codes_DF$Case_no)

#this dataframe has all the GI specimens and biopsies that should be excluded 
#it includes any biopsy that came with another excluded code
#this DF is only for our reference
Must_Exclude_CNum_df <- PP_Weekday[which(PP_Weekday$Case_no %in% Must_Exclude_CNum),]

Surgical_Pathology_Weekday <- PP_Weekday[which((((PP_Weekday$spec_group=="GI") & (!(PP_Weekday$Case_no %in% Must_Exclude_CNum))) | (PP_Weekday$spec_group=="Breast")) & PP_Weekday$spec_sort_order=="A"),]

#PP_Not_Weekday_Excl <- merge(x = PP_Not_Weekday, y= GI_Codes, all.x = TRUE)

#find MRNs with GI_Code = exclude
#Must_Exclude_MRN_Not_Weekday <- PP_Not_Weekday_Excl[which((PP_Not_Weekday_Excl$spec_group=="GI") & (PP_Not_Weekday_Excl$GI.Codes.Must.Include.in.Analysis..All.GI.Biopsies.=="Exclude")),]

#Must_Exclude_MRN_Not_Weekday <- unique(Must_Exclude_MRN_Not_Weekday$MRN)

#Surgical_Pathology_Not_Weekday <- PP_Not_Weekday_Excl[which(((PP_Not_Weekday_Excl$spec_group=="GI") & (!(PP_Not_Weekday_Excl$MRN %in% Must_Exclude_MRN_Not_Weekday)) & (PP_Not_Weekday_Excl$spec_sort_order=="A")) | ((PP_Not_Weekday_Excl$spec_group=="Breast") & (PP_Not_Weekday_Excl$spec_sort_order=="A"))),]

#-----------Reporting Dates-----------#
#1. Weekday Date
Report_Date_Weekday <- unique(as.timeDate(format(PP_Weekday$signed_out_date, "%m/%d/%Y")))
Report_Date_Weekday_Day <- unique(dayOfWeek(Report_Date_Weekday))

#2. Weekend/Holiday Dates
if (is.null(PP_Not_Weekday)){
  Report_Date_Weekend <- NULL
  Report_Date_Weekend_Day <- NULL
}else {
  Report_Date_Weekend <- unique(as.timeDate(format(PP_Not_Weekday$signed_out_date, "%m/%d/%Y")))
  Report_Date_Weekend_Day <- unique(dayOfWeek(Report_Date_Weekend))
}

#------------------------------Data Pre-Processing------------------------------#
############Create a function for Data pre-processing############

pre_processing_pp <- function(Raw_Data){
  if (is.null(Raw_Data) || nrow(Raw_Data)==0){
    Raw_Data_PS <- NULL
    #Raw_Data_PS_Target <- NULL
    Raw_Data_New <- NULL
    Raw_Data_New_Cases_Signed <- NULL
    Raw_Data_New_Cases_Signed_Stratified <- NULL
    Raw_Data_New_Patient_Metric <- NULL
    Raw_Data_New_Lab_Metric <- NULL
    Raw_Data_New_Lab_Metric_V2 <- NULL
    Processed_Data_Table_V2 <-NULL
    Processed_Data_Table <- NULL
    Summarized_Table <- NULL
    Return_Tables <- NULL
  } else {
    #vlookup the Rev_Center and its corresponding patient setting for the PowerPath Data
    Raw_Data_PS <- merge(x=Raw_Data, y=Patient_Setting, all.x = TRUE ) 

    #vlookup targets based on spec_group and patient setting
    Raw_Data_New <- merge(x=Raw_Data_PS, y=TAT_Targets, all.x = TRUE, by = c("spec_group","Patient.Setting"))

    #check if any of the dates was imported as char
    if (is.character(Raw_Data_New$Collection_Date)){
      Raw_Data_New$Collection_Date <- as.numeric(Raw_Data_New$Collection_Date)
      Raw_Data_New$Collection_Date <- as.Date(Raw_Data_New$Collection_Date, origin = "1899-12-30")
    } else {
      Raw_Data_New$Collection_Date <- Raw_Data_New$Collection_Date
    }
    
    #Change all Dates into POSIXct format to start the calculations
    
    Raw_Data_New[c("Case_created_date","Collection_Date","Received_Date","signed_out_date")] <- lapply(Raw_Data_New[c("Case_created_date","Collection_Date","Received_Date","signed_out_date")], as.POSIXct,tz="", format='%m/%d/%y %I:%M %p')
    
    #add columns for calculations: collection to signed out and received to signed out
    #collection to signed out
    #All days in the calendar
    Raw_Data_New$Collection_to_signed_out <- as.numeric(difftime(Raw_Data_New$signed_out_date, Raw_Data_New$Collection_Date, units = "days"))
    
    #recieve to signed out
    #without weekends and holidays
    Raw_Data_New$Received_to_signed_out <- bizdays(Raw_Data_New$Received_Date, Raw_Data_New$signed_out_date)
    
    #Calculate Average for Collection to signed out and number of cases signed
    
    Raw_Data_New_Cases_Signed <- summarise(group_by(Raw_Data_New,spec_group, Patient.Setting), No_Cases_Signed = n())

    Raw_Data_New_Cases_Signed_Stratified <- summarise(group_by(Raw_Data_New,spec_group, Facility, Patient.Setting), No_Cases_Signed = n())    
    Raw_Data_New_Cases_Signed_Stratified <- dcast(Raw_Data_New_Cases_Signed_Stratified, spec_group + Patient.Setting ~ Facility, value.var = "No_Cases_Signed" )

    Raw_Data_New_Cases_Signed_Stratified[is.na(Raw_Data_New_Cases_Signed_Stratified)] <- 0
        
    Raw_Data_New_Patient_Metric <- summarise(group_by(Raw_Data_New,spec_group, Facility,Patient.Setting), Avg_Collection_to_Signed_out=format(ceiling(mean(Collection_to_signed_out, na.rm = TRUE))))
    
    Raw_Data_New_Patient_Metric <- dcast(Raw_Data_New_Patient_Metric, spec_group + Patient.Setting ~ Facility, value.var = "Avg_Collection_to_Signed_out" )
    
    
    #Calculate % Receive to result TAT within target
    Raw_Data_New_Lab_Metric <-summarise(group_by(Raw_Data_New,spec_group, Facility,Patient.Setting), Received_to_Signed_out_within_target = format(round(sum(Received_to_signed_out <= Received.to.signed.out.target..Days., na.rm = TRUE)/sum(Received_to_signed_out >= 0, na.rm = TRUE),2)))
    
    Raw_Data_New_Lab_Metric <- dcast(Raw_Data_New_Lab_Metric, spec_group + Patient.Setting ~ Facility, value.var = "Received_to_Signed_out_within_target" )
    
    Raw_Data_New_Lab_Metric_V2 <- summarise(group_by(Raw_Data_New,spec_group,Patient.Setting), Received_to_Signed_out_within_target = format(round(sum(Received_to_signed_out <= Received.to.signed.out.target..Days., na.rm = TRUE)/sum(Received_to_signed_out >= 0, na.rm = TRUE),2)))

    Summarized_Table <- summarise(group_by(Raw_Data_New,Spec_code, spec_group, Facility,Patient.Setting, Rev_ctr,as.Date(signed_out_date),weekdays(as.Date(signed_out_date)),Received.to.signed.out.target..Days.,Collected.to.signed.out.target..Days.),  No_Cases_Signed = n(), Lab_Metric_TAT_Avg = round(mean(Received_to_signed_out, na.rm = TRUE),0), Lab_Metric_TAT_med = round(median(Received_to_signed_out, na.rm = TRUE),0), Lab_Metric_TAT_sd = round(sd(Received_to_signed_out, na.rm = TRUE),1), Lab_Metric_within_target = format(round(sum(Received_to_signed_out <= Received.to.signed.out.target..Days., na.rm = TRUE)/sum(Received_to_signed_out >= 0, na.rm = TRUE),2)), Patient_Metric_TAT_avg=format(ceiling(mean(Collection_to_signed_out, na.rm = TRUE))), Patient_Metric_TAT_med = round(median(Collection_to_signed_out, na.rm = TRUE),0), Patient_Metric_TAT_sd =round(sd(Collection_to_signed_out, na.rm = TRUE),1))
    
    #here I will merge number of cases signed, received to result TAT, and acollect to result TAT calcs into one table
    
    Processed_Data_Table <- left_join(full_join(Raw_Data_New_Cases_Signed, Raw_Data_New_Lab_Metric), Raw_Data_New_Patient_Metric, by = c("spec_group", "Patient.Setting"))
    Processed_Data_Table <- Processed_Data_Table[!(Processed_Data_Table$Patient.Setting == "Other"),]
    Processed_Data_Table_V2 <- left_join(full_join(Raw_Data_New_Cases_Signed, Raw_Data_New_Lab_Metric_V2), Raw_Data_New_Patient_Metric, by = c("spec_group", "Patient.Setting"))
    Processed_Data_Table_V2 <- Processed_Data_Table_V2[!(Processed_Data_Table_V2$Patient.Setting == "Other"),]
    Return_Tables <- list(Processed_Data_Table, Processed_Data_Table_V2,Raw_Data_New_Cases_Signed_Stratified,Raw_Data_New,Summarized_Table) 
  }
  return(Return_Tables)
}

Cytology_Table_Weekday <- pre_processing_pp(Cytology_Weekday_Raw)[[1]]
Cytology_Table_Not_Weekday <- pre_processing_pp(Cytology_Not_Weekday_Raw)[[1]]
Cytology_Table_Weekday_V2 <- pre_processing_pp(Cytology_Weekday_Raw)[[2]]
Cytology_Table_Not_Weekday_V2 <- pre_processing_pp(Cytology_Not_Weekday_Raw)[[2]]
Cytology_Stratified_Vol_Weekday <- pre_processing_pp(Cytology_Weekday_Raw)[[3]]
Cytology_Stratified_Vol_Not_Weekday <- pre_processing_pp(Cytology_Not_Weekday_Raw)[[3]]
Cytology_Table_Weekday_Raw <- pre_processing_pp(Cytology_Weekday_Raw)[[4]]

Surgical_Pathology_Table_Weekday <- pre_processing_pp(Surgical_Pathology_Weekday)[[1]]
#Surgical_Pathology_Table_Not_Weekday <- pre_processing_pp(Surgical_Pathology_Not_Weekday)[[1]]
Surgical_Pathology_Stratified_Vol_Weekday <- pre_processing_pp(Surgical_Pathology_Weekday)[[3]]
#Surgical_Pathology_Stratified_Vol_Not_Weekday <- pre_processing_pp(Surgical_Pathology_Not_Weekday)[[3]]
Surgical_Pathology_Table_Weekday_Raw <- pre_processing_pp(Surgical_Pathology_Weekday)[[4]]

```

```{r Cytology backlog preprocessing and analysis, echo=FALSE, warning = FALSE, message=FALSE}
#Cytology backlog Calculation

#vlookup the Rev_Center and its corresponding patient setting for the PowerPath Data
Cytology_Backlog_PS <- merge(x=Cytology_Backlog_Raw, y=Patient_Setting, all.x = TRUE ) 

#vlookup targets based on spec_group and patient setting
Cytology_Backlog_PS_Target <- merge(x=Cytology_Backlog_PS, y=TAT_Targets, all.x = TRUE, by = c("spec_group","Patient.Setting"))

#Keep the cyto gyn and cyto non-gyn
Cytology_Backlog <- Cytology_Backlog_PS_Target[which(Cytology_Backlog_PS_Target$spec_group=="CYTO NONGYN" | Cytology_Backlog_PS_Target$spec_group=="CYTO GYN"),]

#Change all Dates into POSIXct format to start the calculations
Cytology_Backlog[c("Case_created_date","Collection_Date","Received_Date","signed_out_date")] <- lapply(Cytology_Backlog[c("Case_created_date","Collection_Date","Received_Date","signed_out_date")], as.POSIXct, tz="" ,format='%m/%d/%y %I:%M %p')

#Backlog Calculations: Date now - case created date
#without weekends and holidays, subtract one so we don't include today's date
Cytology_Backlog$Backlog <- bizdays(Cytology_Backlog$Case_created_date, Today)-1

#Case Volume
Cytology_Backlog_Volume <- summarise(group_by(Cytology_Backlog,spec_group), Cyto_Backlog = format(round(sum(Backlog > Received.to.signed.out.target..Days., na.rm = TRUE),0)), percentile_25th = format(ceiling(quantile(Backlog[Backlog > Received.to.signed.out.target..Days.], prob = 0.25, na.rm = TRUE))), percentile_50th = format(ceiling(quantile(Backlog[Backlog > Received.to.signed.out.target..Days.], prob = 0.5, na.rm = TRUE))), Maximum = format(ceiling(max(Backlog[Backlog > Received.to.signed.out.target..Days.], na.rm = TRUE))))

Cytology_Backlog_Volume$spec_group <- factor(Cytology_Backlog_Volume$spec_group, levels = c("CYTO GYN", "CYTO NONGYN"))

#Days of work
Cytology_case_Volume_DOW <- as.numeric(Cytology_Backlog_Volume$Cyto_Backlog[1])/80

#accessioned volume
#1. Find the date that we need to report --> the date of the last weekday
Accessioned_Date <- as.Date(Cytology_Weekday_Raw$signed_out_date[1])
#2. count the accessioned volume that was accessioned on that date from the cytology report
Cytology_Weekday_Raw$Accessioned_Date_Only <- as.Date(Cytology_Weekday_Raw$Received_Date)
Cytology_Accessioned_Vol1 <- summarise(group_by(Cytology_Weekday_Raw,spec_group), Cyto_Acc_Vol1 = as.numeric(sum(Accessioned_Date == Accessioned_Date_Only, na.rm = TRUE)))

Cytology_Accessioned_Vol1$spec_group <- factor(Cytology_Accessioned_Vol1$spec_group, levels = c("CYTO GYN", "CYTO NONGYN"))

#3. count the accessioned volume that was accessioned on that date from the backlog report
Cytology_Backlog_Raw$Accessioned_Date_Only <- as.Date(Cytology_Backlog_Raw$Received_Date)
Cytology_Accessioned_Vol2 <- summarise(group_by(Cytology_Backlog_Raw,spec_group), Cyto_Acc_Vol2 = as.numeric(sum(Accessioned_Date == Accessioned_Date_Only, na.rm = TRUE)))
#4. sum the two counts
Cytology_Accessioned_Vol3 <- merge(x= Cytology_Accessioned_Vol1, y= Cytology_Accessioned_Vol2)
Cytology_Accessioned_Vol3$Total_Accessioned_Volume <- Cytology_Accessioned_Vol3$Cyto_Acc_Vol1 + Cytology_Accessioned_Vol3$Cyto_Acc_Vol2
Cytology_Accessioned_Vol3$Cyto_Acc_Vol1 <- NULL
Cytology_Accessioned_Vol3$Cyto_Acc_Vol2 <- NULL
Backlog_Accessioned_Table <- merge(x = Cytology_Accessioned_Vol3, y = Cytology_Backlog_Volume, all = TRUE)

#as extra precaution

Table_Template_backlog_Accessioned <- data.frame(matrix(ncol=6, nrow = 2)) 
colnames(Table_Template_backlog_Accessioned) <- c("spec_group", "Total_Accessioned_Volume", "Cyto_Backlog", "percentile_25th", "percentile_50th", "Maximum")
Table_Template_backlog_Accessioned[1] <- c('CYTO GYN','CYTO NONGYN')

Backlog_Accessioned_Table_New <- merge(x = Table_Template_backlog_Accessioned, y=Backlog_Accessioned_Table, all.y=TRUE)
Backlog_Accessioned_Table_New2 <- merge(x = Table_Template_backlog_Accessioned[1], y= Backlog_Accessioned_Table, all.x = TRUE, by = c("spec_group"))

Backlog_Accessioned_Table_New2[is.na(Backlog_Accessioned_Table_New2)] <- 0

#added this line to delete the cyto gyn from the table until we get correct data
Backlog_Accessioned_Table_New3 <- Backlog_Accessioned_Table_New2[-c(1),]
```

```{r Add the current summary to the historical repo for cytology/pathology, echo = FALSE, warning = FALSE, message = FALSE, eval = FALSE}

#1. import the historical repo
Historical_Repo <- read_excel(paste0(user_directory,"/AP & Cytology Historical Repo/" ,"Historical_Repo_Surgical_Pathology","_",Yesterday, ".xlsx"))

#Historical_Repo$X <- NULL
#Historical_Repo$Signed_out_date_only <- as.Date(Historical_Repo$Signed_out_date_only, format='%m/%d/%Y')

#2. append all the summary tables together
Cytology_Table_Weekday_Summarized <- pre_processing_pp(Cytology_Weekday_Raw)[[5]]
Surgical_Pathology_Table_Weekday_Summarized <- pre_processing_pp(Surgical_Pathology_Weekday)[[5]]

current_summary <- rbind(Cytology_Table_Weekday_Summarized, Surgical_Pathology_Table_Weekday_Summarized)

current_summary <- as.data.frame(current_summary)

#standardize the name
colnames(current_summary) <- c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr", "Signed_out_date_only", "Signed_out_day_only", "Lab_metric_target", "Patient_metric_target", "No_cases_signed_out", "Lab_metric_avg", "Lab_metric_med", "Lab_metric_std","Lab_metric_within_target", "Patient_metric_avg", "Patient_metric_med", "Patient_metric_std")

#make sure the data types for both dataframes are the same
common <- names(Historical_Repo)[names(Historical_Repo) %in% names(current_summary)]
Historical_Repo[common] <- lapply(common, function(x) {
  match.fun(paste0("as.", class(current_summary[[x]])))(Historical_Repo[[x]])
})

#3. combine the current summary with the historical repo and write them into a xlsx file
Historical_Repo_Updated <- rbind(Historical_Repo, current_summary)

xlsxFileName <- paste0(user_directory,"/AP & Cytology Historical Repo/", "Historical_Repo_Surgical_Pathology","_",Today,".xlsx")

write_xlsx(Historical_Repo_Updated,xlsxFileName)
```


```{r Custom Functions for subsetting and summarizing data for SCC and Sunquest dashboard tables, warning = FALSE, message = FALSE, echo = FALSE}
# # Custom function to clean and format summarized data (used in "subsetting and summarizing function"lab_sub_summarize" function beflore)
# clean_summarized_data <- function(x) {
#   # Replace NaN TAT percentages with NA
#   x[is.finite(x$ReceiveResultPercent) == FALSE, "ReceiveResultPercent"] <- NA
#   x[is.finite(x$CollectResultPercent) == FALSE, "CollectResultPercent"] <- NA
#   # Look up target TAT for sites with 0 resulted labs using tat_targets reference
#   x$Concate1 <- paste(x$Test, x$DashboardPriority)
#   x$Concate2 <- paste(x$Test, x$DashboardPriority, x$DashboardSetting)
#   
#   x$ReceiveResultTarget[is.na(x$ReceiveResultTarget)] <- ifelse(!is.na(match(x$Concate2[is.na(x$ReceiveResultTarget)], tat_targets$Concate)), 
#                                                             tat_targets$ReceiveToResultTarget[match(x$Concate2[is.na(x$ReceiveResultTarget)], tat_targets$Concate)], ifelse(!is.na(match(x$Concate1[is.na(x$ReceiveResultTarget)], tat_targets$Concate)), 
#                                                             tat_targets$ReceiveToResultTarget[match(x$Concate1[is.na(x$ReceiveResultTarget)], tat_targets$Concate)], tat_targets$ReceiveToResultTarget[match(x$Test[is.na(x$ReceiveResultTarget)], tat_targets$Concate)]))
#   x$CollectResultTarget[is.na(x$CollectResultTarget)] <- ifelse(!is.na(match(x$Concate2[is.na(x$CollectResultTarget)], tat_targets$Concate)), 
#                                                             tat_targets$CollectToResultTarget[match(x$Concate2[is.na(x$CollectResultTarget)], tat_targets$Concate)], ifelse(!is.na(match(x$Concate1[is.na(x$CollectResultTarget)], tat_targets$Concate)), 
#                                                             tat_targets$CollectToResultTarget[match(x$Concate1[is.na(x$CollectResultTarget)], tat_targets$Concate)], tat_targets$CollectToResultTarget[match(x$Test[is.na(x$CollectResultTarget)], tat_targets$Concate)]))
#   x$Concate1 <- NULL
#   x$Concate2 <- NULL
#   # # Look up target TAT for sites with 0 resulted labs
#   # dummy_df <- unique(x[is.na(x$ReceiveResultTarget) != TRUE | is.na(x$CollectResultTarget) != TRUE, c(1,3:6)])
#   # x$ReceiveResultTarget <- dummy_df$ReceiveResultTarget[match(paste(x$Test, x$DashboardPriority,  x$DashboardSetting), paste(dummy_df$Test, dummy_df$DashboardPriority, dummy_df$DashboardSetting))]
#   # x$CollectResultTarget <- dummy_df$CollectResultTarget[match(paste(x$Test, x$DashboardPriority,  x$DashboardSetting), paste(dummy_df$Test, dummy_df$DashboardPriority, dummy_df$DashboardSetting))]
#   # Format target TAT for tables
#     x[c("ReceiveResultTarget", "CollectResultTarget")] <- lapply(x[c("ReceiveResultTarget", "CollectResultTarget")], function(y) ifelse(is.na(y), y, paste0("<=", y, " min")))
#   x[c("ReceiveResultPercent", "CollectResultPercent")] <- lapply(x[c("ReceiveResultPercent", "CollectResultPercent")], percent, digits = 0)
#   # Create new column with test and priority
#   x$TestAndPriority <- paste(x$Test, "-", x$DashboardPriority, "Labs")
#   # Remove unused combinations like Routine ED & ICU labs
#   x <- x[!(x$DashboardPriority == "Routine" & x$DashboardSetting == "ED & ICU"), ]
#   # Format TAT percentages based on status definitions and lab division
# 
#   x <- x %>%  mutate(ReceiveResultPercent = cell_spec(ReceiveResultPercent, "html", color = ifelse(is.na(ReceiveResultPercent), "lightgray", ifelse(((ReceiveResultPercent >= 0.95 & (LabDivision == "Chemistry" | LabDivision == "Hematology")) | (ReceiveResultPercent == 1.00 & LabDivision == "Microbiology RRL")), "green", ifelse(((ReceiveResultPercent >=0.8 &  ReceiveResultPercent < 0.95 & (LabDivision == "Chemistry" | LabDivision == "Hematology")) | (ReceiveResultPercent >= 0.90 & ReceiveResultPercent < 1.0 & LabDivision == "Microbiology RRL")), "orange", "red")))))
#   x <- x %>%  mutate(CollectResultPercent = cell_spec(CollectResultPercent, "html", color = ifelse(is.na(CollectResultPercent), "lightgray", ifelse(CollectResultPercent >= 0.95, "green", ifelse(CollectResultPercent >=0.8 &  CollectResultPercent < 0.95, "orange", "red")))))
#   x
# }
# 
# # Custon function for subsetting and summarizing data for each lab division's TAT dashboard
# lab_sub_summarize <- function(x, LabDivision) {
#   lab_df <- x[x$Division == LabDivision & x$TATInclude == TRUE, ]
#   lab_df$Test <- factor(lab_df$Test, unique(test_code$Test[test_code$Division == LabDivision]))
#   lab_df$DashboardSetting <- factor(lab_df$DashboardSetting, dashboard_pt_setting)
#   lab_df$DashboardPriority <- droplevels(lab_df$DashboardPriority)
#   lab_summary <- lab_df %>%
#     group_by(Test, Site, DashboardPriority, DashboardSetting, ReceiveResultTarget, CollectResultTarget, .drop = FALSE) %>%
#   summarize(ResultedVolume = n(), ReceiveResultInTarget = sum(ReceiveResultInTarget), CollectResultInTarget = sum(CollectResultInTarget),
#             ReceiveResultPercent = round(ReceiveResultInTarget/ResultedVolume, digits = 3), CollectResultPercent = round(CollectResultInTarget/ResultedVolume, digits = 3))
#   lab_summary <- clean_summarized_data(lab_summary)
#   lab_dashboard1 <- melt(lab_summary, id.var = c("Test", "Site", "DashboardPriority", "TestAndPriority", "DashboardSetting", "ReceiveResultTarget", "CollectResultTarget"), measure.vars = c("ReceiveResultPercent", "CollectResultPercent"))
#   lab_dashboard2 <- dcast(lab_dashboard1, Test + DashboardPriority + TestAndPriority + DashboardSetting +ReceiveResultTarget + CollectResultTarget ~ variable + Site, value.var = "value")
#   lab_dashboard2 <- lab_dashboard2[ , c(1:3, 5, 4, 7:12, 6, 4, 13:18)]
#     lab_sub_output <- list(lab_df, lab_summary, lab_dashboard1, lab_dashboard2)
# }

# Custome function to subset, summarize, clean, and format data for dashboards for all lab divisions
lab_sub_summarize <- function(x, LabDivision) {
  # Subset data to be included based on lab division and whether or not TAT meets inclusion criteria
  lab_df <- x[x$Division == LabDivision & x$TATInclude == TRUE, ]
  # Update factors
  lab_df$Test <- factor(lab_df$Test, unique(test_code$Test[test_code$Division == LabDivision]))
  lab_df$DashboardSetting <- factor(lab_df$DashboardSetting, dashboard_pt_setting)
  lab_df$DashboardPriority <- droplevels(lab_df$DashboardPriority)
  # Summarize data based on test, site, priority, setting, and TAT targets. Keep levels so each site is maintained for melting/casting later.
  lab_summary <- lab_df %>%
    group_by(Test, Site, DashboardPriority, DashboardSetting, ReceiveResultTarget, CollectResultTarget, .drop = FALSE) %>%
  summarize(ResultedVolume = n(), ReceiveResultInTarget = sum(ReceiveResultInTarget), CollectResultInTarget = sum(CollectResultInTarget),
            ReceiveResultPercent = round(ReceiveResultInTarget/ResultedVolume, digits = 3), CollectResultPercent = round(CollectResultInTarget/ResultedVolume, digits = 3))
  
  # Clean up summarized data
  # Replace NaN TAT percentages with NA
  lab_summary[is.finite(lab_summary$ReceiveResultPercent) == FALSE, "ReceiveResultPercent"] <- NA
  lab_summary[is.finite(lab_summary$CollectResultPercent) == FALSE, "CollectResultPercent"] <- NA
  # Look up target TAT for sites with 0 resulted labs using tat_targets reference
  lab_summary$Concate1 <- paste(lab_summary$Test, lab_summary$DashboardPriority)
  lab_summary$Concate2 <- paste(lab_summary$Test, lab_summary$DashboardPriority, lab_summary$DashboardSetting)
  
  lab_summary$ReceiveResultTarget[is.na(lab_summary$ReceiveResultTarget)] <- ifelse(!is.na(match(lab_summary$Concate2[is.na(lab_summary$ReceiveResultTarget)], tat_targets$Concate)), 
                                                            tat_targets$ReceiveToResultTarget[match(lab_summary$Concate2[is.na(lab_summary$ReceiveResultTarget)], tat_targets$Concate)], ifelse(!is.na(match(lab_summary$Concate1[is.na(lab_summary$ReceiveResultTarget)], tat_targets$Concate)), 
                                                            tat_targets$ReceiveToResultTarget[match(lab_summary$Concate1[is.na(lab_summary$ReceiveResultTarget)], tat_targets$Concate)], tat_targets$ReceiveToResultTarget[match(lab_summary$Test[is.na(lab_summary$ReceiveResultTarget)], tat_targets$Concate)]))
  lab_summary$CollectResultTarget[is.na(lab_summary$CollectResultTarget)] <- ifelse(!is.na(match(lab_summary$Concate2[is.na(lab_summary$CollectResultTarget)], tat_targets$Concate)), 
                                                            tat_targets$CollectToResultTarget[match(lab_summary$Concate2[is.na(lab_summary$CollectResultTarget)], tat_targets$Concate)], ifelse(!is.na(match(lab_summary$Concate1[is.na(lab_summary$CollectResultTarget)], tat_targets$Concate)), 
                                                            tat_targets$CollectToResultTarget[match(lab_summary$Concate1[is.na(lab_summary$CollectResultTarget)], tat_targets$Concate)], tat_targets$CollectToResultTarget[match(lab_summary$Test[is.na(lab_summary$CollectResultTarget)], tat_targets$Concate)]))
  lab_summary$Concate1 <- NULL
  lab_summary$Concate2 <- NULL
  
  # Format target TAT for tables from numbers to "<=X min"
  lab_summary[c("ReceiveResultTarget", "CollectResultTarget")] <- lapply(lab_summary[c("ReceiveResultTarget", "CollectResultTarget")], function(y) ifelse(is.na(y), y, paste0("<=", y, " min")))
  lab_summary[c("ReceiveResultPercent", "CollectResultPercent")] <- lapply(lab_summary[c("ReceiveResultPercent", "CollectResultPercent")], percent, digits = 0)
  # Create new column with test and priority to be used in tables later
  lab_summary$TestAndPriority <- paste(lab_summary$Test, "-", lab_summary$DashboardPriority, "Labs")
  # Remove unused combinations, specifically Routine ED & ICU labs
  lab_summary <- lab_summary[!(lab_summary$DashboardPriority == "Routine" & lab_summary$DashboardSetting == "ED & ICU"), ]
  # Apply conditional color formatting to TAT percentages based on status definitions for each lab division
  lab_summary <- lab_summary %>%  mutate(ReceiveResultPercent = cell_spec(ReceiveResultPercent, "html", color = ifelse(is.na(ReceiveResultPercent), "lightgray", ifelse(((ReceiveResultPercent >= 0.95 & (LabDivision == "Chemistry" | LabDivision == "Hematology")) | (ReceiveResultPercent == 1.00 & LabDivision == "Microbiology RRL")), "green", ifelse(((ReceiveResultPercent >=0.8 &  ReceiveResultPercent < 0.95 & (LabDivision == "Chemistry" | LabDivision == "Hematology")) | (ReceiveResultPercent >= 0.90 & ReceiveResultPercent < 1.0 & LabDivision == "Microbiology RRL")), "orange", "red")))))
  lab_summary <- lab_summary %>%  mutate(CollectResultPercent = cell_spec(CollectResultPercent, "html", color = ifelse(is.na(CollectResultPercent), "lightgray", ifelse(((CollectResultPercent >= 0.95 & (LabDivision == "Chemistry" | LabDivision == "Hematology")) | (CollectResultPercent == 1.00 & LabDivision == "Microbiology RRL")), "green", ifelse(((CollectResultPercent >=0.8 &  CollectResultPercent < 0.95 & (LabDivision == "Chemistry" | LabDivision == "Hematology")) | (CollectResultPercent >= 0.90 & CollectResultPercent < 1.0 & LabDivision == "Microbiology RRL")), "orange", "red")))))

  # Melt summarized data into a "long" dataframe
  lab_dashboard1 <- melt(lab_summary, id.var = c("Test", "Site", "DashboardPriority", "TestAndPriority", "DashboardSetting", "ReceiveResultTarget", "CollectResultTarget"), measure.vars = c("ReceiveResultPercent", "CollectResultPercent"))
  # Cast dataframe into wide format for use in tables later
  lab_dashboard2 <- dcast(lab_dashboard1, Test + DashboardPriority + TestAndPriority + DashboardSetting +ReceiveResultTarget + CollectResultTarget ~ variable + Site, value.var = "value")
  # Rearrange casted dataframe columns based on desired table aesthetics
  lab_dashboard2 <- lab_dashboard2[ , c(1:3, 5, 4, 7:12, 6, 4, 13:18)]
  # Save outputs in a list
  lab_sub_output <- list(lab_df, lab_summary, lab_dashboard1, lab_dashboard2)
  lab_sub_output
}


# Custom function for styling kable tables
chem_hem_micro_kable <- function(data) {
  kable(data, format = "html", escape = FALSE, align = "c", 
        col.names = c("Test & Priority", "Target", "Setting", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "Target", "Setting", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM")) %>%
  kable_styling(bootstrap_options = "hover", position = "center", font_size = 11) %>%
  column_spec(column = c(1, 9, 17), border_right = "thin solid lightgray") %>%
  add_header_above(c(" ", "Receive to Result Within Target" = (ncol(data)-1)/2, "Collect to Result Within Target" = (ncol(data)-1)/2), background = c("white", "#00AEEF", "#221f72"), color = "white", line = FALSE, font_size = 13) %>%
  column_spec(column = 2:9, background = "#00AEEF", color = "white", include_thead = TRUE) %>%
  column_spec(column = 10:17, background = "#221f72", color = "white", include_thead = TRUE) %>%
  column_spec(column = 2:17, background = "inherit", color = "inherit") %>%
  column_spec(column = 1, width_min = "125px", include_thead = TRUE) %>%
  column_spec(column = c(3, 11), width_min = "100px", include_thead = TRUE) %>%
  row_spec(row = 0, font_size = 13) %>%
  collapse_rows(columns = c(1, 2, 10))
}
```

```{r Subset and format SCC and Sunquest data for each lab division weekday dashboard, warning = FALSE, message = FALSE, echo = FALSE}

# Chemistry
chem_sub_output <-lab_sub_summarize(scc_sun_wday_master, LabDivision = "Chemistry")
chem_subset <- chem_sub_output[[1]]
chemistry_summary <- chem_sub_output[[2]]
chemistry_dashboard1 <- chem_sub_output[[3]]
chemistry_dashboard2 <- chem_sub_output[[4]]

# Chemistry: Manually remove unused combinations
chemistry_dashboard2 <- chemistry_dashboard2[!(((chemistry_dashboard2$Test == "Troponin" | chemistry_dashboard2$Test == "Lactate WB") & (chemistry_dashboard2$DashboardPriority != "All" | chemistry_dashboard2$DashboardSetting == "Amb")) | (chemistry_dashboard2$Test == "BUN" & chemistry_dashboard2$DashboardPriority == "All")), ]
row.names(chemistry_dashboard2) <- 1:nrow(chemistry_dashboard2)

chem_table <- chemistry_dashboard2[ , 3:ncol(chemistry_dashboard2)]

# Hematology
hem_sub_output <-lab_sub_summarize(scc_sun_wday_master, LabDivision = "Hematology")
hem_subset <- hem_sub_output[[1]]
hematology_summary <- hem_sub_output[[2]]
hematology_dashboard1 <- hem_sub_output[[3]]
hematology_dashboard2 <- hem_sub_output[[4]]

hem_table<- hematology_dashboard2[ , 3:ncol(hematology_dashboard2)]

# Microbiology RRL
micro_sub_output <-lab_sub_summarize(scc_sun_wday_master, LabDivision = "Microbiology RRL")
micro_subset <- micro_sub_output[[1]]
micro_summary <- micro_sub_output[[2]]
micro_dashboard1 <- micro_sub_output[[3]]
micro_dashboard2 <- micro_sub_output[[4]]

# Microbiology RRL: Manually remove unused combinations
micro_dashboard2 <- micro_dashboard2[!(micro_dashboard2$Test == "C. diff" & micro_dashboard2$DashboardSetting == "Amb"), ]
row.names(micro_dashboard2) <- 1:nrow(micro_dashboard2)

micro_table <- micro_dashboard2[ , 3:ncol(micro_dashboard2)]
```


```{r Custom functions for formatting Cytology dashboard tables, echo = FALSE, warning = FALSE, message = FALSE}

############Create a function for Table standardization for cyto and patho############
#To add all the missing rows and columns

#Cytology table
Table_Merging_Cyto <- function(Cytology_Table){
  if (is.null(Cytology_Table)){
    Cytology_Table_New <- NULL
    Cytology_Table_New2 <- NULL
  } else {
    #first step is merging the table template with the cytology table and this will include all of the missing columns.

    Cytology_Table_New <- merge(x = Table_Template_Cyto, y= Cytology_Table, all.y=TRUE)
    #second step is merging the table with all of the columns with only the first two columns of the template to include all the missing rows
    Cytology_Table_New2 <- merge(x = Table_Template_Cyto[c(1,2)], y= Cytology_Table_New, all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    rows_order_Cyto <- factor(rownames(Cytology_Table_New2),levels = c(2,1,4,3))
    Cytology_Table_New2 <- Cytology_Table_New2[order(rows_order_Cyto), c("spec_group","Patient.Setting","No_Cases_Signed","MSH.x","MSQ.x","BIMC.x","PACC.x","KH.x","R.x","SL.x", "NYEE.x","MSH.y", "MSQ.y","BIMC.y","PACC.y","KH.y","R.y","SL.y", "NYEE.y")]
  }
  return(Cytology_Table_New2)
}

Table_Merging_Cyto_V2 <- function(Cytology_Table_V2){
  if (is.null(Cytology_Table_V2)){
    Cytology_Table_New_V2 <- NULL
    Cytology_Table_New2_V2 <- NULL
  } else {
    #first step is merging the table template with the cytology table and this will include all of the missing columns.
    Cytology_Table_New_V2 <- merge(x = Table_Template_Cyto_V2, y= Cytology_Table_V2, all.y=TRUE)
    #second step is merging the table with all of the columns with only the first two columns of the template to include all the missing rows
    Cytology_Table_New2_V2 <- merge(x = Table_Template_Cyto_V2[c(1,2)], y= Cytology_Table_New_V2, all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    rows_order_Cyto_V2 <- factor(rownames(Cytology_Table_New2_V2),levels = c(2,1,4,3))

    Cytology_Table_New2_V2 <- Cytology_Table_New2_V2[order(rows_order_Cyto_V2), c("spec_group","Patient.Setting","No_Cases_Signed","Received_to_Signed_out_within_target","MSH","MSQ","BIMC","PACC","KH","R","SL", "NYEE")]
  }
  return(Cytology_Table_New2_V2)
}

Cytology_Table_Weekday_New2 <- Table_Merging_Cyto(Cytology_Table_Weekday)
Cytology_Table_Not_Weekday_New2 <- Table_Merging_Cyto(Cytology_Table_Not_Weekday)
Cytology_Table_Weekday_New2_V2 <- Table_Merging_Cyto_V2(Cytology_Table_Weekday_V2)
Cytology_Table_Not_Weekday_New2_V2 <- Table_Merging_Cyto_V2(Cytology_Table_Not_Weekday_V2)

#Pathology table
Table_Merging_Patho <- function(Surgical_Pathology_Table){
  if (is.null(Surgical_Pathology_Table)){
    Surgical_Pathology_Table_New <- NULL
    Surgical_Pathology_Table_New2 <- NULL
  } else {
    #first step is merging the table template with the cytology table and this will include all of the missing columns.

    Surgical_Pathology_Table_New <- merge(x = Table_Template_Patho, y= Surgical_Pathology_Table, all.y=TRUE)
    
    #second step is merging the table with all of the columns with only the first two columns of the template to include all the missing rows
    Surgical_Pathology_Table_New2 <- merge(x = Table_Template_Patho[c(1,2)], y= Surgical_Pathology_Table_New, all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    rows_order_Patho <- factor(rownames(Surgical_Pathology_Table_New2),levels = c(2,1,4,3))

    Surgical_Pathology_Table_New2 <- Surgical_Pathology_Table_New2[order(rows_order_Patho), c("spec_group","Patient.Setting","No_Cases_Signed","MSH.x","MSQ.x","BIMC.x","PACC.x","KH.x","R.x","SL.x", "MSH.y","MSQ.y","BIMC.y","PACC.y","KH.y","R.y","SL.y")]
  }
  return(Surgical_Pathology_Table_New2)
}

Surgical_Pathology_Table_Weekday_New2 <- Table_Merging_Patho(Surgical_Pathology_Table_Weekday)

#Surgical_Pathology_Table_Not_Weekday_New2 <- Table_Merging_Patho(Surgical_Pathology_Table_Not_Weekday)

pathology_volume_column_order <- c("spec_group","Patient.Setting","MSH","MSQ","BIMC","PACC","KH","R","SL")

cytology_volume_column_order <- c("spec_group","Patient.Setting","MSH","MSQ","BIMC","PACC","KH","R","SL", "NYEE") 

Table_Merging_Volume <- function(Table_Template_Vol, Vol_Table, columns_order_V3){
  if (is.null(Vol_Table)){
    Vol_Table_New <- NULL
    Vol_Table_New2 <- NULL
    rows_order_Vol <- NULL
  } else {

    Vol_Table_New <- merge(x = Table_Template_Vol, y= Vol_Table, all.y=TRUE)
    
    Vol_Table_New2 <- merge(x = Table_Template_Vol[c(1,2)], y= Vol_Table_New, all.x = TRUE, by = c("spec_group", "Patient.Setting"))
    rows_order_Vol <- factor(rownames(Vol_Table_New2),levels = c(2,1,4,3))
    Vol_Table_New2 <- Vol_Table_New2[order(rows_order_Vol), columns_order_V3]
    row.names(Vol_Table_New2) <- NULL
    Vol_Table_New2[is.na(Vol_Table_New2)] <- 0
  }
  return(Vol_Table_New2)
}



Surgical_Pathology_Stratified_Vol_Weekday_New2 <- Table_Merging_Volume(Table_Template_Patho_Vol,Surgical_Pathology_Stratified_Vol_Weekday,pathology_volume_column_order)
Surgical_Pathology_Stratified_Vol_Weekday_New2$spec_group[Surgical_Pathology_Stratified_Vol_Weekday_New2$spec_group=="GI"] <- "GI Biopsies"
Surgical_Pathology_Stratified_Vol_Weekday_New2$spec_group[Surgical_Pathology_Stratified_Vol_Weekday_New2$spec_group=="Breast"] <- "All Breast Specimens"


#Surgical_Pathology_Stratified_Vol_Not_Weekday_New2 <- Table_Merging_Volume(Table_Template_Patho_Vol, Surgical_Pathology_Stratified_Vol_Not_Weekday,pathology_volume_column_order)
Cytology_Stratified_Vol_Weekday_New2 <- Table_Merging_Volume(Table_Template_Cyto_Vol
, Cytology_Stratified_Vol_Weekday,cytology_volume_column_order)

#added this line to delete the cyto gyn from the table until we get correct data
Cytology_Stratified_Vol_Weekday_New3 <- Cytology_Stratified_Vol_Weekday_New2[-c(1,2),]

Cytology_Stratified_Vol_Not_Weekday_New2 <- Table_Merging_Volume(Table_Template_Cyto_Vol
, Cytology_Stratified_Vol_Not_Weekday,cytology_volume_column_order)


############Create a function for conditional formatting############
Conditional_Formatting_Cyto <- function(Table_New2){
  if (is.null(Table_New2)){
    Table_New3 <- NULL
    Table_New4 <- NULL
    Table_New5 <- NULL
  } else {
    
    Table_New2[,4:19] <- lapply(Table_New2[,4:19], as.numeric)

    Table_New2[,4:11] <- lapply(Table_New2[,4:11],percent)
    
    #steps for conditional formatting:
    
    Table_New3 <- melt(Table_New2, id =c("spec_group","Patient.Setting","No_Cases_Signed","MSH.y","MSQ.y","BIMC.y","PACC.y","KH.y","R.y","SL.y", "NYEE.y"))    
    
    Table_New3 <- Table_New3 %>%
      mutate(value = ifelse(is.na(value), cell_spec(value, "html", color = "lightgray"), 
                            ifelse(value > 0.9, cell_spec(value, "html", color  = "green"),
                                   ifelse(value < 0.8, cell_spec(value, "html", color = "red"), cell_spec(value, "html", color = "orange")))))
    
    
    Table_New3 <- dcast(Table_New3,spec_group + Patient.Setting + No_Cases_Signed + BIMC.y + MSH.y + MSQ.y + NYEE.y + PACC.y + R.y + SL.y + KH.y ~ variable )
    
    Table_New4 <- melt(Table_New3, id =c("spec_group","Patient.Setting","No_Cases_Signed","MSH.x","MSQ.x","BIMC.x","PACC.x","KH.x","R.x","SL.x", "NYEE.x"))
    
    
    Table_New4 <- Table_New4 %>%
      mutate(value = ifelse(is.na(value), cell_spec(value, "html", color = "lightgray"), cell_spec(value, "html", color = "black")))
    

    Table_New4 <- dcast(Table_New4,spec_group + Patient.Setting + No_Cases_Signed + BIMC.x + MSH.x + MSQ.x + NYEE.x + PACC.x + R.x + SL.x + KH.x ~ variable )
    
    Table_New5 <- merge(x=Table_New4, y=TAT_Targets[1:3])
    
    rows_order <- factor(rownames(Table_New5),levels = c(2,1,4,3))
    Table_New5 <- Table_New5[order(rows_order), c("spec_group","Received.to.signed.out.target..Days.","Patient.Setting","No_Cases_Signed","MSH.x","MSQ.x","BIMC.x","PACC.x","KH.x","R.x","SL.x", "NYEE.x","MSH.y","MSQ.y","BIMC.y","PACC.y","KH.y","R.y","SL.y", "NYEE.y")]
    Table_New5$Received.to.signed.out.target..Days.<- paste("<= ", Table_New5$Received.to.signed.out.target..Days., "days")
    row.names(Table_New5) <- NULL
    Table_New5$No_Cases_Signed[is.na(Table_New5$No_Cases_Signed)] <- 0
  }
  return(Table_New5)
}


Cytology_Table_Weekday_New3 <- Conditional_Formatting_Cyto(Cytology_Table_Weekday_New2)

#added this line to delete the cyto gyn from the table until we get correct data
Cytology_Table_Weekday_New4 <- Cytology_Table_Weekday_New3[-c(1,2),]

Cytology_Table_Not_Weekday_New3 <- Conditional_Formatting_Cyto(Cytology_Table_Not_Weekday_New2)

Conditional_Formatting_Cyto_V2 <- function(Table_New2_V2){
  if (is.null(Table_New2_V2)){
    Table_New3_V2 <- NULL
    Table_New4_V2 <- NULL
  } else {
    
    Table_New2_V2[4] <- lapply(Table_New2_V2[4], as.numeric)
    Table_New2_V2[4] <- lapply(Table_New2_V2[4],percent, d=0)
    
    #steps for conditional formatting:
    
    Table_New3_V2 <- Table_New2_V2 %>%
      mutate(Received_to_Signed_out_within_target = ifelse(is.na(Received_to_Signed_out_within_target), cell_spec(Received_to_Signed_out_within_target, "html", color = "lightgray"), 
                            ifelse(Received_to_Signed_out_within_target > 0.9, cell_spec(Received_to_Signed_out_within_target, "html", color  = "green"),
                                   ifelse(Received_to_Signed_out_within_target < 0.8, cell_spec(Received_to_Signed_out_within_target, "html", color = "red"), cell_spec(Received_to_Signed_out_within_target, "html", color = "orange")))))

    Table_New4_V2 <- melt(Table_New3_V2, id =c("spec_group","Patient.Setting","No_Cases_Signed","Received_to_Signed_out_within_target"))
    
    
    Table_New4_V2 <- Table_New4_V2 %>%
      mutate(value = ifelse(is.na(value), cell_spec(value, "html", color = "lightgray"), cell_spec(value, "html", color = "black")))
    
    Table_New4_V2 <- dcast(Table_New4_V2,spec_group + Patient.Setting + No_Cases_Signed + Received_to_Signed_out_within_target ~ variable )
    
    Table_New4_V2 <- merge(x=Table_New4_V2, y=TAT_Targets[1:3])
    rows_order_V2 <- factor(rownames(Table_New4_V2),levels = c(2,1,4,3))
    Table_New4_V2 <- Table_New4_V2[order(rows_order_V2), c("spec_group","Received.to.signed.out.target..Days.","Patient.Setting","No_Cases_Signed","Received_to_Signed_out_within_target","MSH","MSQ","BIMC","PACC","KH","R","SL", "NYEE")]
    Table_New4_V2$Received.to.signed.out.target..Days.<- paste("<= ", Table_New4_V2$Received.to.signed.out.target..Days., "days")
    row.names(Table_New4_V2) <- NULL
    Table_New4_V2$No_Cases_Signed[is.na(Table_New4_V2$No_Cases_Signed)] <- 0
  }
  return(Table_New4_V2)
}

Cytology_Table_Weekday_New3_V2 <- Conditional_Formatting_Cyto_V2(Cytology_Table_Weekday_New2_V2)
Cytology_Table_Not_Weekday_New3_V2 <- Conditional_Formatting_Cyto_V2(Cytology_Table_Not_Weekday_New2_V2)

#added this line to delete the cyto gyn from the table until we get correct data
Cytology_Table_Weekday_New3_V3 <- Cytology_Table_Weekday_New3_V2[-c(1,2),]

Conditional_Formatting_Patho <- function(Table_New2){
  if (is.null(Table_New2)){
    Table_New3 <- NULL
    Table_New4 <- NULL
    Table_New5 <- NULL
  } else {
    
    Table_New2[,4:17] <- lapply(Table_New2[,4:17], as.numeric)
    Table_New2[,4:10] <- lapply(Table_New2[,4:10],percent, d=0)
    
    #steps for conditional formatting:
    
    Table_New3 <- melt(Table_New2, id =c("spec_group","Patient.Setting","No_Cases_Signed","MSH.y","MSQ.y","BIMC.y","PACC.y","KH.y","R.y","SL.y"))
    
    
    Table_New3 <- Table_New3 %>%
      mutate(value = ifelse(is.na(value), cell_spec(value, "html", color = "lightgray"), 
                            ifelse(value > 0.9, cell_spec(value, "html", color  = "green"),
                                   ifelse(value < 0.8, cell_spec(value, "html", color = "red"), cell_spec(value, "html", color = "orange")))))
    
    
    Table_New3 <- dcast(Table_New3,spec_group + Patient.Setting + No_Cases_Signed + BIMC.y + MSH.y + MSQ.y + PACC.y + R.y + SL.y + KH.y ~ variable )
    
    Table_New4 <- melt(Table_New3, id =c("spec_group","Patient.Setting","No_Cases_Signed","MSH.x","MSQ.x","BIMC.x","PACC.x","KH.x","R.x","SL.x"))
    
    
    Table_New4 <- Table_New4 %>%
      mutate(value = ifelse(is.na(value), cell_spec(value, "html", color = "lightgray"), cell_spec(value, "html", color = "black")))
    
    Table_New4 <- dcast(Table_New4,spec_group + Patient.Setting + No_Cases_Signed + BIMC.x + MSH.x + MSQ.x + PACC.x + R.x + SL.x + KH.x ~ variable )
    
    Table_New5 <- merge(x=Table_New4, y=TAT_Targets[1:3])
    rows_order <- factor(rownames(Table_New5),levels = c(2,1,4,3))
    Table_New5 <- Table_New5[order(rows_order), c("spec_group","Received.to.signed.out.target..Days.","Patient.Setting","No_Cases_Signed","MSH.x","MSQ.x","BIMC.x","PACC.x","KH.x","R.x","SL.x","MSH.y","MSQ.y","BIMC.y","PACC.y","KH.y","R.y","SL.y")]
    Table_New5$Received.to.signed.out.target..Days.<- paste("<= ", Table_New5$Received.to.signed.out.target..Days., "days")
    row.names(Table_New5) <- NULL
    Table_New5$No_Cases_Signed[is.na(Table_New5$No_Cases_Signed)] <- 0
  }
  return(Table_New5)
}

Surgical_Pathology_Table_Weekday_New3 <- Conditional_Formatting_Patho(Surgical_Pathology_Table_Weekday_New2)

#Surgical_Pathology_Table_Not_Weekday_New3 <- Conditional_Formatting_Patho(Surgical_Pathology_Table_Not_Weekday_New2)

Surgical_Pathology_Table_Weekday_New3$spec_group[Surgical_Pathology_Table_Weekday_New3$spec_group == "GI"] <- "GI Biopsies" 
Surgical_Pathology_Table_Weekday_New3$spec_group[Surgical_Pathology_Table_Weekday_New3$spec_group == "Breast"] <- "All Breast Specimens"

```

```{r Custom function for Anatomic Pathology kable formatting, echo=FALSE, warning = FALSE, message = FALSE}
pathology_standardized_column_names <-  c("Case Type","Target","Setting","No. Cases Signed Out","MSH","MSQ","MSBI","PACC","MSB","MSW","MSSL","MSH","MSQ","MSBI","PACC","MSB","MSW","MSSL")

#cytology_standardized_column_names <-  c("Case Type","Target","Setting","No. Cases Signed Out","MSH", "MSQ","MSBI","PACC","MSB","MSW","MSSL", "NYEE","MSH", "MSQ","MSBI","PACC","MSB","MSW","MSSL", "NYEE")

Table_Formatting <- function (Table_New3,column_names){
  if (is.null(Table_New3)){
    Table_New3 <- NULL
  } else {
    Table_New3%>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = column_names) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    add_header_above(c(" "= 1, "Receive to Result within Target (Business Days)"= 10, "Average Collection to Result TAT (Calendar Days)"=7), background = c("white", "#00B9F2", "#221F72"), color= "white", font_size = 13)%>%
    column_spec(2:11, background = "#00B9F2",color ="white", include_thead = TRUE)%>%
    column_spec(12:18, background = "#221F72", color = "white", include_thead = TRUE)%>%
    column_spec(2:18, background = "inherit", color = "inherit")%>%
    row_spec(row = 0, font_size = 13)%>%
    collapse_rows(columns = c(1,2))
  }
}


Table_Formatting_V2 <- function (Table_New3_V2){
  if (is.null(Table_New3_V2)){
    Table_New3_V2 <- NULL
  } else {
    Table_New3_V2%>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("Case Type","Target","Setting","No. Cases Signed Out","Centralized Lab","MSH","MSQ","MSBI","PACC","MSB","MSW","MSSL", "NYEE")) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    add_header_above(c(" "= 1, "Receive to Result within Target (Business Days)"= 4, "Average Collection to Result TAT (Calendar Days)"=8), background = c("white", "#00B9F2", "#221F72"), color= "white", font_size = 13)%>%
    column_spec(2:5, background = "#00B9F2",color ="white", include_thead = TRUE)%>%
    column_spec(6:13, background = "#221F72", color = "white", include_thead = TRUE)%>%
    column_spec(2:13, background = "inherit", color = "inherit")%>%
    row_spec(row = 0, font_size = 13)%>%
    collapse_rows(columns = c(1,2))
  }
}

pathology_volume_column_names <-  c("Case Type","Setting","MSH","MSQ","MSBI","PACC","MSB","MSW","MSSL")

cytology_volume_column_names <-  c("Case Type","Setting","MSH", "MSQ","MSBI","PACC","MSB","MSW","MSSL", "NYEE")

#Volume table formatting
Table_Formatting_Volume <- function (Volume_Table, column_names){
  if (is.null(Volume_Table)){
    Volume_Table <- NULL
  } else {
    Volume_Table%>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = column_names) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    column_spec(3:length(column_names), background = "#00B9F2",color ="white", include_thead = TRUE)%>%
    column_spec(3:length(column_names), background = "inherit", color = "inherit")%>%
    row_spec(row = 0, font_size = 13)%>%
    collapse_rows(columns = 1)
    #add_header_above(c(" "= 2, "Cases Signed Out by Home Hospital"= 8), background = c("white", "#00B9F2"), color= "white", font_size = 13)
  }
}


```

<br />

## **Weekday Efficiency Indicators** {.tabset}
### Chemistry       
#### *Chemistry KPI (Labs Resulted on `r wday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, 
<span style = "color:orange">Yellow:</span> >=80% & <95%,
<span style = "color:green">Green:</span> >=95%</h5>
```{r Chemistry dashboard table, warning = FALSE, message = FALSE, echo = FALSE}
chem_hem_micro_kable(chem_table)
```
<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>


### Hematology
#### *Hematology KPI (Labs Resulted on `r wday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, 
<span style = "color:orange">Yellow:</span> >=80% & <95%,
<span style = "color:green">Green:</span> >=95%</h5>
```{r Hematology dashboard table, warning = FALSE, message = FALSE, echo = FALSE}
chem_hem_micro_kable(hem_table)
```
<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>

```{r Microbiology RRL volume and TAT table creation, warning = FALSE, message = FALSE, echo = FALSE}
# chem_hem_micro_kable(micro_table)

micro_volume_dashboard1 <- melt(micro_summary, id.var = c("Test", "Site", "DashboardPriority", "TestAndPriority", "DashboardSetting", "ReceiveResultTarget", "CollectResultTarget"), measure.vars = "ResultedVolume")

# Create a table with micro volume with similar formatting to TAT tables to later combine with TAT
micro_volume_dashboard2 <- dcast(micro_volume_dashboard1, Test + DashboardPriority + TestAndPriority + DashboardSetting +ReceiveResultTarget + CollectResultTarget ~ variable + Site, value.var = "value") 

micro_volume_dashboard2[c("ReceiveResultTarget", "CollectResultTarget")] <- "Resulted Volume"
original_length <- ncol(micro_volume_dashboard2)

micro_volume_dashboard2 <- micro_volume_dashboard2[ ,c(1:ncol(micro_volume_dashboard2), (ncol(micro_volume_dashboard2)-(6-1)):ncol(micro_volume_dashboard2))]

micro_volume_dashboard2 <- micro_volume_dashboard2[ , c(1:3, 5, 4, 7:12, 6, 4, 13:18)]
colnames(micro_volume_dashboard2) <- colnames(micro_dashboard2)

micro_volume_dashboard2[ , (original_length):ncol(micro_volume_dashboard2)] <- " "

# Combine TAT table and volume table
micro_tat_vol_comb <- rbind(micro_dashboard2, micro_volume_dashboard2)

micro_tat_vol_comb <- micro_tat_vol_comb[order(micro_tat_vol_comb$Test, micro_tat_vol_comb$ReceiveResultTarget), ]
row.names(micro_tat_vol_comb) <- 1:nrow(micro_tat_vol_comb)

micro_tat_vol_table <- micro_tat_vol_comb[ , 3:ncol(micro_tat_vol_comb)]
```

### Microbiology RRL
#### *Microbiology RRL KPI (Labs Resulted on `r wday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <90%, 
<span style = "color:orange">Yellow:</span> >=90% & <100%,
<span style = "color:green">Green:</span> =100%</h5>
```{r Create Micro RRL TAT and Volume Table, warning = FALSE, message = FALSE, echo = FALSE}
chem_hem_micro_kable(micro_tat_vol_table)
```
<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>

### Missing Collections & Add Ons
#### *Missing Collection Times and Add On Order Volume (Labs Resulted on `r wday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> >15%, 
<span style = "color:orange">Yellow:</span> <=15% & >5%,
<span style = "color:green">Green:</span> <=5%</h5>
```{r Missing Collection Times Tables, warning = FALSE, message = FALSE, echo = FALSE}
# # Determine percentage of labs with missing collection times -------------------------------
missing_collect <- scc_sun_wday_master[scc_sun_wday_master$TATInclude == TRUE, ] %>%
  group_by(Site, .drop = FALSE) %>%
  summarize(ResultedVolume = n(), MissingCollection = sum(MissingCollect, na.rm = TRUE), Percent = MissingCollection/ResultedVolume)

# Format percentage of labs with missing collection times as percentage
missing_collect$Percent <- percent(missing_collect$Percent, digits = 0)

# Apply conditional formatting to percentage of labs with missing collection times
missing_collect <- missing_collect %>%
  mutate(Percent = cell_spec(Percent, "html", color = ifelse(is.na(Percent), "grey",
                                                         ifelse(Percent <= 0.05, "green",
                                                                ifelse(Percent >0.05 &  Percent <= 0.15, "orange", "red")))))

missing_collect_table <- dcast(missing_collect, "Percentage of Specimens" ~ Site, value.var = "Percent")

missing_collect_table %>%
  kable(format = "html", escape = FALSE, align = "c",  
        col.names = c("Site", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM")) %>%
  kable_styling(bootstrap = "hover", position = "float_left", font_size = 11, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Percentage of Labs with Missing Collect Times" = ncol(missing_collect_table)-1), background = c("white", "#00AEEF"), color = "white", line = FALSE, font_size = 13) %>%
  column_spec(column = c(1, ncol(missing_collect_table)), border_right = "thin solid lightgray") %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "#00AEEF", color = "white", include_thead = TRUE) %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "inherit", color = "inherit", width_max = 0.15) %>%
  row_spec(row = 0, font_size =13)


add_on_volume <- scc_sun_wday_master %>%
  group_by(Test, Site, .drop = FALSE) %>%
  summarize(AddOnVolume = sum(AddOnMaster == "AddOn", na.rm = TRUE))

add_on_table <- dcast(add_on_volume, Test ~ Site, value.var = "AddOnVolume")

add_on_table %>%
  kable(format = "html", escape = FALSE, align = "c", 
        col.names = c("Test", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM"), color = "gray") %>%
  kable_styling(bootstrap = "hover", position = "right", font_size = 11, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Volume of Add On Labs" = ncol(missing_collect_table)-1), background = c("white", "#00AEEF"), color = "white", line = FALSE, font_size = 13) %>%
  column_spec(column = c(1, ncol(missing_collect_table)), border_right = "thin solid lightgray") %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "#00AEEF", color = "white", include_thead = TRUE) %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "inherit", color = "inherit") %>%
  row_spec(row = 0, font_size =13)
```
<h6>*Missing collection time analysis includes analytes represented in Chemistry, Hematology, and Microbiology RRL dashboard TAT analysis from ED, ICU, IP Non-ICU, and ambulatory settings.*</h6>

### Surgical Pathology
#### *Surgical Pathology KPI (Specimens Resulted on `r wday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, 
<span style = "color:orange">Yellow:</span> >=80% & <90%,
<span style = "color:green">Green:</span> >=90%</h5>
```{r Surgical Pathology Efficiency Indicator Weekday, echo=FALSE, warning=FALSE, message=FALSE}

Table_Formatting(Surgical_Pathology_Table_Weekday_New3, pathology_standardized_column_names)
```
<h6>*TAT analysis includes all breast specimens and GI biopsies and excludes those with missing timestamps, negative TAT, and not from above settings.*</h6>


### Cytology
<h4><span style = "color:red">Cyto Gyn Pending - Resolution of Data Issues</h4></span style = "color:red">

#### *Cytology KPI (Specimens Resulted on `r wday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, 
<span style = "color:orange">Yellow:</span> >=80% & <90%,
<span style = "color:green">Green:</span> >=90%</h5>
```{r Cytology Efficiency Indicators Weekday, echo=FALSE, warning=FALSE, message=FALSE}
#Table_Formatting_V2(Cytology_Table_Weekday_New3_V2)
#added this line to delete the cyto gyn from the table until we get correct data
rownames(Cytology_Table_Weekday_New3_V3) <- NULL
Table_Formatting_V2(Cytology_Table_Weekday_New3_V3)
```
<h6>*TAT analysis includes excludes specimens with missing timestamps, negative TAT, and not from above settings.*</h6>


```{r Lab KPI Form for Qualitative Indicators, echo=FALSE, message=FALSE, warning=FALSE, eval = FALSE}
#read the excel sheet that is generated from office form responses
KPI_Form <- data.frame(read_excel(choose.files(caption = "Select the KPI form generated today"), 1), stringsAsFactors = FALSE)

#Determine the dates within the KPI excel sheet and report only the latest date
KPI_Form$Completion_Date <- as.Date(as.POSIXct(KPI_Form$Completion.time,tz="",format='%m/%d/%y %I:%M %p'))
KPI_Form$Completion_Hour <- format(KPI_Form$Completion.time, format = "%H:%M:%S")

#List_of_Dates <-unique(KPI_Form$Completion_Date)
#Ordered_Dates <- List_of_Dates[rev(order(List_of_Dates))]
#pick only the newest responses
#KPI_Form_Today <- KPI_Form[which(KPI_Form$Completion_Date == Ordered_Dates[1]),]

KPI_Form_Today  <- KPI_Form[which(KPI_Form$Completion_Date == as.Date(Today) | (KPI_Form$Completion_Date == as.Date(Yesterday) & KPI_Form$Completion_Hour >= "17:00:00")),]

#split the data into 3 datasets

# the first dataset representes the following: CP/AP/CPA/4LABS

Clinical_Labs_Ops_Ind <- KPI_Form_Today[which(KPI_Form_Today$Select.your.branch.facility !="Laboratory Information System (LIS)"),c(6:14,16)]

# the second dataset represents the LIS indicators
LIS_Ops_Ind <- KPI_Form_Today[which(KPI_Form_Today$Select.your.branch.facility == "Laboratory Information System (LIS)"),c(6,19:21)]

# the third and last dataset is the never events and good catches dataset
Never_Events <- KPI_Form_Today[which(KPI_Form_Today$Select.your.branch.facility !="Laboratory Information System (LIS)"), c(6,15,17:18)]

####### Start creating and formatting the tables

#first melt the tables to get the values needed in one column:

#1. Clinical labs table
Clinical_Labs_Melt <- melt(Clinical_Labs_Ops_Ind, id =c( "Select.your.branch.facility", "IF.chosen..RED.or.YELLOW.for.any.of.the.items.listed.above..please.write.a.brief.note.on.each.event.including.source.of.specimen.and.location.of.incident"))

#2. LIS table
LIS_Melt <- melt(LIS_Ops_Ind, id =c("Select.your.branch.facility", "LIS...Unplanned.Service.Changes", "LIS.Preplanned.Downtime...If.Applicable."))
##### create a function to rename the status of the measures to a standard one ( Sfae, Under Stress, and Not Safe)

Renaming <- function(Melted_Dataset){
  Melted_Dataset[Melted_Dataset == "Green (No issues)"] <- "Safe"

  Melted_Dataset[Melted_Dataset == "Yellow (Safe/Under Stress)"| Melted_Dataset == "Yellow (Delays)" |  Melted_Dataset == "Yellow (Delays in pickup/delivery)" | Melted_Dataset == "Yellow (Shortage with no/minimal significant impact)" | Melted_Dataset == "Yellow (Minor issues/Coordinating with Reference Labs)"] <- "Under Stress"

  Melted_Dataset[Melted_Dataset == "Yellow (Safe/Under Stress)"| Melted_Dataset == "Yellow (Delays)" |  Melted_Dataset == "Yellow (Delays in pickup/delivery)" | Melted_Dataset == "Yellow (Shortage with no/minimal significant impact)" | Melted_Dataset == "Yellow (Minor issues/Coordinating with Reference Labs)"] <- "Under Stress"

  Melted_Dataset[Melted_Dataset== "Red (Severe shortage of consumables)" | Melted_Dataset == "Red (Missed pickups)" | Melted_Dataset == "Red (Not Safe/Risk)" | Melted_Dataset == "Red (Severe shortage that halts activity)"| Melted_Dataset =="Red (Major issues/Requires Immediate Attention)"] <- "Not Safe"

return(Melted_Dataset)
}

Clinical_Labs_Melt_New <- Renaming(Clinical_Labs_Melt)
LIS_Melt_New <- Renaming(LIS_Melt)

### create a function to format the data table into the correct colors

Conditional_Formatting_Kpi_Form <- function(Melted_Data_New){
  Melted_Data_New <- Melted_Data_New %>%
      mutate(value = ifelse(is.na(value), cell_spec(value, "html", color = "lightgray"), 
                            ifelse((value == "Safe"|value == "None"), cell_spec(value, "html", color  = "green"),
                                   ifelse((value == "Not Safe" |value == "Present") , cell_spec(value, "html", color = "red"), cell_spec(value, "html", color = "orange")))))

return(Melted_Data_New)  
  }

Formatted_Clinical_Labs_Melt_New <- Conditional_Formatting_Kpi_Form(Clinical_Labs_Melt_New)
Formatted_LIS_New <- Conditional_Formatting_Kpi_Form(LIS_Melt_New)

###### --------------- Now decast the melted clinical table --------------- ######

Formatted_Clinical_Labs_Melt_New2 <- dcast(Formatted_Clinical_Labs_Melt_New,  Select.your.branch.facility +IF.chosen..RED.or.YELLOW.for.any.of.the.items.listed.above..please.write.a.brief.note.on.each.event.including.source.of.specimen.and.location.of.incident ~ variable)

#Change the order of the columns in the table

Columns_Order_Form_Clinical <- c("Select.your.branch.facility", "LabCorp...Consumables", "Vendor.Services..LabLogistics..HealthEx..Reference.Labs..Other.Vendors.", "Environment", "Equipment.Issues..Lab.Equipment..Pneumatic.Tube.System..Others.", "IT.Issues", "Service.Changes", "Acute.Volume.Increase", "Staffing.Issues","IF.chosen..RED.or.YELLOW.for.any.of.the.items.listed.above..please.write.a.brief.note.on.each.event.including.source.of.specimen.and.location.of.incident")

Formatted_Clinical_Labs_Melt_New2 <- Formatted_Clinical_Labs_Melt_New2[,Columns_Order_Form_Clinical]

#Change the order of the rows in the table

Rows_Order_Form_Clinical <- factor(rownames(Formatted_Clinical_Labs_Melt_New2),levels = c(11, 4, 3, 9, 10, 1 , 2, 6, 8, 5, 7))
Formatted_Clinical_Labs_Melt_New2 <- Formatted_Clinical_Labs_Melt_New2[Rows_Order_Form_Clinical,]
rownames(Formatted_Clinical_Labs_Melt_New2) <- NULL

#Change the names in the Facility columns to be standardized

rownames(Formatted_Clinical_Labs_Melt_New2) <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL", "NYEE", "MSSN", "Anatomic Pathology (Centralized)", "Central Processiong & Accessioning (CPA)", "Client Services - 4LABS")

Formatted_Clinical_Labs_Melt_New2$Select.your.branch.facility <- rownames(Formatted_Clinical_Labs_Melt_New2)
rownames(Formatted_Clinical_Labs_Melt_New2) <- NULL

Formatted_Clinical_Labs_Melt_New3 <- Formatted_Clinical_Labs_Melt_New2[,c(1:9)]

Comments_Clinical_Labs <- Formatted_Clinical_Labs_Melt_New2[,c(1,10)]
Comments_Clinical_Labs[is.na(Comments_Clinical_Labs)] <- "No Issues Reported (Safe)"

###### --------------- now decast the LIS  table --------------- ######
Formatted_LIS_New2 <- dcast(Formatted_LIS_New, Select.your.branch.facility + LIS...Unplanned.Service.Changes + LIS.Preplanned.Downtime...If.Applicable. ~ variable)
Columns_Order_Form_LIS <- c("Select.your.branch.facility","LIS...Staffing.Issues", "LIS...Unplanned.Service.Changes", "LIS.Preplanned.Downtime...If.Applicable.")
Formatted_LIS_New2 <- Formatted_LIS_New2[,Columns_Order_Form_LIS]
rownames(Formatted_LIS_New2) <- NULL
Formatted_LIS_New2[is.na(Formatted_LIS_New2)] <- "None"

###### --------------- Formatting Never Events Table --------------- ######

# added 4 extra columns each one for different never event
Never_Events["Specimen(s) Lost"] <- NA
Never_Events["QNS - specimen that cannot be recollected"] <- NA
Never_Events["Treatment based on mislabeled/misidentified specimen"] <- NA
Never_Events["Treatment based on false positive/false negative results"] <- NA

#because we have multiple never events in one cell i created a list with these by:
Splitting_NE_Text <- sapply(Never_Events$NEVER.EVENTS, strsplit,'[;]')

#created a nested for loop with nested if to determine which of these never events are listed
for (i in 1:nrow(Never_Events)){
  for (j in 1:length(Splitting_NE_Text[[i]])){
    if (Splitting_NE_Text[[i]][j] == colnames(Never_Events)[5]){
      Never_Events$`Specimen(s) Lost`[i] = 1
    } else if (Splitting_NE_Text[[i]][j] == colnames(Never_Events)[6]){
      Never_Events$`QNS - specimen that cannot be recollected`[i] = 1
      } else if (Splitting_NE_Text[[i]][j] == colnames(Never_Events)[7]){
        Never_Events$`Treatment based on mislabeled/misidentified specimen`[i] = 1
        } else if (Splitting_NE_Text[[i]][j] == colnames(Never_Events)[8]){
          Never_Events$`Treatment based on false positive/false negative results`[i] = 1
        }
  }
}

Never_Events$NEVER.EVENTS <- NULL

Never_Events_Melt <- melt(Never_Events, id =c("Select.your.branch.facility", "NEVER.EVENTS..Please.provide.number.of.events.and.write.a.brief.note.on.each.one.including.source.of.specimen.and.location.of.incident..If.Applicable.","X.Good.Catch....Misidentified.mislabeled.specimen.s...Please.provide.number.of.events.that.occurred.in.the.last.24.hours...If.Applicable."))

Never_Events_Melt$value[is.na(Never_Events_Melt$value)] <- "None"
Never_Events_Melt$value[Never_Events_Melt$value == 1] <- "Present"

Formatted_Never_Event_Melt <- Conditional_Formatting_Kpi_Form(Never_Events_Melt)

Formatted_Never_Event <- dcast(Formatted_Never_Event_Melt,Select.your.branch.facility+NEVER.EVENTS..Please.provide.number.of.events.and.write.a.brief.note.on.each.one.including.source.of.specimen.and.location.of.incident..If.Applicable.+ X.Good.Catch....Misidentified.mislabeled.specimen.s...Please.provide.number.of.events.that.occurred.in.the.last.24.hours...If.Applicable. ~ variable)

Never_Events_Column_Order_1 <- c("Select.your.branch.facility", "Specimen(s) Lost", "QNS - specimen that cannot be recollected", "Treatment based on mislabeled/misidentified specimen", "Treatment based on false positive/false negative results", "NEVER.EVENTS..Please.provide.number.of.events.and.write.a.brief.note.on.each.one.including.source.of.specimen.and.location.of.incident..If.Applicable.", "X.Good.Catch....Misidentified.mislabeled.specimen.s...Please.provide.number.of.events.that.occurred.in.the.last.24.hours...If.Applicable.")

Formatted_Never_Event <- Formatted_Never_Event[,Never_Events_Column_Order_1]

#Change the order of the rows in the table

Rows_Order_Never_Events <- factor(rownames(Formatted_Never_Event),levels = c(11, 4, 3, 9, 10, 1 , 2, 6, 8, 5, 7))
Formatted_Never_Event <- Formatted_Never_Event[Rows_Order_Never_Events,]
rownames(Formatted_Never_Event) <- NULL

#Change the names in the Facility columns to be standardized

rownames(Formatted_Never_Event) <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL", "NYEE", "MSSN", "Anatomic Pathology (Centralized)", "Central Processiong & Accessioning (CPA)", "Client Services - 4LABS")

Formatted_Never_Event$Select.your.branch.facility <- rownames(Formatted_Never_Event)
rownames(Formatted_Never_Event) <- NULL

#Split the table into three
#table one is the comments only while table two is the details

Good_Catch <- Formatted_Never_Event[,c(1,7)]
Good_Catch[is.na(Good_Catch)] <- "No Issues Reported"

#Good_Catch_New <- Good_Catch[which(!(is.na(Good_Catch$X.Good.Catch....Misidentified.mislabeled.specimen.s...Please.provide.number.of.events.that.occurred.in.the.last.24.hours...If.Applicable.))),]

Never_Events_Comments <- Formatted_Never_Event[,c(1,6)]
Never_Events_Comments[is.na(Never_Events_Comments)] <- "No Issues Reported"

Never_Events_Only <- Formatted_Never_Event[,c(1:5)]

```

<br />

## **Operational and Quality Indicators** {.tabset}
### Operational Indicators
<h4><span style = "color:red">Pending - Scaled Testing of Automated Lab KPI Form</h4></span style = "color:red">

#### *Operational Indicators for Labs at each Site & Centralized Anatomic Pathology (Submitted on `r wday_result_date`)*

```{r Qualitative Indicators for Clinical Labs, echo=FALSE, message=FALSE, warning=FALSE, eval = FALSE}

Formatted_Clinical_Labs_Melt_New3%>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("Facility","Lab Corp Consumable", "Vendor Services", "Enviroment", "Equipment", "IT", "Service Change", "Acute Volume Increase", "Staffing")) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    row_spec(row = 0, font_size = 13)

Comments_Clinical_Labs %>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("Facility","Comments if At Risk or Not Safe")) %>%
    kable_styling(bootstrap_options = "hover", full_width = TRUE, position = "center", row_label_position = "c", font_size = 11) %>%
    row_spec(row = 0, font_size = 13)

```


### Laboratory Information System
<h4><span style = "color:red">Pending - Scaled Testing of Automated Lab KPI Form</h4></span style = "color:red">

#### *Operational Indicators for LIS (Submitted on `r wday_result_date`)*

```{r Qualitative Indicators for LIS, echo=FALSE, message=FALSE, warning=FALSE, eval = FALSE}
Formatted_LIS_New2%>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("","Staffing","Service Change", "Preplanned Downtime")) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    row_spec(row = 0, font_size = 13)
```


### Quality Indicators
<h4><span style = "color:red">Pending - Scaled Testing of Automated Lab KPI Form</h4></span style = "color:red">

#### Never Events 
##### *Reported Never Events for Each Division/Facility (Submitted on `r wday_result_date`)*
```{r Lab KPI Form for Never Events, echo=FALSE, message=FALSE, warning=FALSE, eval = FALSE}
Never_Events_Only %>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("Facility", "Specimen(s) Lost", "QNS - specimen that cannot be recollected", "Treatment based on mislabeled/misidentified specimen", "Treatment based on false positive/false negative results")) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    row_spec(row = 0, font_size = 13)

Never_Events_Comments %>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("Facility", "Comments if There is a Never Event")) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    row_spec(row = 0, font_size = 13)
```

#### Good Catches
##### *Reported Good Catches for Each Division/Facility (Submitted on `r wday_result_date`)*
```{r Lab KPI Form for Good Catches, echo=FALSE, message=FALSE, warning=FALSE, eval = FALSE}
Good_Catch %>%
    select(everything()) %>%
    kable(escape = F, align = "c", col.names = c("Facility", "Good Catch Comment")) %>%
    kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 11) %>%
    row_spec(row = 0, font_size = 13)
```

<br />

## **Weekday 24-Hour Volume Lookback** {.tabset}

```{r Custom function for 24 hour volume lookbacks for Chemistry and Hematology, echo = FALSE, warning = FALSE, message = FALSE}

lab_volume_summarize <- function(x, LabDivision) {
  # Subset data for relevant division
  lab_vol_df <- x[x$Division == LabDivision, ]
  # Update factors and format data
  lab_vol_df$Test <- factor(lab_vol_df$Test, unique(test_code$Test[test_code$Division == LabDivision]))
  lab_vol_df$MasterSetting <- factor(lab_vol_df$MasterSetting, pt_setting_order)
  lab_vol_df$DashboardPriority <- droplevels(lab_vol_df$DashboardPriority)
  
  # Summarize data
  lab_vol_summary <- lab_vol_df %>%
  group_by(Site, Test, DashboardPriority, MasterSetting, .drop = FALSE) %>%
  summarize(ResultedLabs = n())
  # Add column concatenating Test and Priority
  lab_vol_summary$TestPriority <- paste(lab_vol_summary$Test, "-", lab_vol_summary$DashboardPriority, "Labs")
  
  # Remove unused combinations (ie, Routine ED & ICU labs)
  lab_vol_summary <- lab_vol_summary[!((lab_vol_summary$MasterSetting == "ED" | lab_vol_summary$MasterSetting == "ICU")&(lab_vol_summary$DashboardPriority == "Routine")), ]
  
  # Cast summary table
  lab_vol_table <- dcast(lab_vol_summary, Test + DashboardPriority + TestPriority + MasterSetting ~ Site, value.var = "ResultedLabs")
  
  lab_vol_output <- list(lab_vol_df, lab_vol_summary, lab_vol_table)
  return(lab_vol_output)
}

lab_vol_kable_format <- function(table_data) {
  kable(table_data, format = "html", escape = FALSE, align = "c", 
        col.names = c("Test & Priority", "Setting", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM")) %>%
  kable_styling(bootstrap_options = "hover", position = "center", font_size = 11) %>%
  column_spec(column = c(1, 8), border_right = "thin solid lightgray") %>%
  add_header_above(c(" " = 1, "Resulted Lab Volume" = (ncol(table_data)-1)), background = c("white", "#00AEEF"), color = "white", line = FALSE, font_size = 13) %>%
  column_spec(column = 2:8, background = "#00AEEF", color = "white", include_thead = TRUE) %>%
  column_spec(column = 2:8, background = "inherit", color = "inherit") %>%
  # column_spec(column = 1, width_min = "125px", include_thead = TRUE) %>%
  # column_spec(column = c(3, 11), width_min = "100px", include_thead = TRUE) %>%
  row_spec(row = 0, font_size = 13) %>%
  collapse_rows(columns = c(1, 2))
}

```

### Chemistry
#### *Chemistry Resulted Lab Volume (Labs Resulted on `r wday_result_date`)*
```{r Chemistry: 24 hour volume lookback, echo = FALSE, warning = FALSE, message = FALSE}
chem_vol_wday_output <- lab_volume_summarize(scc_sun_wday_master, LabDivision = "Chemistry")
chem_vol_wday_df <- chem_vol_wday_output[[1]]
chem_vol_wday_summary <- chem_vol_wday_output[[2]]
chem_vol_wday_table <- chem_vol_wday_output[[3]]

# Manually remove unused combinations (ie. Troponin & Lactate w/o "All" as priority, BUN with "All" priority)
chem_vol_wday_table2 <- chem_vol_wday_table[!(((chem_vol_wday_table$Test == "Troponin" | chem_vol_wday_table$Test == "Lactate WB")&(chem_vol_wday_table$DashboardPriority != "All")) | (chem_vol_wday_table$Test == "BUN" & chem_vol_wday_table$DashboardPriority == "All")), ]

chem_vol_wday_table2 <- chem_vol_wday_table2[ , 3:ncol(chem_vol_wday_table2)]
rownames(chem_vol_wday_table2) <- 1:nrow(chem_vol_wday_table2)

lab_vol_kable_format(chem_vol_wday_table2)
```


### Hematology
#### *Hematology Resulted Lab Volume (Labs Resulted on `r wday_result_date`)*
```{r Hematology: 24 hour volume lookback, echo = FALSE, warning = FALSE, message = FALSE}
hem_vol_wday_output <- lab_volume_summarize(scc_sun_wday_master, LabDivision = "Hematology")
hem_vol_wday_df <- hem_vol_wday_output[[1]]
hem_vol_wday_summary <- hem_vol_wday_output[[2]]
hem_vol_wday_table <- hem_vol_wday_output[[3]]

hem_vol_wday_table <- hem_vol_wday_table[ , 3:ncol(hem_vol_wday_table)]
rownames(hem_vol_wday_table) <- 1:nrow(hem_vol_wday_table)

lab_vol_kable_format(hem_vol_wday_table)
```

### Surgical Pathology   
#### *Surgical Pathology Signed Out Cases Volume (As of `r wday_result_date`)*
```{r Surgical Pathology Signed Out Cases on a weekday, echo=FALSE, warning=FALSE, message=FALSE}
Table_Formatting_Volume(Surgical_Pathology_Stratified_Vol_Weekday_New2, pathology_volume_column_names)
```

### Cytology   
<h4><span style = "color:red">Cyto Gyn Pending - Resolution of Data Issues</h4></span style = "color:red">

#### *Cytology Accessioned Cases and Backlog Volume (As of `r wday_result_date`)*
```{r Cytology Backlog and Accessioned, echo=FALSE, warning=FALSE, message=FALSE}
#Backlog_Accessioned_Table_New2

#added this line to delete the cyto gyn from the table until we get correct data
rownames(Backlog_Accessioned_Table_New3) <- NULL
Backlog_Accessioned_Table_New3 %>% 
  kable(escape = F, align = "c", col.names = c("Case Type","Cases Accessioned","Backlog Volume", "25th Percentile", "50th Percentile", "Maximum")) %>%
  kable_styling(bootstrap_options = "hover", full_width = TRUE, position = "center", row_label_position = "c", font_size = 11) %>%
  column_spec(2, background = "#00B9F2",color ="white", include_thead = TRUE)%>%
  column_spec(3:6, background = "#221F72",color ="white", include_thead = TRUE)%>%
  column_spec(2:6, background = "inherit", color = "inherit")%>%
  row_spec(row = 0, font_size = 13)%>%
  add_header_above(c(" "= 2, "Elapsed Turn Around Time for Backlogged Cases From the Received Time"= 4), background = c("white", "#221F72"), color= "white", font_size = 13)%>%
  collapse_rows(columns = 1)
```

#### *Cytology Signed Out Cases Volume (As of `r wday_result_date`)*
```{r Cytology Signed Out Cases on a weekday, echo=FALSE, warning=FALSE, message=FALSE}
#Table_Formatting_Volume(Cytology_Stratified_Vol_Weekday_New2, cytology_volume_column_names)

#added this line to delete the cyto gyn from the table until we get correct data
rownames(Cytology_Stratified_Vol_Weekday_New3) <- NULL
Table_Formatting_Volume(Cytology_Stratified_Vol_Weekday_New3, cytology_volume_column_names)

```

`r if (include_not_wday != TRUE) {"<!--"}`

```{r Conditional Block for Weekends, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}

# Chemistry
chem_sub_output_not_wday <-lab_sub_summarize(scc_sun_not_wday_master, LabDivision = "Chemistry")
chem_subset_not_wday <- chem_sub_output_not_wday[[1]]
chemistry_summary_not_wday <- chem_sub_output_not_wday[[2]]
chemistry_dashboard1_not_wday <- chem_sub_output_not_wday[[3]]
chemistry_dashboard2_not_wday <- chem_sub_output_not_wday[[4]]

# Chemistry: Manually remove unused combinations
chemistry_dashboard2_not_wday <- chemistry_dashboard2_not_wday[!(((chemistry_dashboard2_not_wday$Test == "Troponin" | chemistry_dashboard2_not_wday$Test == "Lactate WB") & (chemistry_dashboard2_not_wday$DashboardPriority != "All" | chemistry_dashboard2_not_wday$DashboardSetting == "Amb")) | (chemistry_dashboard2_not_wday$Test == "BUN" & chemistry_dashboard2_not_wday$DashboardPriority == "All")), ]
row.names(chemistry_dashboard2_not_wday) <- 1:nrow(chemistry_dashboard2_not_wday)

chem_table_not_wday <- chemistry_dashboard2_not_wday[ , 3:ncol(chemistry_dashboard2_not_wday)]

# Hematology
hem_sub_output_not_wday <-lab_sub_summarize(scc_sun_not_wday_master, LabDivision = "Hematology")
hem_subset_not_wday <- hem_sub_output_not_wday[[1]]
hematology_summary_not_wday <- hem_sub_output_not_wday[[2]]
hematology_dashboard1_not_wday <- hem_sub_output_not_wday[[3]]
hematology_dashboard2_not_wday <- hem_sub_output_not_wday[[4]]

hem_table_not_wday <- hematology_dashboard2_not_wday[ , 3:ncol(hematology_dashboard2_not_wday)]

# Microbiology RRL
micro_sub_output_not_wday <-lab_sub_summarize(scc_sun_not_wday_master, LabDivision = "Microbiology RRL")
micro_subset_not_wday <- micro_sub_output_not_wday[[1]]
micro_summary_not_wday <- micro_sub_output_not_wday[[2]]
micro_dashboard1_not_wday <- micro_sub_output_not_wday[[3]]
micro_dashboard2_not_wday <- micro_sub_output_not_wday[[4]]

# Microbiology RRL: Manually remove unused combinations
micro_dashboard2_not_wday <- micro_dashboard2_not_wday[!(micro_dashboard2_not_wday$Test == "C. diff" & micro_dashboard2_not_wday$DashboardSetting == "Amb"), ]
row.names(micro_dashboard2_not_wday) <- 1:nrow(micro_dashboard2_not_wday)

micro_table_not_wday <- micro_dashboard2_not_wday[ , 3:ncol(micro_dashboard2_not_wday)]
```

`r #Begin weekend/holiday dashboard visualizations`

<br />

## **Weekend/Holiday Efficiency Indicators** {.tabset}
### Chemistry
#### *Chemistry KPI (Labs Resulted on `r wkend_holiday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, 
<span style = "color:orange">Yellow:</span> >=80% & <95%, 
<span style = "color:green">Green:</span> >=95%</h5>'
```{r Chemistry dashboard table for weekends and holidays, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}
# asis_output(paste('<h4> *Chemistry KPI (Labs Resulted on', wkend_holiday_result_date, ')*</h4>'))
# asis_output('<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, <span style = "color:orange">Yellow:</span> >=80% & <95%, <span style = "color:green">Green:</span> >=95%</h5>')
chem_hem_micro_kable(chem_table_not_wday)
# asis_output('<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>')
```
<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>'

### Hematology
#### *Hematology KPI (Labs Resulted on `r wkend_holiday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, 
<span style = "color:orange">Yellow:</span> >=80% & <95%, 
<span style = "color:green">Green:</span> >=95%</h5>
```{r Hematology dashboard table for weekends/holidays, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}
# asis_output('<h3> Hematology </h3>')
# asis_output(paste('<h4> *Hematology KPI (Labs Resulted on', wkend_holiday_result_date, ')*</h4>'))
# asis_output('<h5>Status Definitions: <span style = "color:red">Red:</span> <80%, <span style = "color:orange">Yellow:</span> >=80% & <95%, <span style = "color:green">Green:</span> >=95%</h5>')
chem_hem_micro_kable(hem_table_not_wday)
# asis_output('<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>')
```
<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>

```{r Microbiology RRL volume and TAT table creation for weekends/holidays, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}

micro_volume_dashboard1_not_wday <- melt(micro_summary_not_wday, id.var = c("Test", "Site", "DashboardPriority", "TestAndPriority", "DashboardSetting", "ReceiveResultTarget", "CollectResultTarget"), measure.vars = "ResultedVolume")

# Create a table with micro volume with similar formatting to TAT tables to later combine with TAT
micro_volume_dashboard2_not_wday <- dcast(micro_volume_dashboard1_not_wday, Test + DashboardPriority + TestAndPriority + DashboardSetting +ReceiveResultTarget + CollectResultTarget ~ variable + Site, value.var = "value") 

micro_volume_dashboard2_not_wday[c("ReceiveResultTarget", "CollectResultTarget")] <- "Resulted Volume"
original_length_not_wday <- ncol(micro_volume_dashboard2_not_wday)

micro_volume_dashboard2_not_wday <- micro_volume_dashboard2_not_wday[ ,c(1:ncol(micro_volume_dashboard2_not_wday), (ncol(micro_volume_dashboard2_not_wday)-(6-1)):ncol(micro_volume_dashboard2_not_wday))]

micro_volume_dashboard2_not_wday <- micro_volume_dashboard2_not_wday[ , c(1:3, 5, 4, 7:12, 6, 4, 13:18)]
colnames(micro_volume_dashboard2_not_wday) <- colnames(micro_dashboard2_not_wday)

micro_volume_dashboard2_not_wday[ , (original_length_not_wday):ncol(micro_volume_dashboard2_not_wday)] <- " "

# Combine TAT table and volume table
micro_tat_vol_comb_not_wday <- rbind(micro_dashboard2_not_wday, micro_volume_dashboard2_not_wday)

micro_tat_vol_comb_not_wday <- micro_tat_vol_comb_not_wday[order(micro_tat_vol_comb_not_wday$Test, micro_tat_vol_comb_not_wday$ReceiveResultTarget), ]
row.names(micro_tat_vol_comb_not_wday) <- 1:nrow(micro_tat_vol_comb_not_wday)

micro_tat_vol_table_not_wday <- micro_tat_vol_comb_not_wday[ , 3:ncol(micro_tat_vol_comb_not_wday)]
```

### Microbiology RRL
#### *Microbiology RRL (Labs Resulted on `r wkend_holiday_result_date`)*
<h5>Status Definitions: <span style = "color:red">Red:</span> <90%, 
<span style = "color:orange">Yellow:</span> >=90% & <100%, 
<span style = "color:green">Green:</span> =100%</h5>
```{r Microbiology RRL dashboard table for weekends/holidays, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}
# asis_output('<h3> Microbiology RRL  </h3>')
# asis_output(paste('<h4> *Microbiology RRL KPI (Labs Resulted on ', wkend_holiday_result_date, ')*</h4>'))
# asis_output('<h5>Status Definitions: <span style = "color:red">Red:</span> <90%, <span style = "color:orange">Yellow:</span> >=90% & <100%, <span style = "color:green">Green:</span> =100%</h5>')
chem_hem_micro_kable(micro_tat_vol_table_not_wday) 
# asis_output('<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>')
```
<h6>*TAT Analysis excludes add on orders, labs with missing timestamps, labs with negative TAT, and labs not from above settings.*</h6>

### Missing Collections & Add Ons
    
#### *Missing Collection Times and Add On Order Volume (Labs Resulted on `r wkend_holiday_result_date`)*

<h5> Missing Collection Status Definitions: <span style = "color:red">Red:</span> >15%, 
<span style = "color:orange">Yellow:</span> <=15% & >5%, 
<span style = "color:green">Green:</span> <=5%</h5>
```{r Missing Collection Times Tables for weekends/holidays, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}
# asis_output('<h3> Missing Collections & Add On Orders </h3>')
# asis_output(paste('<h4>*Missing Collection Times and Add On Order Volume (Labs Resulted on ', wkend_holiday_result_date, ')*</h4>'))
# asis_output('<h5>Status Definitions: <span style = "color:red">Red:</span> >15%, <span style = "color:orange">Yellow:</span> <=15% & >5%, <span style = "color:green">Green:</span> <=5%</h5>')

# Determine percentage of labs with missing collection times -------------------------------
missing_collect_not_wday <- scc_sun_not_wday_master[scc_sun_not_wday_master$TATInclude == TRUE, ] %>%
  group_by(Site, .drop = FALSE) %>%
  summarize(ResultedVolume = n(), MissingCollection = sum(MissingCollect, na.rm = TRUE), Percent = MissingCollection/ResultedVolume)

# Format percentage of labs with missing collection times as percentage
missing_collect_not_wday$Percent <- percent(missing_collect_not_wday$Percent, digits = 0)

# Apply conditional formatting to percentage of labs with missing collection times
missing_collect_not_wday <- missing_collect_not_wday %>%
  mutate(Percent = cell_spec(Percent, "html", color = ifelse(is.na(Percent), "grey",
                                                         ifelse(Percent <= 0.05, "green",
                                                                ifelse(Percent >0.05 &  Percent <= 0.15, "orange", "red")))))

missing_collect_table_not_wday <- dcast(missing_collect_not_wday, "Percentage of Specimens" ~ Site, value.var = "Percent")

missing_collect_table_not_wday %>%
  kable(format = "html", escape = FALSE, align = "c",  
        col.names = c("Site", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM")) %>%
  kable_styling(bootstrap = "hover", position = "float_left", font_size = 11, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Percentage of Labs with Missing Collect Times" = ncol(missing_collect_table)-1), background = c("white", "#00AEEF"), color = "white", line = FALSE, font_size = 13) %>%
  column_spec(column = c(1, ncol(missing_collect_table)), border_right = "thin solid lightgray") %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "#00AEEF", color = "white", include_thead = TRUE) %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "inherit", color = "inherit", width_max = 0.15) %>%
  row_spec(row = 0, font_size =13)

add_on_volume_not_wday <- scc_sun_not_wday_master %>%
  group_by(Test, Site, .drop = FALSE) %>%
  summarize(AddOnVolume = sum(AddOnMaster == "AddOn", na.rm = TRUE))

add_on_table_not_wday <- dcast(add_on_volume_not_wday, Test ~ Site, value.var = "AddOnVolume")

add_on_table_not_wday %>%
  kable(format = "html", escape = FALSE, align = "c",
        col.names = c("Test", "MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM"), color = "gray") %>%
  kable_styling(bootstrap = "hover", position = "right", font_size = 11, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Volume of Add On Labs" = ncol(missing_collect_table)-1), background = c("white", "#00AEEF"), color = "white", line = FALSE, font_size = 13) %>%
  column_spec(column = c(1, ncol(missing_collect_table)), border_right = "thin solid lightgray") %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "#00AEEF", color = "white", include_thead = TRUE) %>%
  column_spec(column = c(2:ncol(missing_collect_table)), background = "inherit", color = "inherit") %>%
  row_spec(row = 0, font_size =13)

# asis_output('<h6>*Missing collection time analysis only includes labs represented in dashboard included in TAT analysis*</h6>
# ')
```
<h6>*Missing collection time analysis only includes labs represented in dashboard included in TAT analysis*</h6>

`r #Start new tabset for weekend/holiday volume lookback`

<br />

## **Weekend/Holiday Volume Lookback** {.tabset}

### Chemistry
#### *Chemistry Resulted Lab Volume (Labs Resulted on `r wkend_holiday_result_date`)*
```{r Chemistry Weekend/Holiday: volume lookback, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}
# asis_output('<h3> Chemistry </h3>')
# asis_output(paste('<h4> *Chemistry Resulted Lab Volume (Labs Resulted on ', wkend_holiday_result_date, ')*</h4>'))
chem_vol_not_wday_output <- lab_volume_summarize(scc_sun_not_wday_master, LabDivision = "Chemistry")
chem_vol_not_wday_df <- chem_vol_not_wday_output[[1]]
chem_vol_not_wday_summary <- chem_vol_not_wday_output[[2]]
chem_vol_not_wday_table <- chem_vol_not_wday_output[[3]]

# Manually remove unused combinations (ie. Troponin & Lactate w/o "All" as priority, BUN with "All" priority)
chem_vol_not_wday_table2 <- chem_vol_not_wday_table[!(((chem_vol_not_wday_table$Test == "Troponin" | chem_vol_not_wday_table$Test == "Lactate WB")&(chem_vol_not_wday_table$DashboardPriority != "All")) | (chem_vol_not_wday_table$Test == "BUN" & chem_vol_not_wday_table$DashboardPriority == "All")), ]

chem_vol_not_wday_table2 <- chem_vol_not_wday_table2[ , 3:ncol(chem_vol_not_wday_table2)]
rownames(chem_vol_not_wday_table2) <- 1:nrow(chem_vol_not_wday_table2)

lab_vol_kable_format(chem_vol_not_wday_table2)
```

### Hematology
#### *Hematology Resulted Lab Volume (Labs Resulted on `r wkend_holiday_result_date`)*
```{r Hematology Weekend/Holiday: volume lookback, eval = include_not_wday, warning = FALSE, message = FALSE, echo = FALSE}
# asis_output('<h3> Hematology </h3>')
# asis_output(paste('<h4> *Hematology Resulted Lab Volume (Labs Resulted on ', wkend_holiday_result_date, ')*</h4>'))

hem_vol_not_wday_output <- lab_volume_summarize(scc_sun_not_wday_master, LabDivision = "Hematology")
hem_vol_not_wday_df <- hem_vol_not_wday_output[[1]]
hem_vol_not_wday_summary <- hem_vol_not_wday_output[[2]]
hem_vol_not_wday_table <- hem_vol_not_wday_output[[3]]

hem_vol_not_wday_table <- hem_vol_not_wday_table[ , 3:ncol(hem_vol_not_wday_table)]
rownames(hem_vol_not_wday_table) <- 1:nrow(hem_vol_not_wday_table)

lab_vol_kable_format(hem_vol_not_wday_table)
```


`r if (include_not_wday != TRUE) {"-->"}`

## **Assumptions and Methodology** {.tabset}

### Chemistry

### Hematology

### Microbiology RRL

### Surgical Pathology

### Cytology

<h6> *End of report.* </h6>

