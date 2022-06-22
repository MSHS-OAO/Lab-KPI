#######
# Code for compiling the historical data for pathology and cytology analysis
# Data compiled include:
# 1. PowerPath daily data for Pathology and Cytology
# 2. EPIC daily data for cytology
# 3. Cytology Backlog Data
#######

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
library(writexl)
library(gsubfn)

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
holiday_det <- isHoliday(as.timeDate(yesterday), holidays = mshs_holiday)

#Set up a calendar for collect to received TAT calculations for Path & Cyto
create.calendar("MSHS_working_days", mshs_holiday,
                weekdays = c("saturday", "sunday"))
bizdays.options$set(default.calendar = "MSHS_working_days")


# Select file/folder path for easier file selection and navigation

if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}


#import historical data for first run vs. future runs
initial_run <- FALSE

if (initial_run == TRUE) {
  existing_powerpath_repo <- NULL
  existing_backlog_repo <- NULL
  ##### pull powerpath daily data
  file_list_sp <-
    list.files(
      path =
        paste0(user_directory, "\\AP & Cytology Signed Cases Reports"),
      pattern =
        paste0("(KPI REPORT - RAW DATA V4_V2)"))

  sp_list <-
    lapply(file_list_sp,
           function(x) read_excel(
             path = paste0(user_directory,
                           "\\AP & Cytology Signed Cases Reports\\", x),
             skip = 1))

  ### fix the collection date for the powerpath daily data
  for (i in (1:length(sp_list))) {
    #check if any of the dates was imported as char
    if (is.character(sp_list[[i]]$Collection_Date)) {
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = as.numeric(Collection_Date))
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = as.Date(Collection_Date,
                                         origin = "1899-12-30"))
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = as.POSIXct(Collection_Date,
                                            tz = "",
                                            format = "%m/%d/%y %I:%M %p"))
    } else {
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = Collection_Date)
    }
  }

  ##### pull the powerpath aggregated data
  aggregated_1 <-
    read_excel(
      path =
        paste0(
          user_directory,
          "\\AP & Cytology Signed Cases Reports\\",
          "KPI_RAW_V4_V2_Jan-Sept-2020 file1.xlsx"))

  aggregated_2 <-
    read_excel(
      path =
        paste0(
          user_directory,
          "\\AP & Cytology Signed Cases Reports\\",
          "KPI_RAW_V4_V2_Jan-Sept-2020 file2.xlsx"))

  ## correct the collection date
  aggregated_1_2 <- rbind(aggregated_1, aggregated_2)
  aggregated_1_2 <- aggregated_1_2 %>%
    mutate(Collection_Date = as.numeric(Collection_Date))
  aggregated_1_2 <- aggregated_1_2 %>%
    mutate(Collection_Date = as.Date(Collection_Date,
                                     origin = "1899-12-30"))
  aggregated_1_2 <- aggregated_1_2 %>%
    mutate(Collection_Date = as.POSIXct(Collection_Date, tz = "",
                                        format = "%m/%d/%y %I:%M %p"))

  ##change the column names of the aggregated data and their
  #order to match the daily data
  colnames(aggregated_1_2) <- c("Facility",
                                "Priority",
                                "spec_group",
                                "spec_sort_order",
                                "Spec_code",
                                "Specimen_description",
                                "CPT_code",
                                "Fee_sch",
                                "Rev_ctr",
                                "Encounter_no",
                                "MRN",
                                "AGE",
                                "Case_no",
                                "Case_created_date",
                                "Collection_Date",
                                "Received_Date",
                                "signed_out_date",
                                "TAT",
                                "signed_out_Pathologist",
                                "Refmd_name",
                                "Refmd_code",
                                "NPI_NO",
                                "patient_type")

  aggregated_1_2 <- aggregated_1_2[, c("Facility",
                                       "Priority",
                                       "spec_group",
                                       "Spec_code",
                                       "Specimen_description",
                                       "CPT_code",
                                       "Fee_sch",
                                       "Rev_ctr",
                                       "Encounter_no",
                                       "MRN",
                                       "AGE",
                                       "Case_no",
                                       "Case_created_date",
                                       "Collection_Date",
                                       "Received_Date",
                                       "signed_out_date",
                                       "TAT",
                                       "signed_out_Pathologist",
                                       "Refmd_name",
                                       "Refmd_code",
                                       "NPI_NO",
                                       "spec_sort_order",
                                       "patient_type")]

  #### bind the rows for the daily data
  sp_df_combined_ <- bind_rows(sp_list)
  sp_df_combined_ <-
    sp_df_combined_[!sp_df_combined_$Facility ==
                             "Page -1 of 1", ]

  #### bind the daily data with the aggregated data
  sp_df_combined <- rbind(aggregated_1_2, sp_df_combined_)

  ##### pull EPIC cytology daily data
  ### pull daily data
  file_list_epic <-
    list.files(
      path =
        paste0(
          user_directory, "\\EPIC Cytology"),
      pattern = "^(MSHS Pathology Orders Epic){1}.*",
      ignore.case = TRUE)

  epic_cyto_list <-
    lapply(file_list_epic, function(x) read_excel(path =
                                                    paste0(user_directory,
                                                           "\\EPIC Cytology\\",
                                                           x)))
  epic_df_combined <- bind_rows(epic_cyto_list)

  #pull backlog cytology daily data
  file_list_backlog <-
    list.files(
      path =
        paste0(
          user_directory, "\\Cytology Backlog Reports"),
      pattern = "^KPI REPORT - CYTOLOGY PENDING CASES")

  backlog_list <-
    lapply(file_list_backlog,
           function(x) read_excel(
             path = paste0(user_directory,
                           "\\Cytology Backlog Reports\\", x),
             skip = 1))

  backlog_rep_date <-
    lapply(
      file_list_backlog,
      function(x) read_excel(
        path =
          paste0(user_directory,
                 "\\Cytology Backlog Reports\\", x),
        range = "A1"))

  backlog_rep_date_new <-
    lapply(backlog_rep_date,
           function(x) strapplyc(colnames(x), "\\d+/\\d+/\\d+",
                                 simplify = TRUE))
  backlog_list_trial <-
    mapply(cbind, backlog_list,
           "Report_Date" = backlog_rep_date_new,
           SIMPLIFY = F)
  backlog_df_combined <- bind_rows(backlog_list_trial)
  backlog_df_combined <-
    backlog_df_combined[!backlog_df_combined$Facility == "Page -1 of 1", ]

} else{
  # Import existing historical repository
  existing_powerpath_repo <-
    readRDS(file =
              choose.files(default =
                             paste0(user_directory,
                                    "/AP & Cytology Historical Repo/*.*"),
                           caption = "Select AP Historical Repository"))
  existing_backlog_repo <-
    readRDS(file =
              choose.files(default =
                             paste0(user_directory,
                                    "/AP & Cytology Historical Repo",
                                    "/Backlog Repo",
                                    "/*.*"),
                           caption = "Select Backlog Repository"))
  #
  # Find last date of resulted lab data in historical repository
  last_date <- as.Date(max(existing_powerpath_repo$Signed_out_date_only),
                       format = "%Y-%m-%d")
  # Determine today's date to determine last possible data report
  todays_date <- today
  # Create vector with possible data report dates
  if (last_date + 2 > todays_date) {
    date_range <- seq(from = todays_date,
                          to = todays_date,
                          by = "day")
  } else {
    date_range <- seq(from = last_date + 2,
                          to = todays_date,
                          by = "day")
  }

  ##### pull powerpath daily data
  file_list_sp <-
    list.files(
      path =
        paste0(
          user_directory,
          "\\AP & Cytology Signed Cases Reports"),
      pattern =
        paste0(
          "^KPI REPORT\\ - \\RAW DATA V4_V2.+", date_range, collapse = "|"))

  sp_list <-
    lapply(
      file_list_sp,
      function(x) read_excel(
        path =
          paste0(user_directory,
                 "\\AP & Cytology Signed Cases Reports\\", x),
        skip = 1))

  for (i in (1:length(sp_list))) {
    #check if any of the dates was imported as char
    if (is.character(sp_list[[i]]$Collection_Date)) {
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = as.numeric(Collection_Date))
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = as.Date(Collection_Date,
                                         origin = "1899-12-30"))
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = as.POSIXct(Collection_Date,
                                            tz = "",
                                            format = "%m/%d/%y %I:%M %p"))
    } else {
      sp_list[[i]] <- sp_list[[i]] %>%
        mutate(Collection_Date = Collection_Date)
    }
  }

  sp_df_combined <- bind_rows(sp_list)
  sp_df_combined <-
    sp_df_combined[!sp_df_combined$Facility ==  "Page -1 of 1", ]

  ##### pull EPIC cytology daily data
  file_list_epic <-
    list.files(
      path =
        paste0(
          user_directory, "\\EPIC Cytology"),
      pattern = paste0("^(MSHS Pathology Orders Epic){1}.*",
                       date_range, collapse = "|"),
      ignore.case = TRUE)

  epic_cyto_list <-
    lapply(file_list_epic,
           function(x) read_excel(path = paste0(user_directory,
                                                "\\EPIC Cytology\\", x)))
  epic_df_combined <- bind_rows(epic_cyto_list)

  #pull backlog cytology daily data
  file_list_backlog <-
    list.files(
      path =
        paste0(
          user_directory,
          "\\Cytology Backlog Reports"),
      pattern =
        paste0(
          "^KPI REPORT - CYTOLOGY PENDING CASES.+", date_range, collapse = "|"))

  backlog_list <-
    lapply(
      file_list_backlog,
      function(x) read_excel(
        path =
          paste0(user_directory,
                 "\\Cytology Backlog Reports\\", x),
        skip = 1))

  backlog_rep_date <-
    lapply(
      file_list_backlog,
      function(x) read_excel(
        path =
          paste0(user_directory,
                 "\\Cytology Backlog Reports\\", x),
        range = "A1"))

  backlog_rep_date_new <-
    lapply(backlog_rep_date,
           function(x) strapplyc(colnames(x), "\\d+/\\d+/\\d+",
                                 simplify = TRUE))

  backlog_list_trial <-
    mapply(cbind, backlog_list,
           "Report_Date" = backlog_rep_date_new,
           SIMPLIFY = F)

  backlog_df_combined <- bind_rows(backlog_list_trial)
  backlog_df_combined <-
    backlog_df_combined[!backlog_df_combined$Facility == "Page -1 of 1", ]

}

# Import analysis reference data
reference_file <- paste0(user_directory,
                         "/Code Reference/",
                         "Analysis Reference 2022-06-21.xlsx")


#-----------Patient Setting Excel File-----------#
#Using Rev Center to determine patient setting
patient_setting <- data.frame(read_excel(reference_file,
                                         sheet = "AP_Patient Setting"),
                              stringsAsFactors = FALSE)

#-----------Anatomic Pathology Targets Excel File-----------#
tat_targets_ap <- data.frame(read_excel(reference_file,
                                        sheet = "AP_TAT Targets"),
                             stringsAsFactors = FALSE)

#-----------GI Codes Excel File-----------#
#Upload the exclusion vs inclusion criteria associated with the GI codes
gi_codes <- data.frame(read_excel(reference_file, sheet = "GI_Codes"),
                       stringsAsFactors = FALSE)

##preprocessing for epic data

#we are only looking for the specimens that were finalized/final edited
epic_weekday_final <-
  epic_df_combined[which(
    epic_df_combined$LAB_STATUS == "Final result" |
      epic_df_combined$LAB_STATUS == "Edited Result - FINAL"), ]

#get only the SPECIMEN_ID Column from Epic data
epic_finalized_specimen <- NULL
epic_finalized_specimen <- as.data.frame(epic_weekday_final$SPECIMEN_ID)
colnames(epic_finalized_specimen) <- c("Case_no")

#keep only the unique specimen IDs
epic_finalized_specimen <- unique(epic_finalized_specimen)

sp_df_combined <- sp_df_combined %>% mutate(Facility_Old = Facility)
sp_df_combined$Facility[sp_df_combined$Facility_Old ==
                                 "*failed to decode utf16*MSH"] <- "MSH"
sp_df_combined$Facility[sp_df_combined$Facility_Old ==
                                 "MSS"] <- "MSH"
sp_df_combined$Facility[sp_df_combined$Facility_Old ==
                                 "STL"] <- "SL"
sp_df_combined$spec_group[sp_df_combined$spec_group ==
                                   "BREAST"] <- "Breast"

#---Extract the All Breast, GI Specimens, cyto gyn and cyto nongyn Data Only

#Merge the exclusion/inclusion cloumn into the combined PP data

sp_df_exc <- merge(x = sp_df_combined, y = gi_codes, all.x = TRUE)

cytology_ <-
  sp_df_exc[which(
    (sp_df_exc$spec_group == "CYTO NONGYN" |
       sp_df_exc$spec_group == "CYTO GYN") &
      sp_df_exc$spec_sort_order == "A"), ]

# order dataframe by signed out date from newest to oldest
cytology_ <- cytology_[order(cytology_$signed_out_date, decreasing = TRUE), ]
# order dataframe based on case no
cytology_ <- cytology_[order(cytology_$Case_no), ]
cytology_ <- unique(cytology_)

#create a column for unique case numbers
cytology_$unique_case_no <- TRUE

#add false next to the not unique case number and the older date
for (i in 1:length(cytology_$Case_no)) {
  ifelse(
    cytology_$Case_no[i + 1] == cytology_$Case_no[i],
    cytology_$unique_case_no[i + 1] <- FALSE,
    cytology_$unique_case_no[i + 1] <- TRUE)
  }

#only keep the unique case number with the newest date
cytology_ <- cytology_[which(cytology_$unique_case_no == TRUE), ]

#only keep the powerpath data that matched with EPIC cytology
cytology <- merge(x = cytology_, y = epic_finalized_specimen)
cytology$unique_case_no <- NULL

#find case numbers with GI_Code = exclude
exclude_gi_codes_df <-
  sp_df_exc[
    which(
      (sp_df_exc$spec_group == "GI") &
        !(sp_df_exc$GI_Code_InclExcl %in%
           "Include")), ]

must_exclude_cnum <- unique(exclude_gi_codes_df$Case_no)

surgical_pathology <-
  sp_df_exc[
    which(
      (((sp_df_exc$spec_group == "GI") &
          (!(sp_df_exc$Case_no %in% must_exclude_cnum))) |
         (sp_df_exc$spec_group == "Breast")) &
        sp_df_exc$spec_sort_order == "A"), ]

sp_df_combined_new <- rbind(cytology, surgical_pathology)
sp_df_combined_new <- unique(sp_df_combined_new)

#------------------------Data Pre-Processing-------------------------#
############Create a function for Data pre-processing############

pre_processing_historical <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    raw_data_ps <- NULL
    raw_data_new <- NULL
    summarized_table <- NULL
  } else {
    #vlookup the Rev_Center and its corresponding patient setting for the
    #PowerPath Data
    raw_data_ps <- merge(x = raw_data, y = patient_setting, all.x = TRUE)

    #make sure to fix the MSBK patient type based on the extra column in the
    #new report
    raw_data_ps$Patient.Setting[raw_data_ps$Rev_ctr == "MSBK" &
                                  (raw_data_ps$patient_type == "A" |
                                     raw_data_ps$patient_type == "O")] <- "Amb"

    raw_data_ps$Patient.Setting[raw_data_ps$Rev_ctr == "MSBK" &
                                  raw_data_ps$patient_type == "IN"] <- "IP"

    #vlookup targets based on spec_group and patient setting
    raw_data_new <- merge(x = raw_data_ps, y = tat_targets_ap,
                          all.x = TRUE, by = c("spec_group", "Patient.Setting"))

    #check if any of the dates was imported as char
    if (is.character(raw_data_new$Collection_Date)) {
      raw_data_new <- raw_data_new %>%
        mutate(Collection_Date = as.numeric(Collection_Date)) %>%
        mutate(Collection_Date = as.Date(Collection_Date,
                                         origin = "1899-12-30"))
    } else {
      raw_data_new <- raw_data_new %>%
        mutate(Collection_Date = Collection_Date)
    }
    #Change all Dates into POSIXct format to start the calculations
    raw_data_new[c("Case_created_date",
                   "Collection_Date",
                   "Received_Date",
                   "signed_out_date")] <-
      lapply(raw_data_new[c("Case_created_date",
                            "Collection_Date",
                            "Received_Date",
                            "signed_out_date")],
             as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p")

    #add columns for calculations:
    #collection to signed out and received to signed out
    #collection to signed out
    #All days in the calendar
    raw_data_new <- raw_data_new %>%
      mutate(Collection_to_signed_out =
               as.numeric(difftime(signed_out_date, Collection_Date,
                                   units = "days")))
    #recieve to signed out
    #without weekends and holidays
    raw_data_new <- raw_data_new %>%
      mutate(Received_to_signed_out = bizdays(Received_Date, signed_out_date))

    #prepare data for first part accessioned volume analysis
    #1. Find the date that we need to report --> the date of the last weekday
    raw_data_new$report_date_only <- as.Date(raw_data_new$signed_out_date) + 1

    #2. count the accessioned volume that was accessioned on that date
    #from the cyto report
    raw_data_new$acc_date_only <- as.Date(raw_data_new$Received_Date)

    #summarize the data to be used for analysis and to be stored as historical
    #repo
    summarized_table <-
      summarise(
        group_by(raw_data_new,
                 Spec_code,
                 spec_group,
                 Facility,
                 Patient.Setting,
                 Rev_ctr,
                 as.Date(signed_out_date),
                 weekdays(as.Date(signed_out_date)),
                 Received.to.signed.out.target..Days.,
                 Collected.to.signed.out.target..Days.,
                 acc_date_only,
                 weekdays(acc_date_only),
                 report_date_only,
                 weekdays(report_date_only)),
        no_cases_signed = n(),
        lab_metric_tat_avg = round(mean(Received_to_signed_out,
                                        na.rm = TRUE), 0),
        lab_metric_tat_med = round(median(Received_to_signed_out,
                                          na.rm = TRUE), 0),
        lab_metric_tat_sd = round(sd(Received_to_signed_out, na.rm = TRUE), 1),
        lab_metric_within_target = as.numeric(format(
          round(
            sum(Received_to_signed_out <= Received.to.signed.out.target..Days.,
                na.rm = TRUE) / sum(
                  Received_to_signed_out >= 0, na.rm = TRUE), 2))),
        patient_metric_tat_avg = as.numeric(format(
          ceiling(mean(Collection_to_signed_out, na.rm = TRUE)))),
        patient_metric_tat_med = round(median(Collection_to_signed_out,
                                              na.rm = TRUE), 0),
        patient_metric_tat_sd = round(sd(Collection_to_signed_out,
                                         na.rm = TRUE), 1),
        cyto_acc_vol = as.numeric(sum((report_date_only - 1) == acc_date_only,
                                      na.rm = TRUE)))

    colnames(summarized_table) <-
      c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
        "Signed_out_date_only", "Signed_out_day_only", "Lab_metric_target",
        "Patient_metric_target", "acc_date_only", "acc_day_only",
        "report_date_only", "report_day_only", "No_cases_signed_out",
        "Lab_metric_avg", "Lab_metric_med", "Lab_metric_std",
        "Lab_metric_within_target", "Patient_metric_avg", "Patient_metric_med",
        "Patient_metric_std", "cyto_acc_vol")


  }
  return(summarized_table)
}

hist_data_summarized <- pre_processing_historical(sp_df_combined_new)
hist_data_summarized <- as.data.frame(hist_data_summarized)

#Bind new repo with old repo
hist_data_summarized_new <- rbind(existing_powerpath_repo, hist_data_summarized)
hist_data_summarized_new <- unique(hist_data_summarized_new)

#main historical repo
ap_start_date <-
  format(min(hist_data_summarized_new$Signed_out_date_only), "%m-%d-%y")
ap_end_date <-
  format(max(hist_data_summarized_new$Signed_out_date_only), "%m-%d-%y")

ap_repo_file_name <- paste0(user_directory,
                            "/AP & Cytology Historical Repo/",
                            "AP Repo", "_",
                            ap_start_date, " to ", ap_end_date,
                            " Created ", Sys.Date(), ".RDS")

saveRDS(hist_data_summarized_new, file = ap_repo_file_name)

#------------------------backlog Data Pre-Processing-------------------------#
#############################################################################
cyto_backlog_hist <- function(cyto_backlog_raw) {
  #cyto backlog Calculation
  #vlookup the Rev_Center and its corresponding patient setting for the
  #PowerPath Data

  cyto_backlog_ps <- merge(x = backlog_df_combined, y = patient_setting,
                           all.x = TRUE)

  #vlookup targets based on spec_group and patient setting
  cyto_backlog_ps_target <- merge(x = cyto_backlog_ps, y = tat_targets_ap,
                                  all.x = TRUE,
                                  by = c("spec_group", "Patient.Setting"))

  #Keep the cyto gyn and cyto non-gyn
  cyto_backlog <-
    cyto_backlog_ps_target[which(
      cyto_backlog_ps_target$spec_group == "CYTO NONGYN" |
        cyto_backlog_ps_target$spec_group == "CYTO GYN"), ]

  #Change all Dates into POSIXct format to start the calculations
  cyto_backlog[c("Case_created_date", "Collection_Date", "Received_Date",
                 "signed_out_date")] <-
    lapply(cyto_backlog[c("Case_created_date", "Collection_Date",
                          "Received_Date", "signed_out_date")],
           as.POSIXct, tz = "", format = "%m/%d/%y %I:%M %p")
  cyto_backlog <- cyto_backlog %>%
    mutate(Report_Date = as.Date(Report_Date, format = "%m/%d/%Y"))

  #Backlog Calculations: Report Date - case created date
  #without weekends and holidays, subtract one so we don't include today's date
  cyto_backlog$backlog <-
    bizdays(cyto_backlog$Case_created_date, cyto_backlog$Report_Date) - 1

  cyto_backlog$acc_date_only <- as.Date(cyto_backlog$Received_Date)

  #summarize the data to be used for analysis and to be stored as historical
  #repo
  summarized_table <-
    summarise(
      group_by(cyto_backlog,
               Spec_code,
               spec_group,
               Facility,
               Patient.Setting,
               Rev_ctr,
               acc_date_only,
               weekdays(acc_date_only),
               Report_Date),
      cyto_backlog = format(
        round(
          sum(
            backlog > Received.to.signed.out.target..Days.,
            na.rm = TRUE), 0)),

      percentile_25th =
        format(
          ceiling(
            quantile(
              backlog[backlog > Received.to.signed.out.target..Days.],
              prob = 0.25, na.rm = TRUE))),

      percentile_50th =
        format(
          ceiling(
            quantile(
              backlog[backlog > Received.to.signed.out.target..Days.],
              prob = 0.5, na.rm = TRUE))),

      maximum = format(
        ceiling(
          max(
            backlog[backlog > Received.to.signed.out.target..Days.],
            na.rm = TRUE))),

      cyto_acc_vol = as.numeric(sum((Report_Date - 1) == acc_date_only,
                                    na.rm = TRUE)))

  summarized_table$maximum[summarized_table$maximum == "-Inf"] <- 0

  #standardize the name for the current summary to match the historical repo
  colnames(summarized_table) <-
    c("Spec_code", "Spec_group", "Facility", "Patient_setting", "Rev_ctr",
      "acc_date_only", "acc_day_only", "Report_Date", "cyto_backlog",
      "percentile_25th", "percentile_50th", "maximum", "cyto_acc_vol")

  return(summarized_table)
}

backlog_data_summarized <- cyto_backlog_hist(backlog_df_combined)
backlog_data_summarized <- as.data.frame(backlog_data_summarized)

#Bind new repo with old repo
backlog_data_summarized_new <-
  rbind(existing_backlog_repo, backlog_data_summarized)
backlog_data_summarized_new <- unique(backlog_data_summarized_new)

#main historical repo
backlog_repo_file_name <-
  paste0(user_directory, "/AP & Cytology Historical Repo", "/Backlog Repo",
         "/Backlog_Repo", "_", today, ".RDS")

saveRDS(backlog_data_summarized_new, file = backlog_repo_file_name)
