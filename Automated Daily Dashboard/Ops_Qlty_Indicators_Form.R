#######
# Code for preprocessing and analyzing the lab KPI form for the second run -----
#######

# Create regular expression for Lab KPI Form file
kpi_form_pattern <- "^(Lab KPI Quality Indicator Form){1}.*"

kpi_form_date <- format(today, "%Y-%m-%d")

kpi_data_file <- list.files(
  path = paste0(user_directory, "/Lab KPI Form"),
  pattern = paste0(kpi_form_pattern,
                   "(",
                   kpi_form_date,
                   ".xlsx)$"))

#read the excel sheet that is generated from office form responses
kpi_form <- data.frame(read_excel(
  path = paste0(user_directory,
                "/Lab KPI Form/",
                kpi_data_file),
  sheet = 1),
  stringsAsFactors = FALSE)

# read_excel(choose.files(default = paste0(user_directory, "/Lab KPI Form/*.*"),
#              caption = "Select the KPI form generated today"), 1),
# stringsAsFactors = FALSE)


#Determine the dates within the KPI excel sheet and report only the latest date
kpi_form <- kpi_form %>%
  mutate(Completion_Date = as.Date(as.POSIXct(Completion.time, tz = "",
                                              format = "%m/%d/%y %I:%M %p")))

kpi_form <- kpi_form %>%
  mutate(Completion_Hour = format(Completion.time, format = "%H:%M:%S"))

#Rename the columns of the kpi form data
colnames(kpi_form) <- c("ID", "Start_time", "Completion_time", "Email",
                        "Name", "Facility", "LabCorp", "Vendor_Services",
                        "Environment", "Equipment", "IT","Service_Changes",
                        "Volume", "Staffing", "Comments", "NEVER_EVENTS",
                        "NEVER_EVENTS_COMMENTS", "Good_Catch", "LIS_Staffing",
                        "LIS_Unplanned_Service", "LIS_Preplanned_Downtime",
                        "Completion_Date", "Completion_Hour")

#keep only unique rows with the latest timestamp
#order data by facility and time by descending order
kpi_form <- kpi_form[with(kpi_form, order(Facility, -ID)), ]
#add a column to include the dupliacted values
kpi_form$duplicated_id <- duplicated(kpi_form[, c("Facility",
                                                  "Completion_Date")])
#only keep the unique ids
kpi_form <-
  kpi_form[which(kpi_form$duplicated_id == "FALSE"), ]

kpi_form_today  <-
  kpi_form[which(kpi_form$Completion_Date == as.Date(today) |
                   (kpi_form$Completion_Date == as.Date(yesterday) &
                      kpi_form$Completion_Hour >= "17:00:00")), ]

#split the data into 3 datasets
# the first dataset representes the following: CP/AP/CPA/4LABS
clinical_labs_ops_ind <-
  kpi_form_today[
    which(kpi_form_today$Facility !=
            "Laboratory Information System (LIS)"), c(6:15)]

# the second dataset represents the LIS indicators
lis_ops_ind <-
  kpi_form_today[
    which(kpi_form_today$Facility ==
            "Laboratory Information System (LIS)"), c(6, 19:21)]

# the third and last dataset is the never events and good catches dataset
never_events <-
  kpi_form_today[
    which(kpi_form_today$Facility !=
            "Laboratory Information System (LIS)"), c(6, 16:18)]

####### Start creating and formatting the tables

#first melt the tables to get the values needed in one column:

#1. Clinical labs table
clinical_labs_melt <-
  melt(clinical_labs_ops_ind,
       id = c("Facility", "Comments"))

#2. LIS table
lis_melt <- melt(lis_ops_ind,
                 id = c("Facility",
                        "LIS_Unplanned_Service",
                        "LIS_Preplanned_Downtime"))

##### create a function to rename the status of the measures to a standard one
#( Safe, Under Stress, and Not Safe)

renaming <- function(melted_dataset) {
  if (is.null(melted_dataset) || nrow(melted_dataset) == 0) {
    melted_dataset <- NULL
  } else {
    melted_dataset[melted_dataset == "Green (No issues)"] <- "Safe"
    
    melted_dataset[
      melted_dataset == "Yellow (Safe/Under Stress)" |
        melted_dataset == "Yellow (Delays)" |
        melted_dataset == "Yellow (Delays in pickup/delivery)" |
        melted_dataset ==
        "Yellow (Shortage with no/minimal significant impact)" |
        melted_dataset ==
        "Yellow (Minor issues/Coordinating with Reference Labs)"] <-
      "Under Stress"
    
    melted_dataset[
      melted_dataset == "Yellow (Safe/Under Stress)" |
        melted_dataset == "Yellow (Delays)" |
        melted_dataset == "Yellow (Delays in pickup/delivery)" |
        melted_dataset ==
        "Yellow (Shortage with no/minimal significant impact)" |
        melted_dataset ==
        "Yellow (Minor issues/Coordinating with Reference Labs)"] <-
      "Under Stress"
    
    melted_dataset[
      melted_dataset == "Red (Severe shortage of consumables)" |
        melted_dataset == "Red (Missed pickups)" |
        melted_dataset == "Red (Not Safe/Risk)" |
        melted_dataset == "Red (Severe shortage that halts activity)" |
        melted_dataset == "Red (Major issues/Requires Immediate Attention)"] <-
      "Not Safe"
  }
  
  return(melted_dataset)
  
}

clinical_labs_melt_new <- renaming(clinical_labs_melt)
lis_melt_new <- renaming(lis_melt)

### create a function to format the data table into the correct colors
conditional_format_form <- function(melted_data_new) {
  if (is.null(melted_data_new) || nrow(melted_data_new) == 0) {
    melted_data_new <- NULL
  } else {
    melted_data_new <-
      melted_data_new %>%
      mutate(
        value =
          ifelse(is.na(value),
                 cell_spec(value, "html", color = "lightgray"),
                 ifelse((value == "Safe" | value == "None"),
                        cell_spec(value, "html", color  = "green"),
                        ifelse((value == "Not Safe" | value == "Present"),
                               cell_spec(value, "html", color = "red"),
                               cell_spec(value, "html", color = "orange")))))
    return(melted_data_new)
    
  }
  
}

formatted_clinical_labs <-
  conditional_format_form(clinical_labs_melt_new)

formatted_lis_new <- conditional_format_form(lis_melt_new)

###### ------------- Now decast the melted clinical table ------------- ######
formatted_clinical_labs1 <-
  dcast(formatted_clinical_labs,
        Facility + Comments ~ variable)

#Change the order of the columns in the table
columns_order_form_clinical <-
  c("Facility", "LabCorp", "Vendor_Services", "Environment", "Equipment", "IT",
    "Service_Changes", "Volume", "Staffing", "Comments")

formatted_clinical_labs1 <-
  formatted_clinical_labs1[, columns_order_form_clinical]

needed_rows <- data.frame(matrix(ncol = 1, nrow = 11))

colnames(needed_rows) <- c("Facility")

needed_rows[1] <-
  c("MSH (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSQ (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSBI (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSB (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSW (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSSL (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "NYEE (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSSN (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSH - Anatomic Pathology (Centralized)",
    "MSH - Central Processing & Accessioning",
    "4LABS - Client Services")

#i need all the columns from x and all the rows from x and y
trial <- right_join(formatted_clinical_labs1, needed_rows)
#
rownames(trial) <- trial$Facility

#Change the order of the rows in the table

formatted_clinical_labs2 <-
  trial[c(
    "MSH (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSQ (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSBI (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSB (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSW (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSSL (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "NYEE (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSSN (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSH - Anatomic Pathology (Centralized)",
    "MSH - Central Processing & Accessioning",
    "4LABS - Client Services"), ]

rownames(formatted_clinical_labs2) <- NULL

#Change the names in the Facility columns to be standardized

rownames(formatted_clinical_labs2) <-
  c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL", "NYEE", "MSSN",
    "Anatomic Pathology (Centralized)",
    "Central Processiong & Accessioning (CPA)",
    "Client Services - 4LABS")

formatted_clinical_labs2 <- formatted_clinical_labs2 %>%
  mutate(Facility = rownames(formatted_clinical_labs2))

rownames(formatted_clinical_labs2) <- NULL

formatted_clinical_labs3 <- formatted_clinical_labs2[, c(1:9)]

comments_clinical_labs <- formatted_clinical_labs2[, c(1, 10)]
comments_clinical_labs[is.na(comments_clinical_labs)] <-
  "No Issues Reported (Safe)"

comments_clinical_labs[comments_clinical_labs == "None" |
                         comments_clinical_labs == "NONE" |
                         comments_clinical_labs == "none" |
                         comments_clinical_labs == "N/A" |
                         comments_clinical_labs == "n/a" |
                         comments_clinical_labs == "na" |
                         comments_clinical_labs == "NA"] <-
  "No Issues Reported (Safe)"

###### --------------- now decast the LIS  table --------------- ######
if (is.null(formatted_lis_new) || nrow(formatted_lis_new) == 0) {
  formatted_lis_new2 <- NULL
} else{
  formatted_lis_new2 <-
    dcast(formatted_lis_new,
          Facility +
            LIS_Unplanned_Service +
            LIS_Preplanned_Downtime ~ variable)
  
  columns_order_form_lis <-
    c("Facility",
      "LIS_Staffing",
      "LIS_Unplanned_Service",
      "LIS_Preplanned_Downtime")
  
  formatted_lis_new2 <- formatted_lis_new2[, columns_order_form_lis]
  rownames(formatted_lis_new2) <- NULL
  formatted_lis_new2[is.na(formatted_lis_new2)] <- "None"
}
###### --------------- Formatting Never Events Table --------------- ######

# added 4 extra columns each one for different never event
never_events["Specimen(s) Lost"] <- NA
never_events["QNS - specimen that cannot be recollected"] <- NA
never_events["Treatment based on mislabeled/misidentified specimen"] <- NA
never_events["Treatment based on false positive/false negative results"] <- NA

#because we have multiple never events in one cell i created a
#list with these by:
split_text <- sapply(never_events$NEVER_EVENTS, strsplit, "[;]")

#created a nested for loop with nested if to determine which of these
#never events are listed
for (i in 1:nrow(never_events)) {
  for (j in 1:length(split_text[[i]])) {
    if (split_text[[i]][j] == colnames(never_events)[5]) {
      never_events$`Specimen(s) Lost`[i] <- 1
    }
    else if (split_text[[i]][j] == colnames(never_events)[6]) {
      never_events$`QNS - specimen that cannot be recollected`[i] <- 1
    }
    else if (split_text[[i]][j] == colnames(never_events)[7]) {
      never_events$`Treatment based on mislabeled/misidentified specimen`[i] <-
        1
    }
    else if (split_text[[i]][j] == colnames(never_events)[8]) {
      never_events$`Treatment based on false positive/false negative results`[i] <- 1
    }
  }
}

never_events <- never_events %>%
  mutate(NEVER_EVENTS = NULL)

never_events_melt <-
  melt(never_events,
       id = c(
         "Facility",
         "NEVER_EVENTS_COMMENTS",
         "Good_Catch"))

never_events_melt$value[is.na(never_events_melt$value)] <- "None"
never_events_melt$value[never_events_melt$value == 1] <- "Present"

never_events_melt[never_events_melt == "None" |
                    never_events_melt == "NONE" |
                    never_events_melt == "none" |
                    never_events_melt == "N/A" |
                    never_events_melt == "n/a" |
                    never_events_melt == "na" |
                    never_events_melt == "NA"] <- "None"

formatted_never_event_melt <- conditional_format_form(never_events_melt)

formatted_never_event_ <-
  dcast(formatted_never_event_melt,
        Facility +
          NEVER_EVENTS_COMMENTS +
          Good_Catch ~ variable)

never_events_column_order_1 <-
  c("Facility",
    "Specimen(s) Lost",
    "QNS - specimen that cannot be recollected",
    "Treatment based on mislabeled/misidentified specimen",
    "Treatment based on false positive/false negative results",
    "NEVER_EVENTS_COMMENTS", "Good_Catch")

formatted_never_event_ <- formatted_never_event_[, never_events_column_order_1]

#Change the order of the rows in the table
trial_neverevent <- right_join(formatted_never_event_, needed_rows)
rownames(trial_neverevent) <- trial_neverevent$Facility

#Change the order of the rows in the table

formatted_never_event <-
  trial_neverevent[c(
    "MSH (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSQ (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSBI (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSB (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSW (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSSL (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "NYEE (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSSN (Clinical Pathology, Blood Bank, Anatomic Pathology Front End, etc.)",
    "MSH - Anatomic Pathology (Centralized)",
    "MSH - Central Processing & Accessioning",
    "4LABS - Client Services"), ]

rownames(formatted_never_event) <- NULL

#Change the names in the Facility columns to be standardized

rownames(formatted_never_event) <-
  c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL", "NYEE", "MSSN",
    "Anatomic Pathology (Centralized)",
    "Central Processiong & Accessioning (CPA)",
    "Client Services - 4LABS")

formatted_never_event <- formatted_never_event %>%
  mutate(Facility = rownames(formatted_never_event))

rownames(formatted_never_event) <- NULL

#Split the table into three
#table one is the comments only while table two is the details

good_catch <- formatted_never_event[, c(1, 7)]
good_catch[is.na(good_catch)] <- "No Issues Reported"

good_catch[good_catch == "None" |
             good_catch == "NONE" |
             good_catch == "none" |
             good_catch == "N/A" |
             good_catch == "n/a" |
             good_catch == "na" |
             good_catch == "NA" |
             good_catch == 0] <- "No Issues Reported"


never_events_comments <- formatted_never_event[, c(1, 6)]
never_events_comments[is.na(never_events_comments)] <- "No Issues Reported"

never_events_comments[never_events_comments == "None" |
                        never_events_comments == "NONE" |
                        never_events_comments == "none" |
                        never_events_comments == "N/A" |
                        never_events_comments == "n/a" |
                        never_events_comments == "na" |
                        never_events_comments == "NA" |
                        never_events_comments == 0] <- "No Issues Reported"

never_events_only <- formatted_never_event[, c(1:5)]
