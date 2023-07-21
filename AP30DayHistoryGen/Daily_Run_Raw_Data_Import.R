#######
# Code for importing the raw data needed to carry-on the first run
# for the lab KPI daily dashboard with different logic based on the DOW.
# Imported data includes:
# 1. SCC data for clinical pathology
# 2. SunQuest data for clinical pathology
# 3. PowerPath data for anatomic pathology
# 4. Epic data for anatomic pathology especially cytology
# 5. Backlog data for anatomic pathology especially cytology-----
#######

#------------------------------Read Excel sheets------------------------------#

# Determine date of resulted labs/specimens
resulted_date <- yesterday

# Format yesterday's date for dashboard visualization
result_date_text <- format(resulted_date,
                           format = "%a %m/%d/%y")

# Create regular expressions for beginning each file type
scc_pattern_start <- "^(Doc){1}.+"
sq_pattern_start <- "^(KPI_Daily_TAT_Report){1}.*"
pp_pattern_start <- "^(KPI REPORT - RAW DATA V4_V2){1}.*"
epic_pattern_start <- "^(MSHS Pathology Orders Epic){1}.*"
cyto_backlog_pattern_start <- "^(KPI REPORT - CYTOLOGY PENDING CASES){1}.*"

# Format resulted date as it would appear in saved files
# This will always be yesterday since this is run on weekdays, weekends, and holidays
report_date_file_format <- format(today, "%Y-%m-%d")



# Find Powerpath file for cases signed out yesterday
pp_data_file <- list.files(
  path = paste0(user_directory, "/AP & Cytology Signed Cases Reports"),
  pattern = paste0(pp_pattern_start,
                   "(",
                   report_date_file_format,
                   ".xls)$"),
  ignore.case = TRUE)

print(pp_data_file)

# Import Powerpath file for cases signed out yesterday, if any exists
if (length(pp_data_file) != 0) {
  pp_data_raw <- read_excel(path =
                             paste0(user_directory,
                                    "/AP & Cytology Signed Cases Reports/",
                                    pp_data_file),
                           skip = 1, 1)
  
  pp_data_raw <- data.frame(pp_data_raw[-nrow(pp_data_raw), ],
                           stringsAsFactors = FALSE)
} else {
  pp_data_raw <- NULL
}


# Find Epic Cytology file for cases signed out yesterday
epic_data_file <- list.files(
  path = paste0(user_directory, "/EPIC Cytology"),
  pattern = paste0(epic_pattern_start,
                   "(",
                   report_date_file_format,
                   ".xlsx)$"),
  ignore.case = TRUE)

# Import Epic Cytology file for cases signed out yesterday, if any exists
if (length(epic_data_file) != 0) {
  epic_data_raw <- read_excel(path =
                               paste0(user_directory,
                                      "/EPIC Cytology/",
                                      epic_data_file),1)
} else {
  epic_data_raw <- NULL
}

print(epic_data_file)
print(epic_data_raw)


# Find Cytology backlog file for backlog as of yesterday
cyto_backlog_data_file <- list.files(
  path = paste0(user_directory, "/Cytology Backlog Reports"),
  pattern = paste0(cyto_backlog_pattern_start,
                   "(",
                   report_date_file_format,
                   ".xls)$"),
  ignore.case = TRUE)

# Import Cytology backlog file for backlog as of yesterday, if any exists
if (length(cyto_backlog_data_file) != 0) {
  cyto_backlog_data_raw <- read_excel(path =
                                   paste0(user_directory,
                                          "/Cytology Backlog Reports/",
                                          cyto_backlog_data_file),
                                 skip = 1, 1)
  
  cyto_backlog_data_raw <- data.frame(
    cyto_backlog_data_raw[-nrow(cyto_backlog_data_raw), ],
    stringsAsFactors = FALSE)
} else {
  cyto_backlog_data_raw <- NULL
}

print(cyto_backlog_data_file)



cyto_daily_repo <- readRDS(paste0(user_directory,
                                  "/Shiny App Repo/APDailySummary",
                                  "/APCytologyRepo30Days.rds"))
patho_daily_repo <- readRDS(paste0(user_directory,
                                  "/Shiny App Repo/APDailySummary",
                                  "/APPathologyRepo30Days.rds"))

# backlog_daily_repo <- readRDS(paste0(user_directory,
#                                    "/Shiny App Repo/APDailySummary",
#                                    "/BacklogRepo30Days.rds"))
