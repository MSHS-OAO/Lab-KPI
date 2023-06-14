# Code for connecting to historical data

# Note: This can change once database structure is implemented

# Clinical Pathology --------
# Import raw data, daily summary, weekly summary, and monthly summary
cp_test_repo <- readRDS(paste0(user_directory,
                               "/Shiny App Repo/CPTestLevelData",
                               "/CPTestData60Days.rds"))

cp_daily_repo <- readRDS(paste0(user_directory,
                             "/Shiny App Repo/CPDailySummary",
                             "/CPDailySummary.rds"))

# Tables will show data for latest date in repository be default
cp_resulted_date <- max(cp_daily_repo$ResultDate)

cp_submitted_daily_summary <- cp_daily_repo %>%
  filter(ResultDate == cp_resulted_date) %>%
  arrange(Site, ResultDate) %>%
  ungroup()


# Anatomic Pathology ---------


# Operations and Quality Indicators -------
ops_qlty_date <- Sys.Date() - 2

ops_qlty_data <- read_excel(
  paste0(user_directory,
         "/Quality and Ops Indicators Form Responses",
         "/MSHS Lab KPI Operations and Quality Indicators Forms.xlsx"))

ops_qlty_responses <- ops_qlty_data

colnames(ops_qlty_responses) <- c(
  "ID", "StartTime", "CompletionTime", "Email", "Name", "Facility",
  "LabCorp", "VendorServices", "Environment", "Equipment", "IT",
  "ServiceChanges", "Volume", "Staffing", "Comments",
  "NeverEvents", "NeverEventsComments", "GoodCatch",
  "LIS_Staffing", "LIS_UnplannedService", "LIS_PreplannedDowntime"
)

ops_qlty_responses <- ops_qlty_responses %>%
  select(-contains("LIS_")) %>%
  arrange(Facility, -ID) %>%
  mutate(CompletionDate = as.Date(CompletionTime),
         CompletionHour = hour(CompletionTime)) %>%
  filter(!duplicated(cbind(Facility, CompletionDate)))

ops_qlty_responses_today <- ops_qlty_responses %>%
  filter(CompletionDate == ops_qlty_date|
           (CompletionDate == ops_qlty_date - 1 &
              CompletionHour >= 17))

ops_indicators_responses <- ops_qlty_responses_today %>%
  select(-ID, -StartTime, -CompletionTime, -Email, -Name,
         -NeverEvents, -NeverEventsComments, -GoodCatch,
         -CompletionDate, -CompletionHour)

never_events_responses <- ops_qlty_responses_today %>%
  select(Facility, NeverEvents, NeverEventsComments)

good_catch_responses <- ops_qlty_responses_today %>%
  select(Facility, GoodCatch)

ops_indicators_responses <- ops_indicators_responses %>%
  pivot_longer(cols = !c(Facility, Comments),
               names_to = "Metric",
               values_to = "Status") %>%
  mutate(Status = ifelse(str_detect(Status, "Green"),
                         "Safe",
                         ifelse(str_detect(Status, "Yellow"),
                                "Under Stress",
                                ifelse(str_detect(Status, "Red"),
                                       "Not Safe", NA))))

# Custom function for formatting -----
ops_qlty_formatting <- function(x) {
  x <- x %>%
    mutate(Status = ifelse(is.na(Status),
                           cell_spec(Status, "html", color = "lightgray"),
                           ifelse(Status %in% c("Safe", Status == "None"),
                                  cell_spec(Status, "html", color = "green"),
                                  ifelse(Status %in% c("Not Safe", "Present"),
                                         cell_spec(Status, "html", color = "red"),
                                         cell_spec(Status, "html", color = "orange")))))
  
  
}

ops_indicators_responses <- ops_qlty_formatting(ops_indicators_responses)

ops_indicators_responses <- ops_indicators_responses %>%
  pivot_wider(names_from = "Metric",
              values_from = "Status") %>%
  relocate(Comments, .after = last_col())

ops_indicators_sites_df <- data.frame("Facility" = ops_indicators_sites)

ops_indicators_responses <- left_join(ops_indicators_facility_df,
                                      ops_indicators_responses,
                                      by = c("Facility" = "Facility"))

ops_indicators_responses <- ops_indicators_responses %>%
  select(-Facility) %>%
  rename("Facility" = "FacilitySimple")

ops_indicators_status <- ops_indicators_responses %>%
  select(-Comments)

ops_indicators_comments <- ops_indicators_responses %>%
  select(Facility, Comments) %>%
  mutate(Comments = ifelse(is.na(Comments) |
                             toupper(Comments) %in% c("NONE", "N/A", "NA"),
                           "No Issues Reported (Safe)", Comments)
  )

