#Required packages: run these every time you run the code
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
library(tidyr)
library(pool)
library(DBI)
library(odbc)
library(dbplyr)


# Set working directory -------------------------------
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)


if ("Presidents" %in% list.files("/SharedDrive/")) {
  user_directory <- paste0("/SharedDrive/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("/SharedDrive/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}

user_path <- paste0(user_directory, "\\*.*")


# Import data for two scenarios - first time compiling repo and updating repo ----------
initial_run <- FALSE

# Determine today's date to determine last possible data report
todays_date <- as.Date(Sys.Date(), format = "%Y-%m-%d")
this_month <- month(todays_date)


# Import Clinical Pathology analysis reference data ---------------
reference_file <- paste0(user_directory,
                         "/Code Reference/",
                         "Analysis Reference 2023-10-13.xlsx")

scc_test_code <- tbl(conn,"LAB_KPI_SCC_TEST_CODES") %>%
  collect()
sun_test_code <- tbl(conn,"LAB_KPI_SUN_TEST_CODES") %>%
  collect()

# scc_test_code <- read_excel(reference_file, sheet = "SCC_TestCodes")
# sun_test_code <- read_excel(reference_file, sheet = "SUN_TestCodes")

# tat_targets <- read_excel(reference_file, sheet = "Turnaround Targets")
tat_targets <- tbl(conn,"LAB_KPI_TURNAROUND_TARGETS") %>%
  collect()

tat_targets <- tat_targets %>%
  mutate(Concate = ifelse(
    PRIORITY == "All" & PT_SETTING == "All", paste(TEST, DIVISION),
    ifelse(PRIORITY != "All" & `PT_SETTING` == "All",
           paste(TEST, DIVISION, PRIORITY),
           paste(TEST, DIVISION, PRIORITY, PT_SETTING))))

scc_icu <- tbl(conn,"LAB_KPI_SCC_ICU") %>%
  collect()
sun_icu <-  tbl(conn,"LAB_KPI_SUN_ICU") %>%
  collect()

scc_icu <- scc_icu %>%
  mutate(SiteCodeName = paste(SITE, WARD, WARD_NAME))

sun_icu <- sun_icu %>%
  mutate(SiteCodeName = paste(SITE, LOC_CODE, LOC_NAME))

scc_setting <- tbl(conn,"LAB_KPI_SCC_CLINICTYPE") %>%
  collect()
sun_setting <- tbl(conn,"LAB_KPI_SUN_LOCTYPE") %>%
  collect()

mshs_site <- read_excel(reference_file, sheet = "SiteNames")

cp_micro_lab_order <- c("Troponin",
                        "Lactate WB",
                        "BUN",
                        "HGB",
                        "PT",
                        "Rapid Flu",
                        "C. diff")

all_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN", "RTC")
hosp_sites <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSM", "MSSN")
infusion_sites <- c("RTC")

pt_setting_order <- c("ED", "ICU", "IP Non-ICU", "Amb", "Other")
pt_setting_order2 <- c("ED & ICU", "IP Non-ICU", "Amb", "Other")
dashboard_pt_setting <- c("ED & ICU", "IP Non-ICU", "Amb")

dashboard_priority_order <- c("All", "Stat", "Routine")

cp_division_order <- c("Chemistry", "Hematology", "Microbiology RRL", "Infusion")

dbDisconnect(conn)
