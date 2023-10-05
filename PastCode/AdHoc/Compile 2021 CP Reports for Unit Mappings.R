# This code is used to compile Sunquest and SCC data for 2021 and
# create a list of all units for ICU mappings.

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

rm(list = ls())

# Set working directory -------------------------------

if ("Presidents" %in% list.files("J://")) {
  user_directory <- paste0("J:/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
} else {
  user_directory <- paste0("J:/deans/Presidents/HSPI-PM/",
                           "Operations Analytics and Optimization/Projects/",
                           "Service Lines/Lab Kpi/Data")
}

user_path <- paste0(user_directory, "\\*.*")


# Find list of data reports from 2021
file_list_scc <- list.files(
  path = paste0(user_directory,
                "\\SCC CP Reports"),
  pattern = "^(Doc){1}.+(2021)\\-[0-9]{2}-[0-9]{2}.xlsx")

# Pattern for daily Sunquest reports
sun_daily_pattern = c(paste0("^(KPI_Daily_TAT_Report ){1}",
                             "(2021){1}\\-[0-9]{2}-[0-9]{2}.xls"),
                      paste0("^(KPI_Daily_TAT_Report_Updated ){1}",
                             "(2021){1}\\-[0-9]{2}-[0-9]{2}.xls"))
  
file_list_sun_daily <- list.files(
  path = paste0(user_directory, "\\SUN CP Reports"),
  pattern = paste0(sun_daily_pattern, collapse = "|"))

# Read in data reports from possible date range
scc_list <- lapply(file_list_scc,
                     function(x)
                       read_excel(path = paste0(user_directory,
                                                "\\SCC CP Reports\\", x)))
sun_daily_list <- lapply(file_list_sun_daily,
                         function(x)
                           (read_excel(
                             path = paste0(user_directory,
                                           "\\SUN CP Reports\\", x),
                             col_types = c("text", "text", "text", "text",
                                           "text", "text", "text", "text",
                                          "text", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "text", "text", "text", "text",
                                          "text", "text", "text", "text",
                                          "text", "text", "text", "text",
                                          "text", "text", "text", "text",
                                          "text", "text", "text", "text",
                                          "text"))))

# Import Clinical Pathology analysis reference data ---------------
reference_file <- paste0(user_directory,
                         "/Code Reference/",
                         "Analysis Reference 2021-03-22.xlsx")

scc_test_code <- read_excel(reference_file, sheet = "SCC_TestCodes")
sun_test_code <- read_excel(reference_file, sheet = "SUN_TestCodes")

scc_icu <- read_excel(reference_file, sheet = "SCC_ICU")
scc_setting <- read_excel(reference_file, sheet = "SCC_ClinicType")
sun_icu <- read_excel(reference_file, sheet = "SUN_ICU")
sun_setting <- read_excel(reference_file, sheet = "SUN_LocType")

mshs_site <- read_excel(reference_file, sheet = "SiteNames")

scc_all_data <- bind_rows(scc_list)
sun_all_data <- bind_rows(sun_daily_list)

scc_site_units <- unique(scc_all_data[, c("SITE", "CLINIC_TYPE", "Ward", "WARD_NAME")])

sun_site_units <- unique(sun_all_data[, c("HospCode", "LocType", "LocCode", "LocName")])

colnames(scc_site_units) <- c("Site", "Setting", "UnitCode", "UnitName")
colnames(sun_site_units) <- c("Site", "Setting", "UnitCode", "UnitName")

all_sites_units <- rbind(scc_site_units, sun_site_units)

all_sites_units <- all_sites_units %>%
  arrange(Site, Setting, UnitName)

ip_units <- all_sites_units %>%
  filter(Setting %in% c("I", "Inpatient"))

mssn_units <- all_sites_units %>%
  filter(Site == "SNCH")

write_xlsx(ip_units, path = paste0(user_directory,
                                   "/AdHoc/2021 Inpatient Units ",
                                   Sys.Date(),
                                   ".xlsx"))


scc_all_data <- scc_all_data %>%
  mutate(NonMSHSite = ifelse(SITE != "Sinai", NA,
                             str_detect(Ward,
                                        "(BIMC)|(RVT)|(BIKH)|(STL)|(SNCH)")))

nonsinai_sites_summary <- scc_all_data %>%
  filter(NonMSHSite) %>%
  group_by(Ward, WARD_NAME, GROUP_TEST_ID, TEST_NAME) %>%
  summarize(Count = n())

nonsinai_test_summary <- scc_all_data %>%
  filter(NonMSHSite) %>%
  group_by(GROUP_TEST_ID, TEST_NAME) %>%
  summarize(Count = n())
