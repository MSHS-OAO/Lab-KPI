#######
# Source code for running the historical repos for CP and AP when needed
# 1. CP Historical repo
# 2. AP Historical repo
#######

#######
# Code for compiling the historical data for Clinical Pathology (CP) analysis
# Data compiled include:
# 1. SCC data for: MSH and MSQ
# 2. SnQuest data for: MSBI, MSB, MSW, MSM, and MSSN
#######
source(here::here("Raw-Data-Summary-Repos/CP_Raw_Data_and_Summary_Repo.R"))

#######
# Code for compiling the historical data for pathology and cytology analysis
# Data compiled include:
# 1. PowerPath daily data for Pathology and Cytology
# 2. EPIC daily data for cytology
# 3. Cytology Backlog Data
#######
source(here::here("Raw-Data-Summary-Repos/AP_Historical_Repo.R"))
