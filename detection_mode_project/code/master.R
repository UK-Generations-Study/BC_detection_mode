# ************************* Detection mode - breast density selection ************************


# Purpose:  creating analytical dataset 



# ___________________________  MASTER  _________________________________

# NOTES: 
#       - run this to execute all parts
#       - file paths are relative, when the project is cloned from github it will set automatically WD, 
#         relative paths automatically reflect the project WD
#     


#set up Date: 13/05/2024 
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 (13/05/2024)


# 
#_____________________________________________________________________________



# 1. SET UP --------------------------------------
source("code/1.setup_import.r")





# 2. PREPARE RISK FACTORS ---------------------------------
# initial processing only, analytical variables will be derived in a different part
source("code/2.prep_riskfactors_v2.r")

# 3. PREPARE CASUMMARY ---------------------------------
# initial processing only, analytical variables will be derived in a different part
source("code/3.prep_casummary_v2.r")

# 4. PREPARE DETECTION MODE -----------------------------
source("code/4.prep_detectionmode.r")

# 5. PREPARE MAMMODENSITY -------------------------------
source("code/5.prep_mammodensity.r")

# 6. CREATE MAMMO DENSITY VARIABLE ------------------------
source("code/6.cr_mammodensity_selection_v3.r")

# 7. COMPRISE ANALYTICAL DATASET  ------------------------

# missing values treated as error codes 888 - good for exploratory analysis and checking reason for missingness
#source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/7.cr_analytical_df_v2.r")

# Missing values are treated as NAs in v3 onwards - better for further analysis - creates "raw" analytical dataset
source("code/7.cr_analytical_df_v5.r")





# stand alone - does not need to be run as part of 1:7 as it loads "raw" analytical dataset created and saved in script 7
# 8. ANALYSIS SPECIFIC PRE-PROCESSING OF ANALYTICAL DATASET ------------------
source("code/8.prep_analytical_df.r")



# IMPORT ANALYTICAL DATA
# origin data - without analytical pre-processing - only to use if pre-processing needs to change
an_df <- readRDS("Q:/SHARED/USERS/MBrayley/Screening/data/an_df.rds")

# complete case data - for sensitivity only
df_compl_case <- readRDS("Q:/SHARED/USERS/MBrayley/Screening/data/an_df_compl_case.rds")

# data with missing as separate categories - the main analytical data
df_miss_cat <- readRDS("Q:/SHARED/USERS/MBrayley/Screening/data/an_df_missing_cat.rds")
