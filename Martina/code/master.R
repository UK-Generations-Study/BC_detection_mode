# ************************* Detection mode - breast density selection ************************


# Purpose:  creating analytical dataset 



# ___________________________  MASTER  _________________________________

# NOTES: 
#       - run this to execute all parts
#     


#set up Date: 13/05/2024 
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 (13/05/2024)


# 
#_____________________________________________________________________________

# 1. SET UP --------------------------------------
source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/1.setup_import.r")

# 2. PREPARE RISK FACTORS ---------------------------------
# initial processing only, analytical variables will be derived in a different part 
source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/2.prep_riskfactors.r")

# 3. PREPARE CASUMMARY ---------------------------------
# initial processing only, analytical variables will be derived in a different part 
source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/3.prep_casummary_v2.r")

# 4. PREPARE DETECTION MODE -----------------------------
source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/4.prep_detectionmode.r")

# 5. PREPARE MAMMODENSITY -------------------------------
source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/5.prep_mammodensity.r")

# 6. CREATE MAMMO DENSITY VARIABLE ------------------------
source("C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/code/6.cr_mammodensity_selection_v3.r")


