# ************************* Detection mode - breast density selection ************************


# Purpose:  creating analytical dataset 



# ___________________________  PART 1. Set up and import  _________________________________

# NOTES: 
#       - this is the first script to be run each time
#       - adapted from Safe Haven exploratory analysis project


#set up Date: 11/04/2024 
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 (11/04/2024)


# 
#_____________________________________________________________________________

# 1. Set up ----
rm(list = ls(all.names = TRUE)) #clears all objects from the environment

# Set working directory 
wd <- "C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode"
setwd(wd)

#install.packages("pacman")
library(pacman)

p_load(readxl, 
       tidyverse,
       stringr,
       psych, 
       reshape2, 
       magrittr, 
       naniar, 
       lubridate, 
       purrr, 
       Epi, 
       epiDisplay, 
       epiR, 
       epitools, 
       pubh, 
       rstatix, 
       haven, 
       fitdistrplus, 
       data.table, 
       openxlsx, 
       meantables, 
       freqtables, 
       knitr, 
       janitor, 
       kableExtra, 
       DiagrammeR, 
       patchwork, 
       summarytools,
       writexl,
       datadictionary,
       broom, # tidy up results from regressions
       gtsummary,
       lmtest, # likelihood-ratio tests
       parameters, # # alternative to tidy up results from regressions
       see,          # alternative to visualise forest plots
       skimr, # alternative to dataset overview 
       gt
       )

options(scipen=200000)

# 2. Import data ---------------------------------------------------------------

## Cancer summary ---------------------
casummary_im <- read_csv("R:/CoreData/v20230927/RDSCaSummary_V1.csv",
                         na = c("NULL", "NUL", ""),
                         guess_max = 40000)
problems(casummary_im)

## Detection mode ------------------------
detection_mode_im <- read_csv("R:/CoreData/v20230927/RDSDetectionMode.csv",
                              na = c("NULL", "NUL", ""),
                              guess_max = 40000)
problems(detection_mode_im)


## Mammo density --------------------------
mammodensity_im <- read_csv("R:/CoreData/v20230927/RDSMammoDensity.csv",
                              na = c("NULL", "NUL", ""),
                              guess_max = 40000)
problems(mammodensity_im)


## Risk factors -----------------------------
riskfactors_im <- read_csv("R:/CoreData/v20230927/RDSRiskFactors.csv",
                            na = c("NULL", "NUL", ""),
                            guess_max = 120000)
problems(riskfactors_im)


# deaths_im <- read_csv("R:/CoreData/v20230927/RDSDeaths.csv",
#                       na = c("NULL", "NUL", ""))
# problems(deaths_im)
# 
# riskfactors_im <- read_csv("R:/CoreData/v20230927/RDSRiskFactors.csv",
#                            na = c("NULL", "NUL", ""),
#                            guess_max = 120000)
# problems(riskfactors_im)


# 3. Create functions ----------------------------------------------------------




# function for checking/describing numeric variables - works with lapply
check_numeric <- function(variable) {
  cat("summary:\n")
  print(summary(variable))
  
  cat("describe:\n")
  print(describe(variable))
  
  cat("tabulate:\n")
  print(table(variable))
  
  cat("histogram:\n")
  print(hist(variable))
}

# quick check of numeric variables - works with lapply
quick_check <- function(variable) {
  cat("summary:\n")
  print(summary(variable))
  
}


