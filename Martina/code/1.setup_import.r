# ************************* Detection mode - breast density selection ************************


# Purpose:  creating analytical dataset 



# ___________________________  PART 1. Set up and import  _________________________________

# NOTES: 
#       - this is the first script to be run each time
#       - adapted from Safe Haven exploratory analysis project


#set up Date: 11/04/2024 
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 (11/04/2024)


# hello
#_____________________________________________________________________________

# 1. Set up ----
rm(list = ls(all.names = TRUE)) #clears all objects from the environment

# Set working directory 
wd <- "Q:/SHARED/USERS/MBrayley/Screening/IvsSD"
setwd(wd)

# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("psych")
# install.packages("reshape2")
# #install.packages("magrittr") # already installed as dependency
# install.packages("naniar")
# install.packages("summarytools")
# install.packages("lubridate")
# #install.packages("purrr") # already installed as dependency
# install.packages("Epi")
# install.packages("epibasix")
# install.packages("epiDisplay")
# install.packages("epiR")
# install.packages("epitools")
# install.packages("pubh")
# install.packages("rstatix")
# install.packages("haven")
# install.packages("fitdistrplus")
# install.packages("data.table")
# #install.packages("ggplot2") # already installed as dependency
# install.packages("openxlsx")
# install.packages("survival")
# install.packages("clipr")
# install.packages("meantables")
# install.packages("freqtables")
# install.packages("knitr")
# install.packages("janitor")
# install.packages("kableExtra")
# install.packages("sjPlot")



library(readxl)
library(tidyverse)
library(psych)
library(reshape2)
library(magrittr)
library(naniar)
library(lubridate)
library(purrr)
library(Epi)
library(epiDisplay)
library(epiR)
library(epitools)
library(pubh)
library(rstatix)
library(haven)
library(fitdistrplus)
library(data.table)
library(openxlsx)
library(survival)
library(clipr)
library(meantables)
library(freqtables)
library(knitr)
library(janitor)
library(kableExtra)
library(DiagrammeR)
#library(sjPlot)
library(patchwork)
library(summarytools)

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
  print(tabulate_ordered(variable))
  
  cat("histogram:\n")
  print(hist(variable))
}

# quick check of numeric variables - works with lapply
quick_check <- function(variable) {
  cat("summary:\n")
  print(summary(variable))
  
}


