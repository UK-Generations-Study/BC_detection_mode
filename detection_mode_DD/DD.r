
install.packages("pacman")
install.packages("datadictionary")
library(datadictionary)

library(pacman)
library(magrittr)

p_load(datadictionary,
       magrittr,
       tidyverse)

# load data 

detection_mode_df <- read_csv("R:/CoreData/v20230927/RDSDetectionMode.csv",
                              na = "NULL",
                              guess_max = 10000)
problems(detection_mode_df)

# process and change var types 

str(detection_mode_df)

factor_vars <- c("confirmed", "incident", "different_casum_row", "dmode_v2", 
                 "cat_dmode_v2", "ancat_dmode_v2", "source_dm2",
                 "dmode_v1", "cat_dmode_v1", "shim_dmode",
                 "gs_sd", "gs_sd_source", "reg_sd", "histo_sd") 


df <- detection_mode_df %>% 
  mutate_at(all_of(factor_vars), as.factor)

str(df)

# create labels 
dmode_labels <- c(tcode = "participant identifier",
                  reginfo_groupdatesite = "unique row identifier",
                  reginfo_clustrino = "unique row identifier",
                  report_groupdatesite = "unique row identifier",
                  report_cluster = "unique row identifier",
                  confirmed = "confirmed cancer flag (source casummary)",
                  incident = "incident cancer flag (source casummary)",
                  different_casum_row = "flag to indicate whether the row assigned dmode2 is different from the row assigned dmode1",
                  dmode_v2 = "fine categories of version 2 detection mode. Dmode_v2 is a combination of the different detection mode sources (screenign data, dmode v1, registry) and assigned by R algorithm (Martina)",
                  cat_dmode_v2 = "collapsed categories of dmode v2, including uncertain categories from dmode_v1",
                  ancat_dmode_v2 = "collapsed categories of dmode v2, excluding uncertain categories from dmode v1",
                  dmode_v2_eno = "screening episode number at which dmode v2 was derived",
                  dm2_screen_date_f = "date of screening episode at which dmode v2 was derived",
                  source_dm2 = "source od dmode v2",
                  diagdate_dm1_f = "diagnosis date associated with dmode v1",
                  dmode_v1 = "detection mode derived from screening data using more manual process (Louise)",
                  cat_dmode_v1 = "collpased categories of dmode_v1",
                  dmode_v1_eno = "screening episode number at which dmode v2 was derived",
                  dm1_screen_date_f = "date of screening episode at which dmode v2 was derived",
                  dmode_v1_src = "source of dmode v1",
                  shim_dmode = "shim detection mode provided in screening data",
                  shim_diagdate_f = "shim diagnosis date provided in screening data",
                  shim_dmode_alert = "shim alert info, available in Safe Haven",
                  gs_sd = "Generations Study Screen Detected flag, derived by Penny as Screen_Detected in casummary from registry or histology source",
                  gs_sd_source = "source of GS_sd data",
                  reg_sd = "Screen Detected flag as reported by Registry in Generations study, derived from Gs_SD and GS_SD source variables",
                  histo_sd = "Screen detected flag as reported by histology in Generations study, derived from gs_sd and gs_sd source variables"
                  )


# create DD 

DD <- create_dictionary(df, var_labels = dmode_labels, file = "Q:/SHARED/USERS/MBrayley/Screening/detection_mode_DD/detection_mode_DD.xlsx")
