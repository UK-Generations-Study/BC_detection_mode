# # ***************** Generating DD for detection mode dataset in RDS **************************

# Martina Brayley
# 02/05/2024

# ______________________________________________________________________________________________________


# set up -----------------------------------------------------------------------------------
install.packages("pacman")
library(pacman)


p_load(datadictionary,
       magrittr,
       tidyverse,
       summarytools,
       janitor,
       writexl)



# load data -------------------------------------------------------------------------------

detection_mode_df <- read_csv("R:/CoreData/v20230927/RDSDetectionMode.csv",
                              na = "NULL",
                              guess_max = 10000)
problems(detection_mode_df)

# process and change var types -----------------------------------------------------------

str(detection_mode_df)

factor_vars <- c("different_casum_row", "dmode_v2", 
                 "cat_dmode_v2", "ancat_dmode_v2", "SD_dg_first_screen", "source_dm2",
                 "dmode_v1", "cat_dmode_v1", "shim_dmode",
                 "gs_sd", "gs_sd_source", "reg_sd", "histo_sd") 


df <- detection_mode_df %>% 
  mutate_at(all_of(factor_vars), as.factor)

str(df)



# create labels -------------------------------------------------------------------------
dmode_labels <- c(tcode = "participant identifier",
                  reginfo_groupdatesite = "unique row identifier",
                  reginfo_clustrino = "unique row identifier",
                  report_groupdatesite = "unique row identifier",
                  report_cluster = "unique row identifier",
                  different_casum_row = "flag to indicate whether the row assigned dmode2 is different from the row assigned dmode1",
                  dmode_v2 = "fine categories of version 2 detection mode. Dmode_v2 is a combination of the different detection mode sources (screenign data, dmode v1, registry) and assigned by R algorithm (Martina)",
                  cat_dmode_v2 = "collapsed categories of dmode v2, including uncertain categories from dmode_v1",
                  ancat_dmode_v2 = "collapsed categories of dmode v2, excluding uncertain categories from dmode v1",
                  dmode_v2_eno = "screening episode number at which dmode v2 was derived",
                  dens_screen_eno = "density related screening episode number at which dmode v2 was derived. For screen detected cases, this is the last attended routine negative screen before diagnosis",
                  dm2_screen_date_f = "date of screening episode at which dmode v2 was derived",
                  dens_dm2_screen_date_f = "density related date of screening episode at which dmode v2 was derived. For screen detected cases, this is the last attended routine negative screen before diagnosis", 
                  SD_dg_first_screen = "flag variable to indicate if screen detected case was diagnosed at the first screen", 
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


# create DD as an object ------------------------------------------------------------------------------
# note: use the data_dictionary package and save as object to environemnt, 
# then process according to GS template
DD <- create_dictionary(df, var_labels = dmode_labels)



# CREATE TAB 1 -----------------------------------------------

# 1. define variables (columns) 

TableLocation <- "Rollout Database in Doverv"
TableName  <- "DetectionMode"
TableDesc <- "Derived from screening data tocategorise mode of breast cancer detection (e.g.Interval/Screen Detected)."

# 2. create dataframe 
Table_info <- data.frame(TableLocation, TableName, TableDesc)

Table_info <- Table_info %>% 
  transmute(`TableLocation (max 255 char)` = TableLocation,
            `TableName (max 50 char)` = TableName,
            `TableDesc (no max char)` =TableDesc)

Table_info


# CREATE TAB 2 -----------------------------------------------

Field_info <- DD %>% 
  transmute(`FieldName (max 50 char)` = item,
         `DataType (max 50 char)` = class,
         FieldLength = NA, # to be derived
         `FieldDesc (max 500 char)` = label, 
         `Notes (max 1200 char)` = NA, # to be added as needed for each variable
         `VarType (max 50 char)` = NA, # to be derived
         `Personally identifiable information (Y/N)` = NA
         ) %>% 
  filter(`FieldName (max 50 char)` != "" # remove empty rows
  )

Field_info %>% tabyl(`FieldName (max 50 char)`)


# CREATE TAB 3 ---------------------------------------------- 

# create a function that pulls out levels from each factor variable

# function: 
create_var_levels <- function(var_name, data_df) {
  # Get levels of the specified variable
  var_levels <- levels(as.factor(data_df[[var_name]]))
  
  # Create the data frame
  df <- data.frame(
    FieldName = var_name,
    Code = var_levels,
    `CodeDesc (max 100 char)` = NA
  )
  
  # Assign the data frame to a variable with the same name
  assign(var_name, df, envir = .GlobalEnv)
  
  # to check
  return(var_name)
}

# Example usage:
# create_var_levels("ancat_dmode_v2", detection_mode_df)


# use function on all factor variables
lapply(factor_vars, create_var_levels, data_df = detection_mode_df)

list <- list(different_casum_row, dmode_v2, cat_dmode_v2,
     ancat_dmode_v2, SD_dg_first_screen, source_dm2,
     dmode_v1, cat_dmode_v1, shim_dmode, 
     gs_sd, gs_sd_source, reg_sd, histo_sd)


Code_info <- bind_rows(list)


# remove duplicated names

Code_info <- Code_info %>%
  mutate(FieldName= ifelse(!duplicated(FieldName), FieldName, ""))


# COMBINE AND SAVE AS EXCEL WORKBOOK ------------------------

write_xlsx(list("Table info" = Table_info,
                "Field info" = Field_info,
                "Code info" = Code_info),
           path = "../detection_mode_DD/detection_mode_DD_GSformat.xlsx")


