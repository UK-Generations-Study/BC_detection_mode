# # ***************** Generating DD for detection mode dataset in RDS **************************

# Martina Brayley
# 02/05/2024

# ______________________________________________________________________________________________________



# create labels -------------------------------------------------------------------------
labels <- c(tcode = "participant identifier",
            date_birth = "date of birth",
            date_entry = "GS study entry date",
            diagdate = "date of diagnosis (origin casummary)",
            diagage = "age at diagnosis",
            AgeatEntry = "age at entry to GS",
            incident = "flag variable for incident cases (origin casummary)",
            side = "breast cancer side (origin casummary",
            # Dmode variables
            source_dm2 = "source od dmode v2",
            dm2_screen_date_f = "date of screening episode at which dmode v2 was derived",
            dens_dm2_screen_date_f = "density related date of screening episode at which dmode v2 was derived. For screen detected cases, this is the last attended routine negative screen before diagnosis",      
            SD_dg_first_screen = "flag variable to indicate if screen detected case was diagnosed at the first screen",
            # mammo density variables
            MammoDat_f = "Date of mammographic image taken",
            mean_density = "mean breast density (calculate as average of both breast of the screen before diagnosis, if only diagnostic images available, the non-affected breast density was taken), missing as NA",
            sd_density = "standard deviation of mean density",
            # derived variables 
            d_R1toBC = "time from entry to GS to breast cancer diagnosis (days)",
            d_R1toBC_y = "time from entry to GS to breast cancer diagnosis (years)",
            d_R1toBC_cat = "time from entry to GS to breast cancer diagnosis (categorical - numbered)",
            d_R1toBC_lab = "time from entry to GS to breast cancer diagnosis (categorical - labelled)",
            
            d_MDtoBC = "time from date of mammographic density to breast cancer diagnosis (days)",
            d_MDtoBC_y = "time from date of mammographic density to breast cancer diagnosis (years)",
            d_MDtoBC_cat = "time from date of mammographic density to breast cancer diagnosis (categorical - numbered)",
            d_MDtoBC_cat = "time from date of mammographic density to breast cancer diagnosis (categorical - labelled)",
            
            d_R1toMD = "time from entry to GS to mammographic density (days)",
            d_R1toMD_y = "time from entry to GS to mammographic density (year)",
            
            d_age_mammo = "age at mammographic density date",
            
            d_inv_status = "invasive status flag (categorical - numbered)",
            d_inv_status_lab = "invasive status flag (categorical - labelled)",
            
            d_grade = "tumour grade, cleaned variable, source casummary (categorical - numbered)",
            d_grade_lab = "tumour grade, cleaned variable, source casummary (categorical - labelled)",
            d_grade_tr = "tumour grade for trick model, source casummary (categorical - numbered)",
            
            d_stage = "tumour stage, cleaned variable, source casummary",
            
            d_morphology = "derived tumour morphology based on ICD codes",
            d_morphology_lab = "derived tumour morphology based on ICD codes (labelled)",
            d_morph7 = "derived tumour morphology condensed to 7 categories",
            d_morph7_lab = "derived tumour morphology condensed to 7 categories (labelled)",
            d_morph7_tr = "tumour morphology for trick model based on morph7",
            d_morph7_tr_lab = "tumour morphology for trick model based on morph7 (labelled)",
            
            d_pos_nodes_n = "number of positive nodes, cleaned, source casummary",
            d_pos_nodes = "number of positive nodes, cleaned, source casummary (categorical)",
            d_pos_nodes_lab = "number of positive nodes, cleaned, source casummary (categorical - labelled)",
            d_pos_nodes_tr = "number of positive nodes, for trick model, source casummary (categorical)",
            d_pos_nodes_tr_lab = "number of positive nodes, for trick model, source casummary (categorical - labelled)",
            
            d_tumour_size_n = "tumour size, cleaned, source casummary",
            d_tumour_size = "tumour size, cleaned, source casummary (categorical)",
            d_tumour_size_lab = "tumour size, cleaned, source casummary (categorical - labelled)",
            d_tmsize_tr = "tumour size, for trick model, source casummary (categorical)",
            d_tmsize_tr_lab = "tumour size, for trick model, source casummary (categorical - labelled)",
            
            d_er_status = "ER status, cleaned, source casummary",
            d_er_status_lab = "ER status, cleaned, source casummary (labelled)",
            d_er_tr = "ER status, for trick model, source casummary",
            d_er_tr_lab = "ER status, for trick model, source casummary (labelled)",
            
            d_pr_status = "PR status, cleaned, source casummary",
            d_pr_status_lab = "PR status, cleaned, source casummary (labelled)",
            d_pr_tr = "PR status, for trick model, source casummary",
            d_pr_tr_lab = "PR status, for trick model, source casummary (labelled)",
            
            d_her2_status = "HER2 status, cleaned, source casummary",
            d_her2_status_lab = "HER2 status, cleaned, source casummary (labelled)",
            d_her2_tr = "HER2 status, for trick model, source casummary",
            d_her2_tr_lab = "HER2 status, for trick model, source casummary (labelled)",
            
            d_R1alcohol_status = "alcohol status, cleaned, source risk factors",
            d_R1alcohol_status_lab = "alcohol status, cleaned, source risk factors (labelled)",
            d_R1alcohol_units  = "alcohol units, cleaned, source risk factors (continuous)",  
            d_R1alcohol_units_cat = "alcohol units, cleaned, source risk factors (categorical)",
            d_R1alcohol_units_lab = "alcohol units, cleaned, source risk factors (categorical - labelled)",
            
            d_bbd = "history of benign breast disease, cleaned, source risk factors",
            d_bbd_lab = "history of benign breast disease, cleaned, source risk factors (labelled)",
            
            d_bmi_entry = "BMI at entry, cleaned, source risk factors (continuous)",
            d_bmi_entry_cat = "BMI at entry, cleaned, source risk factors (categorical)",
            d_bmi_entry_lab = "BMI at entry, cleaned, source risk factors (categorical - labelled)",
            
            d_fambrca = "family history of breast cancer, cleaned, source risk factors (binary)",
            d_fambrca_lab = "family history of breast cancer, cleaned, source risk factors (binary - labelled)",
            d_fambrcaN = "family history of breast cancer, cleaned, source risk factors (categorical)",
            d_fambrcaN_lab = "family history of breast cancer, cleaned, source risk factors (categorical - labelled)",
            
            d_age_menarche = "age at menarche, cleaned, source risk factors (continuous)",
            d_age_menarche_cat = "age at menarche, cleaned, source risk factors (categorical)",
            d_age_menarche_lab = "age at menarche, cleaned, source risk factors (categorical - labelled)",
            
            d_R1menopause = "menopause status at entry, cleaned, source risk factors (categorical)",
            d_R1menopause_lab6 = "menopause status at entry, cleaned, source risk factors (categorical - labelled)",
            d_R1menopause_cat3 = "menopause status at entry, cleaned, source risk factors (condensed categories)",
            d_R1menopause_lab3 = "menopause status at entry, cleaned, source risk factors (condensed categories - labelled)",
            
            d_age_menopause = "age at menopause, cleaned, source risk factors (continuous)",
            d_age_menopause_cat = "age at menopause, cleaned, source risk factors (categorical)",
            d_age_menopause_lab = "age at menopause, cleaned, source risk factors (categorical - labelled)",
            d_age_meno_tr = "age at menopause, for trick model, source risk factors (categorical)",
            d_age_meno_tr_lab = "age at menopause, for trick model, source risk factors (categorical - labelled)",
            
            d_R1hrtstatus = "HRT status at entry, cleaned, source risk factors (categorical)",
            d_R1hrtstatus_lab = "HRT status at entry, cleaned, source risk factors (categorical - labelled)",
            d_R1hrt_tr = "HRT status at entry, for trick model, source risk factors (categorical)",
            d_R1hrt_tr_lab = "HRT status at entry, for trick model, source risk factors (categorical - labelled)",
            
            d_ocstatus = "oral contraceptive status at entry, cleaned, source risk factors (categorical)",            
            d_ocstatus_lab = "oral contraceptive status at entry, cleaned, source risk factors (categorical - labelled)",  
            d_oc_tr = "oral contraceptive status at entry, for trick model, source risk factors (categorical)",
            d_oc_tr_lab = "oral contraceptive status at entry, for trick model, source risk factors (categorical - labelled)",
            
            d_R1physmet_leisure = "physical activity (MET/h/w), cleaned, source risk factors (continuous)", 
            d_R1physmet_leis_quart = "physical activity (MET/h/w), cleaned, source risk factors (quartiles - numbered)", 
            d_R1physmet_leis_quart_m = "physical activity (MET/h/w), cleaned, source risk factors (quartiles - mid point)",
            d_R1physmet_leis_who = "physical activity (MET/h/w), categories by WHO guidelines (equivalent of moderate 150min/week = 9 MET/h/w) (not meeting, meeting, more than double), source risk factors (categorical)",  
            d_R1physmet_leis_who_lab = "physical activity (MET/h/w), categories by WHO guidelines (equivalent of moderate 150min/week = 9 MET/h/w) (not meeting, meeting, more than double), source risk factors (categorical - labelled)",
            
            d_parous_lab = "parity status at entry, cleaned, source risk factors (binary - labelled)",
            
            d_agebirth1_cat = "age at first birth, cleaned, source risk factors (categorical)",
            d_agebirth1_lab = "age at first birth, cleaned, source risk factors (categorical - labelled)",
            d_agebirth1_tr = "age at first birth, for trick model, source risk factors (categorical)",
            d_agebirth1_tr_lab = "age at first birth, for trick model, source risk factors (categorical - labelled)",
            
            d_parity = "number of parous pregnancies, cleaned, source risk factors (continuous)",              
            d_parity_cat = "number of parous pregnancies, cleaned, source risk factors (categorical)", 
            d_parity_lab = "number of parous pregnancies, cleaned, source risk factors (categorical - labelled)",
            d_parity_tr = "number of parous pregnancies, for trick model, source risk factors (categorical)",
            d_parity_tr_lab = "number of parous pregnancies, for trick model, source risk factors (categorical - labelled)",
            
            d_breastfed = "ever breastfed, cleaned, source risk factors (binary)",
            d_bf_duration = "breastfeeding duration, cleaned, source risk factors (continuous)",
            
            d_R1smokingstatus = "smoking status at entry, cleaned, source risk factors (categorical)",    
            d_R1smokingstatus_lab = "smoking status at entry, cleaned, source risk factors (categorical - labelled)",
            
            d_md_avail = "mammographic density availability - flag", 
            d_md_avail_lab = "mammographic density availability - flag (labelled)",
            
            d_md = "mean breast density (calculate as average of both breast of the screen before diagnosis, if only diagnostic images available, the non-affected breast density was taken), 888 error code for missing",
            d_md_qrt = "mean density quartiles (numbered)",
            d_md_qrt_m = "mean density quartiles (mid point label)",
            d_md_cat = "mean density categorical based on quartiles (low = 1st, medium = 2nd and 3rd, high = 4th)",
            
            d_dmode = "mode of detection, derived from screening data (categorical - labelled)"
            
            
                  
)


# create DD as an object ------------------------------------------------------------------------------
# note: use the data_dictionary package and save as object to environemnt, 
# then process according to GS template
DD <- create_dictionary(an_df, var_labels = labels)



# CREATE TAB 1 -----------------------------------------------

# 1. define variables (columns) 

TableLocation <- "GitHub - https://github.com/UK-Generations-Study/BC_detection_mode "
TableName  <- "an_df"
TableDesc <- "Compiled analytical dataset for BC detection mode project"

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

# # create a function that pulls out levels from each factor variable
# 
# # function: 
# create_var_levels <- function(var_name, data_df) {
#   # Get levels of the specified variable
#   var_levels <- levels(as.factor(data_df[[var_name]]))
#   
#   # Create the data frame
#   df <- data.frame(
#     FieldName = var_name,
#     Code = var_levels,
#     `CodeDesc (max 100 char)` = NA
#   )
#   
#   # Assign the data frame to a variable with the same name
#   assign(var_name, df, envir = .GlobalEnv)
#   
#   # to check
#   return(var_name)
# }
# 
# # Example usage:
# # create_var_levels("ancat_dmode_v2", detection_mode_df)
# 
# 
# # use function on all factor variables
# lapply(factor_vars, create_var_levels, data_df = an_df)
# 
# list <- list(different_casum_row, dmode_v2, cat_dmode_v2,
#      ancat_dmode_v2, SD_dg_first_screen, source_dm2,
#      dmode_v1, cat_dmode_v1, shim_dmode, 
#      gs_sd, gs_sd_source, reg_sd, histo_sd)
# 
# 
# Code_info <- bind_rows(list)


# remove duplicated names

# Code_info <- Code_info %>%
#   mutate(FieldName= ifelse(!duplicated(FieldName), FieldName, ""))


# COMBINE AND SAVE AS EXCEL WORKBOOK ------------------------

write_xlsx(list("Table info" = Table_info,
                "Field info" = Field_info),
           path = "C:/Users/MBrayley/OneDrive - The Institute of Cancer Research/Work/GitHub/BC_detection_mode/Martina/screening_an_df_DD.xlsx")


