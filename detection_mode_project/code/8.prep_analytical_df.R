# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 8. Prepare analytical dataset  _________________________________


# Purpose: Preparattion of analytical dataset 

# previously part of analysis-v7_with_missing.qmd

# can be run independtly from parts 2-7


#Date: 21/01/2025
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 21/01/2025
#                  
#_____________________________________________________________________________


# 1. SET UP --------------------------------------
source("code/1.setup_import.r")

# 2. Load dataset created in part 7 --------------------------
df <- readRDS("Q:/SHARED/USERS/MBrayley/Screening/data/an_df.rds")


# 3 processing ----------------------------------------------
# code copied from analysis v7


## Store explanatory variables as an object -------------------------------

# with labels instead of categories --------------------------------------
# select categorical variables that are labelled

# exploratory vars 
all_expl_variables <- c(
  # parity related risk factors
  "d_R1menopause_lab3", 
  "d_age_menopause_lab",
  "d_age_meno_cont",
  "d_R1hrtstatus_lab",
  "d_R1hrt_sttyp_lab",
  "d_parity_lab", 
  "d_parity",
  "d_agebirth1_lab",
  "d_agebirth1_cont",
  "d_age_menarche_lab", 
  "d_age_menarche",
  "d_ocstatus_lab",
  "d_ocstatus2_lab",
  "d_breastfed_lab",
  "d_bf_dur_lab",
  "d_bf_dur_month",
  # other biological risk factors
  "d_bbd_lab", 
  "d_fambrca_lab",
  # lifestyle risk factors
  "d_R1alcohol_units_lab",
  "d_R1alcohol_units",
  "d_R1smokingstatus_lab", 
  "d_bmi_entry_lab",
  "d_bmi_entry_per5",
  "d_bmi_20_lab",
  "d_bmi_20_per5", 
  "d_R1physmet_leis_who_lab",
  "d_R1physmet_leisure")

all_expl_variables_tr <- c(
  # parity related risk factors
  "d_R1menopause_lab3", 
  "d_age_meno_tr_lab",
  "d_age_meno_cont",
  "d_R1hrt_tr_lab",
  "d_R1hrt_sttyp_tr_lab",
  "d_parity_lab",
  "d_parity",
  "d_agebirth1_tr_lab",
  "d_agebirth1_cont",
  "d_age_menarche_lab",
  "d_age_menarche",
  "d_ocstatus_lab",
  "d_ocstatus2_lab",
  "d_breastfed_tr_lab",
  "d_bf_dur_tr_lab",
  # other biological risk factors
  "d_bbd_lab", 
  "d_fambrca_lab",
  # lifestyle risk factors
  
  "d_R1alcohol_units_lab", 
  "d_R1alcohol_units",
  "d_R1smokingstatus_lab", 
  "d_bmi_entry_lab",
  "d_bmi_entry_per5",
  "d_bmi_20_lab",
  "d_bmi_20_per5",
  "d_R1physmet_leis_who_lab",
  "d_R1physmet_leisure"
)


lifestyle <- c("d_R1alcohol_units_lab", 
               "d_R1smokingstatus_lab", 
               "d_bmi_entry_lab", 
               "d_R1physmet_leis_who")

reproductive <- c("d_age_menarche_lab", 
                  "d_ocstatus_lab", 
                  "d_R1menopause_lab3", 
                  "d_age_menopause_lab", 
                  "d_R1hrtstatus_lab", 
                  "d_parity_lab", 
                  "d_agebirth1_lab") # do we want to add breastfeeding? If so, it would also have be in the trick model

orther_rf <- c("d_bbd", 
               "d_fambrca")

# mammo density excluded from the lists - special variable for subanalyses

# covariates 
all_covariates <- c(# time varying variables
  "diagage", 
  "d_R1toBC_y", 
  "d_R1toMD_y", 
  "d_MDtoBC_y",
  # tumour characteristics
  "d_inv_status", 
  "d_grade_lab", 
  "d_morph4", 
  "d_pos_nodes_lab", 
  "d_tumour_size", 
  "d_er_status", 
  "d_pr_status", 
  "d_her2_status")  

tumour_char <- c("d_inv_status", 
                 "d_grade_lab", 
                 "d_morph4_lab", 
                 "d_pos_nodes_lab", 
                 "d_tumour_size_lab", 
                 "d_er_status_lab", 
                 "d_pr_status_lab", 
                 "d_her2_status_lab")

tumour_char_tr <- c("d_inv_status", 
                    "d_grade_tr", 
                    "d_morph4_tr_lab", 
                    "d_pos_nodes_tr_lab", 
                    "d_tmsize_tr_lab", 
                    "d_er_tr_lab", 
                    "d_pr_tr_lab", 
                    "d_her2_tr_lab")

time_vars <- c("diagage", 
               "d_R1toBC_y", 
               "d_R1toMD_y", 
               "d_MDtoBC_y",
               "d_MDtoBC_lab",
               "yeardiag")


df <- df %>% 
  select(d_dmode_n, d_dmode, source_dm2, d_md_avail_lab, d_md_avail, d_parous_lab, d_md, d_md_qrt, d_md10, any_of(all_expl_variables), any_of(all_expl_variables_tr), d_inv_status_lab, any_of(tumour_char), d_stage_cat, any_of(tumour_char_tr), any_of(time_vars), AgeatEntry, d_R1toBC_lab, ImageType, reg_sd)
#names(df)
#str(df)

# create dataset for complete case analysis with NA as missing (this will be used for a comparison of old and new method)
df_cc <- df


# Recode missing values -----------------------------------
vis_dat(df)

#vis_miss(df)

gg_miss_var(df)

# an_df %>% 
#   map(pct_miss)



# convert all factor variables to character 
df <- df %>% 
  mutate(across(where(is.factor), as.character))


# convert all character variables NAs to "Missing" 
df <- df %>%
  mutate(across(where(is.character), ~ replace_na(., "Missing")))

str(df)

vis_dat(df) # only missing are those associated with MD which is a subset of the data

# convert all character variables back to factor
df <- df %>%
  mutate(across(where(is.character), as.factor) 
  )

freq(df)

vis_dat(df)
gg_miss_var(df)

# Relevel categorical variables ---------------------------------------

#Change the reference groups in the following categorical variables:
  
#  BMI: reference = normal weight

#hormone receptor status (ER, PR) and HER2: reference = positive

#BMI amended in this script directly before analysis as for descriptives the original level ordering will be needed

#Also, relevel variables where "Missing" level is not last (levels jumped when converteting NA to "Missing"
#                                                           
#                                                           (smoking, hrt, hrt - trick, oc, pre-menopausal status)


df <- df %>% 
  mutate(d_bmi_entry_lab = fct_relevel(d_bmi_entry_lab, "18.5-25", "<18.5", "25-30", "30+"),
         d_bmi_20_lab = fct_relevel(d_bmi_20_lab, "18.5-25", "<18.5", "25-30", "30+", "Missing"),
         d_R1menopause_lab3 = fct_relevel(d_R1menopause_lab3, "Premenopausal", "Postmenopausal"),
         d_age_menopause_lab2 = d_age_menopause_lab,
         d_age_menopause_lab = case_when(d_age_menopause_lab == "Pre-menopausal" ~ "Missing",
                                         TRUE ~ d_age_menopause_lab),
         
         d_R1hrtstatus_lab = case_when(d_R1hrtstatus_lab == "Pre-menopausal" ~ "Missing",
                                       TRUE ~ d_R1hrtstatus_lab),
         d_R1hrtstatus_lab = fct_relevel(d_R1hrtstatus_lab, "Never", "Former", "Current", "Missing"),
         d_R1hrt_tr_lab = fct_relevel(d_R1hrt_tr_lab, "Never", "Former", "Current",  "Missing"),
         
         d_R1hrt_sttyp_lab = case_when(d_R1hrt_sttyp_lab == "Pre-menopausal" ~ "Missing",
                                       TRUE ~ d_R1hrt_sttyp_lab),
         d_R1hrt_sttyp_lab = fct_relevel(d_R1hrt_sttyp_lab, "Never", "Former", "Current: Estrogen only", "Current: Estrogen and progestogen", "Current: Other HRT", "Missing"),
         d_R1hrt_sttyp_tr_lab = fct_relevel(d_R1hrt_sttyp_tr_lab, "Never", "Former", "Current: Estrogen only", "Current: Estrogen and progestogen", "Current: Other HRT", "Missing"),
         
         d_agebirth1_lab = case_when(d_agebirth1_lab == "Non-parous" ~ "Missing",
                                     TRUE ~ d_agebirth1_lab),
         d_R1physmet_leis_who_lab = fct_relevel(d_R1physmet_leis_who_lab, "<9", "9-17", "18+"),
         d_ocstatus_lab = fct_relevel(d_ocstatus_lab, "Never","Former", "Current", "Missing"),
         d_ocstatus2_lab = fct_relevel(d_ocstatus2_lab, "No", "Yes", "Missing"),
         d_breastfed_lab = fct_relevel(d_breastfed_lab, "No", "Yes", "Missing"),
         d_breastfed_tr_lab = fct_relevel(d_breastfed_tr_lab, "No", "Yes"),
         d_bf_dur_lab = fct_relevel(d_bf_dur_lab, "Never breastfed", "<6 months", "6-12 months", "12-24 months", "24+ months", "Missing"),
         d_bf_dur_tr_lab = fct_relevel(d_bf_dur_tr_lab, "Never breastfed", "<6 months", "6-12 months", "12-24 months", "24+ months"),
         d_R1smokingstatus_lab = fct_relevel(d_R1smokingstatus_lab, "Never","Former", "Current", "Missing"),
         d_pos_nodes_lab = fct_relevel(d_pos_nodes_lab, "Negative", "Positive", "Missing"),
         d_er_status_lab = fct_relevel(d_er_status_lab, "Positive", "Negative", "Missing"),
         d_pr_status_lab = fct_relevel(d_pr_status_lab, "Positive", "Negative", "Missing"),
         d_her2_status_lab = fct_relevel(d_her2_status_lab, "Positive", "Negative", "Missing"),
         d_pos_nodes_tr_lab = fct_relevel(d_pos_nodes_tr_lab, "Negative", "Positive", "Missing"),
         d_er_tr_lab = fct_relevel(d_er_tr_lab, "Positive", "Negative", "Missing"),
         d_pr_tr_lab = fct_relevel(d_pr_tr_lab, "Positive", "Negative", "Missing"),
         d_her2_tr_lab = fct_relevel(d_her2_tr_lab, "Positive", "Negative", "Missing"),
         d_morph4_lab = fct_relevel(d_morph4_lab, "Ductal", "Lobular", "Mixed: ductal and other", "Other", "Missing"),
         reg_sd = as.factor(reg_sd),
         d_MDtoBC_lab = fct_relevel(d_MDtoBC_lab, "<3 years", "3-5 years", "6+ years")
  )

#freq(df)

#str(df)

# Save dataset with missing categories: main analytical dataset
saveRDS(df, file = "Q:/SHARED/USERS/MBrayley/Screening/data/an_df_missing_cat.rds")


# relevel complete case data (only bmi)

df_cc <- df_cc %>% 
  mutate(d_bmi_entry_lab = fct_relevel(d_bmi_entry_lab, "18.5-25", "<18.5", "25-30", "30+"))

# save complete case data
saveRDS(df_cc, file = "Q:/SHARED/USERS/MBrayley/Screening/data/an_df_compl_case.rds")
