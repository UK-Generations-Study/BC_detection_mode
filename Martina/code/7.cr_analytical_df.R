# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 7. Compile analytical dataset  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 07/05/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  
#                 
#_____________________________________________________________________________


# 1. Create a dataset with needed variables --------------------------

## select variables from detection mode -----------------------

dm_vars <- dm_df %>% 
  select(tcode, ancat_dmode_v2, source_dm2, dm2_screen_date_f, dm1_screen_date_f, dens_dm2_screen_date_f, SD_dg_first_screen)

## select variables from cancer df ---------------------------

ca_vars <- cancer_df %>% 
  select(tcode, date_birth, date_entry, diagdate, diagage, 
         incident, side, ICDt, breast_cancer, breast_cancer_invasive, breast_cancer_dcis, 
         stage, grade, er_Status, pr_Status, her2_Status, Tsize, nodes_tot, nodes_pos)

## select variables from mean density ----------------------------

# selecting all as for now as compiled in script 6
density_vars <- mean_density_df 
  
## select variables from risk factor -------------------------------

rf_vars <- riskfactors_df %>% 
  select(tcode)




# 2. Join all together -------------------------------------------

dev_an_df <- ca_vars %>% 
  left_join(dm_vars, by = "tcode") %>% 
  left_join(density_vars, by = "tcode") 

str(dev_an_df)




# 3. Prepare analytical variables --------------------------------------------------

## AGE AND TIMING VARIABLES ------------------------------------------------

### Time between entry and BC diagnosis ------------------------
# continuous only

dev_an_df <- dev_an_df %>% 
  mutate(
    d_R1toBC = as.numeric(diagdate - date_entry),
         d_R1toBC_y = round(R1toBC/365.25, 1))


# checks:
View(dev_an_df[,c("tcode", "diagdate", "date_entry", "R1toBC", "R1toBC_y")])

str(dev_an_df)
summary(dev_an_df$R1toBC_y)

dev_an_df %>% tabyl(R1toBC_y)

### Time between mammo and BC 

dev_an_df <- dev_an_df %>% 
  mutate(
    d_MDtoBC = as.numeric(diagdate - MammoDat_f),
    d_MDtoBC_y = round(MDtoBC/365.25, 1),
    d_MDtoBC_cat = case_when(MDtoBC_y < 3 ~ 1,
                           MDtoBC_y >=3 & MDtoBC_y <6 ~ 2,
                           MDtoBC_y >= 6 ~ 3,
                           TRUE ~ NA),
    d_MDtoBC_clab = factor(
      x = MDtoBC_cat, 
      levels = 1:3,
      labels = c("<3 years", "3-5 years", ">=6 years")
    )
    )

dev_an_df %>% tabyl(MDtoBC_y, MDtoBC_cat)
dev_an_df %>% tabyl(MDtoBC_cat, MDtoBC_clab)

View(dev_an_df[,c("tcode", "diagdate", "MammoDat_f", "MDtoBC_y",  "MDtoBC_cat", "MDtoBC_clab")])

str(dev_an_df)



## TUMOUR CHARACTERISTICS -------------------------------------------

### Invasive status ----------------------------------------------------
# categories: invasive/insitu

dev_an_df <- dev_an_df %>% 
  mutate(d_inv_status = case_when(breast_cancer_invasive == 1 ~ 1,
                                breast_cancer_dcis == 1 ~ 0,
                                TRUE ~ NA),
         d_inv_status_lab = factor(
           x = inv_status,
           levels = 0:1,
           labels = c("DCIS", "Invasive")
         )
         )

View(dev_an_df[,c("tcode", "diagdate", "ICDt", "breast_cancer_invasive", "breast_cancer_dcis", "inv_status", "inv_status_lab")])
dev_an_df %>% tabyl(inv_status, inv_status_lab)
dev_an_df %>% tabyl(inv_status_lab, breast_cancer_invasive)
dev_an_df %>% tabyl(inv_status_lab, breast_cancer_dcis)

### Grade ----------------------------------------------------------------
# categories: 1,2,3,n/k

dev_an_df %>% tabyl(grade)

dev_an_df %>% tabyl(grade, inv_status_lab)

dev_an_df <- dev_an_df %>% 
  mutate(d_grade = as.factor(case_when(grade %in% c("1", "low", "Low") ~ 1,
                             grade %in% c("2", "intermediate", "Intermediate") ~ 2,
                             grade %in% c("3", "high", "High") ~ 3,
                             grade %in% c("4", "7") ~ 777, # invalid value
                             is.na(grade) ~ 888 # missing
                             )),
         d_grade = ordered(x = d_grade, c("1", "2", "3", "777", "888"))
         )


dev_an_df %>% tabyl(grade, d_grade)


### Morphology -----------------------------------------------------------
# categories: ductal, lobular, mixed, mucinous, other 




### n of positive nodes ----------------------------------------------------
# categories: 0, 1-3, 4-10, >10, n/k 





### Size -------------------------------------------------------------
# categories: <21, 21-50, >50, n/k





### ER status ---------------------------------------------------------
# categories: negative, positive, n/k 




### PR status --------------------------------------------------------
# categories: negative, positive, n/k 





### HER2 status ----------------------------------------------------------
# categories: negative, positive, n/k 






















## RISK FACTORS -----------------------------------------------------


## MAMMO DENSITY ----------------------------------------------------
