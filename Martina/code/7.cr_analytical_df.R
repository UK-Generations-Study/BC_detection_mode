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
         incident, side, ICDt, ICDm, breast_cancer, breast_cancer_invasive, breast_cancer_dcis, 
         stage, grade, er_Status, pr_Status, her2_Status, Tsize, nodes_tot, nodes_pos, N)

## select variables from mean density ----------------------------

# selecting all as for now as compiled in script 6
density_vars <- mean_density_df 
  
## select variables from risk factor -------------------------------

rf_vars <- riskfactors_df %>% 
  select(tcode)




# 2. Join all together -------------------------------------------

dev_an_df <- dm_cases %>% 
  left_join(ca_vars, by = "tcode") %>% 
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
         d_R1toBC_y = round(d_R1toBC/365.25, 1))


# checks:
#View(dev_an_df[,c("tcode", "diagdate", "date_entry", "d_R1toBC", "d_R1toBC_y")])

str(dev_an_df)
summary(dev_an_df$d_R1toBC_y)

dev_an_df %>% tabyl(d_R1toBC_y)

### Time between mammo and BC 

dev_an_df <- dev_an_df %>% 
  mutate(
    d_MDtoBC = as.numeric(diagdate - MammoDat_f),
    d_MDtoBC_y = round(d_MDtoBC/365.25, 1),
    d_MDtoBC_cat = case_when(d_MDtoBC_y < 3 ~ 1,
                           d_MDtoBC_y >=3 & d_MDtoBC_y <6 ~ 2,
                           d_MDtoBC_y >= 6 ~ 3,
                           TRUE ~ NA),
    d_MDtoBC_clab = factor(
      x = d_MDtoBC_cat, 
      levels = 1:3,
      labels = c("<3 years", "3-5 years", ">=6 years")
    )
    )

dev_an_df %>% tabyl(d_MDtoBC_y, d_MDtoBC_cat)
dev_an_df %>% tabyl(d_MDtoBC_cat, d_MDtoBC_clab)

#View(dev_an_df[,c("tcode", "diagdate", "MammoDat_f", "d_MDtoBC_y",  "d_MDtoBC_cat", "d_MDtoBC_clab")])

str(dev_an_df)



## TUMOUR CHARACTERISTICS -------------------------------------------

### Invasive status ----------------------------------------------------
# categories: invasive/insitu

dev_an_df <- dev_an_df %>% 
  mutate(d_inv_status = case_when(breast_cancer_invasive == 1 ~ 1,
                                breast_cancer_dcis == 1 ~ 0,
                                TRUE ~ NA),
         d_inv_status_lab = factor(
           x = d_inv_status,
           levels = 0:1,
           labels = c("In situ", "Invasive")
         )
         )

#View(dev_an_df[,c("tcode", "diagdate", "ICDt", "breast_cancer_invasive", "breast_cancer_dcis", "d_inv_status", "d_inv_status_lab")])
dev_an_df %>% tabyl(d_inv_status, d_inv_status_lab)
dev_an_df %>% tabyl(d_inv_status_lab, breast_cancer_invasive)
dev_an_df %>% tabyl(d_inv_status_lab, breast_cancer_dcis)

### Grade ----------------------------------------------------------------
# categories: 1,2,3,n/k

dev_an_df %>% tabyl(grade)

dev_an_df %>% tabyl(grade, d_inv_status_lab)

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
dev_an_df %>% tabyl(d_grade)

### Stage -----------------------------------------------------------------
dev_an_df %>% tabyl(stage)

dev_an_df %>% tabyl(stage, d_inv_status_lab)

dev_an_df <- dev_an_df %>% 
  mutate(d_stage = as.factor(case_when(stage == "0" ~ 0,
                                       startsWith(stage, "1") ~ 1,
                                       startsWith(stage, "2") ~ 2, 
                                       startsWith(stage, "3") ~ 3,
                                       startsWith(stage, "4") ~ 4,
                                       stage == "II" ~ 2,
                                       stage == "III" ~ 3, 
                                       is.na(stage) ~ 888 # missing
                                       )
                             ),
         d_stage = ordered(x = d_stage, c("0", "1", "2", "3", "4", "888"))
         )
  

dev_an_df %>% tabyl(stage, d_stage)
dev_an_df %>% tabyl(d_stage)

### Morphology -----------------------------------------------------------
# categories: ductal, lobular, mixed, mucinous, other 

dev_an_df %>% tabyl(ICDm)

dev_an_df <- dev_an_df %>% 
  mutate(ICDm = as.character(ICDm)
         ) %>% 
  mutate(d_morphology = as.factor(case_when(ICDm == "85213" ~ 7, # Ductular
                                           ICDm == "82113" ~ 5, # Tubular
                                           
                                           startsWith(ICDm, "850") ~ 1, # Ductal
                                           startsWith(ICDm, "851") ~ 2, # Medullary
                                           startsWith(ICDm, "848") ~ 3, # Mucinous or colloid
                                           ICDm != "85213" & startsWith(ICDm, "852") ~ 4, # Lobular
                                           
                                           startsWith(ICDm, "805") ~ 6, # Papillary
                                           
                                           ICDm == "81403" ~ 8, # Adenocarcinoma, NOS
                                           
                                           is.na(ICDm) ~ 10, # not known
                                           
                                           TRUE ~ 9 # other
                                           )
                                 ),
         d_morphology = ordered(x = d_morphology, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
         ) %>% 
  mutate(
          d_morphology_lab = factor(
           x = d_morphology, 
           levels = 1:10,
           labels = c("Ductal", "Medullary", "Mucinous or colloid", "Lobular", "Tubular", "Papillary", "Ductular", "Adenocarcinoma, NOS", "Other", "Not known")
         )
         ) %>% 
  # condensed morphology 
  mutate(d_morph5 = as.factor(case_when(d_morphology == 1 ~ 1,
                             d_morphology == 4 ~ 2,
                             d_morphology == 5 ~ 3,
                             d_morphology %in% c(2, 3, 6, 7, 8, 9) ~ 4,
                             d_morphology == 10 ~ 5)),
         d_morph5 = ordered(x = d_morph5, c("1", "2", "3", "4", "5")),
         d_morph5_lab = factor(x = d_morph5, 
                               levels = 1:5, 
                               labels = c("Ductal", "Lobular", "Tubular", "Other", "Not known"))
         )

         
         
         
dev_an_df %>% tabyl(d_morphology) %>% 
  adorn_totals()

dev_an_df %>% tabyl(ICDm, d_morphology)

dev_an_df %>% tabyl(d_morphology_lab)

dev_an_df %>% tabyl(d_morphology_lab, d_morphology)

dev_an_df %>% tabyl(d_morph5)
dev_an_df %>% tabyl(d_morph5_lab)
dev_an_df %>% tabyl(d_morph5_lab, d_morph5)

### n of positive nodes ----------------------------------------------------
# categories: 0, 1-3, 4-10, >10, n/k 

dev_an_df %>% tabyl(nodes_pos)

dev_an_df %>% tabyl(N)


### positive nodes y/n ----------------------------------------------------
# binary y/n







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
