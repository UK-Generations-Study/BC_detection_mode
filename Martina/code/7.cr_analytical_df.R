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
         stage, grade, er_Status, pr_Status, her2_Status, Tsize, nodes_tot, nodes_pos, N, AgeatEntry)

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
# continuous and categorical 

dev_an_df <- dev_an_df %>% 
  mutate(
    d_R1toBC = as.numeric(diagdate - date_entry),
    d_R1toBC_y = round(d_R1toBC/365.25, 1),
    d_R1toBC_cat = case_when(d_R1toBC_y < 3 ~ 1,
                             d_R1toBC_y >= 3 & d_R1toBC_y <= 5.9 ~ 2,
                             d_R1toBC_y >= 6 & d_R1toBC_y <= 8.9 ~ 3,
                             d_R1toBC_y >= 9 & d_R1toBC_y <= 11.9 ~ 4,
                             d_R1toBC_y >= 12 ~ 5,
                             ),
    d_R1toBC_cat = ordered(x = d_R1toBC_cat, c("1", "2", "3", "4", "5") # ordering so when factor is created it stays ordered
                           ),
    d_R1toBC_lab = factor(x = d_R1toBC_cat,
                          levels = 1:5,
                          labels = c("<3 years", "3 to 5 years", "6 to 8 years", "9 to 11 years", ">12 years" ))
    )


# checks:
#View(dev_an_df[,c("tcode", "diagdate", "date_entry", "d_R1toBC", "d_R1toBC_y")])

str(dev_an_df$)
summary(dev_an_df$d_R1toBC_y)

dev_an_df %>% tabyl(d_R1toBC_y)

dev_an_df %>% tabyl(d_R1toBC_y, d_R1toBC_cat)
dev_an_df %>% tabyl(d_R1toBC_y, d_R1toBC_lab)
dev_an_df %>% tabyl(d_R1toBC_lab)

### Time between mammo and BC --------------------------------

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

### Age at breast cancer diagnosis -------------------------------------
# does not need preparing as it looks okay 

dev_an_df %>% tabyl(diagage)
hist(dev_an_df$diagage)


### Age at entry -----------------------------------------------------
# does not need preparing as it looks okay 
dev_an_df %>% tabyl(AgeatEntry)
hist(dev_an_df$AgeatEntry)

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
  mutate(d_morphology = as.factor(case_when(#specific types with full codes hence first
                                            ICDm == "82113" ~ 5, # Tubular
                                            ICDm == "85213" ~ 7, # Ductular
                                            ICDm == "81403" ~ 8, # Adenocarcinoma, NOS
                                           
                                           # combined types - full codes hence first
                                           ICDm == "85223" ~ 9, # # infiltrating duct and lobular
                                           ICDm == "85233" ~ 10, # infiltrating duct mixed with other types of carcinoma
                                           ICDm == "85243" ~ 11, # Infiltrating lobular mixed with other types of carcinoma
                                           
                                           # more general types - starting with codes hence last 
                                           startsWith(ICDm, "850") ~ 1, # Ductal
                                           startsWith(ICDm, "851") ~ 2, # Medullary
                                           startsWith(ICDm, "848") ~ 3, # Mucinous or colloid
                                           ICDm != "85213" & startsWith(ICDm, "852") ~ 4, # Lobular
                                           
                                           startsWith(ICDm, "805") ~ 6, # Papillary
                                           
                                           
                                           
                                           is.na(ICDm) ~ 888, # not known
                                           
                                           TRUE ~ 12 # other
                                           )
                                 ),
         d_morphology = ordered(x = d_morphology, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "888"))
         ) %>% 
  mutate(
          d_morphology_lab = factor(
           x = d_morphology, 
           levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 888),
           labels = c("Ductal", "Medullary", "Mucinous or colloid", "Lobular", 
                      "Tubular", "Papillary", "Ductular", "Adenocarcinoma, NOS", 
                      "Mixed: Ductal and lobular", "Mixed: Ductal and other", "Mixed: Lobular and other",  
                      "Other", "Not known")
         )
         ) %>%
  # condensed morphology
  mutate(d_morph7 = as.factor(case_when(d_morphology == 1 ~ 1, # ductual
                             d_morphology == 4 ~ 2, # lobular
                             d_morphology == 9 ~ 3, # mixed duct and lobular
                             d_morphology == 10 ~ 4, # mixed duct and other
                             d_morphology == 5 ~ 5, # Tubular
                             d_morphology == 3 ~ 6, # Mucinous or colloid
                             d_morphology %in% c(2, 6, 7, 8, 11, 12) ~ 7, # Other
                             d_morphology == 888 ~ 888)), # Not known
         d_morph7 = ordered(x = d_morph7, c("1", "2", "3", "4", "5", "6", "7", "888")),
         d_morph7_lab = factor(x = d_morph7,
                               levels = c(1, 2, 3, 4, 5, 6, 7, 888),
                               labels = c("Ductal", "Lobular", "Mixed: ductal and lobular", "Mixed: ductal and other", "Tubular", "Mucinous or colloid", "Other", "Not known"))
         )

         
         
         
dev_an_df %>% tabyl(d_morphology) %>% 
  adorn_totals()

dev_an_df %>% tabyl(ICDm, d_morphology)

dev_an_df %>% tabyl(d_morphology_lab)

dev_an_df %>% tabyl(d_morphology_lab, d_morphology)

dev_an_df %>% tabyl(d_morph7)
dev_an_df %>% tabyl(d_morph7_lab)
dev_an_df %>% tabyl(d_morph7_lab, d_morph7)
dev_an_df %>% tabyl(d_morphology_lab, d_morph7_lab)

### n of positive nodes ----------------------------------------------------
# categories: 0, 1-3, 4-10, >10, n/k
# L's notes said this should be replaced by binary nodes variable - below

dev_an_df %>% tabyl(nodes_pos)
str(dev_an_df$nodes_pos)

dev_an_df %>% tabyl(nodes_tot, nodes_pos)

dev_an_df %>% tabyl(N)


### positive nodes y/n ----------------------------------------------------
# binary y/n

dev_an_df <- dev_an_df %>% 
  mutate(d_pos_nodes_n = case_when(nodes_pos == "Y" ~ "777", # unlikely value
                                   is.na(nodes_pos) ~ "888", # is missing
                                   TRUE ~ nodes_pos
                                   
                                      ) ,
         d_pos_nodes_n = as.numeric(d_pos_nodes_n) ,

         d_pos_nodes = as.factor(case_when(d_pos_nodes_n == 0 ~ 0,
                                           d_pos_nodes_n >= 1 & d_pos_nodes_n < 777 ~ 1,
                                           d_pos_nodes_n %in% c(888, 777) ~ 888
                                           )
                                 
                                           
                                 ) ,
        d_pos_nodes = ordered(x = d_pos_nodes, c("0", "1", "888")),
        d_pos_nodes_lab = factor(x = d_pos_nodes,
                                 levels = c(0, 1, 888),
                                 labels = c("Negative", "Positive", "Not known")
        )
         )

dev_an_df %>% tabyl(d_pos_nodes, d_pos_nodes_lab)

dev_an_df %>% tabyl(d_pos_nodes_n)

### Size -------------------------------------------------------------
# categories: <21, 21-50, >50, n/k
dev_an_df %>% tabyl(Tsize)
str(dev_an_df$Tsize)

dev_an_df <- dev_an_df %>% 
  mutate(d_tumour_size_n = case_when(Tsize == "." ~ "777", # unlikely value
                                     is.na(Tsize) ~ "888", # not known
                                     TRUE ~ Tsize
                                     ),
         d_tumour_size_n = as.numeric(d_tumour_size_n),
         
         d_tumour_size = as.factor(case_when(d_tumour_size_n < 21 ~ 1,
                                             d_tumour_size_n >= 21 & d_tumour_size_n <= 50 ~ 2,
                                             d_tumour_size_n > 50 & d_tumour_size_n < 776 ~ 3,
                                             d_tumour_size_n == 888 | d_tumour_size_n == 777 ~ 888 # not known
                                             )
                                   ),
         d_tumour_size = ordered(x = d_tumour_size, c("1", "2", "3", "888")
                                 ),
         d_tumour_size_lab = factor(x = d_tumour_size,
                                levels = c(1, 2, 3, 888),
                                labels = c("<21", "21-50", ">50", "Not known")
                                )
         )

str(dev_an_df$d_tumour_size_n)
dev_an_df %>% tabyl(d_tumour_size_n)


dev_an_df %>% tabyl(d_tumour_size_n)
       
dev_an_df %>% tabyl(Tsize, d_tumour_size) %>% 
  adorn_totals()

dev_an_df %>% tabyl(d_tumour_size_n, d_tumour_size) %>% 
  adorn_totals()

### ER status ---------------------------------------------------------
# categories: negative, positive, n/k 
dev_an_df %>% tabyl(er_Status)

dev_an_df <- dev_an_df %>% 
  mutate(d_er_status = as.factor(case_when(er_Status == "Negative" ~ 0,
                                           er_Status == "Positive" ~ 1,
                                           is.na(er_Status) ~ 888
                                           )
                                 ),
         d_er_status = ordered(x = d_er_status, c("0", "1", "888")
                              ),
         d_er_status_lab = factor(x = d_er_status,
                                  levels = c(0, 1, 888),
                                  labels = c("Negative", "Positive", "Not known"))
         )

dev_an_df %>% tabyl(d_er_status)
dev_an_df %>% tabyl(d_er_status, d_er_status_lab)
dev_an_df %>% tabyl(er_Status, d_er_status_lab)

### PR status --------------------------------------------------------
# categories: negative, positive, n/k 
dev_an_df %>% tabyl(pr_Status)

dev_an_df <- dev_an_df %>% 
  mutate(d_pr_status = as.factor(case_when(pr_Status == "Negative" ~ 0,
                                           pr_Status == "Positive" ~ 1,
                                           is.na(pr_Status) ~ 888
  )
  ),
  d_pr_status = ordered(x = d_pr_status, c("0", "1", "888")
  ),
  d_pr_status_lab = factor(x = d_pr_status,
                           levels = c(0, 1, 888),
                           labels = c("Negative", "Positive", "Not known"))
  )

dev_an_df %>% tabyl(d_pr_status)
dev_an_df %>% tabyl(d_pr_status, d_pr_status_lab)
dev_an_df %>% tabyl(pr_Status, d_pr_status_lab)





### HER2 status ----------------------------------------------------------
# categories: negative, positive, n/k 

dev_an_df %>% tabyl(her2_Status)

dev_an_df <- dev_an_df %>% 
  mutate(d_her2_status = as.factor(case_when(her2_Status == "Negative" ~ 0,
                                           her2_Status == "Positive" | her2_Status == "Borderline" ~ 1,
                                           is.na(her2_Status) ~ 888
  )
  ),
  d_her2_status = ordered(x = d_her2_status, c("0", "1", "888")
  ),
  d_her2_status_lab = factor(x = d_her2_status,
                           levels = c(0, 1, 888),
                           labels = c("Negative", "Positive", "Not known"))
  )

dev_an_df %>% tabyl(d_her2_status)
dev_an_df %>% tabyl(d_her2_status, d_her2_status_lab)
dev_an_df %>% tabyl(her2_Status, d_her2_status_lab)


  
## RISK FACTORS -----------------------------------------------------

### Alcohol (units per week) -----------------------------------
# categories(0, 1-9, 10-19, 20-29, >=30)




### Benign breast disease ----------------------------------------
# binary y/n



### BMI ----------------------------------------------------------
# WHO categories 



### Number of relatives with BC -----------------------------------
# categories: 0, 1, >2



### HRT status -------------------------------------------------------
# categories: never, former, current 



### Age at menarche -----------------------------------------------
# categories: <13, >=13 



### Menopausal status at baseline -----------------------------------
# binary: pre/post 



### Age at menopause -----------------------------------------------
# categories: <=50, 51-53, >53






### OC status --------------------------------------------------------
# categories: never, former, current 





### Physical activity --------------------------------------------------
# categories: METs per week <= 25, 25.1 to 50, 50.1 to 75, > 75 - this might need revising and check recommendations 




### Parity ---------------------------------------------------------
# binary y/n





### Age at first birth -------------------------------------------------
# categories: <20, 20-24, 25-29, 30-34, >=35




### Number of children ----------------------------------------------------
# categories: 1, 2, 3, >=4 





### SES -------------------------------------------------------------
# categories: Affluent achievers, rising prosperity, comfortable communities, 
#             financially streched, urban adversity and non-private households 





### Smoking -----------------------------------------------------------
# categories: never, former, current 






## MAMMO DENSITY ----------------------------------------------------



# df summary -----------------------------------------------------------
stview(dfSummary(dev_an_df))
