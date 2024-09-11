# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 7. Compile analytical dataset  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 07/05/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  2
#                  3 - with missing values as NA instead of 888, trick variables recoded correctly
#                  4 - trick variables recoded back to include missing from the varaible of interest in the refrence group
#_____________________________________________________________________________


# 1. Create a dataset with needed variables --------------------------

## select variables from detection mode -----------------------

dm_vars <- dm_df %>% 
  select(tcode, ancat_dmode_v2, source_dm2, dm2_screen_date_f, dm1_screen_date_f, dens_dm2_screen_date_f, SD_dg_first_screen, reg_sd)

## select variables from cancer df ---------------------------

ca_vars <- cancer_df %>% 
  select(tcode, date_birth, date_entry, diagdate, yeardiag, diagage, 
         incident, side, ICDt, ICDm, breast_cancer, breast_cancer_invasive, breast_cancer_dcis, 
         stage, grade, er_Status, pr_Status, her2_Status, Tsize, nodes_tot, nodes_pos, N, AgeatEntry)

## select variables from mean density ----------------------------

# selecting all as for now as compiled in script 6
density_vars <- mean_density_df %>% 
  select(tcode, MammoDat_f, ImageType, MD_avail, mean_density, sd_density)
  
## select variables from risk factor -------------------------------

rf_vars <- riskfactors_df %>% 
  select(tcode, R1alcoholstatus, alcoholstartage, alcoholstopage, R1alcoholunits,
         brbendis, fambrca, fambrcaN,
         bodysizebminow,
         hrtstatus, R1menopause, meno_age_est,
         menarcheage, ocstatus,
         PhysMetTotal, PhysMetRecTot,
         pregparitycnt, R1smokingstatus, ses,
         x_parous, x_parity, x_age_menarche, x_ocstatus, x_breastfed, x_age_birth_1, x_age_birth_last,
         x_breastfeeding_duration, x_breastfed)




# 2. Join all together -------------------------------------------

dev_an_df <- dm_cases %>% 
  left_join(ca_vars, by = "tcode") %>% 
  left_join(dm_vars, by = "tcode") %>% 
  left_join(rf_vars, by = "tcode") %>% 
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
    d_R1toBC_cat = as.factor(case_when(d_R1toBC_y < 3 ~ 1,
                             d_R1toBC_y >= 3 & d_R1toBC_y < 6 ~ 2,
                             d_R1toBC_y >= 6 & d_R1toBC_y < 9 ~ 3,
                             d_R1toBC_y >= 9 & d_R1toBC_y < 12 ~ 4,
                             d_R1toBC_y >= 12 ~ 5
                             )),
    d_R1toBC_cat = fct_relevel(d_R1toBC_cat, "1", "2", "3", "4", "5"),
    d_R1toBC_lab = factor(x = d_R1toBC_cat,
                          levels = 1:5,
                          labels = c("<3 years", "3-5 years", "6-8 years", "9-11 years", "12+ years" ))
    )


# checks:
#View(dev_an_df[,c("tcode", "diagdate", "date_entry", "d_R1toBC", "d_R1toBC_y")])


summary(dev_an_df$d_R1toBC_y)

dev_an_df %>% tabyl(d_R1toBC_y)

dev_an_df %>% tabyl(d_R1toBC_y, d_R1toBC_cat)
dev_an_df %>% tabyl(d_R1toBC_y, d_R1toBC_lab)
dev_an_df %>% tabyl(d_R1toBC_lab)
dev_an_df %>% tabyl(d_R1toBC_lab, d_R1toBC_cat)

### Time between mammo and BC --------------------------------

dev_an_df <- dev_an_df %>% 
  mutate(
    d_MDtoBC = as.numeric(diagdate - MammoDat_f),
    d_MDtoBC_y = round(d_MDtoBC/365.25, 1),
    d_MDtoBC_cat = as.factor(case_when(d_MDtoBC_y < 3 ~ 1,
                           d_MDtoBC_y >=3 & d_MDtoBC_y <6 ~ 2,
                           d_MDtoBC_y >= 6 ~ 3,
                           TRUE ~ NA)),
    d_MDtoBC_cat = fct_relevel(d_MDtoBC_cat, "1", "2", "3"),
    d_MDtoBC_lab = factor(
      x = d_MDtoBC_cat, 
      levels = c(1, 2, 3),
      labels = c("<3 years", "3-5 years", "6+ years")
    )
    )


dev_an_df %>% tabyl(d_MDtoBC_y, d_MDtoBC_cat)
dev_an_df %>% tabyl(d_MDtoBC_cat, d_MDtoBC_lab)
dev_an_df %>% tabyl(d_MDtoBC_lab)

#View(dev_an_df[,c("tcode", "diagdate", "MammoDat_f", "d_MDtoBC_y",  "d_MDtoBC_cat", "d_MDtoBC_clab")])

str(dev_an_df)

### Time between mammo and entry -------------------------------------
dev_an_df <- dev_an_df %>% 
  mutate(
    d_R1toMD = as.numeric(MammoDat_f - date_entry),
    d_R1toMD_y = round(d_R1toMD/365.25, 1)
    )

summary(dev_an_df$d_R1toMD_y)
hist(dev_an_df$d_R1toMD_y)
#View(dev_an_df[,c("tcode", "date_entry", "MammoDat_f", "d_R1toMD_y")])

### Age at breast cancer diagnosis -------------------------------------
# does not need preparing as it looks okay 

dev_an_df %>% tabyl(diagage)
hist(dev_an_df$diagage)


### Age at entry -----------------------------------------------------
# does not need preparing as it looks okay 
dev_an_df %>% tabyl(AgeatEntry)
hist(dev_an_df$AgeatEntry)


### Age at mammo ------------------------------------------------------

dev_an_df <- dev_an_df %>% 
  mutate(d_age_mammo = if_else(is.na(MammoDat_f) | is.na(date_birth), 888, trunc(as.numeric(MammoDat_f - date_birth)/365.25)
                               )
         )

dev_an_df %>% tabyl(d_age_mammo)

#View(dev_an_df[,c("tcode", "MammoDat_f", "date_birth", "d_age_mammo")])

## TUMOUR CHARACTERISTICS -------------------------------------------

### Invasive status ----------------------------------------------------
# categories: invasive/insitu

dev_an_df <- dev_an_df %>% 
  mutate(d_inv_status = as.factor(case_when(breast_cancer_invasive == 1 ~ 1,
                                breast_cancer_dcis == 1 ~ 0,
                                TRUE ~ NA)),
         d_inv_status_lab = factor(
           x = d_inv_status,
           levels = 0:1,
           labels = c("DCIS", "Invasive")
         )
         )

#View(dev_an_df[,c("tcode", "diagdate", "ICDt", "breast_cancer_invasive", "breast_cancer_dcis", "d_inv_status", "d_inv_status_lab")])
dev_an_df %>% tabyl(d_inv_status, d_inv_status_lab)
dev_an_df %>% tabyl(d_inv_status_lab, breast_cancer_invasive)
dev_an_df %>% tabyl(d_inv_status_lab, breast_cancer_dcis)
dev_an_df %>% tabyl(d_inv_status_lab)

### Grade ----------------------------------------------------------------
# categories: 1,2,3,n/k

# this wrongly includes dcis

dev_an_df %>% tabyl(grade)

dev_an_df %>% tabyl(grade, d_inv_status_lab)

dev_an_df <- dev_an_df %>% 
  mutate(d_grade = case_when(d_inv_status == 0  ~ NA, # dcis grade not valid 
                            d_inv_status == 1 & grade %in% c("1", "low", "Low") ~ 1,
                            d_inv_status == 1 & grade %in% c("2", "intermediate", "Intermediate") ~ 2,
                            d_inv_status == 1 & grade %in% c("3", "high", "High") ~ 3,
                            is.na(grade) |  grade %in% c("4", "7") ~ NA # missing
                             ),
         
         d_grade_cat = as.factor(d_grade),
         d_grade_cat = fct_relevel(d_grade_cat, "1", "2", "3"),
         d_grade_lab = factor(x = d_grade,
                              levels = c(1, 2, 3),
                              labels = c("1", "2", "3"))
         )


dev_an_df %>% tabyl(grade, d_grade)
dev_an_df %>% tabyl(d_grade, d_inv_status_lab)
dev_an_df %>% tabyl(d_grade_lab)
dev_an_df %>% tabyl(d_grade_lab, d_grade_cat)


### Grade - trick ---------------------------------------------------------------

dev_an_df <- dev_an_df %>% 
  mutate(d_grade_tr = as.factor(case_when(d_inv_status == 0 ~ "1", # making dcis part of reference group (grade 1)
                                TRUE ~ as.character(d_grade)
                                )),
         d_grade_tr = fct_relevel(d_grade_tr, "1", "2", "3")
         )

dev_an_df %>% tabyl(d_grade_tr, d_grade)
dev_an_df %>% tabyl(d_grade_tr)
dev_an_df %>% tabyl(d_grade_tr, d_inv_status)

# Louises's logic from stata using raw grade variable 

# dev_an_df <- dev_an_df %>% 
#   mutate(d_grade2 = case_when(
#      grade %in% c("1", "low", "Low") ~ 1,
#      grade %in% c("2", "intermediate", "Intermediate") ~ 2,
#     grade %in% c("3", "high", "High") ~ 3,
#     grade %in% c("4", "7") ~ 999, # invalid value
#     is.na(grade) ~ NA)) # missing
# 
# dev_an_df %>% tabyl(d_grade2)
# 
# dev_an_df <- dev_an_df %>% 
#   mutate(d_grade_L = if_else(d_inv_status == 0 & d_grade2 >= 0 & d_grade2 <= 999, 1, d_grade2))
# 
# dev_an_df %>% tabyl(d_grade_L)
# dev_an_df %>% tabyl(d_grade_L, d_grade2)
# 
# dev_an_df %>% tabyl(d_grade_L, d_grade_tr) %>% 
#   adorn_totals() %>% 
#   adorn_title()  
  

### Stage -----------------------------------------------------------------
dev_an_df %>% tabyl(stage)

dev_an_df %>% tabyl(stage, d_inv_status_lab)

dev_an_df <- dev_an_df %>% 
  mutate(d_stage = as.factor(case_when(d_inv_status == "0" ~ 0,
                                       #stage == "0" ~ 0, # most dcis
                                       d_inv_status == 1 & startsWith(stage, "1") ~ 1,
                                       d_inv_status == 1 & startsWith(stage, "2") ~ 2, 
                                       d_inv_status == 1 & startsWith(stage, "3") ~ 3,
                                       d_inv_status == 1 & startsWith(stage, "4") ~ 4,
                                       d_inv_status == 1 & stage == "II" ~ 2,
                                       d_inv_status == 1 & stage == "III" ~ 3,
                                       #d_inv_status == 0 & (!is.na(stage) | stage !=0) ~ 999, # dcis
                                       is.na(stage) ~ NA # missing
                                       )
                             ),
         d_stage_cat = factor(x = d_stage, 
                                 levels = 0:4,
                                 labels = c("0", "1", "2", "3", "4"))
         )
  

dev_an_df %>% tabyl(stage, d_stage)
dev_an_df %>% tabyl(d_stage)
dev_an_df %>% tabyl(d_stage, d_inv_status)

dev_an_df %>% tabyl(d_stage_cat)
dev_an_df %>% tabyl(stage, d_stage_cat)
# stage is not in the model so no trick needed


### Morphology -----------------------------------------------------------
# categories: ductal, lobular, mixed, mucinous, other 

dev_an_df %>% tabyl(ICDm)

dev_an_df <- dev_an_df %>% 
  mutate(ICDm = as.character(ICDm)
         ) %>% 
  mutate(d_morphology = as.factor(case_when(d_inv_status == 0 ~ NA,
                                            #specific types with full codes hence first
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
                                           
                                           
                                           
                                           is.na(ICDm) ~ NA, # not known
                                           
                                           TRUE ~ 12 # other
                                           )
                                 ),
        d_morphology = fct_relevel(d_morphology, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
         ) %>% 
  mutate(
          d_morphology_lab = factor(
           x = d_morphology, 
           levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
           labels = c("Ductal", "Medullary", "Mucinous or colloid", "Lobular", 
                      "Tubular", "Papillary", "Ductular", "Adenocarcinoma, NOS", 
                      "Mixed: Ductal and lobular", "Mixed: Ductal and other", "Mixed: Lobular and other",  
                      "Other")
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
                             is.na(d_morphology) ~ NA)), # Not known
         d_morph7 = fct_relevel(d_morph7, "1", "2", "3", "4", "5", "6", "7"),
         d_morph7_lab = factor(x = d_morph7,
                               levels = c(1, 2, 3, 4, 5, 6, 7),
                               labels = c("Ductal", "Lobular", "Mixed: ductal and lobular", "Mixed: ductal and other", "Tubular", "Mucinous or colloid", "Other")
                               ),
         # morph with 4 categories only due to small numbers
         d_morph4 = as.factor(case_when(as.character(d_morph7) %in% c(3, 4) ~ "3",
                                        as.character(d_morph7) %in% c(5, 6, 7) ~ "4",
                                        TRUE ~ as.character(d_morph7)
                                        )),
         d_morph4 = fct_relevel(d_morph4, "1", "2", "3", "4"),
         d_morph4_lab = factor(x = d_morph4,
                               levels = 1:4,
                               labels = c("Ductal", "Lobular", "Mixed: ductal and other", "Other"))
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

dev_an_df %>% tabyl(d_morphology_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_morph7_lab, d_inv_status_lab)

dev_an_df %>% tabyl(d_morph7_lab, d_morph4)
dev_an_df %>% tabyl(d_morphology_lab, d_morph4_lab)

### Morphology - trick ------------------------------------------------------
# recode dcis that are not already in ductal to be in ductal (refernce group 1)

# dev_an_df <- dev_an_df %>% 
#   mutate(d_morph7_tr = as.factor(case_when(as.character(d_inv_status) == 0 ~ "1",
#                                  TRUE ~ as.character(d_morph7)
#                                  )),
#          d_morph7_tr_lab = factor(x = d_morph7_tr,
#                                levels = c(1, 2, 3, 4, 5, 6, 7),
#                                labels = c("Ductal", "Lobular", "Mixed: ductal and lobular", "Mixed: ductal and other", "Tubular", "Mucinous or colloid", "Other"))
#          )


dev_an_df <- dev_an_df %>% 
  mutate(d_morph4_tr = as.factor(case_when(as.character(d_inv_status) == 0  ~ "1",
                                           TRUE ~ as.character(d_morph4)
  )),
  d_morph4_tr = fct_relevel(d_morph4_tr, "1", "2", "3", "4"),
  d_morph4_tr_lab = factor(x = d_morph4_tr,
                           levels = 1:4,
                           labels = c("Ductal", "Lobular", "Mixed: ductal and other", "Other"))
  )


dev_an_df %>% tabyl(d_morph4_tr, d_inv_status_lab)
dev_an_df %>% tabyl(d_morph4_tr_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_morph4_tr_lab)

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
  mutate(d_pos_nodes_n = case_when(d_inv_status == 0 ~ NA,
                                   nodes_pos == "Y" ~ "777", # unlikely value
                                   is.na(nodes_pos) ~ "888", # is missing
                                   TRUE ~ nodes_pos # numeric
                                   
                                      ) ,
         d_pos_nodes_n = as.numeric(d_pos_nodes_n) , # probably could have done this as part of the above code

         d_pos_nodes_cat = as.factor(case_when(d_pos_nodes_n == 0 ~ 0,
                                           d_pos_nodes_n >= 1 & d_pos_nodes_n < 777 ~ 1,
                                           d_pos_nodes_n %in% c(888, 777) ~ NA
                                           )
                                 
                                           
                                 ) ,
        d_pos_nodes_cat = fct_relevel(d_pos_nodes_cat, "0", "1"),
        d_pos_nodes_lab = factor(x = d_pos_nodes_cat,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive")
        )
         )

dev_an_df %>% tabyl(d_pos_nodes_cat, d_pos_nodes_lab)

dev_an_df %>% tabyl(d_pos_nodes_n)

dev_an_df %>% tabyl(d_pos_nodes_lab, d_inv_status_lab)

### positive nodes - trick ---------------------------------------------------

# recode dcis to be 0 (no)

dev_an_df <- dev_an_df %>% 
  mutate(d_pos_nodes_tr = case_when(as.character(d_inv_status) == "0" ~ "0",
                                    TRUE ~ as.character(d_pos_nodes_cat)),
         #d_pos_nodes_tr = ordered(x = d_pos_nodes_tr, levels = c("0", "1", "888")),
         d_pos_nodes_tr_lab = factor(x = d_pos_nodes_tr,
                                     levels = c("0", "1"),
                                     labels = c("Negative", "Positive"))
         )

dev_an_df %>% tabyl(d_pos_nodes_tr)
dev_an_df %>% tabyl(d_pos_nodes_tr_lab)
dev_an_df %>% tabyl(d_pos_nodes_tr_lab, d_pos_nodes_lab)
dev_an_df %>% tabyl(d_pos_nodes_tr_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_pos_nodes_lab, d_inv_status_lab)

### Size -------------------------------------------------------------
# categories: <21, 21-50, >50, n/k
dev_an_df %>% tabyl(Tsize)
str(dev_an_df$Tsize)

dev_an_df <- dev_an_df %>% 
  mutate(d_tumour_size_n = case_when(d_inv_status == 0 ~ NA,
                                     Tsize == "." ~ "777", # unlikely value
                                     is.na(Tsize) ~ "888", # not known
                                     TRUE ~ Tsize
                                     ),
         d_tumour_size_n = as.numeric(d_tumour_size_n),
         
         d_tumour_size = as.factor(case_when(d_tumour_size_n < 21 ~ 1,
                                             d_tumour_size_n >= 21 & d_tumour_size_n <= 50 ~ 2,
                                             d_tumour_size_n > 50 & d_tumour_size_n < 776 ~ 3,
                                             d_tumour_size_n == 888 | d_tumour_size_n == 777 ~ NA # not known
                                             )
                                   ),
         d_tumour_size = fct_relevel(d_tumour_size, "1", "2", "3"),
                                
         d_tumour_size_lab = factor(x = d_tumour_size,
                                levels = c(1, 2, 3),
                                labels = c("<21", "21-50", "50+")
                                )
         )

str(dev_an_df$d_tumour_size_n)
dev_an_df %>% tabyl(d_tumour_size_n)


dev_an_df %>% tabyl(d_tumour_size_n)
       
dev_an_df %>% tabyl(Tsize, d_tumour_size) %>% 
  adorn_totals()

dev_an_df %>% tabyl(d_tumour_size_n, d_tumour_size) %>% 
  adorn_totals()

dev_an_df %>% tabyl(d_tumour_size_lab)
dev_an_df %>% tabyl(d_tumour_size_lab, d_tumour_size)
dev_an_df %>% tabyl(d_tumour_size_lab, d_inv_status_lab)

### Size - trick ---------------------------------------------------------

# make dcis to be all in <21 category if not missing 

dev_an_df %>% tabyl(d_tumour_size_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_tumour_size)

dev_an_df <- dev_an_df %>% 
  mutate(d_tmsize_tr = case_when(as.character(d_inv_status) == "0" ~ "1", 
                                 TRUE ~ as.character(d_tumour_size)
                                 ),
         #d_tmsize_tr = ordered(x = d_tmsize_tr, levels = c("1", "2", "3", "888")
                              # ),
         d_tmsize_tr_lab = factor(x = d_tmsize_tr,
                                  levels = c(1, 2, 3),
                                  labels = c("<21", "21-50", "50+"))
         )

dev_an_df %>% tabyl(d_tmsize_tr, d_tumour_size)
dev_an_df %>% tabyl(d_tmsize_tr, d_inv_status_lab)
dev_an_df %>% tabyl(d_tmsize_tr)
dev_an_df %>% tabyl(d_tmsize_tr_lab)


### ER status ---------------------------------------------------------
# categories: negative, positive, n/k 
dev_an_df %>% tabyl(er_Status)

dev_an_df <- dev_an_df %>% 
  mutate(d_er_status = as.factor(case_when(d_inv_status == 0 ~ NA,
                                           er_Status == "Negative" ~ 0,
                                           er_Status == "Positive" ~ 1,
                                           is.na(er_Status) ~ NA
                                           )
                                 ),
         d_er_status = fct_relevel(d_er_status, "1", "0"
                              ),
         d_er_status_lab = factor(x = d_er_status,
                                  levels = c(1, 0),
                                  labels = c("Positive", "Negative"))
         )

dev_an_df %>% tabyl(d_er_status)
dev_an_df %>% tabyl(d_er_status, d_er_status_lab)
dev_an_df %>% tabyl(er_Status, d_er_status_lab)
dev_an_df %>% tabyl(d_er_status_lab, d_inv_status_lab)

### ER status - trick --------------------------------------------------------
# recode all dcis to be positive

# Louise has coded all missin dcis as positive as well - is that right? 
# also all missing dicis to positive - need to check that 

dev_an_df <- dev_an_df %>% 
  mutate(d_er_tr = case_when(as.character(d_inv_status) == "0" ~ "1", 
                             TRUE ~ as.character(d_er_status)
  #),
 # d_er_tr = fct_relevel(d_er_tr, "1", "0"
  ),
  d_er_tr_lab = factor(x = d_er_tr,
                           levels = c(1, 0),
                           labels = c("Positive", "Negative"))
  )

dev_an_df %>% tabyl(d_er_tr)
dev_an_df %>% tabyl(d_er_tr, d_er_tr_lab)
dev_an_df %>% tabyl(d_er_tr_lab, d_er_status_lab)
dev_an_df %>% tabyl(d_er_tr_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_er_tr_lab)

### PR status --------------------------------------------------------
# categories: negative, positive, n/k 
dev_an_df %>% tabyl(pr_Status)

dev_an_df <- dev_an_df %>% 
  mutate(d_pr_status = as.factor(case_when(d_inv_status == 0 ~ NA,
                                           pr_Status == "Negative" ~ 0,
                                           pr_Status == "Positive" ~ 1,
                                           is.na(pr_Status) ~ NA
  )
  ),
 d_pr_status = fct_relevel(d_pr_status, "1", "0"
  ),
  d_pr_status_lab = factor(x = d_pr_status,
                           levels = c(1, 0),
                           labels = c("Positive", "Negative"))
  )

dev_an_df %>% tabyl(d_pr_status)
dev_an_df %>% tabyl(d_pr_status, d_pr_status_lab)
dev_an_df %>% tabyl(pr_Status, d_pr_status_lab)
dev_an_df %>% tabyl(d_pr_status_lab, d_inv_status_lab)

### PR status - trick --------------------------------------------------------
# recode all dcis to be positive

# Louise has coded all missin dcis as positive as well - is that right? 

dev_an_df <- dev_an_df %>% 
  mutate(d_pr_tr = case_when(as.character(d_inv_status) == "0" ~ "1", 
                             TRUE ~ as.character(d_pr_status)
  #),
  #d_pr_tr = fct_relevel(d_pr_tr, "1", "0"
  ),
  d_pr_tr_lab = factor(x = d_pr_tr,
                       levels = c(1, 0),
                       labels = c("Positive", "Negative"))
  )

dev_an_df %>% tabyl(d_pr_tr)
dev_an_df %>% tabyl(d_pr_tr, d_pr_tr_lab)
dev_an_df %>% tabyl(d_pr_tr_lab, d_pr_status_lab)
dev_an_df %>% tabyl(d_pr_tr_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_pr_tr_lab)



### HER2 status ----------------------------------------------------------
# categories: negative, positive, n/k 

dev_an_df %>% tabyl(her2_Status)

dev_an_df <- dev_an_df %>% 
  mutate(d_her2_status = as.factor(case_when(d_inv_status == 0 ~ NA,
                                             her2_Status == "Negative" ~ 0,
                                           her2_Status == "Positive"  ~ 1,
                                           is.na(her2_Status) | her2_Status == "Borderline" ~ NA
  )
  ),
  d_her2_status = fct_relevel(d_her2_status, "1", "0"
   ),
  d_her2_status_lab = factor(x = d_her2_status,
                           levels = c(1, 0),
                           labels = c("Positive", "Negative"))
  )

dev_an_df %>% tabyl(d_her2_status)
dev_an_df %>% tabyl(d_her2_status, d_her2_status_lab)
dev_an_df %>% tabyl(her2_Status, d_her2_status_lab)
dev_an_df %>% tabyl(d_her2_status_lab, d_inv_status_lab)


### HER2 status - trick --------------------------------------------------
# recode all dcis to be negative if not missing

# Louise has coded all missin dcis as positive as well - is that right? 

dev_an_df <- dev_an_df %>% 
  mutate(d_her2_tr = case_when(as.character(d_inv_status) == "0" ~ "1", 
                             TRUE ~ as.character(d_her2_status)
  #),
  # d_her2_tr = fct_relevel(d_her2_tr, "1", "0"
  ),
  d_her2_tr_lab = factor(x = d_her2_tr,
                       levels = c(1, 0),
                       labels = c("Positive", "Negative"))
  )

dev_an_df %>% tabyl(d_her2_tr)
dev_an_df %>% tabyl(d_her2_tr, d_her2_tr_lab)
dev_an_df %>% tabyl(d_her2_tr_lab, d_her2_status_lab)
dev_an_df %>% tabyl(d_her2_tr_lab)
dev_an_df %>% tabyl(d_her2_status_lab, d_inv_status_lab)
dev_an_df %>% tabyl(d_her2_tr_lab, d_inv_status_lab)

  
## RISK FACTORS -----------------------------------------------------

### Alcohol (units per week) -----------------------------------
# categories(0, 1-9, 10-19, 20-29, >=30)

# process alcohol status: 
str(dev_an_df$R1alcoholstatus)
dev_an_df %>% tabyl(R1alcoholstatus)

dev_an_df <- dev_an_df %>% 
  mutate(d_R1alcohol_status = ifelse(R1alcoholstatus %in% c(9, NA), 888, R1alcoholstatus
                                   ),
         d_R1alcohol_status_cat = as.factor(d_R1alcohol_status),
         d_R1alcohol_status_cat= fct_relevel(d_R1alcohol_status_cat, "0", "1", "2"
                                   ),
         d_R1alcohol_status_lab = factor(x = d_R1alcohol_status,
                                       levels = 0:2,
                                       labels = c("Never", "Current", "Past"))
         )

dev_an_df %>% tabyl(d_R1alcohol_status)
dev_an_df %>% tabyl(d_R1alcohol_status_lab)
dev_an_df %>% tabyl(d_R1alcohol_status, d_R1alcohol_status_lab)

# process alcohol units per week (r1): 
str(dev_an_df$R1alcoholunits)
dev_an_df %>% tabyl(R1alcoholunits)
summary(dev_an_df$R1alcoholunits)

dev_an_df <- dev_an_df %>% 
  mutate(d_R1alcohol_units = ifelse(is.na(R1alcoholunits), 8888, R1alcoholunits # using 8888 as 888 value could be plausible
  ),
  d_R1alcohol_units_cat = as.factor(case_when(d_R1alcohol_units == 0 ~ 0, # 0 units
                                    d_R1alcohol_units > 0 & d_R1alcohol_units < 10 ~ 1, # 1 to 9 units
                                    d_R1alcohol_units >= 10 & d_R1alcohol_units < 20 ~ 2, # 10 to 19 units  
                                    d_R1alcohol_units >= 20 & d_R1alcohol_units < 30 ~ 3, # 20 to 29 units
                                    d_R1alcohol_units >= 30 & d_R1alcohol_units <  8888 ~ 4
                                    )),
  d_R1alcohol_units_cat = fct_relevel(d_R1alcohol_units_cat, "0", "1", "2", "3", "4"),
  d_R1alcohol_units_lab = factor(x = d_R1alcohol_units_cat,
                                 levels = 0:4,
                                 labels = c("0", "1-9", "10-19", "20-29", "30+"))
                                    
                            
  )

dev_an_df %>% tabyl(d_R1alcohol_units)
dev_an_df %>% tabyl(d_R1alcohol_units_cat)
check <- dev_an_df %>% tabyl(d_R1alcohol_units, d_R1alcohol_units_cat)
dev_an_df %>% tabyl(d_R1alcohol_units_lab)
check <- dev_an_df %>% tabyl(d_R1alcohol_units, d_R1alcohol_units_lab)
dev_an_df %>% tabyl(d_R1alcohol_units_cat, d_R1alcohol_units_lab)

# tab units and status to check consistency 
dev_an_df %>% tabyl(d_R1alcohol_units_lab, d_R1alcohol_status_lab)

### Benign breast disease ----------------------------------------
# binary y/n

dev_an_df %>% tabyl(brbendis)
str(dev_an_df$brbendis)

dev_an_df <- dev_an_df %>% 
  mutate(d_bbd = as.factor(case_when(brbendis == 1 ~ 1,
                           brbendis == 2 ~ 0,
                           TRUE ~ NA # no missing
                           )),
         d_bbd = fct_relevel(d_bbd, "0", "1"),
         d_bbd_lab = factor(x = d_bbd, 
                            levels = c(0, 1),
                            labels = c("No", "Yes"))
         )

dev_an_df %>% tabyl(d_bbd)
dev_an_df %>% tabyl(d_bbd_lab)
dev_an_df %>% tabyl(d_bbd, d_bbd_lab)


### BMI at baseline ----------------------------------------------------------
# WHO categories 

# numerical prep: 
dev_an_df %>% tabyl(bodysizebminow)
str(dev_an_df$bodysizebminow)

dev_an_df <- dev_an_df %>% 
  mutate(d_bmi_entry = case_when(bodysizebminow == 999 ~ 888, # recode 999 to 888 for consistency with other variables
                                 is.na(bodysizebminow) ~ 888, # recode missing NAs to 888 (no NAs)
                                 bodysizebminow < 13.5 | bodysizebminow > 60 ~ 989, # extreme value
                                 TRUE ~ bodysizebminow)
         )

dev_an_df %>% tabyl(d_bmi_entry)

# categorical prep
dev_an_df <- dev_an_df %>% 
  mutate(d_bmi_entry_cat = as.factor(case_when(d_bmi_entry < 18.5 ~ 1, # < 18.5
                                     d_bmi_entry >= 18.5 & d_bmi_entry < 25 ~ 2, # 18.5 to 25
                                     d_bmi_entry >= 25 & d_bmi_entry < 30 ~ 3, # 25 to 30
                                     d_bmi_entry >= 30 & d_bmi_entry < 888 ~ 4, # >= 30
                                     d_bmi_entry %in% c(888, 989) ~ NA, # not known
                                     TRUE ~ NA
                                     )),
         d_bmi_entry_cat = fct_relevel(d_bmi_entry_cat, "1", "2", "3", "4"),
         d_bmi_entry_lab = factor(x = d_bmi_entry_cat,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("<18.5", "18.5-25", "25-30", "30+"))
         )

dev_an_df %>% tabyl(d_bmi_entry_cat)
check <- dev_an_df %>% tabyl(d_bmi_entry, d_bmi_entry_cat)
dev_an_df %>% tabyl(d_bmi_entry_cat, d_bmi_entry_lab)

### Number of relatives with BC -----------------------------------
# categories: 0, 1, >2

# prepare categorical: 
dev_an_df %>% tabyl(fambrca)

dev_an_df <- dev_an_df %>% 
  mutate(d_fambrca = as.factor(case_when(fambrca == 0 ~ 0,
                               fambrca == 1 ~ 1,
                               TRUE ~ NA)), # no missing
         d_fambrca = fct_relevel(d_fambrca, "0", "1"),
         d_fambrca_lab = factor(x = d_fambrca,
                                levels = c(0, 1),
                                labels = c("No", "Yes"))
         )

dev_an_df %>% tabyl(d_fambrca)
dev_an_df %>% tabyl(d_fambrca_lab)
dev_an_df %>% tabyl(d_fambrca, d_fambrca_lab)

# prepare count of relatives: 
dev_an_df %>% tabyl(fambrcaN)


dev_an_df <- dev_an_df %>% 
  mutate(d_fambrcaN_cat = as.factor(case_when(fambrcaN == 0 ~ 0,
                               fambrcaN == 1 ~ 1,
                               fambrcaN >= 2 ~ 2,
                               TRUE ~ NA)), # no missing
         d_fambrcaN_cat = fct_relevel(d_fambrcaN_cat, "0", "1", "2"),
         d_fambrcaN_lab = factor(x = d_fambrcaN_cat,
                                levels = c(0, 1, 2),
                                labels = c("0", "1", "2+")
                                )
  )

dev_an_df %>% tabyl(d_fambrcaN_cat)
dev_an_df %>% tabyl(d_fambrcaN_lab)
dev_an_df %>% tabyl(d_fambrcaN_cat, d_fambrcaN_lab)


# cross check: 
dev_an_df %>% tabyl(fambrcaN, fambrca) %>% 
  adorn_totals()

dev_an_df %>% tabyl(d_fambrcaN_cat, d_fambrca) %>% 
  adorn_totals()



### Age at menarche -----------------------------------------------
# categories: <13, >=13 

# 17/06/2024: changed categories from <13 to >=13 to <12, 12-13 and 14+ as agreed with Montse and Amy

dev_an_df %>% tabyl(menarcheage)
str(dev_an_df$menarcheage)

dev_an_df <- dev_an_df %>% 
  mutate(d_age_menarche = ifelse(menarcheage %in% c(999, 888, NA), NA, menarcheage),
         d_age_menarche_cat = as.factor(case_when(d_age_menarche < 12 ~ 1, # less than 12 
                                        d_age_menarche >= 12 & d_age_menarche < 14 ~ 2, # 12 to 13 and over
                                        d_age_menarche >= 14 & d_age_menarche < 888 ~ 3, # 14 and over
                                        is.na(d_age_menarche) ~ NA # not known
                                        )),
         d_age_menarche_cat = fct_relevel(d_age_menarche_cat, "1", "2", "3"),
         d_age_menarche_lab = factor(x = d_age_menarche_cat,
                                     levels = c(1, 2, 3),
                                     labels = c("<12", "12-13", "14+"))
    
  )

dev_an_df %>% tabyl(d_age_menarche)
dev_an_df %>% tabyl(d_age_menarche_cat)
dev_an_df %>% tabyl(d_age_menarche_lab)
dev_an_df %>% tabyl(menarcheage, d_age_menarche_lab)
dev_an_df %>% tabyl(d_age_menarche_lab, d_age_menarche_cat)

### Menopausal status at baseline -----------------------------------
# binary: pre/post 

dev_an_df %>% tabyl(R1menopause)
str(dev_an_df$R1menopause)

dev_an_df <- dev_an_df %>% 
  mutate(d_R1menopause = case_when(is.na(R1menopause) | R1menopause %in% c(8, 10) ~ NA, # not know or questionnaire not completed
                                   TRUE ~ R1menopause),
         d_R1menopause_lab6 = factor(x = d_R1menopause,
                                    levels = c(1, 2, 3, 4, 9),
                                    labels = c("Postmenopausal", "Premenopausal", 
                                               "Assumed postmenopausal", "Assumed premenopausal", 
                                               "Never had periods")
                                    ), # labels from rds DD 
         
         # condensed categories to pre and post meno 
         d_R1menopause_cat3 = as.factor(case_when(d_R1menopause %in% c(1, 3) ~ 1,
                                        d_R1menopause %in% c(2, 4) ~ 0
                                        #d_R1menopause %in% c(9, 888) ~ 888 # no cases here
                                        )),
         d_R1menopause_cat3 = fct_relevel(d_R1menopause_cat3, "0", "1"),
         d_R1menopause_lab3 = factor(x = d_R1menopause_cat3,
                                     levels = c(0, 1),
                                     labels = c("Premenopausal", "Postmenopausal")
                                     )
         )


dev_an_df %>% tabyl(d_R1menopause)
dev_an_df %>% tabyl(d_R1menopause_lab6)
dev_an_df %>% tabyl(d_R1menopause_lab3)
dev_an_df %>% tabyl(d_R1menopause_lab6, d_R1menopause_lab3)
dev_an_df %>% tabyl(d_R1menopause_lab6, d_R1menopause_cat3)

dev_an_df %>% tabyl(AgeatEntry, d_R1menopause_lab3)


### Age at menopause -----------------------------------------------
# categories: <=50, 51-53, >53

#11/07/2024
# new categories <50, 51-54, 55+

dev_an_df %>% tabyl(meno_age_est)
str(dev_an_df$meno_age_est)

# from MSc data functions: 

# x_age_menopause
# will crosscheck against age at last birth variable - has to be older
# could potentially recalculate meno_age_est using x_age_birth_last as a guide
# potential issues with assuming 888 for last birth makes age_meno_est valid - could have a later age for birth but due to parity discrepancies not recorded it. Currently treating this as a downside of the method, could remove.
# dev_an_df <- dev_an_df %>% 
#   mutate(d_age_menopause = case_when(d_R1menopause %in% c(1, 3) & x_age_birth_last < 700 & x_age_birth_last < meno_age_est & !is.na(meno_age_est) & meno_age_est != 999 ~ meno_age_est, # age last birth known and before meno_age_est
#                                      d_R1menopause %in% c(1, 3) & x_age_birth_last %in% c(777,888) & !is.na(meno_age_est) & meno_age_est != 999 ~ meno_age_est, # age last birth not known/NA so assume age is valid
#                                      d_R1menopause %in% c(1, 3) ~ 999, # Postmeno, but due to conditions above, not able to calculate the age
#                                      d_R1menopause %in% c(9, 888) ~ 888, # Menopausal status not known - could use a different error code? Different levels of unknown for this variable 
#                                      TRUE ~ 777), # Pre menopausal 
#          d_age_menopause_cat = as.factor(case_when(d_age_menopause <= 50 ~ 1,
#                                          d_age_menopause > 50 & d_age_menopause <= 53 ~ 2,
#                                          d_age_menopause > 53 & d_age_menopause < 700 ~ 3,
#                                          d_age_menopause == 777 ~ 777,
#                                          d_age_menopause %in% c(888, 999) ~ NA,
#                                          TRUE ~ NA)),
#          d_age_menopause_cat = fct_relevel(d_age_menopause_cat, "1", "2", "3", "777"),
#          d_age_menopause_lab = factor(x = d_age_menopause_cat,
#                                       levels = c(1, 2, 3, 777),
#                                       labels = c("<50", "51-53", "53+", "Pre-menopausal"))
#   )


# new coding 
dev_an_df <- dev_an_df %>% 
  mutate(d_age_menopause = case_when(d_R1menopause %in% c(1, 3) & x_age_birth_last < 700 & x_age_birth_last < meno_age_est & !is.na(meno_age_est) & meno_age_est != 999 ~ meno_age_est, # age last birth known and before meno_age_est
                                     d_R1menopause %in% c(1, 3) & x_age_birth_last %in% c(777,888) & !is.na(meno_age_est) & meno_age_est != 999 ~ meno_age_est, # age last birth not known/NA so assume age is valid
                                     d_R1menopause %in% c(1, 3) ~ 999, # Postmeno, but due to conditions above, not able to calculate the age
                                     d_R1menopause %in% c(9, 888) ~ 888, # Menopausal status not known - could use a different error code? Different levels of unknown for this variable 
                                     TRUE ~ 777), # Pre menopausal 
         d_age_menopause_cat = as.factor(case_when(d_age_menopause < 50 ~ 1,
                                                   d_age_menopause >= 50 & d_age_menopause < 55 ~ 2,
                                                   d_age_menopause >= 55 & d_age_menopause < 700 ~ 3,
                                                   d_age_menopause == 777 ~ 777,
                                                   d_age_menopause %in% c(888, 999) ~ NA,
                                                   TRUE ~ NA)),
         d_age_menopause_cat = fct_relevel(d_age_menopause_cat, "1", "2", "3", "777"),
         d_age_menopause_lab = factor(x = d_age_menopause_cat,
                                      levels = c(1, 2, 3, 777),
                                      labels = c("<50", "50-54", "55+", "Pre-menopausal"))
  )



dev_an_df %>% tabyl(d_age_menopause)
dev_an_df %>% tabyl(d_age_menopause, d_R1menopause_lab6)
dev_an_df %>% tabyl(d_age_menopause_lab)
dev_an_df %>% tabyl(d_age_menopause_cat)
dev_an_df %>% tabyl(d_age_menopause_lab, d_age_menopause_cat)


### age at menopause - trick --------------------------------------------------
# pre menopausal to go into < 50 group 

dev_an_df <- dev_an_df %>% 
  mutate(d_age_meno_tr = case_when(as.character(d_age_menopause_cat) == 777 ~ "1",
                                   TRUE ~ as.character(d_age_menopause_cat)),
         d_age_meno_tr_lab = factor(x = d_age_meno_tr,
                                 levels = c(1, 2, 3),
                                 labels = c("<50", "50-54", "55+")))

dev_an_df %>% tabyl(d_age_meno_tr)
dev_an_df %>% tabyl(d_age_meno_tr, d_R1menopause_lab3)
dev_an_df %>% tabyl(d_age_meno_tr_lab)


### HRT status -------------------------------------------------------
# categories: never, former, current 
dev_an_df %>% tabyl(hrtstatus)
str(dev_an_df$hrtstatus)

dev_an_df <- dev_an_df %>% 
  mutate(d_R1hrtstatus = case_when(hrtstatus %in% c(888,999,9999) | is.na(hrtstatus) ~ NA, # Merging error values into one - potential loss of information but also easier to track
                                   d_R1menopause_cat3 == 0 ~ 777, # pre menopausal
                                   TRUE ~ hrtstatus),
         d_R1hrtstatus_cat = as.factor(d_R1hrtstatus),
         d_R1hrtstatus_cat = fct_relevel(d_R1hrtstatus_cat, "0", "1", "2", "777"),
         d_R1hrtstatus_lab = factor(x = d_R1hrtstatus,
                                    levels = c(0, 1, 2, 777),
                                    labels = c("Never", "Former", "Current", "Pre-menopausal"))
  )

dev_an_df %>% tabyl(d_R1hrtstatus)
dev_an_df %>% tabyl(d_R1hrtstatus_cat, d_R1hrtstatus)
dev_an_df %>% tabyl(d_R1hrtstatus, d_R1hrtstatus_lab)
dev_an_df %>% tabyl(AgeatEntry, d_R1hrtstatus)
# HRT should only be in menopausal women

dev_an_df %>% tabyl(d_R1menopause_lab3, d_R1hrtstatus_lab)

### HRT status - trick ------------------------------------------------------------
# recode pre-menopausal as never 

dev_an_df <- dev_an_df %>% 
  mutate(d_R1hrt_tr = case_when(d_R1menopause_cat3 == 0 & !is.na(d_R1hrtstatus)  ~ 0,
                                   TRUE ~ d_R1hrtstatus),
         d_R1hrt_tr_lab = factor(x = d_R1hrt_tr,
                                    levels = c(0, 1, 2),
                                    labels = c("Never", "Former", "Current")))

dev_an_df %>% tabyl(d_R1hrt_tr)
dev_an_df %>% tabyl(d_R1hrt_tr_lab, d_R1menopause_lab3)
dev_an_df %>% tabyl(d_R1hrt_tr_lab)




### OC status --------------------------------------------------------
# categories: never, former, current 
dev_an_df %>% tabyl(ocstatus)
str(dev_an_df$ocstatus)

dev_an_df <- dev_an_df %>% 
  mutate(d_ocstatus = ifelse(ocstatus %in% c(9999, 999, 888, NA), NA, ocstatus),
         d_ocstatus_cat = as.factor(d_ocstatus),
         d_ocstatus_cat = fct_relevel(d_ocstatus_cat, "0", "1", "2"),
         d_ocstatus_lab = factor(x = d_ocstatus,
                                 levels = c(0, 1, 2),
                                 labels = c("Never", "Former", "Current")
                                 )
         )

dev_an_df %>% tabyl(d_ocstatus)
dev_an_df %>% tabyl(d_ocstatus_lab)
dev_an_df %>% tabyl(d_ocstatus_lab, d_ocstatus)
dev_an_df %>% tabyl(d_ocstatus_lab, ocstatus)
dev_an_df %>% tabyl(d_ocstatus_lab, d_ocstatus_cat)



### Physical activity total --------------------------------------------------

#NOTE: 20/05/2024 agreed to use leisure activity

# # categories: METs per week <= 25, 25.1 to 50, 50.1 to 75, > 75 - this might need revising and check recommendations 
# 
# dev_an_df %>% tabyl(PhysMetTotal)
# summary(dev_an_df$PhysMetTotal)
# str(dev_an_df$PhysMetTotal)
# 
# n_miss(dev_an_df$PhysMetTotal)
# 
# dev_an_df %>% 
#   filter(PhysMetTotal < 900) %>% 
#   ggplot(aes(PhysMetTotal)) +
#   geom_histogram()
# 
# # process qunatitative
# dev_an_df <- dev_an_df %>% 
#   mutate(d_R1physmet_total = as.numeric(if_else(PhysMetTotal %in% c(8888, 9999, NA), 8888, PhysMetTotal))
#          ) %>% 
#   ungroup()
# 
# dev_an_df %>% 
#   filter(PhysMetTotal < 900) %>% 
#   mean_table(d_R1physmet_total)
# 
# check <- dev_an_df %>% 
#   filter(PhysMetTotal < 900) %>% 
#   tabyl(d_R1physmet_total)
# 
# 
# # quartiles
# # create quintiles
# dev_an_df <- dev_an_df %>% 
#   mutate(d_R1physmet_quart = case_when(d_R1physmet_total == 8888 ~ NA,
#                                       d_R1physmet_total != 8888 ~ ntile(d_R1physmet_total, 4),
#                                       TRUE ~ NA )
#   ) %>% 
#   group_by(d_R1physmet_quart
#   ) %>% 
#   mutate(d_R1physmet_quart_m = case_when(d_R1physmet_total == 8888 ~ NA,
#                                         d_R1physmet_total != 8888 ~ round(median(d_R1physmet_total), 2),
#                                         TRUE ~ NA)
#   ) %>% 
#   ungroup() %>% 
#   mutate(d_R1physmet_quart = factor(
#     x = d_R1physmet_quart,
#     levels = 1:4
#   ))
# 
# dev_an_df %>% tabyl(d_R1physmet_quart, d_R1physmet_quart_m)
# 
# check <- dev_an_df %>% tabyl(d_R1physmet_total, d_R1physmet_quart)
# 
# 
# # meeting guidelines 
# METhw <- 9
# 
# dev_an_df <- dev_an_df %>% 
#   mutate(d_R1physmet_who = case_when(d_R1physmet_total < METhw ~ 0,
#                                     d_R1physmet_total >= METhw & d_R1physmet_total < 8888 ~ 1,
#                                     d_R1physmet_total == 8888 ~ 888),
#          d_R1physmet_who_lab = factor(x = d_R1physmet_who,
#                                       levels = c(0, 1, 888),
#                                       labels = c("No", "Yes", "Not known"))
#         )
# 
# dev_an_df %>% tabyl(d_R1physmet_who_lab)
# 
# # only 64 don't meet guidelines so probably not very useful 
# 
# 
# # Louise's original categories 
# 
# dev_an_df <- dev_an_df %>% 
#   mutate(d_R1physmet_cat = case_when(d_R1physmet_total <= 25.0 ~ 1,
#                                      d_R1physmet_total > 25.0 & d_R1physmet_total <= 50.0 ~ 2,
#                                      d_R1physmet_total > 50.0 & d_R1physmet_total <= 75.0 ~ 3,
#                                      d_R1physmet_total > 75.0 & d_R1physmet_total < 8888 ~ 4,
#                                      d_R1physmet_total == 8888 ~ 888
#                                      ), 
#           d_R1physmet_lab = factor(x = d_R1physmet_cat,
#                                    levels = c(1, 2, 3, 4, 888),
#                                    labels = c("<=25.0", "25.1 to 50.0", "50.1 to 75.0", ">75.1", "Not known")
#                                    ) 
#          )
# 
# dev_an_df %>% tabyl(d_R1physmet_cat, d_R1physmet_lab)
# check <- dev_an_df %>% tabyl(d_R1physmet_total, d_R1physmet_lab)

### Physical activity Leisure --------------------------------------------------

dev_an_df %>% tabyl(PhysMetRecTot)
summary(dev_an_df$PhysMetRecTot)
str(dev_an_df$PhysMetRecTot)

n_miss(dev_an_df$PhysMetRecTot)

dev_an_df %>% 
  filter(PhysMetRecTot < 900) %>% 
  ggplot(aes(PhysMetRecTot)) +
  geom_histogram()

# process qunatitative
dev_an_df <- dev_an_df %>% 
  mutate(d_R1physmet_leisure = as.numeric(if_else(PhysMetRecTot %in% c(8888, 9999, NA), NA, PhysMetRecTot))
  ) %>% 
  ungroup()

dev_an_df %>% 
  #filter(PhysMetRecTot < 900) %>% 
  mean_table(d_R1physmet_leisure)

check <- dev_an_df %>% 
  #filter(PhysMetRecTot < 900) %>% 
  tabyl(d_R1physmet_leisure)


# quartiles
# create quintiles
dev_an_df <- dev_an_df %>% 
  mutate(d_R1physmet_leis_quart =  as.factor(ntile(d_R1physmet_leisure, 4))
  ) %>% 
  group_by(d_R1physmet_leis_quart
  ) %>% 
  mutate(d_R1physmet_leis_quart_m = as.factor(round(median(d_R1physmet_leisure), 2))
  ) %>% 
  ungroup() %>% 
  mutate(d_R1physmet_leis_quart = factor(
    x = d_R1physmet_leis_quart,
    levels = c(1,2,3,4)
  ))

dev_an_df %>% tabyl(d_R1physmet_leis_quart, d_R1physmet_leis_quart_m)

check <- dev_an_df %>% tabyl(d_R1physmet_leisure, d_R1physmet_leis_quart)


# meeting guidelines 
METhw <- 9

dev_an_df <- dev_an_df %>% 
  mutate(d_R1physmet_leis_who = as.factor(case_when(d_R1physmet_leisure < METhw ~ 0,
                                     d_R1physmet_leisure >= METhw & d_R1physmet_leisure < 2*METhw ~ 1,
                                     d_R1physmet_leisure >= 2*METhw ~ 2
                                     )),
         d_R1physmet_leis_who = fct_relevel(d_R1physmet_leis_who, "0", "1", "2"),
         d_R1physmet_leis_who_lab = factor(x = d_R1physmet_leis_who,
                                      levels = c(0, 1, 2),
                                      labels = c("<9", "9-17", "18+"))
  )

dev_an_df %>% tabyl(d_R1physmet_leis_who_lab)

check <- dev_an_df %>% tabyl(d_R1physmet_leisure, d_R1physmet_leis_who_lab)
dev_an_df %>% tabyl(d_R1physmet_leis_who_lab, d_R1physmet_leis_who)




### Parity ---------------------------------------------------------
# binary y/n - ever parous

#processed in script 2 (functions from MSc data processing)
dev_an_df %>% tabyl(x_parous)

dev_an_df %>% tabyl(pregparitycnt)

dev_an_df %>% tabyl(x_parity)

dev_an_df <- dev_an_df %>% 
  mutate(d_parous_cat = as.factor(x_parous),
         d_parous_cat = fct_relevel(d_parous_cat, "0", "1"),
         d_parous_lab = factor(x = x_parous,
                               levels = c(0, 1),
                               labels = c("Not parous", "Parous")))

dev_an_df %>% tabyl(d_parous_lab, x_parous)

dev_an_df %>% tabyl(x_parity, d_parous_lab)




### Number parous pregnancies ----------------------------------------------------
# categories: 1, 2, 3, >=4 

#processed in script 2 (functions from MSc data processing)
dev_an_df %>% tabyl(pregparitycnt)

dev_an_df %>% tabyl(x_parity)



dev_an_df <- dev_an_df %>% 
  mutate(d_parity = x_parity,
         # categorical according to Louise 4 categories but 4th level has low numbers - changed to 3
         d_parity_cat = as.factor(case_when(d_parity == 0 ~ 0,
                                            d_parity == 1 ~ 1,
                                            d_parity == 2 ~ 2,
                                            d_parity >= 3 ~ 3,
         )),
         d_parity_cat = fct_relevel(d_parity_cat, "0", "1", "2", "3"),
         d_parity_lab = factor(x = d_parity_cat,
                               level = c(0, 1, 2, 3),
                               labels = c("0", "1", "2", "3+")
         )
  )

dev_an_df %>% tabyl(d_parity)
dev_an_df %>% tabyl(d_parity_cat)
dev_an_df %>% tabyl(d_parity_lab)

### N parous pregnancies - trick -------------------------------------------------

# 17/06 - not needed as agreed to remove parity status and use parity as the default variable

# dev_an_df <- dev_an_df %>% 
#   mutate(d_parity_tr = as.factor(case_when(as.character(d_parity_cat) == 0 | as.character(d_parity_cat) ==1 ~ "1",
#                                            TRUE ~ as.character(d_parity_cat))
#   ),
#   d_parity_tr = fct_relevel(d_parity_tr, "1", "2", "3"),
#   d_parity_tr_lab = factor(x = d_parity_tr,
#                            levels = c(1, 2, 3),
#                            labels = c("1", "2", ">=3")))
# 
# dev_an_df %>% tabyl(d_parity_tr)
# dev_an_df %>% tabyl(d_parity_tr, d_parity_cat)
# 
# 
# dev_an_df %>% tabyl(d_parity_tr_lab, d_parous_lab)
# dev_an_df %>% tabyl(d_parity_tr_lab)
# 
# dev_an_df %>% tabyl(d_parity_tr_lab, d_parity_lab)



### Age at first birth -------------------------------------------------
# categories: <20, 20-24, 25-29, 30-34, >=35

dev_an_df %>% tabyl(x_age_birth_1) # 777 not parous, 888 not known


dev_an_df <- dev_an_df %>% 
  mutate(d_agebirth1_cat = as.factor(case_when(x_age_birth_1 < 20 ~ 1,
                                               x_age_birth_1 >= 20 & x_age_birth_1 < 25 ~ 2,
                                               x_age_birth_1 >= 25 & x_age_birth_1 < 30 ~ 3,
                                               x_age_birth_1 >= 30 & x_age_birth_1 < 35 ~ 4,
                                               x_age_birth_1 >= 35 & x_age_birth_1 < 777 ~ 5,
                                               x_age_birth_1 == 777 ~ 777, # non parous
                                               x_age_birth_1 == 888 ~ NA,
  )),
  d_agebirth1_cat = fct_relevel(d_agebirth1_cat, "1", "2", "3", "4", "5", "777"),
  d_agebirth1_lab = factor(x = d_agebirth1_cat,
                           levels = c(1, 2, 3, 4, 5, 777),
                           labels = c("<20", "20-24", "25-29", "30-34", "35+", "Non-parous"))
  )

dev_an_df %>% tabyl(d_agebirth1_cat, d_agebirth1_lab)
dev_an_df %>% tabyl(x_age_birth_1, d_agebirth1_cat)





### Age at first birth - trick -------------------------------------------
# recode all non parous to be in the lowest age group 

dev_an_df <- dev_an_df %>% 
  mutate(d_agebirth1_tr = case_when(as.character(d_agebirth1_cat) == 777 ~ "1",
                             TRUE ~ as.character(d_agebirth1_cat)),
         d_agebirth1_tr_lab = factor(x = d_agebirth1_tr,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("<20", "20-24", "25-29", "30-34", "35+")))

dev_an_df %>% tabyl(d_agebirth1_tr)
dev_an_df %>% tabyl(d_agebirth1_tr_lab, d_parous_lab)
dev_an_df %>% tabyl(d_agebirth1_tr_lab)

dev_an_df %>% tabyl(d_agebirth1_tr_lab, d_agebirth1_lab)
dev_an_df %>% tabyl(d_agebirth1_tr_lab, d_parity_lab)


# trick v2 - coding missing 


### Ever breast fed -----------------------------------------------------------------

# Note - derived in script 2 in parity processing functions 
# entry status - see comments in script 2 for details

dev_an_df %>% tabyl(x_breastfed)
str(dev_an_df$x_breastfed)

# rename for consistency with analytical variables:

dev_an_df <- dev_an_df %>% 
  mutate(d_breastfed = x_breastfed)



### Duration of breastfeeding -----------------------------------------------------
# cut off at entry so baseline
# Cumulative duration of breastfeeding weeks for all parous (>=26 weeks) pregnancies (calculated up to entry date)
# Note - derived in script 2 in parity processing functions 

dev_an_df %>% tabyl(x_breastfeeding_duration)
str(dev_an_df$x_breastfeeding_duration)

dev_an_df <- dev_an_df %>% 
  mutate(d_bf_duration = x_breastfeeding_duration)


### SES -------------------------------------------------------------
# categories: Affluent achievers, rising prosperity, comfortable communities, 
#             financially streched, urban adversity and non-private households 

# NOTE: 20/05/2024 - agreed not to use acorn



### Smoking -----------------------------------------------------------
# categories: never, former, current 

dev_an_df %>% tabyl(R1smokingstatus)
str(dev_an_df$R1smokingstatus)

dev_an_df <- dev_an_df %>% 
  mutate(d_R1smokingstatus = ifelse(R1smokingstatus %in% c(6, 9, NA), NA, R1smokingstatus),
         d_R1smokingstatus_cat = as.factor(d_R1smokingstatus),
         d_R1smokingstatus_cat = fct_relevel(d_R1smokingstatus_cat, "0", "1", "2"),
         d_R1smokingstatus_lab = factor(x = d_R1smokingstatus,
                                        levels = c(0, 1, 2),
                                        labels = c("Never", "Former", "Current"))
         )

dev_an_df %>% tabyl(d_R1smokingstatus)
dev_an_df %>% tabyl(d_R1smokingstatus_lab)
dev_an_df %>% tabyl(d_R1smokingstatus_cat)




## MAMMO DENSITY ----------------------------------------------------

summary(dev_an_df$mean_density)
dev_an_df %>% tabyl(MD_avail)
dev_an_df %>% tabyl(mean_density, MD_avail)

## recode MD avail variable

dev_an_df <- dev_an_df %>% 
  mutate(d_md_avail = case_when(MD_avail == "Y" ~ 1,
                              is.na(MD_avail) ~ 0
                              ),
         d_md_avail_lab = factor(x = d_md_avail,
                               levels = 0:1,
                               labels = c("No", "Yes")
                               )
         )

dev_an_df %>% tabyl(MD_avail, d_md_avail_lab)

## recode mean density - if missing then 888 

dev_an_df <- dev_an_df %>% 
  mutate(d_md = case_when(d_md_avail == 1 ~ mean_density,
                        d_md_avail == 0 ~ NA )) # originally 888 - all missing should be those without available density, if NA present then there is an error somewhere

summary(dev_an_df$d_md)

check <- dev_an_df %>% tabyl(d_md, d_md_avail)
str(dev_an_df$d_md)

# scale so 1 unit is 10% - original variable 1% 
dev_an_df <- dev_an_df %>% 
  mutate(d_md10 = d_md/10)

View(dev_an_df[,c("d_md10", "d_md")])


### quartiles --------------------------------------------------------------------

# mean_density variable has missing as NA, md variable has missing as 888 
# there should be the same number of missing as the 888s in MD, if more there is an error 


# not coding missing as 888 here as it wasn't working properly and may introduce error in analysis
dev_an_df <- dev_an_df %>% 
  mutate(
         d_md_qrt = ntile(mean_density, 4)
  ) %>%
  group_by(d_md_qrt
  ) %>%
  mutate(d_md_qrt_m = round(median(mean_density), 2)
  ) %>%
  ungroup(
    
  ) %>% 
  mutate(d_md_qrt = factor(x = d_md_qrt, 
                           levels = 1:4))


dev_an_df %>% tabyl(d_md_qrt)
dev_an_df %>% tabyl(d_md_qrt_m)

check <- dev_an_df %>% tabyl(d_md, d_md_qrt)

# dev_an_df %>% 
#   group_by(d_md_qrt) %>% 
#   mean_table(mean_density)


### high/low density --------------------------------------------------------------

dev_an_df <- dev_an_df %>% 
  mutate(d_md_cat = as.factor(case_when(d_md_qrt == 1 ~ "Low",
                            d_md_qrt == 4 ~ "High",
                            d_md_qrt %in% c(2, 3) ~ "Normal",
                            is.na(d_md_qrt) ~ "Not known")),
         d_md_cat = fct_relevel(d_md_cat, "Low", "Normal", "High", "Not known"))

dev_an_df %>% tabyl(d_md_cat)
dev_an_df %>% tabyl(d_md_cat, d_md_qrt)




## MODE OF DETECTION --------------------------------------------------------------------

dev_an_df %>% tabyl(ancat_dmode_v2)

# rename for shorter name

dev_an_df <- dev_an_df %>% 
  mutate(d_dmode = ancat_dmode_v2,
         # numeric dmode for modelling
         d_dmode_n = case_when(d_dmode == "I" ~ 1,
                              d_dmode == "SD" ~ 0,
                              TRUE ~ NA)) %>% 
  ungroup()





# df summary - quick descriptives for all variables
#stview(dfSummary(dev_an_df))

#dfSummary(dev_an_df)

# 4. Comprise analytical dataset ------------------------------------------------------------
an_df <- dev_an_df %>% 
  select(tcode, date_birth, date_entry, diagdate, yeardiag, diagage, AgeatEntry, incident, side, 
         source_dm2, dm2_screen_date_f, dens_dm2_screen_date_f, SD_dg_first_screen, reg_sd,
         MammoDat_f, ImageType, mean_density, sd_density, 
         starts_with("d_")) 
  

#stview(dfSummary(an_df))

#dfSummary(an_df)

#skim(an_df)

#stview(skim(an_df))


# save R data - html files will be rendered quicker
saveRDS(an_df, file = "Q:/SHARED/USERS/MBrayley/Screening/data/an_df.rds")














