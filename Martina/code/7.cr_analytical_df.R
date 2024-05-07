# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 6. Compile analytical dataset  _________________________________


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
  select(tcode, date_birth, date_entry, diagdate, diagage, side)

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

## TIMING VARIABLES ------------------------------------------------

### Time between entry and BC diagnosis ------------------------

dev_an_df <- dev_an_df %>% 
  mutate(
    R1toBC = as.numeric(diagdate - date_entry),
         R1toBC_y = round(R1toBC/365.25, 1))

# add categorical
# checks:
View(dev_an_df[,c("tcode", "diagdate", "date_entry", "R1toBC", "R1toBC_y")])

str(dev_an_df)
summary(dev_an_df$R1toBC_y)

dev_an_df %>% tabyl(R1toBC_y)

### Time between entry and mammo density 

dev_an_df <- dev_an_df %>% 
  mutate(
    R1toBC = as.numeric(diagdate - date_entry),
    R1toBC_y = round(R1toBC/365.25, 1))

# add categorical

View(dev_an_df[,c("tcode", "diagdate", "date_entry", "R1toBC", "R1toBC_y")])

str(dev_an_df)



## TUMOUR CHARACTERISTICS -------------------------------------------


## RISK FACTORS -----------------------------------------------------


## MAMMO DENSITY ----------------------------------------------------
