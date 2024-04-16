# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 6. Select mammo density data  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  
#                 
#_____________________________________________________________________________


# 1. Create a dataset for selecting mammodensity reading --------------------------

## select variables from detection mode -----------------------

dm_vars <- dm_df %>% 
  select(tcode, ancat_dmode_v2, source_dm2, dm2_screen_date_f, dm1_screen_date_f)

## select variables from cancer df ---------------------------

ca_vars <- cancer_df %>% 
  select(tcode, diagdate)

## join with mammo dataset 

relevant_mammo_df <- mammodensity_df %>% 
  left_join(dm_vars, by = "tcode") %>% 
  left_join(ca_vars, by = "tcode") %>% 
  mutate(ancat_dmode_v2 = as.factor(ancat_dmode_v2))

str(relevant_mammo_df)


# 2. Identify relevant mammo date ----------------------------------------------------

# create a variable with a difference between dm2_screen_date and mammoDat and diagnosis 


# IMPORTANT - there are some missing dates for dmode2 where source is dmode1 - need to go back to SH to investigate

relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(scr_date_diff = as.numeric(MammoDat_f - dm2_screen_date_f),
         diag_date_diff = as.numeric(MammoDat_f - diagdate)
         ) %>% 
  ungroup()


# remove all positive difference as this means that mammo density was taken after diagnosis

relevant_mammo_df <- relevant_mammo_df %>% 
  filter(diag_date_diff <= 0)

# take the absolute value of the difference between mammo and diagdate 
relevant_mammo_df <- relevant_mammo_df %>% 
  mutate(diag_date_diff = abs(diag_date_diff))

relevant_mammo_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(diag_date_diff)


# create order of mammo dates based on smallest difference between diagdate and mammo date - rank

relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(mammo_dg_diff_rank = dense_rank(diag_date_diff)) %>% 
  ungroup()


#  IN PROGRESS - create a variable for selecting the appropriate rows based on dates -------------
relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(selected = case_when(ancat_dmode_v2 == "I" & mammo_rank == 1 ~ "Y",
                              TRUE ~ NA)
         )

# NOTES: create flags for interval selected rows (check restricitons on difference number),
# then create flag for SD selected rows (also bases on intervals between dates - need to do exploratory analysis on this)