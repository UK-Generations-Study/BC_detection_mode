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


relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(scr_date_diff = as.numeric(MammoDat_f - dm2_screen_date_f),
         diag_date_diff = as.numeric(MammoDat_f - diagdate)
         ) %>% 
  ungroup()


# remove all positive differences as this means that mammo density was taken after diagnosis

# relevant_mammo_df <- relevant_mammo_df %>% 
#   filter(diag_date_diff <= 0)

# take the absolute value of the difference between mammo and diagdate 
relevant_mammo_df <- relevant_mammo_df %>% 
  mutate(abs_diag_date_diff = case_when(diag_date_diff <= 0 ~ abs(diag_date_diff),
                                        scr_date_diff > 0 ~ NA),
         abs_scr_date_diff = case_when(scr_date_diff < 10 ~ abs(scr_date_diff),
                                       scr_date_diff >= 10 ~ NA)
         )
# note: for abs_scr_date_diff - using 10 days because the mammo images could be uploaded a bit later 
# than the screen date, as long as it is very close to screen date (can be changed to a different cut off)


relevant_mammo_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(abs_diag_date_diff)

relevant_mammo_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(abs_scr_date_diff)

# create order of mammo dates based on smallest difference between diagdate and mammo date - rank

relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(mammo_dg_diff_rank = dense_rank(abs_diag_date_diff),
         scr_mammo_diff_rank = dense_rank(abs_scr_date_diff)) %>% 
  ungroup()


#  IN PROGRESS - create a variable for selecting the appropriate rows based on dates -------------
# relevant_mammo_df <- relevant_mammo_df %>% 
#   group_by(tcode) %>% 
#   arrange(tcode, MammoDat_f) %>% 
#   mutate(I_mammo_date = case_when(ancat_dmode_v2 == "I" & mammo_dg_diff_rank == 1 ~ "Y",
#                               TRUE ~ NA)
#          )

# NOTES: create flags for interval selected rows (check restricitons on difference number),
# then create flag for SD selected rows (also bases on intervals between dates - need to do exploratory analysis on this)

# Interval flag variables ------------------------------------
# Note - by the decision tree conditions 

one_year <- 365.25
one_month <- one_year/12

# DO CHECKS BEFORE RESUMING ON THE NUMBER OF CASES FOR MAMMO DENSITY SELECTION 

## 1. Mammo date between at least 3 months before diagnosis? ------------------------
relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(
    # variables needed for decision tree flow 
    mammo_3m_before_dg = if_else(mammo_dg_diff_rank == 1 & abs_diag_date_diff > (3*one_month), "Y", "N"),
         
         screen_closeto_mammo = if_else(scr_date_diff > -30 & scr_date_diff < 10, "Y", "N"),
          
        screen_dir_bef_mammo = if_else(scr_mammo_diff_rank == 1, "Y", "N"),
         
         mlo_available = if_else(any(View %in% c("LMLO", "RMLO")), "Y", "N"),
         
         reader_SB = if_else(Reader_Internal == "SB", "Y", "N"),
    
    mammo_before_dg = if_else(diag_date_diff > 0, "Y", "Y")
         ) %>% 
  ungroup()


# add more variables needed for SD once the relevant SD date is derived

# Interval density selection -------------------------------------------------
relevant_mammo_df <- relevant_mammo_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(
    I_density_flag = case_when(
    # Decision tree path 1 - rows with mammo close to screen date, MLO view Y, images by SB Y
                                      ancat_dmode_v2 == "I" &
                                      mammo_3m_before_dg == "Y" &
                                      screen_closeto_mammo == "Y" &
                                      mlo_available == "Y" & 
                                      reader_SB == "Y" ~ "1",
                                      
    # Decision tree path 2 - as above but images by SB not available
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_closeto_mammo == "Y" &
      mlo_available == "Y" & 
      reader_SB == "N" ~ "2",
    
     # Decision tree path 3 - rows with mammo close to screen date, MLO view N, images by SB Y
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_closeto_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "Y" ~ "3",
    
    # Decision tree path 4 - same as path 3 but images by SB not available 
    is.na(I_density_flag) & 
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_closeto_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "N" ~ "4",
    
    # Decision tree path 5 - rows with mammo not close to screen date, MLO view Y, images by SB Y
    is.na(I_density_flag) & 
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "Y" & 
      reader_SB == "Y" ~ "5",
    
    
    
    # Decision tree path 6 - same as path 5 but images by SB N
    is.na(I_density_flag) & 
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "Y" & 
      reader_SB == "N" ~ "6",
    
    
    # Decision tree path 7 - rows with mammo not close to screen date, MLO view N, images by SB Y
    is.na(I_density_flag) & 
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "Y" ~ "7",
    
    
    # Decision tree path 8 - same as 7 but images by SB N
    is.na(I_density_flag) & 
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "N" ~ "8",
    
    
                                    TRUE ~ NA
                                      )
         ) %>% 
  ungroup()


# explore how many dates per participants 


# total
dates_sum <- relevant_mammo_df %>%
  group_by(tcode) %>%
  summarise(count = n_distinct(MammoDat_f))

n_distinct(relevant_mammo_df$tcode)


# assigned
dates_sum_selected <- relevant_mammo_df %>% 
  filter(!is.na(I_density_flag)) %>% 
  group_by(tcode) %>% 
  summarise(count = n_distinct(MammoDat_f))

assigned <- relevant_mammo_df %>% 
  filter(!is.na(I_density_flag)) 
n_distinct(assigned$tcode)
