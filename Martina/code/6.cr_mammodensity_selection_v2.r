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
  select(tcode, ancat_dmode_v2, source_dm2, dm2_screen_date_f, dm1_screen_date_f, dens_dm2_screen_date_f, SD_dg_first_screen)

## select variables from cancer df ---------------------------

ca_vars <- cancer_df %>% 
  select(tcode, diagdate)

## join with mammo dataset 

relevant_mammo_df <- mammodensity_df %>% 
  left_join(dm_vars, by = "tcode") %>% 
  left_join(ca_vars, by = "tcode") %>% 
  mutate(ancat_dmode_v2 = as.factor(ancat_dmode_v2))

str(relevant_mammo_df)
n_distinct(relevant_mammo_df$tcode)


# 2. Identify relevant mammo date ----------------------------------------------------

# for simplicity filter out those without mammo density
density_df <- relevant_mammo_df %>% 
  filter(mammo_flag == "Y")

n_distinct(density_df$tcode)


# create a variable with a difference between dens_dm2_screen_date and mammoDat and diagnosis 

density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(scr_date_diff = as.numeric(MammoDat_f - dens_dm2_screen_date_f),
         diag_date_diff = as.numeric(MammoDat_f - diagdate)
         ) %>% 
  ungroup()

# explore the differences
density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(scr_date_diff)

density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(diag_date_diff)

hist(density_df$scr_date_diff)
hist(density_df$diag_date_diff)



# take the absolute value of the difference between dates for easier ranking (easier to handle positive values than negative) 
density_df <- density_df %>% 
  mutate(abs_diag_date_diff = case_when(diag_date_diff <= 0 ~ abs(diag_date_diff),
                                        scr_date_diff > 0 ~ NA),
         abs_scr_date_diff = case_when(scr_date_diff < 10 ~ abs(scr_date_diff),
                                       scr_date_diff >= 10 ~ NA)
         )
# note: for abs_scr_date_diff - using 10 days because the mammo images could be uploaded a bit later 
# than the screen date, as long as it is very close to screen date (can be changed to a different cut off)


density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(abs_diag_date_diff)

density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(abs_scr_date_diff)

# create order of mammo dates based on smallest difference between diagdate and mammo date and screen and mammo dates - rank

density_df <- density_df %>% 
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
density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(
    # variables needed for decision tree flow 
    mammo_3m_before_dg = if_else(mammo_dg_diff_rank == 1 & abs_diag_date_diff > (3*one_month), "Y", "N"),
         
         screen_closeto_mammo = if_else(scr_date_diff > -30 & scr_date_diff < 10, "Y", "N"),
          
        screen_dir_bef_mammo = if_else(scr_mammo_diff_rank == 1, "Y", "N"), # screen directly before mammo
         
         mlo_available = if_else(any(View %in% c("LMLO", "RMLO")), "Y", "N"),
         
         reader_SB = if_else(any(Reader_Internal == "SB"), "Y", "N"),
    
    mammo_before_dg = if_else(diag_date_diff > 0, "Y", "Y")
         ) %>% 
  ungroup()


# add more variables needed for SD once the relevant SD date is derived

# Interval density selection -------------------------------------------------

## step 1 - assign those with only 1 mammo date
dev_density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(I_density_flag = case_when(ancat_dmode_v2 == "I" &
                                    date_count == 1 &
                                    mammo_3m_before_dg == "Y" &
                                      mlo_available == "Y" & # any row with MLO per ID
                                      reader_SB == "Y" & # any row with SB per ID
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal == "SB"
                                    ~ "1", 
                                      T ~ NA
                                      )
         ) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) & # if any row 
                                    ancat_dmode_v2 == "I" &
                                      date_count == 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "Y" &
                                      reader_SB == "N" &
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal != "SB"
                                    ~ "2", 
                                    T ~ I_density_flag
                                    )
         ) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) & 
                                    ancat_dmode_v2 == "I" &
                                      date_count == 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "N" & # ano MLO views
                                      reader_SB == "Y" & # no readings by SB 
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal == "SB"
                                    ~ "3", 
                                    T ~ I_density_flag)
  ) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) & 
                                    ancat_dmode_v2 == "I" &
                                      date_count == 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "N" & # ano MLO views
                                      reader_SB == "N" & # no readings by SB 
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal != "SB"
                                    ~ "4", 
                                    T ~ I_density_flag)
  ) %>% 
  ungroup()

# tabulate how many assigned
checks <- dev_density_df %>% 
  select(tcode, ancat_dmode_v2, I_density_flag, date_count) %>% 
  group_by(tcode) %>% 
  arrange(tcode, I_density_flag) %>% 
  slice(1)
  
n_distinct(checks$tcode)
checks %>% tabyl(I_density_flag)
checks %>% tabyl(date_count)
checks %>% tabyl(date_count, ancat_dmode_v2)



# SD density selection -------------------------------------------------

## step 1 - assign those with only 1 mammo date
dev_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(SD_density_flag = case_when(ancat_dmode_v2 == "SD" &
                                      date_count == 1 &
                                       SD_dg_first_screen == "N" &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "Y" & # any row with MLO per ID
                                      reader_SB == "Y" & # any row with SB per ID
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal == "SB" 
                                       
                                    ~ "1", 
                                    T ~ NA
  )
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & # if any row 
                                      ancat_dmode_v2 == "SD" &
                                      date_count == 1 &
                                       SD_dg_first_screen == "N" &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "Y" &
                                      reader_SB == "N" &
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal != "SB"
                                    ~ "2", 
                                    T ~ SD_density_flag
  )
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                      date_count == 1 &
                                       SD_dg_first_screen == "N" &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "N" & # ano MLO views
                                      reader_SB == "Y" & # no readings by SB 
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal == "SB"
                                    ~ "3", 
                                    T ~ SD_density_flag)
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                      date_count == 1 &
                                       SD_dg_first_screen == "N" &
                                      mammo_3m_before_dg == "Y" &
                                      mlo_available == "N" & # ano MLO views
                                      reader_SB == "N" & # no readings by SB 
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal != "SB"
                                    ~ "4", 
                                    T ~ SD_density_flag)
  ) %>% 
  ungroup()


# tabulate how many assigned
checks <- dev_density_df %>% 
  select(tcode, ancat_dmode_v2, SD_density_flag, date_count, SD_dg_first_screen) %>% 
  group_by(tcode) %>% 
  arrange(tcode, SD_density_flag) %>% 
  slice(1)

n_distinct(checks$tcode)
checks %>% tabyl(SD_density_flag)
checks %>% tabyl(date_count)
checks %>% tabyl(date_count, ancat_dmode_v2)
checks %>% tabyl(date_count, SD_density_flag, ancat_dmode_v2)
checks %>% tabyl(SD_density_flag, SD_dg_first_screen)


# check number of dates for those diagnosed at first screen - to see if there are 
# any discrepancies between screening data and mammo date as 612 were categories before 
# condition on excluding dg at first screen was included, then 590. So it seems that around 22 have more dates. 

checks <- dev_density_df %>% 
  filter(SD_dg_first_screen == "Y") %>% 
  group_by(tcode) %>%
  summarise(date_count = n_distinct(MammoDat_f))

# there are 5 IDs that have 2 mammo dates - explore more 

# OLD CODE --------------

dev_density_df <- density_df %>% 
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
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_closeto_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "N" ~ "4",
    
    # Decision tree path 5 - rows with mammo not close to screen date, MLO view Y, images by SB Y
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "Y" & 
      reader_SB == "Y" ~ "5",
    
    
    
    # Decision tree path 6 - same as path 5 but images by SB N
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "Y" & 
      reader_SB == "N" ~ "6",
    
    
    # Decision tree path 7 - rows with mammo not close to screen date, MLO view N, images by SB Y
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "Y" ~ "7",
    
    
    # Decision tree path 8 - same as 7 but images by SB N
    ancat_dmode_v2 == "I" &
      mammo_3m_before_dg == "Y" &
      screen_dir_bef_mammo == "Y" &
      mlo_available == "N" & 
      reader_SB == "N" ~ "8",
    
    
                                    TRUE ~ NA
                                      )
         ) %>% 
  ungroup()



# assigned
dates_sum_selected <- dev_density_df %>% 
  filter(!is.na(I_density_flag)) %>% 
  group_by(tcode) %>% 
  summarise(count = n_distinct(MammoDat_f))

assigned <- dev_density_df %>% 
  filter(!is.na(I_density_flag)) 

n_distinct(assigned$tcode)

dm_df %>% tabyl(ancat_dmode_v2)
