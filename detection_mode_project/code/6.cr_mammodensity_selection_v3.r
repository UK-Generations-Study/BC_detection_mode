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
  select(tcode, diagdate, diagage, side)

## join with mammo dataset -----------------------------------

relevant_mammo_df <- mammodensity_df %>% 
  left_join(dm_vars, by = "tcode") %>% 
  left_join(ca_vars, by = "tcode") %>% 
  mutate(ancat_dmode_v2 = as.factor(ancat_dmode_v2))

# checks:
str(relevant_mammo_df)
n_distinct(relevant_mammo_df$tcode)




# 2. Create variables needed for density estimation  ----------------------------------------------------

# for simplicity filter out those without mammo density
density_df <- relevant_mammo_df %>% 
  filter(mammo_flag == "Y")

n_distinct(density_df$tcode)



## difference in dates ----------------------------

# create a variable with a difference between dens_dm2_screen_date and mammoDat and diagnosis 

density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(scr_date_diff = as.numeric(MammoDat_f - dens_dm2_screen_date_f),
         diag_date_diff = as.numeric(MammoDat_f - diagdate)
         ) %>% 
  ungroup()


# explore the differences:
density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(scr_date_diff)

density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(diag_date_diff)

#hist(density_df$scr_date_diff)
#hist(density_df$diag_date_diff)



# take the absolute value of the difference between dates for easier ranking (easier to handle positive values than negative) 
density_df <- density_df %>% 
  mutate(abs_diag_date_diff = case_when(diag_date_diff <= 0 ~ abs(diag_date_diff),
                                        scr_date_diff > 0 ~ NA),
         abs_scr_date_diff = case_when(scr_date_diff < 10 ~ abs(scr_date_diff),
                                       scr_date_diff >= 10 ~ NA)
         )
# note: for abs_scr_date_diff - using 10 days because the mammo images could be uploaded a bit later 
# than the screen date, as long as it is very close to screen date (can be changed to a different cut off)

# checks: 
density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(abs_diag_date_diff)

density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(abs_scr_date_diff)

# create rank of mammo dates based on smallest difference between diagdate and mammo date and screen and mammo dates - rank

density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(mammo_dg_diff_rank = dense_rank(abs_diag_date_diff),
         scr_mammo_diff_rank = dense_rank(abs_scr_date_diff)) %>% 
  ungroup()


## Create flag variables based on the decision tree conditions -------------------------------


one_year <- 365.25
one_month <- one_year/12


density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(
    # variables needed for decision tree flow 
    mammo_3m_before_dg = if_else(abs_diag_date_diff > (3*one_month), "Y", "N"),
    
    mammo_1y_before_dg = if_else(abs_diag_date_diff > (one_year), "Y", "N"),
         
         screen_closeto_mammo = if_else(scr_date_diff > -30 & scr_date_diff < 10, "Y", "N"),
          
        screen_dir_bef_mammo = if_else(scr_mammo_diff_rank == 1, "Y", "N"), # screen directly before mammo
         
         mlo_available = if_else(any(View %in% c("LMLO", "RMLO")), "Y", "N"), # case has any row with MLO
         
         reader_SB = if_else(any(Reader_Internal == "SB"), "Y", "N"), # case has any row with SB reader
    
    mammo_before_dg = if_else(diag_date_diff > 0, "Y", "Y"),
    
    dg_mammo = if_else(diag_date_diff > (3*(-one_month)) & diag_date_diff < (3*one_month), "Y", "N"),
    
    contralateral = as.factor(case_when(as.factor(side) == "1" & View %in% c("RMLO", "RCC") ~ "Y",
                              as.factor(side) == "2" & View %in% c("LMLO", "LCC") ~ "Y",
                              as.factor(side) == "3" ~ "bilateral",
                              as.factor(side) == "1" & View %in% c("LMLO", "LCC") ~ "N",
                              as.factor(side) == "2" & View %in% c("RMLO", "RCC") ~ "N",
                              TRUE ~ NA))
         ) %>% 
  ungroup()


# checks: 
density_df %>% tabyl(mammo_3m_before_dg)
density_df %>% tabyl(mammo_1y_before_dg)
density_df %>% tabyl(screen_closeto_mammo)
density_df %>% tabyl(mlo_available)
density_df %>% tabyl(screen_dir_bef_mammo)
density_df %>% tabyl(reader_SB)
density_df %>% tabyl(mammo_before_dg)
density_df %>% tabyl(dg_mammo)
density_df %>% tabyl(contralateral)


# create a variable to indicate diagnostic mammmo (3 months before and 3 months after diagnosis?) - exploratory
density_df <- density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(diag_diff_cat = as.factor(case_when(diag_date_diff <= -365 ~ "more than 1 year before dg",
                                             diag_date_diff <= -90 & diag_date_diff > -365 ~ "3m to 1y before dg",
                                   diag_date_diff > -90 & diag_date_diff <= -1 ~ "within 3m before dg",
                                   diag_date_diff >= 0 & diag_date_diff <= 90 ~ "within 3m after dg",
                                   diag_date_diff > 90 & diag_date_diff <= 365 ~ "3m to 1y after dg",
                                   diag_date_diff > 365 ~ "more than 1 year after dg"))
    
  ) %>% 
  ungroup()


# checks:
density_df %>% tabyl(diag_diff_cat)
density_df %>% tabyl(diag_diff_cat, ancat_dmode_v2)
density_df %>% 
  group_by(diag_diff_cat, ancat_dmode_v2) %>% 
  mean_table(diag_date_diff)





# 3. Interval density row selection -------------------------------------------------

# based on the decision tree conditions and using the above created variables, create flag variable for Interval cases,
# flagging the rows that meet the decision tree criteria for Interval cases paths. There are multiple options, 
# for simplicity tag each decision tree path with a different number within the flag variable. 
# There rows then will be used for calculating the average density for cases. 

## step 1: assign those with only 1 mammo date ---------------------------------------
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




## step 2: assign those with more than 1 mammo date (n 114) -------------------------
dev_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                      ancat_dmode_v2 == "I" &
                                      date_count > 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                      mlo_available == "Y" & # any row with MLO per ID
                                      reader_SB == "Y" & # any row with SB per ID
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal == "SB"
                                    ~ "5", 
                                    T ~ I_density_flag
  )
  ) %>% 
    mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                        ancat_dmode_v2 == "I" &
                                        date_count > 1 &
                                        mammo_3m_before_dg == "Y" &
                                        mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                        mlo_available == "Y" & # any row with MLO per ID
                                        reader_SB == "N" & # any row with SB per ID
                                        View %in% c("LMLO", "RMLO") &
                                        Reader_Internal != "SB"
                                      ~ "6", 
                                      T ~ I_density_flag
    )
  ) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                      ancat_dmode_v2 == "I" &
                                      date_count > 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                      mlo_available == "N" & # any row with MLO per ID
                                      reader_SB == "Y" & # any row with SB per ID
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal == "SB"
                                    ~ "7", 
                                    T ~ I_density_flag
  )
  ) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                      ancat_dmode_v2 == "I" &
                                      date_count > 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                      mlo_available == "N" & # any row with MLO per ID
                                      reader_SB == "N" & # any row with SB per ID
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal != "SB"
                                    ~ "8", 
                                    T ~ I_density_flag
  )
  ) %>% 
  # second sweep
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                      ancat_dmode_v2 == "I" &
                                      date_count > 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                      mlo_available == "Y" & # any row with MLO per ID
                                      reader_SB == "Y" & # reader SB available for this ID
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal == "SB"
                                    ~ "9",  # this one picks up those who have rank 1 not done by SB reader 
                                    T ~ I_density_flag
  )
  ) %>% 
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                      ancat_dmode_v2 == "I" &
                                      date_count > 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                      mlo_available == "N" & # any row with MLO per ID
                                      reader_SB == "Y" & # reader SB available for this ID
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal == "SB"
                                    ~ "10",  # this one picks up those who have rank 1 not done by SB reader and don't have any MLOs
                                    T ~ I_density_flag
  )
  ) %>%
  mutate(I_density_flag = case_when(all(is.na(I_density_flag)) &
                                      ancat_dmode_v2 == "I" &
                                      date_count > 1 &
                                      mammo_3m_before_dg == "Y" &
                                      mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                      #mlo_available == "N" & # any row with MLO per ID
                                      #reader_SB == "Y" & # reader SB available for this ID
                                      View %in% c("LMLO", "RMLO", "RCC", "LCC") &
                                      Reader_Internal != "SB"
                                    ~ "11",  # this one picks up those who have rank 1 not done by SB reader and don't have any MLOs
                                    T ~ I_density_flag
  )
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
checks %>% tabyl(I_density_flag, date_count, ancat_dmode_v2)

density_df %>%  tabyl(diag_diff_cat, ancat_dmode_v2)







# 4. SD density selection -------------------------------------------------

# 22/04/2024 - excluded SD_dg_first_screen (flag for diagnosis at first screen from screening data) from the code because the functionality of the variable doesn't seem to be so good
# around half of cases have mammo before the supposedly diagnostic screening date. 

# based on the decision tree conditions and using the above created variables, create flag variable for SD cases,
# flagging the rows that meet the decision tree criteria for Interval cases paths. There are multiple options, 
# for simplicity tag each decision tree path with a different number within the flag variable. 
# There rows then will be used for calculating the average density for cases. 



## step 1 - assign those with only 1 mammo date ---------------------------------
dev_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(SD_density_flag = case_when(ancat_dmode_v2 == "SD" &
                                      date_count == 1 &
                                       #SD_dg_first_screen == "N" &
                                      mammo_1y_before_dg == "Y" &
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
                                       #SD_dg_first_screen == "N" &
                                      mammo_1y_before_dg == "Y" &
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
                                       #SD_dg_first_screen == "N" &
                                      mammo_1y_before_dg == "Y" &
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
                                       #SD_dg_first_screen == "N" &
                                      mammo_1y_before_dg == "Y" &
                                      mlo_available == "N" & # ano MLO views
                                      reader_SB == "N" & # no readings by SB 
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal != "SB"
                                    ~ "4", 
                                    T ~ SD_density_flag)
  ) %>% 
  ungroup()






## step 2 - assign those with more than 1 mammo date ------------------------------------
dev_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                       mlo_available == "Y" & # any row with MLO per ID
                                       reader_SB == "Y" & # any row with SB per ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal == "SB" 
                                     
                                     ~ "5", 
                                     T ~ SD_density_flag
  )
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & # if any row 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                       mlo_available == "Y" &
                                       reader_SB == "N" &
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal != "SB"
                                     ~ "6", 
                                     T ~ SD_density_flag)
  
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                       mlo_available == "N" & # ano MLO views
                                       reader_SB == "Y" & # no readings by SB 
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal == "SB"
                                     ~ "7", 
                                     T ~ SD_density_flag)
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "1" & # choose date directly before diagnosis
                                       mlo_available == "N" & # ano MLO views
                                       reader_SB == "N" & # no readings by SB 
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal != "SB"
                                     ~ "8", 
                                     T ~ SD_density_flag)
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                      ancat_dmode_v2 == "SD" &
                                      date_count > 1 &
                                      mammo_1y_before_dg == "Y" &
                                      mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                      mlo_available == "Y" & # any row with MLO per ID
                                      reader_SB == "Y" & # reader SB available for this ID
                                      View %in% c("LMLO", "RMLO") &
                                      Reader_Internal == "SB"
                                    ~ "9",  # this one picks up those who have rank 1 not done by SB reader 
                                    T ~ SD_density_flag
  )
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                      ancat_dmode_v2 == "SD" &
                                      date_count > 1 &
                                      mammo_1y_before_dg == "Y" &
                                      mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                      mlo_available == "N" & # any row with MLO per ID
                                      reader_SB == "Y" & # reader SB available for this ID
                                      View %in% c("LCC", "RCC") &
                                      Reader_Internal == "SB"
                                    ~ "10",  # this one picks up those who have rank 1 not done by SB reader and don't have any MLOs
                                    T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                       mlo_available == "Y" & # any row with MLO per ID
                                       reader_SB == "N" & # reader SB available for this ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal != "SB"
                                     ~ "11",  # this one picks up those who have rank 1 not done by SB reader and don't have any MLOs
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                       mlo_available == "N" & # any row with MLO per ID
                                       reader_SB == "N" & # reader SB available for this ID
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal != "SB"
                                     ~ "12",  # this one picks up those who have rank 1 not done by SB reader and don't have any MLOs
                                     T ~ SD_density_flag
  )
  ) %>%
 # Id has a row with a diagnositic mammo (rank 1 & dg mammo = Y) read by SB
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) &
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       any(dg_mammo == "Y" & mammo_dg_diff_rank == "1" & Reader_Internal == "SB") & # main condition here (as heading)
                                       mammo_1y_before_dg == "Y" &
                                       mammo_dg_diff_rank == "2" & # choose date directly before diagnosis - second sweep
                                       #mlo_available == "Y" & # any row with MLO per ID
                                       #reader_SB == "N" & # reader SB available for this ID
                                       View %in% c("LMLO", "RMLO", "RCC", "LCC") &
                                       Reader_Internal != "SB"
                                     ~ "13",  # this one picks up those who have rank 1 not done by SB reader and don't have any MLOs
                                     T ~ SD_density_flag
  )
  ) %>%
  ungroup()




## step 3 assign those with diagnostic date ----------------------------------------


# 23/04/2024 decided to not use the SD flag for first screen - use interval based on data 3m before to 3m after diagnosis


# comment on SD flag for first screen exploration: ******************************************** 
# check number of dates for those diagnosed at first screen - to see if there are 
# any discrepancies between screening data and mammo date as 612 were categories before 
# condition on excluding dg at first screen was included, then 590. So it seems that around 22 have more dates. 

checks <- dev_density_df %>% 
  filter(SD_dg_first_screen == "Y") %>% 
  group_by(tcode) %>%
  summarise(date_count = n_distinct(MammoDat_f))

# there are 5 IDs that have 2 mammo dates - explored manually 
# ****************************************************************************************************



dev_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
           ancat_dmode_v2 == "SD" &
           date_count == 1 &
           dg_mammo == "Y" & # diagnostic mammo
          contralateral == "Y" &
           mlo_available == "Y" & # any row with MLO per ID
           reader_SB == "Y" & # any row with SB per ID
           View %in% c("LMLO", "RMLO") &
           Reader_Internal == "SB" 
         
         ~ "14", 
         T ~ SD_density_flag
  )
  ) %>% 
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count == 1 &
                                       dg_mammo == "Y" & # diagnostic mammo
                                       contralateral == "Y" &
                                       mlo_available == "Y" & # any row with MLO per ID
                                       reader_SB == "N" & # any row with SB per ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal != "SB" 
                                     
                                     ~ "15", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count == 1 &
                                       dg_mammo == "Y" & # diagnostic mammo
                                       contralateral == "Y" &
                                       mlo_available == "N" & # any row with MLO per ID
                                       reader_SB == "Y" & # any row with SB per ID
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal == "SB" 
                                     
                                     ~ "16", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count == 1 &
                                       dg_mammo == "Y" & # diagnostic mammo
                                       contralateral == "Y" &
                                       mlo_available == "N" & # any row with MLO per ID
                                       reader_SB == "N" & # any row with SB per ID
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal != "SB" 
                                     
                                     ~ "17", 
                                     T ~ SD_density_flag
  )
  ) %>%
  # date more than 1
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       all(dg_mammo == "Y") & # all diagnostic mammo within participant (if they have date more than 3m before diagnosis that one should be selected)
                                       contralateral == "Y" &
                                       mlo_available == "Y" & # any row with MLO per ID
                                       reader_SB == "Y" & # any row with SB per ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal == "SB" 
                                     
                                     ~ "18", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       all(dg_mammo == "Y") & # all diagnostic mammo within participant (if they have date more than 3m before diagnosis that one should be selected)
                                       contralateral == "Y" &
                                       mlo_available == "Y" & # any row with MLO per ID
                                       reader_SB == "N" & # any row with SB per ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal != "SB" 
                                     
                                     ~ "19", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       all(dg_mammo == "Y")   & # all diagnostic mammo within participant (if they have date more than 3m before diagnosis that one should be selected)
                                       contralateral == "Y" &
                                       mlo_available == "N" & # any row with MLO per ID
                                       reader_SB == "Y" & # any row with SB per ID
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal == "SB" 
                                     
                                     ~ "20", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       all(dg_mammo == "Y") & # all diagnostic mammo within participant (if they have date more than 3m before diagnosis that one should be selected)
                                       contralateral == "Y" &
                                       mlo_available == "N" & # any row with MLO per ID
                                       reader_SB == "N" & # any row with SB per ID
                                       View %in% c("LCC", "RCC") &
                                       Reader_Internal != "SB" 
                                     
                                     ~ "21", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       #all(dg_mammo == "Y") & # all diagnostic mammo within participant (if they have date more than 3m before diagnosis that one should be selected)
                                       all(diag_date_diff > -(3*one_month)) & # diagnostic mammo or post diagnostic mammo only 
                                       contralateral == "Y" &
                                       dg_mammo == "Y" &
                                       mlo_available == "Y" & # any row with MLO per ID
                                       #reader_SB == "N" & # any row with SB per ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal == "SB" 
                                     
                                     ~ "22", 
                                     T ~ SD_density_flag
  )
  ) %>%
  mutate(SD_density_flag = case_when(all(is.na(SD_density_flag)) & 
                                       ancat_dmode_v2 == "SD" &
                                       date_count > 1 &
                                       #all(dg_mammo == "Y") & # all diagnostic mammo within participant (if they have date more than 3m before diagnosis that one should be selected)
                                       all(diag_date_diff > -(3*one_month)) & # diagnostic mammo or post diagnostic mammo only 
                                       contralateral == "Y" &
                                       dg_mammo == "Y" &
                                       mlo_available == "Y" & # any row with MLO per ID
                                       #reader_SB == "N" & # any row with SB per ID
                                       View %in% c("LMLO", "RMLO") &
                                       Reader_Internal != "SB" 
                                     
                                     ~ "23", 
                                     T ~ SD_density_flag
  )
  ) %>%
  ungroup()



# tabulate how many assigned
checks <- dev_density_df %>% 
  select(tcode, ancat_dmode_v2, SD_density_flag, I_density_flag, date_count, SD_dg_first_screen, diag_diff_cat) %>% 
  group_by(tcode) %>% 
  arrange(tcode, SD_density_flag) %>% 
  slice(1)

n_distinct(checks$tcode)
checks %>% tabyl(SD_density_flag)
checks %>% tabyl(date_count)
checks %>% tabyl(date_count, ancat_dmode_v2)
checks %>% tabyl(date_count, SD_density_flag, ancat_dmode_v2)
checks %>% tabyl(SD_density_flag, SD_dg_first_screen)
checks %>% tabyl(SD_density_flag, ancat_dmode_v2)
checks %>% tabyl(I_density_flag, ancat_dmode_v2)


density_df %>%  tabyl(diag_diff_cat, ancat_dmode_v2) %>% 
  adorn_totals()

checks %>%  tabyl(diag_diff_cat, ancat_dmode_v2) %>% 
  adorn_totals()



# 5. Explore unassigned -------------------------------------------------------

## SD -------------------------
SD_unassiged <- dev_density_df %>% 
  group_by(tcode) %>% 
  filter(ancat_dmode_v2 == "SD",
         all(is.na(SD_density_flag))) %>% 
    
  mutate(posneg = case_when(diag_date_diff < 0 ~ "neg",
                            diag_date_diff > 0 ~ "pos"),
         allpos = if_else(all(posneg == "pos"), "Y", "N")
  ) %>% 
  ungroup()

n_distinct(SD_unassiged$tcode)

allpos_ids <- SD_unassiged %>% 
  select(tcode, allpos) %>% 
  filter(allpos == "Y") %>% 
  distinct()
  


## interval -------------------------
I_unassiged <- dev_density_df %>% 
  group_by(tcode) %>% 
  filter(ancat_dmode_v2 == "I",
         all(is.na(I_density_flag))) %>% 
  
  mutate(posneg = case_when(diag_date_diff < -(3*one_month) ~ "neg",
                            diag_date_diff > 0 ~ "pos",
                            diag_date_diff >= -(3*one_month) & diag_date_diff <= 0 ~ "diag"),
         allpos = case_when(all(posneg == "pos") ~ "allpos",
                            all(posneg == "diag") ~ "alldiag",
                            T ~ "N")
  ) %>% 
  ungroup()

n_distinct(I_unassiged$tcode)

allpos_ids <- I_unassiged %>% 
  select(tcode, allpos) %>% 
  filter(allpos == "allpos") %>% 
  distinct()

alldiag_ids <- I_unassiged %>% 
  select(tcode, allpos) %>% 
  filter(allpos == "alldiag") %>% 
  distinct()







# 3. Calculate average density -------------------------------------------------------------

## step 1: create a variable from I and SD mammo flags to flag the rows for mammo selection------------------
# (simple Y/N) for ease

dev_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  mutate(mammo_row = case_when(!is.na(I_density_flag) ~ "Y",
                               !is.na(SD_density_flag) ~ "Y",
                               TRUE ~ NA
                               )
         ) %>% 
  ungroup()

dev_density_df %>% tabyl(I_density_flag, mammo_row)
dev_density_df %>% tabyl(SD_density_flag, mammo_row)


## step 2: compute the mean ----------------------------------------------
mean_density_df <- dev_density_df %>% 
  group_by(tcode) %>% 
  arrange(tcode, MammoDat_f) %>% 
  filter(mammo_row == "Y") %>% 
  mutate(mean_density = round(mean(Density_Reading),1),
         sd_density = round(SD(Density_Reading),1),
         MD_avail = "Y"
         ) %>% 
  select(tcode, MammoDat_f, MD_avail, mean_density, sd_density, ancat_dmode_v2) %>% 
  distinct() %>% 
  ungroup()


# checks:
dev_density_df %>% tabyl(B2Risk_Study, mammo_row) %>%  adorn_totals()

mean_density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  summarise(n = n())


mean_density_df %>% 
  group_by(ancat_dmode_v2) %>% 
  mean_table(mean_density)

