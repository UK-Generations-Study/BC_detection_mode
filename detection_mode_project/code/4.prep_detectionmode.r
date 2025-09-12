# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 4. Prepare detection mode data  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  
#                 
#_____________________________________________________________________________

# 1. Initial checks on imported data -------------------------------------------------------------------------------


str(detection_mode_im)

summary(detection_mode_im)

detection_mode_im %>% 
  map(pct_miss)

detection_mode_im %>% 
  map(n_miss)

get_dupes(detection_mode_im)

descr(detection_mode_im)

n_distinct(detection_mode_im$tcode)


# remove tcode NAs - these pulled out of the study so ignore them
dm_df <- detection_mode_im %>%
  filter(!is.na(tcode)) 



dm_df <- dm_df %>%
  mutate(
         reginfo_groupdatesite = as.factor(reginfo_groupdatesite),
         reginfo_clusterino = as.factor(reginfo_clusterino),
         report_groupdatesite = as.factor(report_groupdatesite),
         report_cluster = as.factor(report_cluster)
  )

# checks:
str(dm_df)



# select eligible cancer cases cases 

# add indicator variable for being from dmode

dm_df <- dm_df |> 
  mutate(dm = 1)

# # original
# dm_df <- cases %>% 
#   left_join(dm_df, by = c("tcode", "reginfo_groupdatesite", "reginfo_clusterino", "report_groupdatesite", "report_cluster"))

# with inner join
dm_df <- cases %>%
  inner_join(dm_df, by = c("tcode", "reginfo_groupdatesite", "reginfo_clusterino", "report_groupdatesite", "report_cluster"))

# descriptives
#stview(dfSummary(dm_df))

dm_df %>% tabyl(ancat_dmode_v2) %>% adorn_totals()

# how many cases have dmode? 
dm_df |> filter(!is.na(ancat_dmode_v2)) |> 
  tabyl(ancat_dmode_v2) |> 
  adorn_totals()


###############################################################################################

# dm_df |> tabyl(dm)
# 
# 
# 
# # try with missing ancat_dmode_v2 and missing dmode_v2 (ancat has more missing because some narrower categories have not been converted to umbrella categories)
# 
# missing_dm <- dm_df |> 
#   left_join(cancer_df, by = c("tcode" = "TCode")) |> 
#   filter(is.na(ancat_dmode_v2)) # change to dmode_v2 to see diference
# 
# 
# # 1101 missing with ancat_dmode_v2
# # 1030 missing with dmode_v2
# 
# 
# missing_dm |> tabyl(diagage)
# 
# # create variable for outside of the screening interval: 
# missing_dm <- missing_dm |> 
#   mutate(out_scrage = case_when(diagage <50 ~ "out",
#                                 diagage >70 ~ "out",
#                                 TRUE ~ "in"))
# 
# missing_dm |> tabyl(out_scrage)
# # 434 in  and 667 out wtih ancat_dmode v2 
# # 397 in and 633 out with ancat_dmode 
# 
# # look at the diag year for "in" - might be post linkage 
# 
# missing_dm |> 
#   filter(out_scrage == "in") |> 
#   tabyl(diagyear)
# 
# missing_dm |> 
#   filter(out_scrage == "in") |> 
#   tabyl(diagyear, dm)
# 
# 
# temp <- missing_dm |> 
#   filter(out_scrage == "in",
#          is.na(dm)) |> 
#   select(tcode, diagdate_f, diagyear, diagdate, ancat_dmode_v2, dmode_v1, dm2_screen_date_f, shim_dmode, dmode_v1_src)
# 
# # dm = 1 were complicated manual categories in dmode_v1




########################################################################################
# 13/09/2024 - investigating if we can include cases without screening data

# detection_mode_im %>% tabyl(ancat_dmode_v2) # 331 NAs
# 
# dm_df2 <- dm_df %>%
#   left_join(cancer_df, by = c("tcode", "reginfo_groupdatesite", "reginfo_clusterino", "report_groupdatesite", "report_cluster")
#             ) %>%
#   filter(is.na(ancat_dmode_v2))
# 
# dm_df2 %>% tabyl(diagage) %>%
#   adorn_totals()
# 
# dm_df2 %>% tabyl(screen_Detected)
# 
# dm_df2 %>% tabyl(diagage, screen_Detected)
# 
# dm_df3 <- dm_df2 %>%
#   filter(diagage < 50)
# 
# dm_df3 %>% tabyl(screen_Detected)


#####################################################################################





# select only interval and SD dmode categories from ancat_dmode_v2 
dm_df <- dm_df %>% 
  filter(ancat_dmode_v2 %in% c("I", "SD"))

#stview(dfSummary(dm_df))

# create a list of I and SD cases (this will be used to select appropriate cases)
dm_cases <- dm_df %>% 
  select(tcode)
