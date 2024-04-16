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

dm_df <- cases %>% 
  left_join(dm_df, by = c("tcode", "reginfo_groupdatesite", "reginfo_clusterino", "report_groupdatesite", "report_cluster"))

# descriptives
stview(dfSummary(dm_df))

# select only interval and SD dmode categories from ancat_dmode_v2 
dm_df <- dm_df %>% 
  filter(ancat_dmode_v2 %in% c("I", "SD"))

stview(dfSummary(dm_df))

# create a list of I and SD cases (this will be used to select appropriate cases)
dm_cases <- dm_df %>% 
  select(tcode)
