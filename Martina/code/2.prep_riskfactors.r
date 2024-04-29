# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 2. Prepare risk factors data  _________________________________


# Purpose: preparation of risk factor data



#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  
#                 
#_____________________________________________________________________________

# 1. Initial checks on imported data -------------------------------------------------------------------------------


str(riskfactors_im)

summary(riskfactors_im)

riskfactors_im %>% 
  map(pct_miss)

riskfactors_im %>% 
  map(n_miss)

get_dupes(riskfactors_im)

descr(riskfactors_im)

n_distinct(riskfactors_im$TCode)





# Remove records with missing tcode and rename TCode to tcode
riskfactors_df <- riskfactors_im %>%
  mutate(tcode = TCode) %>% 
  filter(!is.na(tcode)) %>% 
  select(-TCode)

# Check for duplicates
n_distinct(riskfactors_df)

# remove duplicates if any 
riskfactors_df <- riskfactors_df %>% 
  distinct()

# Drop records without questionnaire
riskfactors_df <- riskfactors_df %>%
  filter(!is.na(AgeatEntry))


print(paste("Number in riskfactors after removing those with no tcode, duplicates, or no recruitment questionnaire:", nrow(riskfactors_df) ))


# convert datetime to date format and create age variables
riskfactors_df <- riskfactors_df %>%
  mutate(
    date_birth = as.Date(ADOB_F),  
    date_entry = as.Date(EntryDate_F),
    date_fup_start = as.Date(fupca_start_F),
    date_fup_end = as.Date(fupca_end_F),
    date_r2 = as.Date(R2Date_F),
    date_r3 = as.Date(R3Date_F),
    date_r4 = as.Date(R4Date_F),
    
    age_start365 = (date_fup_start - date_birth),
    age_end365 = (date_fup_end - date_birth),
    start_time365 = 0,
    end_time365 = (date_fup_end - date_fup_start),
    
    age_entry_365 = date_entry - date_birth,
    age_r2_365 = date_r2 - date_birth,
    age_r3_365 = date_r3 - date_birth,
    age_r4_365 = date_r4 - date_birth,
    
    date_preg_1 = as.Date(pregdate_1_f),
    date_preg_2 = as.Date(pregdate_2_f),
    date_preg_3 = as.Date(pregdate_3_f),
    date_preg_4 = as.Date(pregdate_4_f),
    date_preg_5 = as.Date(pregdate_5_f),
    date_preg_6 = as.Date(pregdate_6_f),
    date_preg_7 = as.Date(pregdate_7_f),
    date_preg_8 = as.Date(pregdate_8_f),
    date_preg_9 = as.Date(pregdate_9_f),
    
    age_pregnancy365_1 = (date_preg_1 - date_birth),
    age_pregnancy365_2 = (date_preg_2 - date_birth),
    age_pregnancy365_3 = (date_preg_3 - date_birth),
    age_pregnancy365_4 = (date_preg_4 - date_birth),
    age_pregnancy365_5 = (date_preg_5 - date_birth),
    age_pregnancy365_6 = (date_preg_6 - date_birth),
    age_pregnancy365_7 = (date_preg_7 - date_birth),
    age_pregnancy365_8 = (date_preg_8 - date_birth),
    age_pregnancy365_9 = (date_preg_9 - date_birth)
  )
