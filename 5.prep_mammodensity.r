# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 5. Prepare mammo density data  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  
#                 
#_____________________________________________________________________________

# 1. Initial checks on imported data -------------------------------------------------------------------------------


str(mammodensity_im)

summary(mammodensity_im)

mammodensity_im %>% 
  map(pct_miss)

mammodensity_im %>% 
  map(n_miss)

mammo_dupes <- get_dupes(mammodensity_im)

descr(mammodensity_im)

n_distinct(mammodensity_im$TCode)


# 2. remove tcode NAs and duplicates --------------------------------------------------

# tcode NAs  pulled out of the study so ignore them
mammodensity_df <- mammodensity_im %>%
  mutate(tcode = TCode) %>% 
  filter(!is.na(tcode)) %>% 
  select(-TCode) %>% 
  distinct()


mammodensity_df %>% tabyl(Reader_Internal)

describe(mammodensity_df)
# freq(mammodensity_df)

summary <- dfSummary(mammodensity_df)

stview(summary)


# select eligible cases - interval and SD cases 

mammodensity_df <- dm_cases %>% 
  left_join(mammodensity_df, by = "tcode")

# descriptives
stview(dfSummary(mammodensity_df))


# change variable formats 
str(mammodensity_df)

mammodensity_df <- mammodensity_df %>% 
  mutate(View = as.factor(View),
         MammoDat_f = as.Date(MammoDat_f),
         Method = as.factor(Method),
         Reader_Internal = as.factor(Reader_Internal),
         B2Risk_Study = as.factor(B2Risk_Study),
         ImageType = as.factor(ImageType),
         UploadDate = as.Date(UploadDate))
