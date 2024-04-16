# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 3. Prepare cancer summary data  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 
#                  
#                 
#_____________________________________________________________________________


# 1. Initial checks on imported data -------------------------------------------------------------------------------


str(casummary_im)

summary(casummary_im)

casummary_im %>% 
  map(pct_miss)

casummary_im %>% 
  map(n_miss)

get_dupes(casummary_im)

descr(casummary_im)

n_distinct(casummary_im$tcode)


# remove tcode NAs - these pulled out of the study so ignore them
casummary_df <- casummary_im %>%
  filter(!is.na(tcode)) 

# merge date of birth from risk factors 
birth <- riskfactors_df %>% 
  select(tcode, date_birth, date_entry)

casummary_df <- casummary_df %>% 
  left_join(birth, by = "tcode")



# 2. Select BC cases ----------------------------------------------------------------------------------------------------
# NOTE: based on Louise's case selection for dmode v1. This selection is only for deriving dmode v2.
# analytical projects will have a separate case selections based on the project specific criteria. 
# not using confirmed cases criteria to ensure all cases with NHSBSP data are captured (agreed 02/04/2024)



## create variables needed for cancer selections -------------------------------


casummary_df <- casummary_df %>%
  #filter(confirmed == 1) %>%  # needs discussion
  mutate(
    # Create new variables and recode position 0 to 999 so 'no registry' or 'no reported' info is last
    reginfo_clusterino99 = ifelse(Reginfo_Clusterino == 0, 999, Reginfo_Clusterino),
    report_cluster99 = ifelse(Report_Cluster == 0, 999, Report_Cluster),
    # fix date format
    diagdate = as.Date(diagdate_f),
    # Identify any invasive cancer (including breast cancer) and DCIS-breast cancer (exclude NMSC)
    cancer = as.factor(ifelse(
      !((str_sub(ICDt, 1, 3) == 'C44') | (str_sub(ICDt, 1, 3) == '173')) &
        ((str_sub(ICDt, 1, 1) == 'C') |
           (str_sub(ICDt, 1, 1) == '1') |
           (str_sub(ICDt, 1, 2) == '20') |
           (str_sub(ICDt, 1, 3) == 'D05') |
           (str_sub(ICDt, 1, 4) == '2330')), 1, 0)),
    # Identify invasive or DCIS-breast cancer
    breast_cancer = as.factor(ifelse(
      (str_sub(ICDt, 1, 3) == 'C50') |
        (str_sub(ICDt, 1, 3) == '174') |
        (str_sub(ICDt, 1, 3) == 'D05') |
        (str_sub(ICDt, 1, 4) == '2330'), 1, 0)),
    # Identify invasive breast cancer
    breast_cancer_invasive = as.factor(ifelse(
      (str_sub(ICDt, 1, 3) == 'C50') |
        (str_sub(ICDt, 1, 3) == '174'), 1, 0)),
    # Identify DCIS-breast cancer
    breast_cancer_dcis = as.factor(ifelse(
      (str_sub(ICDt, 1, 3) == 'D05') |
        (str_sub(ICDt, 1, 4) == '2330'), 1, 0)) 
  ) %>% 
  # Order cancer diagnosis within participants by date
  group_by(tcode) %>%
  arrange(tcode, diagage, diagdate, Reginfo_Clusterino, Report_Cluster) %>%
  # cancer diagnosis order 
  mutate(ca_order = as.factor(order(tcode)),
         # BC diagnosis (dcis and inv) order within participants by date 
         BC_order = as.factor(if_else(breast_cancer == 1, cumsum(breast_cancer == 1), 0)),
         # BC diagnosis (inv only) order within participants by date
         BC_inv_order = as.factor(if_else(breast_cancer_invasive == 1, cumsum(breast_cancer_invasive == 1), 0)),
         # Flag for first cancer is BC (dcis and inv)
         first_ca_BC = as.factor(if_else(BC_order == 1 & ca_order == 1, 1, 0)),
         # Flag for first cancer is BC (invasive only)
         first_ca_inv_BC = as.factor(if_else(BC_inv_order == 1 & ca_order == 1, 1, 0))
  ) %>% 
  ungroup()


## select cases -------------------------------------------------------------------------
# including invasive and DCIS BC
# both confirmed  
# confirmed and incident
# check if we need only first ever cancers

# NEED TO FIGURE OUT HOW TO DEAL WITH BILATERAL CASES

cancer_df <- casummary_df %>%
  group_by(tcode) %>% 
  filter(confirmed == 1,
         incident == 1,
         breast_cancer == 1, # BC (dcis and inv) cases
         BC_order == 1) %>% # first breast cancer 
  ungroup()

# cancer_df <- casummary_df %>%
#   group_by(tcode) %>% 
#   filter(confirmed == 1,
#          incident == 1,
#          breast_cancer == 1, # BC (dcis and inv) cases
#          BC_order == 1, # first breast cancer
#          ca_order == 1) %>%  # first ever cancer
#   ungroup()

cancer_df %>% tabyl(confirmed)
cancer_df %>% tabyl(incident)
cancer_df %>% tabyl(breast_cancer_invasive)

n_distinct(cancer_df$tcode)

# if we want to select confirmed cases in this step can use min(BC_order) as the first breast cancer might've been uncofirmed - need to check this logic though
# cancer_df <- casummary_df %>%
#   group_by(tcode) %>% 
#   filter(confirmed == 1,
#          incident == 1,
#          breast_cancer == 1, # BC (dcis and inv) cases
#          min(as.numeric(BC_order)) %>% # first breast cancer 
#            ungroup()



#OLD
# cancer_df <- casummary_im %>% 
#   filter(First_inv_insitu_BrCa == "1", 
#          startsWith(ICDt, "C50") | startsWith(ICDt, "D05"))

str(cancer_df)




# 3. change variable types ----------------------------------------------------------------------------------------------


str(cancer_df)

#factor_vars <- c("incident", "confirmed", "First_inv_insitu_BrCa", "First_inv_BrCa", "firstatsite", "Screen_Detected")

cancer_df <- cancer_df %>%
  mutate(diagdate = as.Date(diagdate),
         reginfo_groupdatesite = as.factor(Reginfo_GroupDateSite),
         reginfo_clusterino = as.factor(Reginfo_Clusterino),
         report_groupdatesite = as.factor(Report_GroupDateSite),
         report_cluster = as.factor(Report_Cluster)
  ) %>%
  select(-Reginfo_Clusterino, -Reginfo_GroupDateSite,
         -Report_Cluster, -Report_GroupDateSite)

# checks:
str(cancer_df)

# create a list of cases with identifiers 
cases <- cancer_df %>% 
  select(tcode, reginfo_groupdatesite, reginfo_clusterino, report_groupdatesite, report_cluster)