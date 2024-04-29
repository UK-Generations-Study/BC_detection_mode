# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 3. Prepare cancer summary data  _________________________________


# Purpose: preparation of casummary data and case selection

# adapted from final dmode algorithm scripts in Safe Haven


#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1
#                  2 (changing case selection criteria to first primary only and dcis only)
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

# tabulate insitu cases to decide which ICD code to use 

casummary_df %>% 
  filter(startsWith(ICDt, "D05")) %>% 
  tabyl(ICDt)



## create variables needed for cancer selections -------------------------------

# updated code with case_when 
# 29/04/2024

casummary_df <- casummary_df %>%
  #filter(confirmed == 1) %>%  # needs discussion
  mutate(
    # Create new variables and recode position 0 to 999 so 'no registry' or 'no reported' info is last
    reginfo_clusterino99 = ifelse(Reginfo_Clusterino == 0, 999, Reginfo_Clusterino),
    report_cluster99 = ifelse(Report_Cluster == 0, 999, Report_Cluster),
    # fix date format
    diagdate = as.Date(diagdate_f),
    # Identify any invasive cancer (including breast cancer) and insitu breast cancer (exclude NMSC)
    # note: edit codes for insitu breast cancers as needed - now includes all insitu, might need to use only DCIS
    cancer = as.factor(case_when(
      startsWith(ICDt, "C44") | startsWith(ICDt, "173") ~ 0, # exclude NMSC
      
      startsWith(ICDt, "C") |  startsWith(ICDt, "1") | startsWith(ICDt, "20") ~ 1, # include cancers
      ICDt %in% c("D051", "D059", "D05Z", "2330") ~ 1, # include dcis
      TRUE ~ 0
                                )
                        ),
    # Identify invasive or insitu breast cancer
    breast_cancer = as.factor(case_when(
      startsWith(ICDt, "C50") | startsWith(ICDt, "174") ~ 1, # include invasive
      ICDt %in% c("D051", "D059", "D05Z", "2330") ~ 1, # include dcis
      TRUE ~ 0
                                        )
                              ), 
    
    # Identify invasive breast cancer
    breast_cancer_invasive = as.factor(if_else(startsWith(ICDt, "C50") | startsWith(ICDt, "174"), 1, 0
                                               )
                                       ),
    
    # Identify DCIS-breast cancer
    breast_cancer_dcis = as.factor(if_else( ICDt %in% c("D051", "D059", "D05Z", "2330"), 1, 0
                                            )
                                   )
    
    ) 
    
 
  
  
casummary_df <- casummary_df %>%
  # Order cancer diagnosis within participants by date
  group_by(tcode) %>%
  arrange(tcode, diagage, diagdate, Reginfo_Clusterino, Report_Cluster) %>%
  # cancer diagnosis order
  mutate(ca_order = as.factor(order(tcode)),
         # confirmed only cancer order
         conf_ca_order = as.factor(if_else(confirmed ==1 & cancer == 1, cumsum(cancer == 1), 0)),
         # BC diagnosis (dcis and inv) order within participants by date
         BC_order = as.factor(if_else(breast_cancer == 1, cumsum(breast_cancer == 1), 0)),
         # confirmed only BC diagnosis (dcis and inv) order within participants by date
         conf_BC_order = as.factor(if_else(confirmed == 1 & breast_cancer == 1, cumsum(breast_cancer == 1), 0)),
         # BC diagnosis (inv only) order within participants by date
         BC_inv_order = as.factor(if_else(breast_cancer_invasive == 1, cumsum(breast_cancer_invasive == 1), 0)),
         # Flag for first cancer is BC (dcis and inv)
         first_ca_BC = as.factor(if_else(BC_order == 1 & ca_order == 1, 1, 0)),
         # Flag for first cancer is BC (invasive only)
         first_ca_inv_BC = as.factor(if_else(BC_inv_order == 1 & ca_order == 1, 1, 0))
  ) %>%
  ungroup()



## select cases -------------------------------------------------------------------------


# 29/04/2024 
cancer_df <- casummary_df %>% 
  group_by(tcode) %>% 
  filter(incident == 1,
         breast_cancer == 1,
         conf_BC_order ==1,
         conf_ca_order == 1)


cancer_df %>% tabyl(confirmed)
cancer_df %>% tabyl(incident)
cancer_df %>% tabyl(breast_cancer_invasive)

cancer_df %>% tabyl(ICDt)

n_distinct(cancer_df$tcode)



# 3. change variable types ----------------------------------------------------------------------------------------------


str(cancer_df)

#factor_vars <- c("incident", "confirmed", "First_inv_insitu_BrCa", "First_inv_BrCa", "firstatsite", "Screen_Detected")

cancer_df <- cancer_df %>%
  mutate(diagdate = as.Date(diagdate),
         reginfo_groupdatesite = as.factor(Reginfo_GroupDateSite),
         reginfo_clusterino = as.factor(Reginfo_Clusterino),
         report_groupdatesite = as.factor(Report_GroupDateSite),
         report_cluster = as.factor(Report_Cluster),
         side = as.factor(side)
  ) %>%
  select(-Reginfo_Clusterino, -Reginfo_GroupDateSite,
         -Report_Cluster, -Report_GroupDateSite)

# checks:
str(cancer_df)

# create a list of cases with identifiers 
cases <- cancer_df %>% 
  select(tcode, reginfo_groupdatesite, reginfo_clusterino, report_groupdatesite, report_cluster)