

# run set up first

# import data 

library(here)
# set up
setup_path <- here("code", "1.setup_import.r")
source(setup_path)


# dataset with missing as separate categories -----------------------------------
df <- readRDS("Q:/SHARED/USERS/MBrayley/Screening/data/an_df.rds")
str(df)

# dcis tab

# needs processing casummary
icd <- cancer_df |> 
  select(tcode, ICDt)


df <- df |> 
  left_join(icd, by = "tcode")


df |> tabyl(ICDt)


# shim tabulations -----------------------------------------------------
shim <- detection_mode_im |> 
  select(tcode, shim_dmode)

df <- df |> 
  left_join(shim, by = "tcode")

df |> tabyl(shim_dmode, d_dmode)



# dmode numbers for flow chart--------------------------------------------------------

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


# with inner join - to remove those who have not been linked to nhsbsp 
dm_df <- cases %>%
  inner_join(dm_df, by = c("tcode", "reginfo_groupdatesite", "reginfo_clusterino", "report_groupdatesite", "report_cluster"))


dm_df %>% tabyl(ancat_dmode_v2) %>% adorn_totals()

dm_df |> tabyl(dmode_v2) |> adorn_totals()

# try with missing ancat_dmode_v2 and missing dmode_v2 (ancat has more missing because some narrower categories have not been converted to umbrella categories)

missing_dm <- dm_df |> 
  left_join(cancer_df, by = c("tcode" = "TCode")) |> 
  filter(is.na(ancat_dmode_v2)) # change to dmode_v2 to see diference


missing_dm |> tabyl(diagage)

# create variable for outside of the screening interval: 
missing_dm <- missing_dm |> 
  mutate(out_scrage = case_when(diagage <50 ~ "out",
                                diagage >70 ~ "out",
                                TRUE ~ "in"))

missing_dm |> tabyl(out_scrage)
missing_dm |> tabyl(diagyear)

# dm_df <- dm_df |> 
#   filter(ancat_dmode_v2 %in% c("SD", "I"))

dm_df <- dm_df |> 
  filter(ancat_dmode_v2 %in% c("SD", "I", "LA", "DNA", "IA"))

dm_df |> tabyl(ancat_dmode_v2) |> adorn_totals()




# flow chart tabs ---------------------------------------------------------

names(df)

df |> tabyl(d_dmode)

# set ns

n_GS <- 113757 # total gs


n_bsp <- 65121 # nhsbsp

n_bc <- 4064 # eligible bc cases

n_bcwithbsp <- 3039

n_isd <- 1940

# excluded first round
n_GS - n_bsp

# excluded second round 
n_bsp - n_bcwithbsp


# excluded third round
n_bcwithbsp - n_isd



# tabulations for supplement -----------------------------------------------------------

# source tabulation for all dmode cases 
dm_df |> tabyl(source_dm2) |> 
  adorn_totals()
# 86% derived screening data (dmode v1 and v2 combined) when included all DM categories (n 3039)
# 80% derived screening data when included only flow chart derived dm (n 2073)

# expert derived in 3039 = 7%
# expert derived in 2073 = 3%




dm_df <- dm_df |> 
  mutate(dm_3cat = case_when(ancat_dmode_v2 == "SD" ~ "SD",
                             ancat_dmode_v2 == "I" ~ "I", 
                             TRUE ~ "Other"),
         reg_sd_new = case_when(reg_sd == "Y" ~ "SD",
                                reg_sd == "N" ~ "I",
                                TRUE ~ "Unknown"))

dm_df |> tabyl(ancat_dmode_v2, dm_3cat)

dm_df |> 
  filter(source_dm2 != "reg_sd") |> 
  tabyl(reg_sd, dm_3cat) |> 
  adorn_totals(where = c("row", "col")) |> 
  adorn_percentages(denominator = "row") |> 
   adorn_pct_formatting() |> 
  adorn_ns() 
  


dm_df |> 
  filter(source_dm2 != "reg_sd") |> 
  tabyl(dmode_v2, reg_sd_new) |> 
   adorn_totals(where = c("row","col")) |> 
  adorn_title() 

dm_df |> 
  filter(source_dm2 != "reg_sd") |> 
  tabyl(ancat_dmode_v2, reg_sd) |> 
  adorn_totals(where = c("row","col")) |> 
  adorn_title()


dm_df |> 
  tabyl(dmode_v2, ancat_dmode_v2)

dm_df |> 
  tabyl(ancat_dmode_v2)
 

# reg_sd completeness ----------------------
df <- dm_df |> 
  left_join(cancer_df, by = "tcode")


df |> tabyl(diagyear, reg_sd) |> 
  adorn_percentages() |> 
  adorn_pct_formatting()


# tabulate detection mode by source
df %>% tabyl(ancat_dmode_v2, source_dm2) %>% 
  adorn_totals() %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns()

# density ---------------------------------------

df |> tabyl(d_md_avail_lab, d_dmode)

