

# run set up first

# import data 
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

dm_df <- dm_df |> 
  filter(ancat_dmode_v2 %in% c("SD", "I"))

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
