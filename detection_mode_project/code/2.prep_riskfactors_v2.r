# ************************* Detection mode - breast density selection ************************



# ___________________________  PART 2. Prepare risk factors data  _________________________________


# Purpose: preparation of risk factor data



#Date: 12/04/2024
# Martina Brayley (Martina.Brayley@icr.ac.uk)

# version control: 1 

#update 16/09/2024 - added HRT type variable (hrtprep)
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
  #mutate(tcode = TCode) %>% 
  filter(!is.na(TCode)) 
  #select(-TCode)

n_distinct(riskfactors_df$TCode)

# Check for duplicates
n_distinct(riskfactors_df)

# remove duplicates if any 
riskfactors_df <- riskfactors_df %>% 
  distinct()

# Drop records without questionnaire
riskfactors_df <- riskfactors_df %>%
  filter(!is.na(AgeatEntry))


print(paste("Number in riskfactors after removing those with no tcode, duplicates, or no recruitment questionnaire:", nrow(riskfactors_df) ))


# # convert datetime to date format and create age variables
# riskfactors_df <- riskfactors_df %>%
#   mutate(
#     date_birth = as.Date(ADOB_F),  
#     date_entry = as.Date(EntryDate_F),
#     date_fup_start = as.Date(fupca_start_F),
#     date_fup_end = as.Date(fupca_end_F),
#     date_r2 = as.Date(R2Date_F),
#     date_r3 = as.Date(R3Date_F),
#     date_r4 = as.Date(R4Date_F),
#     
#     age_start365 = (date_fup_start - date_birth),
#     age_end365 = (date_fup_end - date_birth),
#     start_time365 = 0,
#     end_time365 = (date_fup_end - date_fup_start),
#     
#     age_entry_365 = date_entry - date_birth,
#     age_r2_365 = date_r2 - date_birth,
#     age_r3_365 = date_r3 - date_birth,
#     age_r4_365 = date_r4 - date_birth,
#     
#     date_preg_1 = as.Date(pregdate_1_f),
#     date_preg_2 = as.Date(pregdate_2_f),
#     date_preg_3 = as.Date(pregdate_3_f),
#     date_preg_4 = as.Date(pregdate_4_f),
#     date_preg_5 = as.Date(pregdate_5_f),
#     date_preg_6 = as.Date(pregdate_6_f),
#     date_preg_7 = as.Date(pregdate_7_f),
#     date_preg_8 = as.Date(pregdate_8_f),
#     date_preg_9 = as.Date(pregdate_9_f),
#     
#     age_pregnancy365_1 = (date_preg_1 - date_birth),
#     age_pregnancy365_2 = (date_preg_2 - date_birth),
#     age_pregnancy365_3 = (date_preg_3 - date_birth),
#     age_pregnancy365_4 = (date_preg_4 - date_birth),
#     age_pregnancy365_5 = (date_preg_5 - date_birth),
#     age_pregnancy365_6 = (date_preg_6 - date_birth),
#     age_pregnancy365_7 = (date_preg_7 - date_birth),
#     age_pregnancy365_8 = (date_preg_8 - date_birth),
#     age_pregnancy365_9 = (date_preg_9 - date_birth)
#   )
# 
# 
# View(riskfactors_df[,c("tcode", "date_entry", "date_birth", "AgeatEntry", "age_start365", "age_entry_365")])
# 

# Functions -----------------------------------------------------------------

menarcheage_sensible_range <- FALSE

append_oc_data <- function(data) {
  # Sanity checking all those with ocstatus==0 should have ocever %in% c(0/888/999) (true)
  # all(data[!is.na(data$ocstatus) & data$ocstatus == 0,]$ocever %in% c(0, 888, 999,NA))
  # Sanity checking all those with ocstatus %in% c(1, 2) should have ocever %in% c(1/888/999) (true)
  # all(data[!is.na(data$ocstatus) & data$ocstatus %in% c(1, 2),]$ocever %in% c(1, 888, 999,NA))
  
  # Collapse 888/999 into 888
  #data$x_ocever <- ifelse(data$ocever %in% c(999, 888, NA), 888, data$ocever)
  data$x_ocstatus <- ifelse(data$ocstatus %in% c(9999, 999, 888, NA), 888, data$ocstatus)
  
  # Sanity checking that ocLage is always after oc1age (true)
  # all(apply(data[,c('oc1age', 'ocLage')], 1, function(x) all(diff(x[!is.na(x) & !x%in%c(999, 888)]) >= 0)))
  
  # Sanity checking that ocLage and oc1age are always before AgeatEntry (true)
  # all((is.na(data$oc1age) | data$oc1age %in% c(999,888)) | (data$oc1age <= data$AgeatEntry))
  # all((is.na(data$ocLage) | data$ocLage %in% c(999,888)) | (data$ocLage <= data$AgeatEntry))
  
  # Sanity checking that if ocstatus==0, oc1age and ocLage should be 999/888/NA (true)
  # all(data[!is.na(data$ocstatus) & data$ocstatus == 0,]$oc1age %in% c(999, 888, NA))
  
  # Collapse 888/999 into 888
  data$x_age_started_oc <- ifelse(data$oc1age %in% c(999, 888, NA), 888, data$oc1age)
  data$x_age_lastused_oc <- ifelse(data$ocLage %in% c(999, 888, NA), 888, data$ocLage)
  
  data$x_oclength <- as.numeric(data$oclength)
  
  # oclength is summed total duration of OC use; not yet in dataframe ( (true)07/05/2024)
  # Sanity checking that if ocstatus==0, oclength should be 999/888/NA
  # all(data[!is.na(data$ocstatus) & data$ocstatus == 0,]$x_oclength %in% c(999, 888, NA))
  
  # Sanity checking that oclength is always <= AgeatEntry (quite liberal, but true)
  # all(data$x_oclength %in% c(999,888,NA) | (data$x_oclength <= data$AgeatEntry))
  
  # Collapse 888/999 into 888
  data$x_oclength <- ifelse(data$x_oclength %in% c(999, 888, NA), 888, data$x_oclength)
  
  return(data)
}



append_menarche_data <- function(data) { 
  # Sanity checking all those with menarcheever==no should have menarcheage==999/888/777/NA (true)
  # all(data[!is.na(data$menarcheever) & data$menarcheever == 2,]$menarcheage %in% c(999, 888, 777, NA))
  # Sanity checking all those with menarcheage == 777 should have menarcheever==2/8/9 (true)
  # all(data[!is.na(data$menarcheage) & data$menarcheage == 777,]$menarcheever %in% c(2, 8, 9))
  # Sanity checking that menarcheage is always before AgeatEntry (true)
  # all((is.na(data$menarcheage) | data$menarcheage %in% c(999,888,777)) | (data$menarcheage <= data$AgeatEntry))
  
  # Collapse 888/999 into 888
  data$x_age_menarche <- ifelse(data$menarcheage %in% c(999, 888, NA), 888, data$menarcheage)
  
  # Set those outside of sensible range to 888, if required. 12 yrs +/- 5 yrs
  if(menarcheage_sensible_range) data$x_age_menarche <- ifelse(data$x_age_menarche <= 7 | data$x_age_menarche >= 17, 888, data$x_age_menarche)
  
  return(data)
}

add_identification_data <- function(data){
  
  # x_tcode
  data$x_tcode <- data$TCode
  
  # dates
  # if NA then replace value with 8000-08-08, which is considered as an error date here
  data <- data |>
    dplyr::mutate(
      
      x_dob_shifted = dplyr::case_when(
        !is.na(ADOB_F) ~ as.Date(ADOB_F, format = "%Y-%m-%d"),
        TRUE ~ as.Date("8000-08-08", format = "%Y-%m-%d")),
      
      x_date_entry_shifted = dplyr::case_when(
        !is.na(EntryDate_F) ~ as.Date(EntryDate_F, format = "%Y-%m-%d"),
        TRUE ~ as.Date("8000-08-08", format = "%Y-%m-%d"))
      
      # These variables have been removed as they are not at R0
      
      # x_date_second_fup = dplyr::case_when(
      #   !is.na(R2Date_F) ~ as.Date(R2Date_F, format = "%Y-%m-%d"),
      #   TRUE ~ as.Date("8000-08-08", format = "%Y-%m-%d")),
      # 
      # x_date_third_fup = dplyr::case_when(
      #   !is.na(R3Date_F) ~ as.Date(R3Date_F, format = "%Y-%m-%d"),
      #   TRUE ~ as.Date("8000-08-08", format = "%Y-%m-%d")),
      # 
      # x_date_fourth_fup = dplyr::case_when(
      #   !is.na(R4Date_F) ~ as.Date(R4Date_F, format = "%Y-%m-%d"),
      #   TRUE ~ as.Date("8000-08-08", format = "%Y-%m-%d"))
      
    )
  
  # ages
  # if dob NA then replace value with 888 as can't know if they returned questionnaire
  # if date NA then replace value with 777 as we know DOB so only way to NA is no return
  data <- data |>
    dplyr::mutate(
      
      # Using base AgeatEntry as using dates causes discrepancies with the leap days
      x_age_entry = dplyr::case_when(
        is.na(AgeatEntry) ~ 777,
        TRUE ~ AgeatEntry)
      
      # These variables have been removed as they are not at R0
      
      # x_age_second_fup = dplyr::case_when(
      #   is.na(R2Age) ~ 777,
      #   TRUE ~ R2Age),
      # 
      # x_age_third_fup = dplyr::case_when(
      #   is.na(R3Age) ~ 777,
      #   TRUE ~ R3Age),
      # 
      # x_age_fourth_fup = dplyr::case_when(
      #   is.na(R4Age) ~ 777,
      #   TRUE ~ R4Age),
      
    )
  
  
  # output data
  return(data)
  
}








# Function to remove dates past the entry date - can be generalised for any date
remove_dates_past_entry_parity <- function(variable, entry_date){
  
  output <- dplyr::case_when(
    entry_date == as.Date("8000-08-08", format = "%Y-%m-%d") ~ as.Date("8000-08-08", format = "%Y-%m-%d"), # Unknown entry date -> can't know if it was before/after entry
    is.na(variable) ~ as.Date("8000-08-08", format = "%Y-%m-%d"), # If variable is NA then transfer to NA value for dates
    lubridate::time_length(lubridate::interval(entry_date, variable)) >= 0 ~ as.Date("8000-08-08", format = "%Y-%m-%d"), # If after entry, turn to NA
    TRUE ~ variable)
  
}

# Function to cut off breastfeeding values at entry
# Currently this process ignores parity differences, we only care about the total length of KNOWN breasfeeding
# NAs go to 0 for this very reason - we only want to calculate the total of known breastfeeding so an NA is 0 for these purposes
# This is something that can be discussed if others think this isn't a wise decision
rework_bf_to_entry <- function(x_date_entry_shifted, x_parity, pregdate, bf, index){
  
  # NOTE: lubirdate can't handle non integer weeks - have decided to take floor which will change 1 value
  bf <- floor(bf)
  
  dplyr::case_when(
    x_parity < index ~ 0, # No birth
    x_date_entry_shifted == as.Date("8000-08-08", format = "%Y-%m-%d") ~ 0, # Don't know entry date so can't know if bf overlaps
    pregdate == as.Date("8000-08-08", format = "%Y-%m-%d") ~ 0, # Similar to above
    is.na(bf) | bf == 999 ~ 0, 
    bf < 0 | bf > 208 ~ 0, # Range here is 0 - 4 years. This is arbitrary and based on previous limits imposed
    lubridate::time_length(lubridate::interval(start = (pregdate + weeks(bf)), end = x_date_entry_shifted)) <= 0 ~ lubridate::time_length(lubridate::interval(pregdate, x_date_entry_shifted), unit = "week"), # If breastfeeding overlaps with entry, then cut it short
    TRUE ~ bf) # If not then just keep as normal
  
  
  
}






add_parity_data <- function(data){
  
  # browser()
  
  # Getting list of variables on input so we can only output necessary variables
  variables_start <- names(data)
  
  # Get number of pre-menarche participants (0)
  # table(data$x_age_menarche) # This means no problems with checking parity
  
  # Need to edit birth dates so all are before date of entry
  # Need to change these to being new variables so that existing variables stay constant
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("pregdate"), ~as.Date(., format = "%Y-%m-%d"), .names = paste0("x_", "{.col}"))) |>
    dplyr::mutate_at(vars(dplyr::starts_with("x_pregdate")), ~remove_dates_past_entry_parity(., x_date_entry_shifted))
  
  # Need to get pregnancy count from the dates
  data <- data |>
    dplyr::mutate(
      
      # Getting number of dates of births we have from each participant
      dates_preg_count = rowSums(dplyr::across(dplyr::starts_with("x_pregdate_"), ~ .x != as.Date("8000-08-08", format = "%Y-%m-%d"))),
      
      # Creating a variable that assesses the difference between recorded parity and the parity collected from dates
      parity_difference = dplyr::case_when(
        pregparitycnt == dates_preg_count | (pregparitycnt == -1 & dates_preg_count == 0) ~ 0, # No difference in these cases
        pregparitycnt > dates_preg_count ~ 1, # We don't have enough dates for the parity - cannot give accurate first birthdate/last birthdate
        pregparitycnt < dates_preg_count ~ 2, # No such instances - would prepare for it but this code is temporary so won't worry for now
        TRUE ~ 1 # This occurs if pregparitycnt is NA, which I have placed into group 1 as an error
      ),
      
      # x_parous
      # have trusted pregparitycnt here - if pregparitycnt > 0 but dates_preg_count = 0 have trusted the number is correct, they just haven't recorded the date
      # No TRUE here means any cases missed should be picked up as NAs generated
      x_parous = dplyr::case_when(
        is.na(pregparitycnt) | pregparitycnt == 99 ~ 888, # Unknown parity
        pregparitycnt %in% c(-1, 0) ~ 0, # Not parous
        pregparitycnt > 0 ~ 1), # Parous
      
      # x_parity
      # have trusted pregparitycnt here - if pregparitycnt > 0 but dates_preg_count = 0 have trusted the number is correct, they just haven't recorded the date
      x_parity = dplyr::case_when(
        is.na(pregparitycnt) | pregparitycnt == 99 ~ 888, # Unknown parity
        pregparitycnt %in% c(-1, 0) ~ 0, # Not parous
        TRUE ~ pregparitycnt),
      
      # x_age_birth_1
      # Here incorporate differences in pregparitycnt and dates - if not enough dates, put NA
      x_age_birth_1 = dplyr::case_when(
        parity_difference == 1 ~ 888,
        x_parous == 0 ~ 777, # Not parous - no birth
        x_dob_shifted == as.Date("8000-08-08", format = "%Y-%m-%d") ~ 888, # Don't know DOB so can't know age at first birth
        x_pregdate_1_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(x_dob_shifted, x_pregdate_1_f), unit = "year")), # Have used floor here but could keep it a float
        TRUE ~ 888), 
      
      # Here implementing a range check
      x_age_birth_1 = dplyr::case_when(
        x_age_birth_1 < x_age_menarche & !x_age_birth_1 %in% c(777,888) & x_age_menarche != 888 ~ 888, # One such ocurrence of birth before menarche so have removed the date here
        # x_age_birth_1 <= 10 & !x_age_birth_1 %in% c(777,888) ~ 888,
        # x_age_birth_1 >= 70 & !x_age_birth_1 %in% c(777,888) ~ 888, # Quite a conservative estimate here - could be worth bringing this value down
        TRUE ~ x_age_birth_1),
      
      
      # x_age_birth_last
      # Here incorporate differences in pregparitycnt and dates - if not enough dates, put NA
      # This code can probably be made much more efficient - if whoever is reading this knows of a more efficient solution please let me know!
      # Suspicious value here with 0. 
      x_age_birth_last = dplyr::case_when(
        parity_difference == 1 ~ 888,
        x_parous == 0 ~ 777, # Not parous - no birth
        x_dob_shifted == as.Date("8000-08-08", format = "%Y-%m-%d") ~ 888, # Don't know DOB so can't know age at last birth
        pregparitycnt == 1 & x_pregdate_1_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_1_f), unit = "year")),
        pregparitycnt == 2 & x_pregdate_2_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_2_f), unit = "year")),
        pregparitycnt == 3 & x_pregdate_3_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_3_f), unit = "year")),
        pregparitycnt == 4 & x_pregdate_4_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_4_f), unit = "year")),
        pregparitycnt == 5 & x_pregdate_5_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_5_f), unit = "year")),
        pregparitycnt == 6 & x_pregdate_6_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_6_f), unit = "year")),
        pregparitycnt == 7 & x_pregdate_7_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_7_f), unit = "year")),
        pregparitycnt == 8 & x_pregdate_8_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_8_f), unit = "year")),
        pregparitycnt == 9 & x_pregdate_9_f != as.Date("8000-08-08", format = "%Y-%m-%d") ~ floor(lubridate::time_length(lubridate::interval(ADOB_F, x_pregdate_9_f), unit = "year")),
        TRUE ~ 888),
      
      # Here implementing a range check
      x_age_birth_last = dplyr::case_when(
        x_age_birth_last < x_age_menarche & !x_age_birth_last %in% c(777,888) & x_age_menarche != 888 ~ 888, # No such occurences here
        # x_age_birth_last <= 10 & !x_age_birth_last %in% c(777,888) ~ 888,
        # x_age_birth_last >= 70 & !x_age_birth_last %in% c(777,888) ~ 888, # Quite a conservative estimate here - could be worth bringing this value down
        TRUE ~ x_age_birth_last),
      
      # x_breastfeeding_duration
      # Need to cut off the breastfeeding at the date of entry. So need to add new variables for when they stopped breastfeeding, then cut them off at entry
      
      dplyr::across(dplyr::starts_with("bf_"), ~{
        index <- as.numeric(gsub("bf_", "", cur_column()))
        rework_bf_to_entry(x_date_entry_shifted = x_date_entry_shifted, x_parity = x_parity, pregdate = get(paste0("x_pregdate_", index, "_f")), bf = ., index = index)
      }, .names = paste0("x_", "{.col}")),
      
      # Summing over the bf variables
      x_breastfeeding_duration = rowSums(dplyr::across(dplyr::starts_with("x_bf_"))),
      
      
      # x_breastfed
      x_breastfed = dplyr::if_else(x_breastfeeding_duration > 0, "Yes", "No")
      
    )
  
  # Subset variables to the variable set we need
  data <- data |>
    dplyr::select(dplyr::all_of(variables_start), x_parous, x_parity, x_age_birth_1, x_age_birth_last, x_breastfeeding_duration, x_breastfed)
  
  
  
}


# Prepare data ---------------------------------------------

riskfactors_df <- riskfactors_df %>% 
  add_identification_data() %>% 
  append_oc_data() %>% 
  append_menarche_data() %>% 
  add_parity_data()


riskfactors_df <- riskfactors_df %>% 
  mutate(tcode = x_tcode,
         date_birth = x_dob_shifted,
         date_entry = x_date_entry_shifted)


# str(riskfactors_df)
#names(riskfactors_df)
# riskfactors_df %>% tabyl(x_breastfed)
