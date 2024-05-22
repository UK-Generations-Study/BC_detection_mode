
library(haven)



Ldata <- read_dta("R:/IvsSD/CaseCase ana/Stata/RF_TC_MD_ISD_rawfromDB.dta")

names(Ldata)

Ldata %>% tabyl(grade_tc)




# compare study IDs 



L_cases <- Ldata %>% 
  select(studyid) 
