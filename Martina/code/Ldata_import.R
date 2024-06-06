
library(haven)



Ldata <- read_dta("R:/IvsSD/CaseCase ana/Stata/RF_TC_MD_ISD_rawfromDB.dta")

Ldata <- read_dta("R:/IvsSD/CaseCase ana/Stata/RF_TC_MD_ISD.dta")

names(Ldata)

Ldata %>% tabyl(grade_tc)




# compare study IDs 



L_cases <- Ldata %>% 
  select(studyid) 

Ldata %>% tabyl(er_stat)
Ldata %>% tabyl(er_stat, inv_status)
Ldata %>% tabyl(er_lj, inv_status)


# comparing grade
Ldata %>% tabyl(grade_lj, inv_status) %>% 
  adorn_totals(where = c("row", "col"))

dev_an_df %>% tabyl(d_grade, d_inv_status_lab) %>% 
  adorn_totals(where = c("row", "col"))


# comparing er status:
Ldata %>% tabyl(er_stat, inv_status) %>% 
  adorn_totals(where = c("row", "col"))

dev_an_df %>% tabyl(d_er_status, d_inv_status_lab) %>% 
  adorn_totals(where = c("row", "col"))
# trick model
dev_an_df %>% tabyl(d_er_tr, d_inv_status_lab) %>% 
  adorn_totals(where = c("row", "col"))


# comparing pr status: 
# comparing er status:
Ldata %>% tabyl(pr_stat, inv_status) %>% 
  adorn_totals(where = c("row", "col"))

Ldata %>% tabyl(pr_lj, inv_status) %>% 
  adorn_totals(where = c("row", "col"))

dev_an_df %>% tabyl(d_pr_status, d_inv_status_lab) %>% 
  adorn_totals(where = c("row", "col"))

# trick model
dev_an_df %>% tabyl(d_pr_tr, d_inv_status_lab) %>% 
  adorn_totals(where = c("row", "col"))
