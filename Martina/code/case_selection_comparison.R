# comparison of case selection by brca flag (originally used by Louise) and the new reproducible selection 


original_cases <- casummary_im %>% 
  filter(First_inv_insitu_Brca == 1,
         incident == 1,
         confirmed == 1)

# n = 4245 

4245 - 4172 # 73


original_cases_ids <- original_cases %>% 
  mutate(reginfo_groupdatesite = as.factor(Reginfo_GroupDateSite),
         reginfo_clusterino = as.factor(Reginfo_Clusterino),
         report_cluster = as.factor(Report_Cluster),
         report_groupdatesite = as.factor(Report_GroupDateSite)) %>% 
  select(tcode, reginfo_groupdatesite, reginfo_clusterino, report_groupdatesite, report_cluster) %>% 
  mutate(original_flag = "Y")

str(cases)
str(original_cases_ids)

cases_comp <- cases %>% 
  full_join(original_cases_ids, by = c("tcode", "reginfo_groupdatesite",
                                       "reginfo_clusterino",
                                       "report_groupdatesite",
                                       "report_cluster") )

cases_comp %>% tabyl(original_flag)
