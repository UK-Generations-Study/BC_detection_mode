
icd <- cancer_df |> 
  select(tcode, ICDt)


df <- an_df |> 
  left_join(icd, by = "tcode")


df |> tabyl(ICDt)
