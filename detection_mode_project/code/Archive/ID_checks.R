

# descr df 
df1 <- df %>% 
  select(tcode, d_dmode, all_of(all_expl_variables), diagage, d_R1toBC_y 
  ) %>% 
  tidyr::drop_na(all_of(all_expl_variables), diagage, d_R1toBC_y) %>% 
  select(tcode) %>% 
  mutate(df1 = "Y")

n_distinct(df1)


# LR df 
df2 <- df %>% 
  select(tcode, d_dmode, all_of(all_expl_variables_tr), diagage, d_R1toBC_y 
  ) %>% 
  tidyr::drop_na(all_of(all_expl_variables_tr), diagage, d_R1toBC_y) %>% 
  select(tcode)

n_distinct(df2)

# method 1
diff <- df2 %>% 
  anti_join(df1, by = "tcode")

# method 2
diff <- setdiff(df1, df2)

# method 3
# Perform a full outer join
merged_data <- merge(df1, df2, by = "tcode", all = TRUE, suffixes = c(".df1", ".df2"))

# Identify rows that are present in df1 but not in df2
extra_row <- merged_data[is.na(merged_data$column_in_df2), ]
print(extra_row)


check <- df %>% 
  select(tcode, d_dmode, all_of(all_expl_variables), all_of(all_expl_variables_tr))
