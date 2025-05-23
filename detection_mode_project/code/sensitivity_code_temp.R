library(ggrepel)

base_output <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\mode_of_detection\repo\detection_mode_project\outputs\sensitivity_analysis_raw\base.xlsx)")[,-(2:3)]
sensitive_output <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\mode_of_detection\repo\detection_mode_project\outputs\sensitivity_analysis_raw\postmeno.xlsx)")[,-(2:3)]

files_to_compare <- list.files(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\mode_of_detection\repo\detection_mode_project\outputs\sensitivity_analysis_raw)", full.names = T)
files_to_compare <- files_to_compare[!grepl("base\\.xlsx", files_to_compare)]

for(file in files_to_compare){
  
  variable_name <- case_when(
    grepl("complete\\_case\\.xlsx", file) ~ "Compete Case",
    grepl("erpos\\.xlsx", file) ~ "ER Positive",
    grepl("postmeno\\.xlsx", file) ~ "Postmenopausal",
    grepl("postmeno\\_hrtnever\\.xlsx", file) ~ "Postmenopausal + HRT: Never",
    grepl("notimevar.xlsx", file) ~ "No Time Adjustment",
    TRUE ~ NA
  )
  
  subtitle_name <- case_when(
    grepl("complete\\_case\\.xlsx", file) ~ "Cohort filtered to participants with no missing data",
    grepl("erpos\\.xlsx", file) ~ "Cohort filtered to cases with ER positive tumours",
    grepl("postmeno\\.xlsx", file) ~ "Cohort filtered to postmenopausal women",
    grepl("postmeno\\_hrtnever\\.xlsx", file) ~ "Cohort filtered to postmenopausal women who have never taken MHT",
    grepl("notimevar.xlsx", file) ~ "No time variables used in adjustment (time from R1 to mammogram etc)",
    TRUE ~ NA
  )
  
  sensitive_output <- read_excel(file)[,-(2:3)]
  
  output_clean <- base_output |>
    filter(!is.na(`**p-value**...9`)) |>
    mutate(model = "base") |>
    rbind(sensitive_output |> filter(!is.na(`**p-value**...9`)) |> mutate(model = "Sensitive")) |>
    mutate(
      
      p_value = case_when(
        `**p-value**...9` == ">0.9" ~ 0.95,
        `**p-value**...9` == "<0.001" ~ 0.0005,
        TRUE ~ as.numeric(`**p-value**...9`)
      )
      
    )
    
  output_clean_plot <- output_clean |>
    pivot_wider(names_from = "model", values_from = "p_value", id_cols = `**Characteristic**`)
  
  plot <- ggplot(output_clean_plot, aes(x = base, y = Sensitive)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
    geom_abline(slope = 1, intercept = 0) +
    geom_point() +
    geom_label_repel(min.segment.length = unit(0, "lines"),
                     aes(label = `**Characteristic**`)) +
    scale_y_log10(limits = output_clean |> pull(p_value) |> range()) +
    scale_x_log10(limits = output_clean |> pull(p_value) |> range()) +
    theme_minimal() +
    labs(
      x = "Base Model",
      y = variable_name
    ) +
    coord_equal() +
    ggtitle(paste0("Comparison of Base P Values to ", variable_name, " P Values"),
            subtitle = subtitle_name)
  
  print(plot)
  
  ggsave(plot, filename = paste0(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\mode_of_detection\repo\detection_mode_project\outputs\sensitivity_analysis_figures\figure_)", letters[which(files_to_compare == file)], ".png"),
         bg = "white", width = 12, height = 12)
  
}
