
# General functions ------------------------------------------------------------------

## function for checking/describing numeric variables - works with lapply -------------------
check_numeric <- function(variable) {
  cat("summary:\n")
  print(summary(variable))
  
  cat("describe:\n")
  print(describe(variable))
  
  cat("tabulate:\n")
  print(table(variable))
  
  cat("histogram:\n")
  print(hist(variable))
}

## quick check of numeric variables - works with lapply ---------------------------------
quick_check <- function(variable) {
  cat("summary:\n")
  print(summary(variable))
  
}

## p-value function formatting 
format_p_values <- function(data, p_value_raw_column) {
  data %>%
    mutate(p_val1 = case_when(
      {{p_value_raw_column}} < 0.001 ~ "<0.001",
      {{p_value_raw_column}} >= 0.001 & {{p_value_raw_column}} < 0.01 ~ as.character(round({{p_value_raw_column}}, 3)),
      round({{p_value_raw_column}}, 2) == 1 ~ "0.99",
      TRUE ~ as.character(format(round({{p_value_raw_column}}, 2), nsmall = 1))
    ))
}




# Univariate analysis function ------------------------------------------------------


# # Define the function
# perform_univariate_analysis <- function(data, outcome, exposure) {
#   # Cross-tabulation with column percentages
#   cat("\nCross-tabulation for", exposure, ":\n")
#   tab_result <- tabpct(data[[outcome]], data[[exposure]], percent = "col", graph = FALSE)
#   print(tab_result)
#   
#   # Odds Ratio and chi-squared
#   cat("\nOdds Ratio and Chi-squared for", exposure, ":\n")
#   or_chi_result <- cc(data[[outcome]], data[[exposure]], graph = FALSE)
#   print(or_chi_result)
#   
#   # Logistic Regression
#   model <- glm(formula = as.formula(paste(outcome, "~", exposure)),
#                data = data,
#                family = binomial(link = "logit"))
#   
#   # Display summary of logistic regression in log odds scale
#   cat("\nLogistic Regression Summary for", exposure, ":\n")
#   print(summary(model))
#   
#   # Display odds ratios using epiDisplay package
#   cat("\nOdds Ratios for", exposure, ":\n")
#   logistic.display(model)
# }
# 
# # Example usage of the function with your dataframe and variables
# # Replace 'an_df', 'd_dmode_n', and 'd_R1alcohol_status_lab' with your actual dataframe and variable names
# perform_univariate_analysis(data = an_df, outcome = "d_dmode_n", exposure = "d_R1alcohol_status_lab")
# 
# # To run this for multiple exposures:
# exposures <- all_expl_variables
# 
# 
# for (exposure in exposures) {
#   cat("\nAnalyzing exposure:", exposure, "\n")
#   perform_univariate_analysis(data = an_df, outcome = "d_dmode_n", exposure = exposure)
# }



# v2 handling error for fisher exact test in odds ratios  ----------------------------------------

# Define the function
perform_univariate_analysis <- function(data, outcome, exposure) {
  # Cross-tabulation with column percentages
  cat("\nCross-tabulation for", exposure, ":\n")
  tab_result <- tabpct(data[[outcome]], data[[exposure]], percent = "col", graph = FALSE)
  print(tab_result)
  
  # Odds Ratio and chi-squared
  cat("\nOdds Ratio and Chi-squared for", exposure, ":\n")
  tryCatch({
    or_chi_result <- cc(data[[outcome]], data[[exposure]], graph = FALSE)
    print(or_chi_result)
  }, error = function(e) {
    cat("An error occurred during OR and chi-squared calculation:", e$message, "\n")
  })
  
  # Logistic Regression
  model <- glm(formula = as.formula(paste(outcome, "~", exposure)),
               data = data,
               family = binomial(link = "logit"))
  
  # Display summary of logistic regression in log odds scale
  cat("\nLogistic Regression Summary for", exposure, ":\n")
  print(summary(model))
  
  # Display odds ratios using epiDisplay package
  cat("\nOdds Ratios for", exposure, ":\n")
  logistic.display(model)
}

# # Example usage of the function with your dataframe and variables
# # Replace 'an_df', 'd_dmode_n', and 'd_R1alcohol_status_lab' with your actual dataframe and variable names
# perform_univariate_analysis(data = an_df, outcome = "d_dmode_n", exposure = "d_agebirth1_lab")
# 
# # To run this for multiple exposures:
# exposures <- all_expl_variables
# 
# for (exposure in exposures) {
#   cat("\nAnalyzing exposure:", exposure, "\n")
#   perform_univariate_analysis(data = an_df, outcome = "d_dmode_n", exposure = exposure)
# }




# Functions for figures -----------------------------------------------------------------

## function to extract global p-value from linear regression ------------------------

#define function to extract overall p-value of model
extract_overall_p_from_lm <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

## extract overall p-value of model -----------------------------------------------------
# e.g. extract_overall_p_from_lm(model)


# function to format single p value (not a dataframe)
format_single_p_value <- function(p_value) {
  formatted_p <- case_when(
    p_value < 0.001 ~ "<0.001",
    p_value >= 0.001 & p_value < 0.01 ~ as.character(round(p_value, 3)),
    round(p_value, 2) == 1 ~ "0.99",
    TRUE ~ as.character(format(round(p_value, 2), nsmall = 1))
  )
  return(formatted_p)
}

## function to create violin plots ----------------------------------------------------

create_violin_plot <- function(data, response_var, explanatory_var, 
                               y_label, title, category_labels = NULL, 
                               annotation_pos) {
  # Fit the linear model
  formula <- as.formula(paste(response_var, "~", explanatory_var))
  lm_model <- lm(formula, data = data)
  
  # Extract and format the overall p-value
  global_p <- extract_overall_p_from_lm(lm_model)
  formatted_p <- format_single_p_value(global_p)
  
  # Create the plot
  plot <- data %>%
    ggplot(aes(x = .data[[explanatory_var]], y = .data[[response_var]], fill = .data[[explanatory_var]])) +
    geom_violin(alpha = 0.4) +
    geom_boxplot(width = 0.1) +
    ylim(0, 100) +
    labs(title = title, x = "", y = y_label) +
    theme_base() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2") # +
    # annotate("text",
    #          x = annotation_pos$ref_x,
    #          y = annotation_pos$ref_y,
    #          label = "reference",
    #          col = "black",
    #          size = 3.5) +
    # annotate("text",
    #          x = annotation_pos$p_x,
    #          y = annotation_pos$p_y,
    #          label = paste("p-trend", formatted_p),
    #          col = "black",
    #          size = 3.5)

  # Add custom x-axis labels if provided
  if (!is.null(category_labels)) {
    plot <- plot + scale_x_discrete(labels = category_labels)
  }
  
  return(plot)
}



# TEST FUNCTION 

# # Define annotation positions
# annotation_positions <- list(ref_x = 1, ref_y = 90, p_x = 2, p_y = 90)
# 
# # Define category labels
# category_labels <- c("I" = "Interval", "SD" = "Screen detected")
# 
# # Call the function (e.g.) category labels are optional
# plot1 <- create_violin_plot(
#   data = md_df,
#   response_var = "d_md",
#   explanatory_var = "d_dmode",
#   y_label = "Breast density (%)",
#   title = "Mode of Detection",
#   category_labels = category_labels,
#   annotation_pos = annotation_positions
# )


