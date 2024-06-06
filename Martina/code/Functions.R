
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

# Example usage of the function with your dataframe and variables
# Replace 'an_df', 'd_dmode_n', and 'd_R1alcohol_status_lab' with your actual dataframe and variable names
perform_univariate_analysis(data = an_df, outcome = "d_dmode_n", exposure = "d_agebirth1_lab")

# To run this for multiple exposures:
exposures <- all_expl_variables

for (exposure in exposures) {
  cat("\nAnalyzing exposure:", exposure, "\n")
  perform_univariate_analysis(data = an_df, outcome = "d_dmode_n", exposure = exposure)
}



