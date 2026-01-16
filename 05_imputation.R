## =================================================================================================
# Preamble further analysis: Data imputation using the MICE package with a multivariate approach

df_before_imputation <- df_recoded %>%
  dplyr::select(
    age_r, sex_r, living_area_r, education_r_r, financial_stability_r, 
    family_status_r_r, living_situation_r_r, age_at_diagnosis_r, 
    time_from_diagnosis_r, who5_depr, comorb_r, disturbances_sleep_APD, 
    falls_r, assist_mov_r, ms_fluctuation_APD, qol_sum_disease_r, 
    qol_ms_r, qol_nms_r, qol_conditions_r, qol_gain_r, qol_loss_independence_r, 
    qol_unpredict_r, qol_invasive_therapy_r, qol_other_therapy_r, 
    qol_effect_medication_r, qol_taking_medication_r, qol_family_contact_r, 
    qol_friends_contact_r, qol_affectp_contact_r, qol_support_yes_r, 
    qol_stigma_r, qol_leisure_yes_r, qol_finances_r, qol_badmob_r, 
    qol_badadl_r, qol_sum_ident_r, qol_feeling_needed_r, qol_thoughts_future_r, 
    qol_self_image_r, qol_religion_r
  ) 

# Percentage of the data that is missing!
total_missing <- sum(is.na(df_before_imputation))
total_values <- prod(dim(df_before_imputation))
missing_percentage <- (total_missing / total_values) * 100

# Output the percentage of missing values
cat("Percentage of missing values in selected variables for analysis:", missing_percentage, "%\n")

pdf(file.path(getwd(), "results", "suppl_fig1.aggr_plot_output.pdf"), width = 11, height = 8.5)  # Save the aggr plot output to a PDF file in landscape orientation

aggr_plot <- aggr(
  df_before_imputation,
  col = c('navyblue', 'red'),
  numbers = TRUE,
  sortVars = TRUE,
  labels = colnames(df_before_imputation),
  cex.axis = 0.5,
  gap = 3,
  ylab = c("Histogram of missing data", "Pattern")
)


text(
  x = 0.25,  # Centered horizontally
  y = .84, # Position below the plot (adjust if needed)
  sprintf("Percentage of missing values in \nselected variables for analysis is: %.2f%%", missing_percentage), 
  cex = 1.2,
  pos = 1 # Aligns text below the specified coordinates
)


dev.off() # Close the PDF device to save the file

# Start imputation
# Define the variables to be included as covariates in each imputation model
inlist <- c(
  "age", "sex", "education", "comorb", "time_from_diagnosis", 
  "family_status", "falls", "B012_01", "who5_depr"
)

# Generate predictor matrix with minimum proportion of usable cases set to 0.5
pred <- quickpred(
  df_before_imputation, 
  minpuc = 0.5, 
  include = inlist
)

# Perform multiple imputation using the MICE package
generate_imputation <- mice(
  data = df_before_imputation,
  predictorMatrix = pred,
  method = "midastouch", # Imputation method
  m = 10,                # Number of imputed datasets
  maxit = 5,             # Number of iterations
  diagnostics = TRUE     # Enable diagnostics
)


imputed_data <- complete(generate_imputation, 1)
if (flag_check) {
  aggr_plot <- aggr(
    imputed_data,
    col = c('navyblue', 'red'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = colnames(imputed_data),
    cex.axis = 0.5,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )
}
