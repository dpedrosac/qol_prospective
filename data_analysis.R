# This is code to run analyses on a prospective study on quality of life of subjetcs with
# Parkinson's disease; 
# Code developed by Leonie Moormann and David Pedrosa

# Version 2.3 # 2024-19-11, # Smaller adjustments in code for biplot (EFA) and in code for x-axis (correlation matrix EFA), smaller adjustments in OR
# Version 2.2 # 2024-12-11, # OR with binomial GLM, visualization of OR
# Version 2.1 # 2024-11-11, # Smaller adjustments in TableOne, visualization of health-care conditions, PartD and E
# Version 2.0 # 2024-10-11, # Adding visualization of TableOne and EFA
# Version 1.9 # 2024-06-11, # Smaller adjustments, changes in nomenclature (e.g., df_recoded -> df_recoded)
# Version 1.8 # 2024-03-11, # Removing PCA, recoding variables, performing moderated regression to evaluate interactions of predictors, revise OLR (m1)
# Version 1.7 # 2024-18-10, # Adding PCA of factors and investigation of interaction of predictors, draft OLR
# Version 1.6 # 2024-20-06, # Correcting recode_string in "recode_variables.R", extracting factor score of EFA, draft of OLR and Bootstrapping
# Version 1.5 # 2024-07-30, # Adding: Correlation matrix and Cronbach's alpha reliability analysis (EFA), draft of GLM, working ordinal logistic regression + cleaning up the script (deleting discarded analysis and adding explanations), adding distance-based donor selection in the imputation, minor adjustments in recoding "comorb_r" in "recode_variables.R"
# Version 1.4 # 2024-07-21, # Adjustments in the multiple imputation, draft of exploratory factor analysis
# Version 1.3 # 2024-07-15, # Some changes in recode_values.R, minor adjustments and first working draft of multiple imputation
# Version 1.3 # 2024-07-15, # added draft of PCA and added information in TableOne
# Version 1.2 # 2024-07-09, # added TableOne and basis for multiple imputation

# Flag for performing additional checks
flag_check <- FALSE

username <- Sys.getenv("USERNAME")
# Load required packages and directories
if (username == "Leonie") {
 	setwd("C:/Users/Leonie/HESSENBOX/Daten/analysis_R")
 } else if (username == "dpedrosac") {
 	setwd("/media/storage/qol_prospective/")
 }

if (!dir.exists("results")) dir.create("results") # Creates a folder in which all content is saved for publication later 

source("packages.R")
source("directories.R")

# Load raw data from questionnaires:
df_raw <- readxl::read_xlsx(path = file.path(data_dir, "data_QoL_fin.xlsx"),
  sheet = "QoL_Parkinson",
  range = NULL,
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 1,.name_repair = "unique"
  )


source("recode_variables.R")
view_df(df_recoded)


# ==================================================================================================
## Part 1: Create TableOne for participants
source("CreateTableOne.R")

table(df_raw$comorb)
table(df_recoded$comorb_r)

## =================================================================================================
# Preamble further analysis: Data imputation using the MICE package with a multivariate approach
source("imputation.R")


# Generate and save density plot to PDF in landscape orientation
pdf(file.path(getwd(), "results", "suppl_fig1b.densityplots_afterimputation.pdf"), width = 11, height = 8.5)

densityplot(
  generate_imputation,
  xlim = c(0, 7),    # Set x-axis range for density plot
  ylim = c(0, 1)   # Set y-axis range for density plot
)

dev.off()  # Close the PDF device to save the file


## =================================================================================================
# Part 2: Exploratory factor analysis (EFA), source: https://rpubs.com/pjmurphy/758265

# 2.1: Correlation matrix
efa_vars <- c(
  "qol_ms_r", "qol_nms_r", "qol_conditions_r", "qol_gain_r", "qol_loss_independence_r", 
  "qol_unpredict_r","qol_effect_medication_r", "qol_taking_medication_r", "qol_invasive_therapy_r", 
  "qol_other_therapy_r", "qol_family_contact_r", "qol_friends_contact_r", "qol_affectp_contact_r", 
  "qol_support_yes_r", "qol_stigma_r", "qol_leisure_yes_r", "qol_finances_r", "qol_badmob_r", 
  "qol_badadl_r", "qol_sum_ident_r", "qol_feeling_needed_r", "qol_thoughts_future_r", "qol_self_image_r", 
  "qol_religion_r"
)

df_EFA <- imputed_data %>% dplyr::select(all_of(efa_vars))

# Evaluate number of missings
if (flag_check) {
	colSums(is.na(df_EFA))
	miss <- aggr(df_EFA, col=c('navyblue','red'),
		     numbers=TRUE, sortVars=TRUE,
		     labels=names(df_EFA), cex.axis=.5,
		     gap=3, ylab=c("Missing data","Pattern"))
}

# Calculate the correlation matrix and round to 2 decimal places
cor_matrix <- round(cor(df_EFA, use = "complete.obs"), 2)

 
# Set values below the threshold to NA
cor_matrix[abs(cor_matrix) <= 0.29] <- NA
cor_matrix[abs(cor_matrix) > .99] <- .01
results.cor_matrix <- cor_matrix

# 2.1.1: Visualize correlation matrix
# Retain only the lower triangle
cor_matrix[upper.tri(cor_matrix)] <- NA  # Mask the upper triangle

cor_data <- as.data.frame(as.table(cor_matrix)) %>%
  drop_na() %>%  # Remove NA values to keep only significant correlations
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)  # Rename columns for clarity

# ... a somehow bulky hack 
text_threshold <- 0.29
cor_data <- cor_data %>%
  mutate(Label = ifelse(abs(Correlation) >= text_threshold, round(Correlation, 2), NA))

var_cor_efa_y <- c(
  "qol_ms_r" = " A) motor symptoms", 
  "qol_nms_r" = "B) non-motor symptoms",
  "qol_conditions_r" = " C) health care conditions", 
  "qol_gain_r" = "D) secondary disease gain", 
  "qol_loss_independence_r" = " E) loss of independence",
  "qol_unpredict_r" = "F) unpredictability", 
  "qol_invasive_therapy_r" = "G) invasive therapy",
  "qol_other_therapy_r" = "H) other therapy", 
  "qol_effect_medication_r" = "I) effect of PD medication", 
  "qol_taking_medication_r" = "J) taking PD medication", 
  "qol_family_contact_r" = "K) contact with family",
  "qol_friends_contact_r" = "L) contact with friends", 
  "qol_affectp_contact_r" = "M) contact with other affected people", 
  "qol_support_yes_r" = "N) experience of support", 
  "qol_stigma_r" = "O) stigmatisation", 
  "qol_leisure_yes_r" = "P) leisure acitivities", 
  "qol_finances_r" = "Q) economic situation", 
  "qol_badmob_r" ="R) restrictions in mobility", 
  "qol_badadl_r" = "S) restrictions in ADL", 
  "qol_sum_ident_r" = "T) identity overall", 
  "qol_feeling_needed_r" = "U) feeling needed", 
  "qol_thoughts_future_r" = "V) thoughts about the future", 
  "qol_self_image_r" = "W) self-image", 
  "qol_religion_r" = "X) religion"
)

var_cor_efa_x <- c(
  "qol_ms_r" = " A", 
  "qol_nms_r" = "B",
  "qol_conditions_r" = " C", 
  "qol_gain_r" = "D", 
  "qol_loss_independence_r" = " E",
  "qol_unpredict_r" = "F", 
  "qol_invasive_therapy_r" = "G",
  "qol_other_therapy_r" = "H", 
  "qol_effect_medication_r" = "I", 
  "qol_taking_medication_r" = "J", 
  "qol_family_contact_r" = "K",
  "qol_friends_contact_r" = "L", 
  "qol_affectp_contact_r" = "M", 
  "qol_support_yes_r" = "N", 
  "qol_stigma_r" = "O", 
  "qol_leisure_yes_r" = "P", 
  "qol_finances_r" = "Q", 
  "qol_badmob_r" ="R", 
  "qol_badadl_r" = "S", 
  "qol_sum_ident_r" = "T", 
  "qol_feeling_needed_r" = "U", 
  "qol_thoughts_future_r" = "V", 
  "qol_self_image_r" = "W", 
  "qol_religion_r" = "X"
)

pdf(file.path(getwd(), "results", "fig1.corrEFA.pdf"), width = 11, height = 8.5)
# Plot the correlation matrix with ggplot2
ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +                             # Tile with white borders
  geom_text(aes(label = round(Label, 2)), size = 3) + # Add correlation values inside the squares
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) + # Color gradient
  scale_x_discrete(labels = var_cor_efa_x) + 
  scale_y_discrete(labels = var_cor_efa_y) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 10),  # Rotate and size x-axis labels
    axis.text.y = element_text(size = 10),                        # Size y-axis labels
    axis.title = element_blank()                                  # Remove axis titles
  ) +
  labs(title = "Correlation Matrix (Lower Triangle, Filtered, |r| > 0.29)") +
  coord_fixed()  # Ensures tiles are square
dev.off()

# Define the output PDF for saving the plot
pdf(file.path(getwd(), "results", "fig1.corrEFA.pdf"), width = 11, height = 8.5)


#TODO: Not entirely happy with the way it is plotted. Would suggest trying to get only lower triangle to improve readibility. Further comment: lower triangle is plotted but the x-axis is not necessary this way. If you can get rid of it you save space.

# 2.2: Exclude variables that don't have any correlations > 0.29
cor_matrix <- cor(df_EFA, use = "complete.obs")
mask <- abs(cor_matrix) > 0.29  # Mask to identify correlations above 0.29
diag(mask) <- FALSE             # Exclude self-correlations (diagonal)

# Identify and exclude variables with no significant correlations > 0.29
exclude_vars <- colnames(df_EFA)[colSums(mask) == 0]
data_EFA <- df_EFA[, !(colnames(df_EFA) %in% exclude_vars)]

# 2.3: Kaiser-Meyer-Olkin (KMO) Measure to retain variables with MSA > 0.50
results.kmo <- KMO(data_EFA)
data_EFA <- data_EFA[, results.kmo$MSAi > 0.50]

# 2.4: Bartlett’s Test for Sphericity to test factorability
results.bartlett <- cortest.bartlett(data_EFA)

# 2.5: Determine number of factors to extract using eigenvalues and scree plot
pdf(file.path(getwd(), "results", "suppl_fig2a.screeplotEFA.pdf"), width = 11, height = 8.5)
results.eigenvalues <- eigen(cor(data_EFA))$values  # Eigenvalues of the correlation matrix

plot(results.eigenvalues, 
	pch = 19,
	type = "b",
	ylab="Eigenvalue of factor analysis", 
	xlab="Factor number",
	main = "Scree Plot")
abline(v=4, col="red")
abline(h=1)
# scree(data_EFA, pc = FALSE)                 # Scree plot to visualize eigenvalues
dev.off()

# 2.6: Extract factors with oblique rotation
fit_oblique <- factanal(data_EFA, factors = 4, rotation = "promax")
print(fit_oblique, digits = 2, cutoff = 0.3, sort = TRUE)

# Extracted Factors:
# factor 1: social life
# factor 2: restrictions/ physical limitations/ disadvantages  
# factor 3: influence PD on identity, thoughts and feelings
# factor 4: medication for PD


# 2.7: Check for factor loading < 0.4 and exclude variables with factor loading < 0.4
# Extract the factor loadings
loadings_matrix <- fit_oblique$loadings

# Threshold for factor loadings
threshold <- 0.4

# Check if the maximum loading of an item is >= 0.4
items_to_keep <- apply(loadings_matrix, 1, function(x) any(abs(x) >= threshold))

# 2.8: Create a new dataset with the filtered items
filtered_data_EFA <- data_EFA[, items_to_keep]


colnames(filtered_data_EFA) # <- TODO: you may want to change this to meaningful names so that your table in the xlsx files is correct and your heatmap and so on


# 2.9 Perform the EFA again
fit_oblique_filtered <- factanal(filtered_data_EFA, 4, rotation="promax", scores = "regression")
print(fit_oblique_filtered, digits=2, cutoff=0.3, sort=TRUE)
results.fit_oblique <- fit_oblique_filtered


# 2.10: Export results 
# Option 1: Create a function to build a table with the information from the EFA, source: https://stackoverflow.com/questions/63856304/create-data-frame-from-efa-output-in-r 
# Step 1: Export Factor Loadings and Uniquenesses
fa_table <- function(x, cut) {
  loadings <- unclass(x$loadings) %>% as.data.frame() %>% round(2)
  loadings[abs(loadings) < cut] <- ""
  add_info <- cbind(
    commonality = x$communalities,
    uniqueness = x$uniquenesses,
    complexity = x$complexity
  ) %>% 
    as.data.frame() %>%
    rownames_to_column("item")
  
  result_table <- loadings %>%
    rownames_to_column("item") %>%
    left_join(add_info, by = "item") %>%
    mutate(across(where(is.numeric), round, 3))
  
  
  return(result_table)
}

proportion_variance <- fit_oblique_filtered$PVAL["Proportion Var",]

# Use function "fa_table" with new EFA results and cut-off (e.g. 0.3)
efa_table_final <- fa_table(fit_oblique_filtered, cut = 0.3)



# Use package "openxlsx" to save table in Excel 
wb3 <- createWorkbook()
addWorksheet(wb3, "efa_results_final")
writeData(wb3, "efa_results_final", efa_table_final)
setColWidths(wb3, "efa_results_final", cols = 1:ncol(efa_table_final), widths = "auto")
saveWorkbook(wb3, file = file.path(getwd(), "results", "efa_table_final.xlsx"), overwrite = TRUE)

# 2.11: Export Factor Correlations
extract_factor_correlations <- function(x) {
  if (!is.null(x$correlation)) {
    n_factors <- ncol(x$loadings)
    factor_correlation <- as.data.frame(round(x$correlation[1:n_factors, 1:n_factors], 3))
    rownames(factor_correlation) <- colnames(factor_correlation) <- paste0("Factor", 1:n_factors)
    return(factor_correlation)
  } else {
    message("Keine Faktorenkorrelationen vorhanden.")
    return(NULL)
  }
}

# Use function "extract_factor_correlations" with EFA results
factor_correlations_final <- extract_factor_correlations(fit_oblique_filtered)

# Create correlation matrix with row names
factor_correlation_with_rowname_final <- cbind(Factor = rownames(factor_correlations_final), factor_correlations_final)

# Use package "openxlsx" to save table in Excel 
wb4 <- createWorkbook()
addWorksheet(wb4, "efa_factor_correlations_final")
writeData(wb4, "efa_factor_correlations_final", factor_correlation_with_rowname_final)
setColWidths(wb4, "efa_factor_correlations_final", cols = 1:ncol(factor_correlation_with_rowname_final), widths = "auto")
saveWorkbook(wb4, file = file.path(getwd(), "results", "efa_factor_correlations_final.xlsx"), overwrite = TRUE)


## 3.11: Cronbach alpha (reliability analysis)
# Factor1: 
social_life <- imputed_data[,c("qol_family_contact_r", "qol_friends_contact_r", "qol_affectp_contact_r", "qol_support_yes_r")]
# Factor4:
restrictions <- imputed_data[,c("qol_sum_disease_r","qol_loss_independence_r", "qol_badmob_r", "qol_badadl_r", "qol_nms_r", "qol_unpredict_r")]
# Factor3:
medication <- imputed_data[,c("qol_effect_medication_r", "qol_taking_medication_r")]
# Factor2:
identity <- imputed_data[,c("qol_sum_ident_r", "qol_thoughts_future_r", "qol_self_image_r")]

results.cronbach.social_life <- psych::alpha(social_life)
results.cronbach.social_life
results.cronbach.restrictions <- psych::alpha(restrictions)
results.cronbach.restrictions
results.cronbach.medication <- psych::alpha(medication)
results.cronbach.medication
results.cronbach.identity <- psych::alpha(identity)
results.cronbach.identity

# TODO: You need this later? You want to keep this as xlsx?

## 3.12: Extracting factor scores for regression
factor_scores <- fit_oblique_filtered$scores

if (flag_check==TRUE) {
	print(factor_scores)
}

# 2.13 Visualization of EFA
# Heat map: Shows Magnitude of the Variable Loadings onto the Factors and shows Division into Four Factors
pdf(file.path(getwd(), "results", "fig2b.heatmapEFA.pdf"), width = 11, height = 8.5)
heatmap.2(fit_oblique_filtered$loadings,
          cexRow = 1,        
          cexCol = 1.5,        
          srtCol = 45,         
          key = TRUE, 
          margins = c(10, 15),      
          lhei = c(1, 3),       
          lwid = c(1, 3),
          main = "Intensity of Factor Loading"
)
dev.off()

# 2.13.2 Show variables loading on factors (source: https://rpubs.com/pjmurphy/758265)

pdf(file.path(getwd(), "results", "fig2c.overview.pdf"), width = 11, height = 8.5)
loads <- fit_oblique_filtered$loadings
# TODO: As before, you may want to give your factors more meaningful names, using something like colnames(loads)<-c("Social life", "Physical Restrictions", "Influence on identity", "PD medication")
fa.diagram(loads)
dev.off()


# 2.13.3 Biplot: Create a pair plot matrix (all possible combinations of the factors are plotted against each other)

# Option 1: every variable has a different colored dot and there is a chart legend in a different pdf file
# Frist, the plot
pdf(file.path(getwd(), "results", "fig2d.pairplot.pdf"), width = 11, height = 8.5)


loadings <- fit_oblique_filtered$loadings
variable_names <- rownames(fit_oblique_filtered$loadings)
color_palette <- rainbow(nrow(loadings))

pairs(loadings, 
      main="Pair Plot of Factor Loadings", 
      pch=19,         
      col=color_palette,  
      labels=colnames(loadings),  
      cex.labels=0.8) 

dev.off()

# Second, the chart legend
pdf(file.path(getwd(), "results", "fig2d.legend.pdf"), width = 3, height = 4)

# Create an empty plot without axes and grid lines
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), 
     xaxt = "n", yaxt = "n", bty = "n") 

legend("center", 
       legend = variable_names,  
       col = color_palette,      
       pch = 19,                 
       title = "Variables",      
       cex = 0.7,                
       ncol = 1,                 
       xpd = TRUE,               
       bty = "o",                
       box.lwd = 1,              
       box.col = "black")        

dev.off()

# Changing colors? 
# Color families for the factors
factor_colors <- list(
  Factor1 = colorRampPalette(c("lightgreen", "darkgreen"))(5),  
  Factor2 = colorRampPalette(c("lightgray", "darkgray"))(4),   
  Factor3 = colorRampPalette(c("lightblue", "darkblue"))(4),   
  Factor4 = colorRampPalette(c("lightcoral", "darkred"))(4)    
)

# Loadings and variables
loadings <- fit_oblique_filtered$loadings
variable_names <- rownames(loadings)

## Initialization
color_palette <- rep(NA, nrow(loadings))  # Placeholder for the colors
color_counters <- list(Factor1 = 0, Factor2 = 0, Factor3 = 0, Factor4 = 0)  # Counters for colors

# Iteration over the rows of loadings
for (i in 1:nrow(loadings)) {
  factor_index <- which.max(abs(loadings[i, ]))  # Factor with the highest loading
  factor_name <- colnames(loadings)[factor_index]  # Name of the factor
  
  # Update counter and assign color
  color_counters[[factor_name]] <- color_counters[[factor_name]] + 1
  color_palette[i] <- factor_colors[[factor_name]][color_counters[[factor_name]]]
}

# Plot with the customized color scheme
pdf(file.path(getwd(), "results", "fig2d.pairplot.pdf"), width = 11, height = 8.5)

pairs(loadings, 
      main = "Pair Plot of Factor Loadings", 
      pch = 19,          
      col = color_palette,  
      labels = colnames(loadings),  
      cex.labels = 1.5,  
      font.labels = 2)   

dev.off()

# Create the legend
pdf(file.path(getwd(), "results", "fig2d.legend.pdf"), width = 3, height = 4)

# Initialize an empty plot
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), 
     xaxt = "n", yaxt = "n", bty = "n") 

legend("center", 
       legend = variable_names,  
       col = color_palette,     
       pch = 19,                 
       title = "Variables",      
       cex = 0.7,                
       ncol = 1,                 
       xpd = TRUE,               
       bty = "o",                
       box.lwd = 1,              
       box.col = "black")        


dev.off()


# Option 2: All Variable loading on one factor have one color and names of the variables are written inside the plot
pdf(file.path(getwd(), "results", "fig2e.pairplot.pdf"), width = 15, height = 10.5)
fa.plot(fit_oblique_filtered, 
        labels = rownames(fit_oblique_filtered$loadings), 
        cex = 0.8, 
        main = "Pair Plot of Factor Analysis")
dev.off()

# TODO: This one is a bit hard to digest! But you just need 1 of them, pick one and don't put it in the supplementary files, this is the second result (1. TableOne, 2. EFA)

## =================================================================================================
# Part 3: Odds Ratios: 
# Old: (How much more likely (or less likely) is an 'extremely or very negative' rating of the impact of Parkinson's disease on quality of life (QoL) across groups defined by different demographic and health-related characteristics?)
# Please don't delete the comments marked with 'old' yet.

# Preamble merging dataframes and checking for multicollinearity
imputed_data_with_scores <- bind_cols(imputed_data, factor_scores)

dependent_variable = "qol_sum_disease_r"
factorsOR1 <- c("sex_r", "comorb_r", "disturbances_sleep_APD", 
                "age_r", "education_r_r", "time_from_diagnosis_r", "family_status_r_r", 
                "living_area_r", "who5_depr_r", "ms_fluctuation_APD", "falls_r", 
                "Factor1", "Factor2", "Factor3", "Factor4")

# Extract data of interest into new dataframe and estimate (isolated) OR to the dv ("needed_healthcare_but_did_not_receive_it_duringCovid.C4")
df_OR1_complete <- imputed_data_with_scores %>% dplyr::select(all_of(c(dependent_variable, factorsOR1))) # create dataframe with data of interest, that is dv and regressors

# Old: (Does PD has an extremely or very negative influence on overall QoL?)
# Old: ((Yes = 1,2, No = 3 (somewhat negative), 4 (not at all), 5,6,7 = positive))

# How much more likely is a more positive rating of QoL (every answer except "extremly or very negative") depending on different demographic and health-related characteristics?
# (extremely or very negative = 1,2, more positive = 3,4,5)

df_OR1_complete <- df_OR1_complete %>%
  mutate(
    dv = fct_collapse(
      as.factor(qol_sum_disease_r),
      "negative" = c("1", "2"),
      "positive" = c("3", "4", "5"))) %>%
  filter(!is.na(dv))



# Get Odds ratios for every factor of interest	  

convert_to_integer <- function(df, vars) {
  for (var in vars) {
    df[[var]] <- as.integer(df[[var]])
  }
  return(df)
}

var_integer <- c("sex_r", "comorb_r", "disturbances_sleep_APD", 
                 "age_r", "education_r_r", "time_from_diagnosis_r", "family_status_r_r", 
                 "living_area_r", "who5_depr_r", "ms_fluctuation_APD", "falls_r")

df_OR1_complete <- convert_to_integer(df_OR1_complete, var_integer)


results1 = c()
for (fac in factorsOR1) { # for loop over factors of interest
  mod <- as.formula(sprintf("I(dv=='positive') ~ %s", fac)) # formula for (unadjusted) GLM
  fit_temp = glm(mod, data=df_OR1_complete, family="binomial") # estimate model
  results1 = rbind(	results1, c(exp(coef(fit_temp)[2]), 	# OR
                                exp(confint.default(fit_temp)[2]),  	# lower CI
                                exp(confint.default(fit_temp)[4]),  	# upper CI
                                summary(fit_temp)$coefficients[2,4])) 	# significance value
}

results_OR1 <- data.frame(yAxis = length(factorsOR1):1, 
                          factors=factorsOR1,
                          boxOdds = results1[,1], 
                          boxCILow = results1[,2],
                          boxCIHigh = results1[,3],
                          pvalue=results1[,4])
results_OR1 <- results_OR1 %>% mutate(significance = case_when(pvalue <= .001 ~ 'p < .001', 
                                                               (pvalue < .05 & pvalue >.001) ~ 'p < .05',
                                                               pvalue > .05 ~ 'ns')) %>% 
  mutate(dot_color = case_when(pvalue <= .001 ~ "#6B340D", 
                               (pvalue < .05 & pvalue >.001) ~ "#B8A31F",
                               pvalue > .05 ~ "#08306B"))

results_OR1


# Visualization of (unadjusted) OR results
# comment: haven't changed caption and title yet 

fac_names <- c("Gender*", "Comorbidity", "Sleep disturbances", 
               "Age", "Highes level of education", "Time since diagnosis", "Family status", 
               "Living area","Depressed mood", "Fluctuation of motor symptoms", "Falls", 
               "Factor1", "Factor2", "Factor3", "Factor4")

results_OR1$factors <- fac_names
results_OR1$significance <- factor(results_OR1$significance, levels = c('ns', 'p < .05', 'p < .001'))


pdf(file.path(getwd(), "results", "test.pdf"), width = 11, height = 8.5)
fontsize = 15
results_OR1 %>%
  dplyr::arrange(boxOdds) %>%
  mutate(factors = factor(factors, levels = factors)) %>%  # Update der Faktorenlevels
  ggplot(aes(x = boxOdds, y = factors)) +
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color = "grey") +  # vertikale Linie für OR = 1
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +  # Fehlerbalken
  geom_point(aes(x = boxOdds, y = factors, color = significance), size = 1.5, show.legend = TRUE) +  # Punkte basierend auf Signifikanz
  guides(colour = guide_legend(reverse = TRUE)) +  # Legende umkehren
  scale_colour_manual(values = c("p < .001" = "#6B340D", "p < .05" = "#B8A31F", "ns" = "#08306B")) +
  theme_minimal() +
  theme(text = element_text(size = 12),
        panel.grid.minor = element_blank(), 
        legend.position = c(0.9, 0.1), legend.title=element_blank(), legend.box.background = element_rect(colour = "black"),
        legend.background = element_rect(colour = "black", fill="white"),
        legend.text = element_text(size = fontsize),
        plot.title = element_text(vjust = 4, hjust = .5, color = "black", size = fontsize, face = "bold"),
        axis.line.x = element_line(linewidth = .25, colour = "black"), 
        axis.ticks.x = element_line(linewidth = .25, colour = "black"), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = fontsize),
        plot.caption = element_text(hjust = 0, face="italic"), 
        plot.margin = margin(t = 20, r = 0, b = 0, l = 0, unit = "pt")) +
  coord_capped_cart(bottom='right') + 
  theme(panel.grid.minor = element_blank(), 
        legend.position = c(0.9, 0.2), 
        legend.title = element_blank()) +  
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 40), limits = c(0.08, 40)) +  
  ylab("") +
  xlab("Odds ratio (log scale)") + 
  labs(title = "PD-patients' odds of extremely or very negative impact of PD on overall QoL",
       caption = "*Gender was coded so that OR > 1 means that being female indicates a higher risk of negative impact of PD on QoL")
dev.off()
#Reminder: adjust caption if necessary


## =======================================================================================================
## Part 4: Visualization Health care Conditions, Part D and E of the questionnaire
## =============================================================================
## Conditions healthcare

# Variables representing healthcare conditions with positive influence
conditions1_vars <- c("C029_01", "C029_02", "C029_03", "C029_04", "C029_05", "C029_06", "C029_07")
conditions2_vars <- c("C030_01", "C030_02", "C030_03", "C030_04", "C030_05", "C030_06", "C030_07")

# Counting when the condition was marked as having a positive influence (Value = 2)
conditions1_count <- sapply(df_recoded[, conditions1_vars], function(x) sum(x == 2, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df1 <- data.frame(conditions1 = names(conditions1_count), quantity1 = conditions1_count)

# Counting when the condition was marked as having a negative influence (Value = 2)
conditions2_count <- sapply(df_recoded[, conditions2_vars], function(x) sum(x == 2, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df2 <- data.frame(conditions2 = names(conditions2_count), quantity2 = conditions2_count)

pdf(file.path(getwd(), "results", "suppl_fig4a.conditpos.pdf"), width = 8, height = 5.5)
# Creating a bar plot for positive healthcare conditions (value 2 in C029_ variables)
ggplot(df1, aes(x = conditions1, y = quantity1)) +
  geom_bar(stat = "identity", fill ="cadetblue") +
  labs(x = "Positive Conditions", y = "Count", 
       title = "Positive Healthcare Conditions", subtitle = "Conditions reported to positively impact Parkinson's patients") +
  scale_x_discrete(labels = c("C029_01" = "Teamwork \nbetween HCP", "C029_02" = "Quick appointments", "C029_03" = "Good communication \nwith physicans",
                              "C029_04" = "Good availability of \nHCP in region", "C029_05" = "Available transportation \noptions", "C029_06" = "Option to receive \ncare in a home setting", 
                              "C029_07" = "Financial support")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Rotate x-axis labels for clarity
dev.off()

pdf(file.path(getwd(), "results", "suppl_fig4b.conditneg.pdf"), width = 8, height = 5.5)
# Creating a bar plot for negative healthcare conditions (value 2 in C030_ variables)
ggplot(df2, aes(x = conditions2, y = quantity2)) +
  geom_bar(stat = "identity", fill ="cadetblue") +
  labs(x = "Negative Conditions", y = "Count", 
       title = "Negative Healthcare Conditions", subtitle = "Conditions reported to negatively impact Parkinson's patients") +
  scale_x_discrete(labels = c("C030_01" = "No teamwork \nbetween HCP", "C030_02" = "Long waiting times \nfor appointments", "C030_03" = "Bad communication \nwith physicans",
                              "C030_04" = "Bad availability of \nHCP in region", "C030_05" = "Long travel \ndistances to HCP", "C030_06" = "No option to receive \ncare in a home setting", 
                              "C030_07" = "High costs")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Rotate x-axis labels for clarity
dev.off()

## ============================================================================
## Part D

### Positive impact: Which QoL-Domains have the most positive impact on overall QoL? (Participants selected 1st, 2nd and 3rd place)

# Most important positive aspects - 1st place
# Variables representing QoL-Domains
pos_vars <- c("D002_02","D002_22","D002_03","D002_04","D002_06","D002_08","D002_09",
              "D002_10","D002_11","D002_12","D002_14","D002_15","D002_16","D002_17",
              "D002_18","D002_19","D002_20","D002_21")

# Counting when the LQ domain was chosen as most important (1st place = 1)
pos1_count <- sapply(df_recoded[, pos_vars], function(x) sum(x == 1, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df3 <- data.frame(pos1 = names(pos1_count), quantity3 = pos1_count)


partD1 <- ggplot(df3, aes(x = pos1, y = quantity3, fill = pos1)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects 1st place - Positive Effect", subtitle = "Categories chosen as most important (1st place) for a positive impacte on the QoL") +
  scale_fill_manual(values = c("D002_02" = "deeppink4"), name="1st place", breaks=c("D002_02"), labels= c("Effect of PD \nmedication")) +
  scale_x_discrete("categories", labels = c("D002_02" = "Effect PD medication", "D002_22" = "Invasive therapy", 
                                            "D002_03" = "Other Therapy", "D002_04" = "Health-care conditions", 
                                            "D002_06" = "Secondary disease gain", "D002_08" = "Independence",
                                            "D002_09" = "Contact with family", "D002_10" = "Contact with friends", 
                                            "D002_11" = "Contact with other affected people",
                                            "D002_12"= "Support","D002_14" = "ADL",
                                            "D002_15" = "Leisure activities","D002_16" ="Finances",
                                            "D002_17" = "Mobility","D002_18" = "Feeling needed",
                                            "D002_19" = "Self-image","D002_20" = "Future perspective",
                                            "D002_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))


# Most important positive aspects - 2nd place
# Counting when the LQ domain was chosen as second most important (2nd place = 2)
pos2_count <- sapply(df_recoded[, pos_vars], function(x) sum(x == 2, na.rm = TRUE))


# Convert frequency counts into a DataFrame for visualization with ggplot
df4 <- data.frame(pos2 = names(pos2_count), quantity4 = pos2_count)


partD2 <- ggplot(df4, aes(x = pos2, y = quantity4, fill = pos2)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects 2nd place - Positive Effect", subtitle = "Categories chosen as second most important (2nd place) for a positive impact on QoL") +
  scale_fill_manual(values = c("D002_09" = "deeppink3"), name="2nd place", breaks=c("D002_09"), labels= c("Contact with \nfamily")) +
  scale_x_discrete("categories", labels = c("D002_02" = "Effect PD medication", "D002_22" = "Invasive therapy", 
                                            "D002_03" = "Other Therapy", "D002_04" = "Health-care conditions", 
                                            "D002_06" = "Secondary disease gain", "D002_08" = "Independence",
                                            "D002_09" = "Contact with family", "D002_10" = "Contact with friends", 
                                            "D002_11" = "Contact with other affected people",
                                            "D002_12"= "Support","D002_14" = "ADL",
                                            "D002_15" = "Leisure activities","D002_16" ="Finances",
                                            "D002_17" = "Mobility","D002_18" = "Feeling needed",
                                            "D002_19" = "Self-image","D002_20" = "Future perspective",
                                            "D002_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))



# Most important positive aspects - 3rd place
# Counting when the LQ domain was chosen as third most important (3rd place = 3)
pos3_count <- sapply(df_recoded[, pos_vars], function(x) sum(x == 3, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df5 <- data.frame(pos3= names(pos3_count), quantity5 = pos3_count)

partD3 <- ggplot(df5, aes(x = pos3, y = quantity5, fill = pos3)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects 3rd place - Positive Effect", subtitle = "Categories chosen as third most important (3rd place) for a positive impact on QoL") +
  scale_fill_manual(values = c("D002_02" = "deeppink2"), name="3rd place", breaks=c("D002_02"), labels= c("Effect of PD \nmedication")) +
  scale_x_discrete("categories", labels = c("D002_02" = "Effect PD medication", "D002_22" = "Invasive therapy", 
                                            "D002_03" = "Other Therapy", "D002_04" = "Health-care conditions", 
                                            "D002_06" = "Secondary disease gain", "D002_08" = "Independence",
                                            "D002_09" = "Contact with family", "D002_10" = "Contact with friends", 
                                            "D002_11" = "Contact with other affected people",
                                            "D002_12"= "Support","D002_14" = "ADL",
                                            "D002_15" = "Leisure activities","D002_16" ="Finances",
                                            "D002_17" = "Mobility","D002_18" = "Feeling needed",
                                            "D002_19" = "Self-image","D002_20" = "Future perspective",
                                            "D002_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

# Most important positive aspects - overall
# Counting when the LQ domain was chosen as most important overall (= 1,2,3)
posfin_count <- sapply(df_recoded[, pos_vars], function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df6 <- data.frame(posfin= names(posfin_count), quantity6 = posfin_count)


partD4 <- ggplot(df6, aes(x = posfin, y = quantity6, fill = posfin)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects overall - Positive Effect", subtitle = "Categories, which were chosen as 1rd, 2nd or 3rd place") +
  scale_fill_manual(values = c("D002_02" ="deepskyblue4", "D002_09" = "deepskyblue3", "D002_08" = "deepskyblue2"), name="TOP 3", breaks=c("D002_02", "D002_09", "D002_08"), labels= c("Effect of PF \nmedication", "Contact with \nfamily", "Independence \ndespite PD")) +
  scale_x_discrete("categories", labels = c("D002_02" = "Effect PD medication", "D002_22" = "Invasive therapy", 
                                            "D002_03" = "Other Therapy", "D002_04" = "Health-care conditions", 
                                            "D002_06" = "Secondary disease gain", "D002_08" = "Independence",
                                            "D002_09" = "Contact with family", "D002_10" = "Contact with friends", 
                                            "D002_11" = "Contact with other affected people",
                                            "D002_12"= "Support","D002_14" = "ADL",
                                            "D002_15" = "Leisure activities","D002_16" ="Finances",
                                            "D002_17" = "Mobility","D002_18" = "Feeling needed",
                                            "D002_19" = "Self-image","D002_20" = "Future perspective",
                                            "D002_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

pdf(file.path(getwd(), "results", "suppl_fig5a.partDpos.pdf"), width = 11, height = 8.5)
ggarrange(partD1, partD2, partD3, partD4,
          ncol = 2, nrow = 2,
          labels = "AUTO",
          font.label = list (size = 25, color="black", face="bold"))
dev.off()


## Negative impact: Which QoL-Domains have the most negative impact on overall QoL? (Participants selected 1st, 2nd and 3rd place)

# Variables representing LQ domains
neg_vars <- c("D003_01","D003_02","D003_22","D003_23","D003_03","D003_04","D003_07",
              "D003_08","D003_09","D003_10","D003_11","D003_12","D003_13","D003_14",
              "D003_15","D003_16","D003_17","D003_18","D003_19","D003_20","D003_21")

# Most important negative aspects - 1st place
# Counting when the LQ domain was chosen as most negative important (1st place = 1)
neg1_count <- sapply(df_recoded[, neg_vars], function(x) sum(x == 1, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df7 <- data.frame(neg1= names(neg1_count), quantity7 = neg1_count)

partD5 <- ggplot(df7, aes(x = neg1, y = quantity7, fill = neg1)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects 1st place - Negative Effect", subtitle = "Categories chosen as most important (1st place) for a negative impacte on the QoL") +
  scale_fill_manual(values = c("D003_01" = "deeppink4"), name="1st place", breaks=c("D003_01"), labels= c("Symptoms \nof PD")) +
  scale_x_discrete("categories", labels = c("D003_01" = "Symptoms of PD","D003_02" = "Effect of PD medication",
                                            "D003_22" = "Taking PD medication","D003_23" = "Invasive therapy",
                                            "D003_03" = "Other therapy","D003_04" = "Health-care conditions",
                                            "D003_07" = "Unpredictability of PD","D003_08" = "Lack of independence",
                                            "D003_09" = "(Lack of) contact with family","D003_10" = "(Lack of) contact with friends",
                                            "D003_11" ="(Lack of) contact with other patients","D003_12" = "Lack of support",
                                            "D003_13" ="Stigmatisation","D003_14" = "Restrictions in ADL",
                                            "D003_15" ="Leisure activities","D003_16" = "Finances",
                                            "D003_17" = "Mobility","D003_18" = "Not feeling needed",
                                            "D003_19" ="Self-image","D003_20" = "Future perspective",
                                            "D003_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

# Most important negative aspects - 2nd place
# Counting when the LQ domain was chosen as second most negative important (2nd place = 2)
neg2_count <- sapply(df_recoded[, neg_vars], function(x) sum(x == 2, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df8 <- data.frame(neg2= names(neg2_count), quantity8 = neg2_count)

partD6 <- ggplot(df8, aes(x = neg2, y = quantity8, fill = neg2)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects 1st place - Negative Effect", subtitle = "Categories chosen as most important (2nd place) for a negative impacte on the QoL") +
  scale_fill_manual(values = c("D003_07" = "deeppink3"), name="2nd place", breaks=c("D003_07"), labels= c("Unpredictability \nof PD")) +
  scale_x_discrete("categories", labels = c("D003_01" = "Symptoms of PD","D003_02" = "Effect of PD medication",
                                            "D003_22" = "Taking PD medication","D003_23" = "Invasive therapy",
                                            "D003_03" = "Other therapy","D003_04" = "Health-care conditions",
                                            "D003_07" = "Unpredictability of PD","D003_08" = "Lack of independence",
                                            "D003_09" = "(Lack of) contact with family","D003_10" = "(Lack of) contact with friends",
                                            "D003_11" ="(Lack of) contact with other patients","D003_12" = "Lack of support",
                                            "D003_13" ="Stigmatisation","D003_14" = "Restrictions in ADL",
                                            "D003_15" ="Leisure activities","D003_16" = "Finances",
                                            "D003_17" = "Mobility","D003_18" = "Not feeling needed",
                                            "D003_19" ="Self-image","D003_20" = "Future perspective",
                                            "D003_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

# Most important negative  aspects - 3rd place
# Counting when the LQ domain was chosen as third most negative important (3rd place = 3)
neg3_count <- sapply(df_recoded[, neg_vars], function(x) sum(x == 3, na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df9 <- data.frame(neg3= names(neg3_count), quantity9 = neg3_count)

partD7 <- ggplot(df9, aes(x = neg3, y = quantity9, fill = neg3)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects 1st place - Negative Effect", subtitle = "Categories chosen as most important (3rd place) for a negative impacte on the QoL") +
  scale_fill_manual(values = c("D003_20" = "deeppink2"), name="3rd place", breaks=c("D003_20"), labels= c("Thoughts \nabout the future")) +
  scale_x_discrete("categories", labels = c("D003_01" = "Symptoms of PD","D003_02" = "Effect of PD medication",
                                           "D003_22" = "Taking PD medication","D003_23" = "Invasive therapy",
                                           "D003_03" = "Other therapy","D003_04" = "Health-care conditions",
                                           "D003_07" = "Unpredictability of PD","D003_08" = "Lack of independence",
                                           "D003_09" = "(Lack of) contact with family","D003_10" = "(Lack of) contact with friends",
                                           "D003_11" ="(Lack of) contact with other patients","D003_12" = "Lack of support",
                                           "D003_13" ="Stigmatisation","D003_14" = "Restrictions in ADL",
                                           "D003_15" ="Leisure activities","D003_16" = "Finances",
                                           "D003_17" = "Mobility","D003_18" = "Not feeling needed",
                                           "D003_19" ="Self-image","D003_20" = "Future perspective",
                                           "D003_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

# Most important negative aspects - overall
#Counting when the LQ domain was chosen as third most negative important (3rd place = 3)
negfin_count <- sapply(df_recoded[, neg_vars], function(x) sum(x %in% c(1, 2, 3), na.rm = TRUE))

# Convert frequency counts into a DataFrame for visualization with ggplot
df10 <- data.frame(negfin= names(negfin_count), quantity10 = negfin_count)

partD8 <- ggplot(df10, aes(x = negfin, y = quantity10, fill = negfin)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Count", title = "Most Important QoL Aspects overall - Negative Effect", subtitle = "Categories, which were chosen as 1rd, 2nd or 3rd place") +
  scale_fill_manual(values = c("D003_07" ="deepskyblue4", "D003_01" = "deepskyblue3", "D003_20" = "deepskyblue2"), name="TOP 3", breaks=c("D003_07", "D003_01", "D003_20"), labels= c("Unpredictability \nof PD", "Symptoms \nof PD", "Thoughts \nabout the future")) +
  scale_x_discrete("categories", labels = c("D003_01" = "Symptoms of PD","D003_02" = "Effect of PD medication",
                                            "D003_22" = "Taking PD medication","D003_23" = "Invasive therapy",
                                            "D003_03" = "Other therapy","D003_04" = "Health-care conditions",
                                            "D003_07" = "Unpredictability of PD","D003_08" = "Lack of independence",
                                            "D003_09" = "(Lack of) contact with family","D003_10" = "(Lack of) contact with friends",
                                            "D003_11" ="(Lack of) contact with other patients","D003_12" = "Lack of support",
                                            "D003_13" ="Stigmatisation","D003_14" = "Restrictions in ADL",
                                            "D003_15" ="Leisure activities","D003_16" = "Finances",
                                            "D003_17" = "Mobility","D003_18" = "Not feeling needed",
                                            "D003_19" ="Self-image","D003_20" = "Future perspective",
                                            "D003_21" = "Religion")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

pdf(file.path(getwd(), "results", "suppl_fig5b.partDneg.pdf"), width = 11, height = 8.5)
ggarrange(partD5, partD6, partD7, partD8,
          ncol = 2, nrow = 2,
          labels = "AUTO",
          font.label = list (size = 25, color="black", face="bold"))
dev.off()


## Part E: Missing LQ Domains (answers of open text question categorized)

pdf(file.path(getwd(), "results", "suppl_fig6.partE.pdf"), width = 8, height = 5.5)
data_partE <- data.frame(
  partE = c("sport","coping mechanism","research","sexual life","profession","travel","pets","alternative medicine", "psychotherapeutic support"), 
  quantity = c(6, 5, 4, 4, 3, 2, 1, 1,2))
ggplot(data_partE, aes (x = partE, y = quantity, fill = quantity)) +
  geom_bar(stat="identity")+
  labs(x = "Categories", y = "Count", title = "Part E", subtitle = "Additional QoL domains based on the answers in part E") +
  coord_flip() 
dev.off()

## =================================================================================================
# (Part 3:  Ordinal logistic regression)

# Preamble merging dataframes and checking for multicollinearity
imputed_data_with_scores <- bind_cols(imputed_data, factor_scores)

# 3.1: Define and convert selected variables to numeric
var_indep <- c("sex_r", "comorb_r", "disturbances_sleep_APD", 
               "age_r", "education_r_r", "time_from_diagnosis_r", "family_status_r_r", 
               "living_area_r", "who5_depr_r", "ms_fluctuation_APD", "falls_r", 
               "Factor1", "Factor2", "Factor3", "Factor4")
imputed_data_with_scores <- imputed_data_with_scores %>%
  mutate_at(vars(one_of(var_indep)), as.numeric)

# 3.2: Calculate correlation matrix using Kendall's method and convert to long format
subset_cor <- imputed_data_with_scores %>% dplyr::select(all_of(var_indep))
korr_tab <- cor(subset_cor, use = "pairwise.complete.obs", method = "kendall")

cor_data <- as.data.frame(as.table(korr_tab)) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)

# 3.3: Add a column to highlight correlations above the threshold
highlight_threshold <- 0.8
cor_data <- cor_data %>%
  mutate(Highlight = ifelse(abs(Correlation) > highlight_threshold, "Above Threshold", "Below Threshold"))

# 3.4: Plot the correlation matrix with ggplot2, highlighting correlations above the threshold

var_labels_regression <- c(
  "sex_r" = "gender", 
  "age_r" = "age",
  "comorb_r" = "comorbidity", 
  "disturbances_sleep_APD" = "sleep disturbances", 
  "education_r_r" = "highest educational qualification",
  "time_from_diagnosis_r" = "time since PD diagnosis", 
  "family_status_r_r" = "family status",
  "living_area_r" = "living area", 
  "who5_depr_r" = "depressed mood", 
  "ms_fluctuation_APD" = "motor symptoms fluctuation", 
  "falls_r" = "falls",
  "Factor1" = "Factor 1", 
  "Factor2" = "Factor 2", 
  "Factor3" = "Factor 3", 
  "Factor4" = "Factor 4"
)

pdf(file.path(getwd(), "results", "suppl_fig3a.corrIndepVar.pdf"), width = 11, height = 8.5)
ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +                                   # Create tiles for each correlation pair
  geom_text(aes(label = round(Correlation, 2), color = Highlight), size = 3) + # Color text for highlighted correlations
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +  # Color gradient for correlations
  scale_color_manual(values = c("Above Threshold" = "black", "Below Threshold" = "grey50")) + # Text color for highlight
  labs(title = "Correlation Matrix with Highlights", x = "", y = "") +
  scale_x_discrete(labels = var_labels_regression) + 
  scale_y_discrete(labels = var_labels_regression) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

#TODO: I would only keep the lower triangle since both are symmetric. I would get rid of the diagonal by zeroing it out and you don't really need the highlights, since there is no "above or below threshold", is there?

## ================================================
# Variables coded as ordinal
var_ord <- c("age_r", "time_from_diagnosis_r", 
             "living_area_r", "education_r_r")

for (variable in var_ord) {
  imputed_data_with_scores[[variable]] <-  factor(imputed_data_with_scores[[variable]], ordered = TRUE)
}

# Variables coded as nominal
var_fac <- c("sex_r", "family_status_r_r", "falls_r", "comorb_r", "disturbances_sleep_APD",
             "ms_fluctuation_APD")

for (variable in var_fac) {
  imputed_data_with_scores[[variable]] <-  as.factor(imputed_data_with_scores[[variable]])
}

## ================================================
# Interaction
# Performing moderator analysis with all variables and their interaction to find significant interactions, which we can include in our final model
# source: https://stats.stackexchange.com/questions/497318/how-to-perform-moderator-analysis-in-ordinal-logistic-regression


imputed_data_with_scores$qol_sum_disease_r <- as.factor(imputed_data_with_scores$qol_sum_disease_r)

# List of independent variables
independent_vars_interaction <- c("sex_r", "comorb_r", "age_r",
                                  "living_area_r", "who5_depr_r", "ms_fluctuation_APD",
                                  "Factor1", "Factor2", "Factor3", "Factor4")

# Formula for the clm model with all interactions
formula <- as.formula(paste("qol_sum_disease_r ~ (", paste(independent_vars_interaction, collapse = " + "), ")^2"))

# Fitting the model with clm and all interactions
interaction_model <- clm(formula, data = imputed_data_with_scores, Hess = TRUE, link =c("logit"))

# Summary of the model
summary(interaction_model)


## =================================================
# Ordinal logistic regression with polr: Does Parkinsons Disease influence your quality of life?

imputed_data_with_scores$qol_sum_disease_r <- as.factor(imputed_data_with_scores$qol_sum_disease_r)
m1 <- polr(qol_sum_disease_r ~ sex_r + age_r + ms_fluctuation_APD + comorb_r + 
             living_area_r + who5_depr_r + Factor1 + Factor2 + Factor3 + Factor4 +
             sex_r:comorb_r + sex_r:living_area_r + comorb_r:age_r + age_r:ms_fluctuation_APD +
             age_r:Factor3 + living_area_r:ms_fluctuation_APD + living_area_r:Factor2 + 
             living_area_r:Factor3 + Factor3:Factor4,
           data = imputed_data_with_scores, Hess = TRUE)


summary(m1)

#creating p values
##store table
(ctable <- coef(summary(m1)))

## calculate and store p values
p1 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p1))
## odds ratios
(ci <- confint(m1))
confint.default(m1) #konfidenzindervalle
exp(coef(m1))
exp(cbind(OR = coef(m1), ci))

# Ruling out multicollinearity (again)
vif(m1)

# Testing proportional odds assumption 
brant.test(m1)

# Calculating pseudo R^2 to evaluate goodness of fit
pR2(m1)

## ================================================
#Stepwise model selection by AIC
#source: https://rforpoliticalscience.com/2020/10/23/choose-model-variables-by-aic-in-a-stepwise-algorithm-with-the-mass-package-in-r/
m1_simplified <- stepAIC(m1, direction = "both")
summary(m1_simplified)

#creating p values
##store table
(ctable_m1_simplified <- coef(summary(m1_simplified)))

## calculate and store p values
p1_m1_simplified <- pnorm(abs(ctable_m1_simplified[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable_m1_simplified <- cbind(ctable_m1_simplified, "p value" = p1_m1_simplified))
## odds ratios
(ci_m1_simplified <- confint(m1_simplified))
confint.default(m1_simplified) #konfidenzindervalle
exp(coef(m1_simplified))
exp(cbind(OR = coef(m1_simplified), ci_m1_simplified))

# Ruling out multicollinearity (again)
vif(m1_simplified)

# Testing proportional odds assumption 
brant.test(m1_simplified)

# Calculating pseudo R^2 to evaluate goodness of fit
pR2(m1_simplified)



