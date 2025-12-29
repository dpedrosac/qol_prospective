#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Exploratory Factor Analysis (EFA) of QoL Variables in Parkinson’s Disease
#
# Description:
#   Performs exploratory factor analysis (EFA) on QoL questionnaire variables
#   in a prospective Parkinson’s disease study. Includes correlation screening,
#   factor extraction (oblique rotation), iterative item filtering, reliability
#   analysis, and export of results and figures to the results/ directory.
#
# Authors:
#   Leonie Moormann
#   David Pedrosa
#
# Last updated:
#   2024-12-25
#
# R version:
#   >= 4.3.1
# -------------------------------------------------------------------------

## =================================================================================================
# Part 2: Exploratory factor analysis (EFA), source: https://rpubs.com/pjmurphy/758265

# Visualization controls
plot_corr <- TRUE                # master switch for any correlation visualization
corr_plot_type <- "heatmap"      # "pairs" or "heatmap"
corr_threshold <- 0.29           # threshold used for filtering correlations
label_threshold <- 0.30          # label threshold (only used in pair plot)


# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

efa_config <- list(
  n_factors = 4,
  corr_threshold = 0.29,
  loading_threshold = 0.40,
  factor_assignment_threshold = 0.30
)

efa_vars <- c(
  "qol_ms_r", "qol_nms_r", "qol_conditions_r", "qol_gain_r", "qol_loss_independence_r",
  "qol_unpredict_r", "qol_effect_medication_r", "qol_taking_medication_r", "qol_invasive_therapy_r",
  "qol_other_therapy_r", "qol_family_contact_r", "qol_friends_contact_r", "qol_affectp_contact_r",
  "qol_support_yes_r", "qol_stigma_r", "qol_leisure_yes_r", "qol_finances_r", "qol_badmob_r",
  "qol_badadl_r", "qol_sum_ident_r", "qol_feeling_needed_r", "qol_thoughts_future_r",
  "qol_self_image_r", "qol_religion_r"
)

label_map <- c(
  qol_ms_r = "motor symptoms",
  qol_nms_r = "non-motor symptoms",
  qol_conditions_r = "health-care conditions",
  qol_gain_r = "secondary disease gain",
  qol_loss_independence_r = "loss of independence",
  qol_unpredict_r = "unpredictability of PD",
  qol_effect_medication_r = "effect of PD medication",
  qol_taking_medication_r = "taking PD medication",
  qol_invasive_therapy_r = "invasive therapy",
  qol_other_therapy_r = "other therapy",
  qol_family_contact_r = "contact family",
  qol_friends_contact_r = "contact friends",
  qol_affectp_contact_r = "contact people with PD",
  qol_support_yes_r = "experiencing support",
  qol_stigma_r = "stigmatization",
  qol_leisure_yes_r = "leisure activities",
  qol_finances_r = "finances",
  qol_badmob_r = "mobility impairment",
  qol_badadl_r = "ADL impairment",
  qol_sum_ident_r = "identity",
  qol_feeling_needed_r = "feeling needed",
  qol_thoughts_future_r = "thoughts about the future",
  qol_self_image_r = "self-image",
  qol_religion_r = "religion"
)

# ---- Helpers -------------------------------------------------------------

make_cor_matrix <- function(df, threshold = corr_threshold, use = "complete.obs") {
  m <- round(stats::cor(df, use = use), 2)
  m[abs(m) <= threshold] <- NA
  diag(m) <- NA
  m
}

cor_to_long_unique <- function(cor_matrix) {
  # Unique pairs only (upper triangle), keep sign
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  as.data.frame(as.table(cor_matrix)) |>
    dplyr::filter(!is.na(Freq)) |>
    dplyr::transmute(
      var1 = as.character(Var1),
      var2 = as.character(Var2),
      correlation = as.numeric(Freq)
    )
}

open_graphics_device <- function(path, type = c("pdf", "png"),
                                 width = 12, height = 11, dpi = 300) {
  type <- match.arg(type)

  if (type == "pdf") {
    grDevices::pdf(path, width = width, height = height)
  } else {
    grDevices::png(
      filename = path,
      width = width,
      height = height,
      units = "in",
      res = dpi,
      type = "cairo"
    )
  }
}

plot_cor_pairs <- function(cor_long, outfile, label_threshold = label_threshold) {
  cor_long <- cor_long |>
    dplyr::mutate(
      pair_sorted = paste(var1, var2, sep = " + "),
      label = dplyr::if_else(abs(correlation) >= label_threshold, sprintf("%.2f", correlation), NA_character_)
    ) |>
    dplyr::arrange(dplyr::desc(correlation))

  grDevices::pdf(outfile, width = 13, height = 8.5)
  p <- ggplot2::ggplot(cor_long, ggplot2::aes(x = correlation, y = stats::reorder(pair_sorted, correlation))) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = pair_sorted), linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.4, size = 3, na.rm = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Correlation",
      y = NULL,
      title = "Correlation plot (filtered)",
      subtitle = "Unique pairs only",
      caption = "Pairs with |r| below the threshold were omitted."
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 11),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10, face = "italic")
    )
  print(p)
  grDevices::dev.off()
}

plot_cor_heatmap <- function(
  cor_matrix,
  out_base,
  title = sprintf("Correlation heatmap (filtered by values of r > %.2f)", corr_threshold),
  formats = c("pdf", "png")
) {

  # Keep only upper triangle
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA

  cor_df <- as.data.frame(as.table(cor_matrix)) |>
    dplyr::transmute(
      var_x = factor(Var2, levels = colnames(cor_matrix)),
      var_y = factor(Var1, levels = rev(rownames(cor_matrix))),
      correlation = as.numeric(Freq)
    )

  cor_df_plot <- dplyr::filter(cor_df, !is.na(correlation))

  grDevices::pdf(outfile, width = 12, height = 11)

  p <- ggplot2::ggplot(
    cor_df_plot,
    ggplot2::aes(x = var_x, y = var_y, fill = correlation)
  ) +
    ggplot2::geom_tile(color = NA, linewidth = 0) +
    ggplot2::scale_fill_gradient2(
      low = "#4575B4",
      mid = "white",
      high = "#D73027",
      midpoint = 0,
      na.value = "transparent",
      name = "r"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(correlation), "", sprintf("%.2f", correlation))),
      size = 4
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::labs(title = title)

  print(p)
  for (fmt in formats) {
    out_file <- paste0(out_base, ".", fmt)

    if (fmt == "pdf") {
      grDevices::pdf(out_file, width = 15, height = 11)
      print(p)
      grDevices::dev.off()

    } else if (fmt == "png") {
      grDevices::png(
        filename = out_file,
        width = 2600,
        height = 1800,
        res = 300
      )
      print(p)
      grDevices::dev.off()

    } else {
      warning(sprintf("Unsupported format '%s' (use 'pdf' or 'png').", fmt))
    }
  }

  invisible(TRUE)
}

# ---- Data prep -----------------------------------------------------------

df_EFA <- imputed_data %>% dplyr::select(dplyr::all_of(efa_vars))

if (flag_check) {
  # Missingness overview
  print(colSums(is.na(df_EFA)))
  VIM::aggr(
    df_EFA,
    col = c("navyblue", "red"),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(df_EFA),
    cex.axis = 0.5,
    gap = 3,
    ylab = c("Missing data", "Pattern")
  )
}

# Correlations (filtered)
cor_matrix <- make_cor_matrix(df_EFA, threshold = corr_threshold, use = "complete.obs")
results.cor_matrix <- cor_matrix

if (plot_corr) {
  out_dir <- file.path(getwd(), "results")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # Apply human-readable labels to matrix dimnames
  dimnames(cor_matrix) <- list(
    dplyr::recode(rownames(cor_matrix), !!!label_map),
    dplyr::recode(colnames(cor_matrix), !!!label_map)
  )

  if (corr_plot_type == "pairs") {
    cor_long <- cor_to_long_unique(cor_matrix)
    # cor_to_long_unique returns var names after relabeling via dimnames
    outfile <- file.path(out_dir, "fig1a.corrEFA_pairs.pdf")
    plot_cor_pairs(cor_long, outfile, label_threshold = label_threshold)

  } else if (corr_plot_type == "heatmap") {
    #outfile <- file.path(out_dir, "fig1a.corrEFA_heatmap.pdf")
    plot_cor_heatmap(
      cor_matrix,
      out_base = file.path(getwd(), "results", "fig1a.corrEFA_heatmap"),
      formats = c("pdf", "png")
 )
    # plot_cor_heatmap(cor_matrix, outfile)

  } else {
    warning(sprintf("Unknown corr_plot_type='%s'. Use 'pairs' or 'heatmap'.", corr_plot_type))
  }
}


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
results.kmo

# 2.4: Bartlett’s Test for Sphericity to test factorability
results.bartlett <- cortest.bartlett(data_EFA)
results.bartlett

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
fit_oblique <- factanal(data_EFA, factors = efa_config$n_factors, rotation = "promax")
print(fit_oblique, digits = 2, sort = TRUE) #cutoff = 0.3 vor sort= TRUE

# Extracted Factors:
  # social life
  # restrictions/ physical limitations/ disadvantages
  # influence of PD on identity, thoughts and feelings/Gefühlslage/Innere Haltung/mindset/ emotional state
  # medication for PD


# 2.7: Check for factor loading < 0.4 and exclude variables with factor loading < 0.4
# Extract the factor loadings
loadings_matrix <- fit_oblique$loadings

# Threshold for factor loadings
threshold <- efa_config$loading_threshold

# Check if the maximum loading of an item is >= 0.4
items_to_keep <- apply(loadings_matrix, 1, function(x) any(abs(x) >= threshold))

# 2.8: Create a new dataset with the filtered items
filtered_data_EFA <- data_EFA[, items_to_keep]


# 2.9 Perform the EFA again
fit_oblique_filtered <- factanal(filtered_data_EFA, efa_config$n_factors, rotation="promax", scores = "regression")
print(fit_oblique_filtered, digits=2, sort=TRUE) #cutoff = 0.3 vor sort= TRUE
results.fit_oblique <- fit_oblique_filtered


# Again: Check for factor loading < 0.4 and exclude variables with factor loading < 0.4
# Extract the factor loadings
#loadings_matrix_filtered <- fit_oblique_filtered$loadings

# Again: Threshold for factor loadings
#threshold <- efa_config$loading_threshold

# Again: Check if the maximum loading of an item is >= 0.4
#new_items_to_keep <- apply(loadings_matrix_filtered, 1, function(x) any(abs(x) >= threshold))

# Again: Create a new dataset with the filtered items
#new_filtered_data_EFA <- filtered_data_EFA[, new_items_to_keep]


# Again: Perform the EFA again
# new_fit_oblique_filtered <- factanal(new_filtered_data_EFA, 4, rotation="promax", scores = "regression")
# print(new_fit_oblique_filtered, digits=2, cutoff=0.3, sort=TRUE)
#results.fit_oblique <- fit_oblique_filtered


# 2.10 Dynamic Renaming of Factors Based on Marker Variables: Assigning Factor Names According to the Corresponding Marker Variables

# Define which variables should belong to which factors
factor_markers <- list(
  social_contacts = c("qol_family_contact_r", "qol_friends_contact_r"),
  restrictions = c("qol_badadl_r"),
  medication = c("qol_taking_medication_r", "qol_effect_medication_r"),
  mindset = c("qol_thoughts_future_r")
)

# Function for dynamic renaming of factors
rename_factors <- function(fit_oblique_filtered, markers) {
  # Extract the factor loadings
  factor_loadings <- fit_oblique_filtered$loadings

  # Rename the factors based on the marker variables
  renamed_factors <- setNames(rep(NA, ncol(factor_loadings)), colnames(factor_loadings))

  for (marker_name in names(markers)) {
    marker_variables <- markers[[marker_name]]

    # Find the factor with the highest loading for each marker variable and assign the factor name
    for (marker_variable in marker_variables) {
      # Find the index of the highest factor loading for the marker variable
      factor_with_highest_loading <- which.max(factor_loadings[marker_variable, ])
      renamed_factors[factor_with_highest_loading] <- marker_name
    }
  }

  # Fill missing factor names with generic labels
  unnamed_factors <- is.na(renamed_factors)
  renamed_factors[unnamed_factors] <- paste0("Factor", which(unnamed_factors))

  renamed_factors
}

# Function to dynamically assign variables to factors based on factor loadings
get_factor_variables <- function(factor_loadings, threshold = efa_config$factor_assignment_threshold) {
  factor_variables <- list()

  # Iterate over all factors and find variables that load on each one
  for (i in 1:ncol(factor_loadings)) {
    # Find all variables with a factor loading above the threshold
    variables <- rownames(factor_loadings)[abs(factor_loadings[, i]) >= threshold]
    factor_variables[[colnames(factor_loadings)[i]]] <- variables
  }

  return(factor_variables)
}

# Apply the renaming function to the factor analysis results
# Reuse the previously fitted EFA model (do not refit here)
new_factor_names <- rename_factors(fit_oblique_filtered, factor_markers)

# Apply the function to get the factor variables dynamically
factor_variables <- get_factor_variables(fit_oblique_filtered$loadings, threshold = efa_config$factor_assignment_threshold)

# Print new factor names and associated variables
cat("Renamed Factors:\n") # Assignment of factors to the new factor names, helpful to interpret Cronbach's alpha!
print(new_factor_names)

cat("\nFactor Variables:\n")
print(factor_variables) # Details on which variables load onto which factors!


# 2.11 Calculate Cronbach's Alpha
for (factor_name in names(factor_variables)) {
  variables <- factor_variables[[factor_name]]

  # Check if variables exist in the dataset
  existing_variables <- variables[variables %in% colnames(filtered_data_EFA)]

  if (length(existing_variables) > 1) {
    # Calculate Cronbach's Alpha for these variables
    alpha_result <- alpha(filtered_data_EFA[, existing_variables])
    cat(paste("Cronbach's Alpha for", factor_name, ":", alpha_result$total$raw_alpha, "\n"))
  } else {
    cat(paste("Not enough variables to compute Cronbach's Alpha for", factor_name, "\n"))
  }
}


## Bonus: Cronbach alpha (reliability analysis) when interested in item statistics
# Attention: The variable list must be manually adjusted depending on the loadings of the variables on the factors (It is helpful to check "cat("Renamed Factors:\n") and "cat("\nFactor Variables:\n") a few lines above).

social_contacts <- imputed_data[,c("qol_family_contact_r", "qol_friends_contact_r", "qol_affectp_contact_r", "qol_support_yes_r")]

restrictions <- imputed_data[,c("qol_loss_independence_r", "qol_badmob_r", "qol_badadl_r", "qol_nms_r", "qol_unpredict_r")]

medication <- imputed_data[,c("qol_effect_medication_r", "qol_taking_medication_r")]

identity <- imputed_data[,c("qol_sum_ident_r", "qol_thoughts_future_r", "qol_self_image_r", "qol_unpredict_r")]

results.cronbach.social_contacts <- psych::alpha(social_contacts)
results.cronbach.social_contacts
results.cronbach.restrictions <- psych::alpha(restrictions)
results.cronbach.restrictions
results.cronbach.medication <- psych::alpha(medication)
results.cronbach.medication
results.cronbach.identity <- psych::alpha(identity)
results.cronbach.identity

# 2.11: Export results
# Create a function to build a table with the information from the EFA, source: https://stackoverflow.com/questions/63856304/create-data-frame-from-efa-output-in-r
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

# proportion_variance <- fit_oblique_filtered$PVAL["Proportion Var",] #TODO: remove?

# Use function "fa_table" with new EFA results and cut-off (e.g. 0.3)
efa_table_final <- fa_table(fit_oblique_filtered, cut = 0.3)
efa_table <- fa_table(fit_oblique, cut = 0.3)


# Use package "openxlsx" to save table in Excel
wb3 <- createWorkbook()
addWorksheet(wb3, "efa_results_final")
writeData(wb3, "efa_results_final", efa_table_final)
setColWidths(wb3, "efa_results_final", cols = 1:ncol(efa_table_final), widths = "auto")
saveWorkbook(wb3, file = file.path(getwd(), "results", "efa_table_final.xlsx"), overwrite = TRUE)

wb3b <- createWorkbook()
addWorksheet(wb3b, "efa_results_before_cut_off")
writeData(wb3b, "efa_results_before_cut_off", efa_table)
setColWidths(wb3b, "efa_results_before_cut_off", cols = 1:ncol(efa_table), widths = "auto")
saveWorkbook(wb3b, file = file.path(getwd(), "results", "efa_table.xlsx"), overwrite = TRUE)


# Attention: Change names of factors in excel-file (factor structure changes when imputation is rerun)
# For correct factor name check:
cat("Renamed Factors:\n")
print(new_factor_names)
# Possibly change names of variables loading onto factors too


# 2.12: Export Factor Correlations
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
factor_correlations <- extract_factor_correlations(fit_oblique)

# Create correlation matrix with row names
factor_correlation_with_rowname_final <- cbind(Factor = rownames(factor_correlations_final), factor_correlations_final)
factor_correlation_with_rowname <- cbind(Factor = rownames(factor_correlations), factor_correlations)

# Use package "openxlsx" to save table in Excel
wb4 <- createWorkbook()
addWorksheet(wb4, "efa_factor_correlations_final")
writeData(wb4, "efa_factor_correlations_final", factor_correlation_with_rowname_final)
setColWidths(wb4, "efa_factor_correlations_final", cols = 1:ncol(factor_correlation_with_rowname_final), widths = "12.67")
saveWorkbook(wb4, file = file.path(getwd(), "results", "efa_factor_correlations_final.xlsx"), overwrite = TRUE)

wb4b <- createWorkbook()
addWorksheet(wb4b, "efa_factor_correlations")
writeData(wb4b, "efa_factor_correlations", factor_correlation_with_rowname)
setColWidths(wb4b, "efa_factor_correlations", cols = 1:ncol(factor_correlation_with_rowname), widths = "12.67")
saveWorkbook(wb4b, file = file.path(getwd(), "results", "efa_factor_correlations.xlsx"), overwrite = TRUE)


# Attention: Change names of factors in excel-file (factor structure changes when imputation is rerun)
# For correct factor name check:
cat("Renamed Factors:\n")
print(new_factor_names)
# Possibly change names of variables loading onto factors too


# 2.15 Visualization of EFA

# Preamble: Renaming the variables loading onto the factors (Possibly has to be adjusted after rerunning imputation!)
colnames(filtered_data_EFA)
rownames(fit_oblique_filtered$loadings) # check current item names before applying label mapping


variable_label_map <- c(
  qol_loss_independence_r = "loss of independence",
  qol_effect_medication_r = "effect of PD medication",
  qol_taking_medication_r = "taking PD medication",
  qol_invasive_therapy_r = "invasive therapy",
  qol_other_therapy_r = "other therapy",
  qol_stigma_r = "stigmatization",
  qol_finances_r = "finances",
  qol_gain_r = "secondary disease gain",
  qol_unpredict_r = "unpredictability of PD",
  qol_religion_r = "religion",
  qol_ms_r = "motor symptoms",
  qol_nms_r = "non-motor symptoms",
  qol_conditions_r = "health-care conditions",
  qol_family_contact_r = "contact with family",
  qol_friends_contact_r = "contact with friends",
  qol_affectp_contact_r = "contact with other affected people",
  qol_support_yes_r = "experiencing support",
  qol_badmob_r = "bad mobility",
  qol_badadl_r = "restrictions in ADL",
  qol_sum_ident_r = "identity overall",
  qol_feeling_needed_r = "feeling needed",
  qol_thoughts_future_r = "thoughts about the future",
  qol_self_image_r = "self-image",
  qol_leisure_yes_r = "leisure activities"
)

old_item_names <- rownames(fit_oblique_filtered$loadings)
new_item_names <- unname(variable_label_map[old_item_names])

if (any(is.na(new_item_names))) {
  missing <- old_item_names[is.na(new_item_names)]
  warning(
    sprintf(
      "No label mapping found for %d item(s): %s",
      length(missing),
      paste(missing, collapse = ", ")
    ),
    call. = FALSE
  )
  new_item_names[is.na(new_item_names)] <- old_item_names[is.na(new_item_names)]
}

rownames(fit_oblique_filtered$loadings) <- new_item_names

# Check if the row names have been changed correctly
rownames(fit_oblique_filtered$loadings)


# 2.15.1 Heat map: Shows Magnitude of the Variable Loadings onto the Factors and shows Division into Four Factors

# Dynamic renaming of factor columns
renamed_loadings <- fit_oblique_filtered$loadings

# Iterate through the factor column names and replace them with the new names
factor_columns <- colnames(renamed_loadings)[grepl("^Factor", colnames(renamed_loadings))]
for (col in factor_columns) {
  # Extract the index of the factor (e.g., "Factor1" -> 1)
  factor_index <- as.numeric(gsub("Factor", "", col))

  # Replace the old name with the new name
  colnames(renamed_loadings)[colnames(renamed_loadings) == col] <- new_factor_names[factor_index]
}

# Export the heatmap with the new column names
pdf(file.path(getwd(), "results", "fig2b.heatmapEFA.pdf"), width = 11, height = 8.5)
heatmap.2(renamed_loadings,
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


# 2.15.2 Show variables loading on factors (source: https://rpubs.com/pjmurphy/758265)

# First, dynamically rename the factors
loads <- fit_oblique_filtered$loadings

# Iterate through the column names and replace them with the new names
factor_columns <- colnames(loads)[grepl("^Factor", colnames(loads))]
for (col in factor_columns) {
  # Extract the index of the factor (e.g., "Factor1" -> 1)
  factor_index <- as.numeric(gsub("Factor", "", col))

  # Replace the old name with the new name
  colnames(loads)[colnames(loads) == col] <- new_factor_names[factor_index]
}

# Create the diagram with the renamed factors
pdf(file.path(getwd(), "results", "fig2c.overview.pdf"), width = 11, height = 8.5)
fa.diagram(loads)
dev.off()


# 2.15.3 Pairwise factor plot: Create a pair plot matrix (all possible combinations of the factors are plotted against each other)

# Generate the pair plot with the renamed factors
pdf(file.path(getwd(), "results", "suppl_fig2e.pairplot.pdf"), width = 15, height = 10.5)
fa.plot(loads,
        labels = rownames(loads),
        cex = 0.8,
        main = "Pair Plot of Factor Analysis")
dev.off()
