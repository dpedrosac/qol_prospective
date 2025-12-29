#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Odds Ratio Analysis of Negative QoL Impact in Parkinson’s Disease
#
# Description:
#   Estimates unadjusted odds ratios (ORs) for an "extremely/very negative"
#   rating of the impact of Parkinson’s disease on overall quality of life.
#   Predictor variables include demographic/clinical covariates and EFA factor
#   scores. Results are exported and (optionally) visualized as a forest plot.
#
# Authors:
#   Leonie Moormann
#   David Pedrosa
#
# Last updated:
#   2024-12-28
#
# R version:
#   >= 4.3.1
#
# Notes:
#   - Expects `imputed_data`, `fit_oblique_filtered`, and `new_factor_names`
#     to exist in the environment.
#   - Requires packages: dplyr, forcats, ggplot2 (and optionally openxlsx).
#   - Writes outputs to `results/`.
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

flag_check <- FALSE
plot_or <- TRUE
plot_formats <- c("pdf", "png")
plot_base <- file.path(getwd(), "results", "fig5.OR")

dependent_variable <- "qol_sum_disease_r"

predictors <- c(
  "sex_r", "comorb_r", "disturbances_sleep_APD",
  "age_r", "education_r_r", "time_from_diagnosis_r", "family_status_r_r",
  "living_area_r", "who5_depr_score", "ms_fluctuation_APD", "falls_r",
  "social_contacts", "medication", "restrictions", "mindset"
)

# Named label map (robust; no manual reordering needed)
predictor_labels <- c(
  sex_r = "Female",
  comorb_r = "Comorbidity",
  disturbances_sleep_APD = "Sleep disturbances",
  age_r = "Age",
  education_r_r = "Highest level of education",
  time_from_diagnosis_r = "Time since diagnosis",
  family_status_r_r = "Living with a partner",
  living_area_r = "Area of residence (city to rural living)",
  who5_depr_score = "Depressed mood",
  ms_fluctuation_APD = "Fluctuation of motor symptoms",
  falls_r = "Falls",
  social_contacts = "Positive rating of 'Social contacts' for QoL",
  medication = "Positive rating of 'Medication' for QoL",
  restrictions = "Positive rating of 'Restrictions' for QoL",
  mindset = "Positive rating of 'Mindset' for QoL"
)

# Plot settings
fontsize <- 18
x_limits <- c(0.08, 40)
x_breaks <- c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 40)

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

ensure_results_dir <- function() {
  out_dir <- file.path(getwd(), "results")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  invisible(out_dir)
}

make_binary_outcome <- function(x) {
  # Negative: 1 or 2; Positive: 3,4,5. Returns factor with levels: positive, negative.
  forcats::fct_collapse(
    as.factor(x),
    negative = c("1", "2"),
    positive = c("3", "4", "5")
  ) |>
    forcats::fct_relevel("positive", "negative")
}

or_from_glm <- function(fit, term) {
  b <- stats::coef(fit)[term]
  se <- sqrt(diag(stats::vcov(fit)))[term]

  # Wald 95% CI (matches confint.default behavior; avoids index gymnastics)
  ci_low <- b - 1.96 * se
  ci_high <- b + 1.96 * se

  p <- summary(fit)$coefficients[term, "Pr(>|z|)"]

  data.frame(
    term = term,
    or = exp(b),
    ci_low = exp(ci_low),
    ci_high = exp(ci_high),
    p_value = p,
    stringsAsFactors = FALSE
  )
}

fit_unadjusted_or <- function(df, outcome_col, predictor) {
  # Creates formula: I(outcome == 'negative') ~ predictor
  f <- stats::as.formula(sprintf("I(%s == 'negative') ~ %s", outcome_col, predictor))
  fit <- stats::glm(f, data = df, family = stats::binomial())

  # Determine which coefficient corresponds to the predictor.
  # For numeric predictors: term = predictor
  # For factors: term = paste0(predictor, <level>) -> we choose the first non-intercept term.
  term <- setdiff(names(stats::coef(fit)), "(Intercept)")[1]

  or_from_glm(fit, term) |>
    dplyr::mutate(predictor = predictor)
}

p_to_sig <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p <= 0.001 ~ "p < .001",
    p < 0.05 ~ "p < .05",
    TRUE ~ "ns"
  )
}

open_device <- function(path, fmt, width = 15, height = 8.5, res = 300) {
  if (fmt == "pdf") {
    grDevices::pdf(path, width = width, height = height)
  } else if (fmt == "png") {
    grDevices::png(path, width = width * res, height = height * res, res = res)
  } else {
    stop(sprintf("Unsupported format '%s'. Use 'pdf' or 'png'.", fmt), call. = FALSE)
  }
}

plot_or_forest <- function(results, out_base, formats = c("pdf")) {
  # Order by OR
  plot_df <- results |>
    dplyr::arrange(or) |>
    dplyr::mutate(label = factor(label, levels = label)) |>
    dplyr::mutate(sig = factor(sig, levels = c("ns", "p < .05", "p < .001")))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = or, y = label, color = sig)) +
    ggplot2::geom_vline(xintercept = 1, linewidth = 0.75, linetype = "dashed", color = "grey") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_low, xmax = ci_high), linewidth = 0.5, height = 0.2, color = "gray50") +
    ggplot2::geom_point(size = 2.2, show.legend = TRUE) +
    ggplot2::scale_x_log10(breaks = x_breaks, limits = x_limits) +
    ggplot2::scale_colour_manual(values = c("p < .001" = "#6B340D", "p < .05" = "#B8A31F", "ns" = "#08306B")) +
    ggplot2::labs(
      x = "Odds ratio (log scale)",
      y = NULL,
      title = "Odds of extremely or very negative impact of PD on overall QoL"
    ) +
    ggplot2::theme_minimal(base_size = fontsize) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = c(0.9, 0.2),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(colour = "black", fill = "white"),
      legend.box.background = ggplot2::element_rect(colour = "black"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size=18)
    )

  for (fmt in formats) {
    out_file <- paste0(out_base, ".", fmt)
    open_device(out_file, fmt)
    print(p)
    grDevices::dev.off()
  }

  invisible(TRUE)
}

# -------------------------------------------------------------------------
# 1) Factor scores and merge with imputed data
# -------------------------------------------------------------------------

ensure_results_dir()

factor_scores <- fit_oblique_filtered$scores
if (is.null(factor_scores)) {
  stop("No factor scores found in `fit_oblique_filtered$scores`.", call. = FALSE)
}

if (length(new_factor_names) != ncol(factor_scores)) {
  stop("`new_factor_names` length does not match number of factor score columns.", call. = FALSE)
}
colnames(factor_scores) <- new_factor_names

imputed_data_with_scores <- dplyr::bind_cols(imputed_data, as.data.frame(factor_scores))

if (flag_check) {
  print(head(imputed_data_with_scores))
  print(table(imputed_data_with_scores[[dependent_variable]], useNA = "ifany"))
}

# -------------------------------------------------------------------------
# 2) Build modeling dataset and outcome
# -------------------------------------------------------------------------

needed_cols <- c(dependent_variable, predictors)
missing_cols <- setdiff(needed_cols, names(imputed_data_with_scores))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

df_or <- imputed_data_with_scores |>
  dplyr::select(dplyr::all_of(needed_cols)) |>
  dplyr::mutate(dv = make_binary_outcome(.data[[dependent_variable]])) |>
  dplyr::filter(!is.na(dv))

# Optional: coerce selected coded variables to integer (kept from your original intent)
vars_integer <- c(
  "sex_r", "comorb_r", "disturbances_sleep_APD",
  "age_r", "education_r_r", "time_from_diagnosis_r", "family_status_r_r",
  "living_area_r", "who5_depr_score", "ms_fluctuation_APD", "falls_r"
)
vars_integer <- intersect(vars_integer, names(df_or))
df_or[vars_integer] <- lapply(df_or[vars_integer], function(x) as.integer(as.character(x)))

# -------------------------------------------------------------------------
# 3) Fit unadjusted ORs (one predictor at a time)
# -------------------------------------------------------------------------

results_or <- dplyr::bind_rows(lapply(predictors, function(p) fit_unadjusted_or(df_or, "dv", p))) |>
  dplyr::mutate(
    sig = p_to_sig(p_value),
    label = dplyr::coalesce(predictor_labels[predictor], predictor)
  )

if (flag_check) {
  print(results_or)
}

# -------------------------------------------------------------------------
# 4) Optional visualization
# -------------------------------------------------------------------------

if (plot_or) {
  plot_or_forest(results_or, out_base = plot_base, formats = plot_formats)
}

# -------------------------------------------------------------------------
# 5) Export results table (CSV; simple and journal-friendly)
# -------------------------------------------------------------------------

out_csv <- file.path(getwd(), "results", "odds_ratios_unadjusted.csv")
utils::write.csv(results_or, out_csv, row.names = FALSE)

