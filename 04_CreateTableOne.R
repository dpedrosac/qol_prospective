#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Table 1 (Participant Characteristics) for QoL in Parkinson’s Disease
#
# Description:
#   Creates a Table 1 (participant characteristics) for the QoL prospective
#   Parkinson’s disease study using the recoded analysis dataset (df_recoded).
#   Selected binary items are displayed as a single row (Yes: n (%)).
#
# Authors:
#   Leonie Moormann
#   David Pedrosa
#
# Last updated:
#   2025-12-28
#
# R version:
#   >= 4.3.1
#
# Notes:
#   - Expects df_recoded to exist in the environment (created upstream).
#   - Requires packages: dplyr, tableone, openxlsx (loaded via packages.R).
#   - Output is written to results/Table1.xlsx by default.
#   - For binary variables, we force a robust Yes/No factor with "Yes" as the
#     first level so that showAllLevels = FALSE prints the "Yes" row only.
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

output_filename <- "Table1.xlsx"
nonnormal_vars <- c("Time since diagnosis", "Education level")

# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

as_factor_character <- function(x) factor(as.character(x))

apply_factor_labels <- function(df, levels_labels) {
  for (var in names(levels_labels)) {
    spec <- levels_labels[[var]]
    df[[var]] <- factor(
      as.character(df[[var]]),
      levels = spec$levels,
      labels = spec$labels
    )
  }
  df
}

make_yes_factor <- function(x, yes_value) {
  x_chr <- as.character(x)

  out <- dplyr::case_when(
    is.na(x_chr) ~ NA_character_,
    x_chr == as.character(yes_value) ~ "Yes",
    TRUE ~ "No"
  )

  factor(out, levels = c("Yes", "No"))
}

create_table_one <- function(df, vars, pretty_names, factor_vars, nonnormal = character()) {
  df_table <- dplyr::select(df, dplyr::all_of(vars))
  names(df_table) <- pretty_names

  tab <- tableone::CreateTableOne(
    vars = pretty_names,
    factorVars = factor_vars,
    data = df_table
  )

  print(tab, nonnormal = nonnormal, showAllLevels = FALSE)
  tab
}

export_table_one_xlsx <- function(table_one, path, nonnormal = character()) {
  m <- print(table_one, nonnormal = nonnormal, showAllLevels = FALSE)
  out <- cbind(Variable = rownames(m), m)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Table1")
  openxlsx::writeData(wb, "Table1", out)
  openxlsx::setColWidths(wb, "Table1", cols = seq_len(ncol(out)), widths = "auto")
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)

  invisible(path)
}

# -------------------------------------------------------------------------
# Data preparation
# -------------------------------------------------------------------------

df_tableone <- df_recoded

factor_vars_raw <- c(
  "age_r",
  "sex_r",
  "living_area_r",
  "education_r_r",
  "financial_stability_r_r",
  "family_status_r_r",
  "living_situation_r_r",
  "comorb_r",
  "time_from_diagnosis_r",
  "age_at_diagnosis_r",
  "disturbances_sleep_APD",
  "ms_fluctuation_APD",
  "assist_mov_r",
  "falls_r"
)

factor_vars_raw <- intersect(factor_vars_raw, names(df_tableone))
df_tableone[factor_vars_raw] <- lapply(df_tableone[factor_vars_raw], as_factor_character)

# -------------------------------------------------------------------------
# Factor levels and labels (non-binary + preliminary binary labeling)
# -------------------------------------------------------------------------

levels_labels <- list(
  age_r = list(
    levels = c("1", "2", "3"),
    labels = c("≤ 60 years", "61 - 69 years", "≥ 70 years")
  ),
  sex_r = list(
    levels = c("1", "2", "3"),
    labels = c("Male", "Female", "Divers")
  ),
  education_r_r = list(
    levels = c("1", "2", "3"),
    labels = c("Primary school", "Higher secondary school", "University degree or higher")
  ),
  financial_stability_r_r = list(
    levels = c("1", "2"),
    labels = c("Most of the time", "Some of the time or less")
  ),
  family_status_r_r = list(
    levels = c("1", "2"),
    labels = c("Without a partner", "With a partner")
  ),
  living_situation_r_r = list(
    levels = c("1", "2"),
    labels = c("Independently", "With professional support")
  ),
  living_area_r = list(
    levels = c("1", "2", "3"),
    labels = c("Large city", "Medium-small city", "Rural area")
  ),
  who5_depr_score = list(
    levels = c("0", "1"),
    labels = c("No depression", "Impaired quality of life")
  ),
  comorb_r = list(
    levels = c("1", "2"),
    labels = c("Not present", "Present")
  ),
  time_from_diagnosis_r = list(
    levels = c("1", "2", "3", "4", "5"),
    labels = c("<2 years", "2-5 years", "5-10 years", "10-15 years", ">15 years")
  ),
  age_at_diagnosis_r = list(
    levels = c("1", "2"),
    labels = c("<50 years", "≥50 years")
  ),
  disturbances_sleep_APD = list(
    levels = c("0", "1"),
    labels = c("No", "Yes")
  ),
  ms_fluctuation_APD = list(
    levels = c("0", "1"),
    labels = c("No", "Yes")
  ),
  assist_mov_r = list(
    levels = c("0", "1"),
    labels = c("No assisted movement", "Assisted movement")
  ),
  falls_r = list(
    levels = c("1", "2"),
    labels = c("No", "Yes")
  )
)

df_tableone <- apply_factor_labels(
  df = df_tableone,
  levels_labels = levels_labels
)

# -------------------------------------------------------------------------
# Force "Yes-only" display for selected binary variables (robust; no drops)
# -------------------------------------------------------------------------

df_tableone <- df_tableone %>%
  dplyr::mutate(
    disturbances_sleep_APD = make_yes_factor(disturbances_sleep_APD, yes_value = "Yes"),
    age_at_diagnosis_r     = make_yes_factor(age_at_diagnosis_r,     yes_value = "<50 years"),
    ms_fluctuation_APD     = make_yes_factor(ms_fluctuation_APD,     yes_value = "Yes"),
    assist_mov_r           = make_yes_factor(assist_mov_r,           yes_value = "Assisted movement"),
    falls_r                = make_yes_factor(falls_r,                yes_value = "Yes")
  )

# -------------------------------------------------------------------------
# Table 1 variable specification
# -------------------------------------------------------------------------

table_vars <- c(
  "age",
  "age_r",
  "sex_r",
  "education_r_r",
  "financial_stability_r_r",
  "family_status_r_r",
  "living_situation_r_r",
  "living_area_r",
  "comorb_r",
  "age_at_diagnosis_r",
  "time_from_diagnosis_r",
  "disturbances_sleep_APD",
  "ms_fluctuation_APD",
  "assist_mov_r",
  "falls_r"
)

table_var_labels <- c(
  "Age",
  "Age categorized",
  "Gender",
  "Education level",
  "Financial stability",
  "Relationship status",
  "Household situation",
  "Living area",
  "Comorbidity",
  "Age at diagnosis <50 years",
  "Time since diagnosis",
  "Sleep disturbances",
  "Motor symptoms fluctuation",
  "Assisted movement",
  "Falls"
)

factor_vars_pretty <- setdiff(
  table_var_labels,
  c("Age", "Age categorized")
)

# -------------------------------------------------------------------------
# Create Table 1
# -------------------------------------------------------------------------

table1 <- create_table_one(
  df = df_tableone,
  vars = table_vars,
  pretty_names = table_var_labels,
  factor_vars = factor_vars_pretty,
  nonnormal = nonnormal_vars
)

# -------------------------------------------------------------------------
# Export Table 1
# -------------------------------------------------------------------------

export_path <- file.path(
  getwd(),
  "results",
  output_filename
)

export_table_one_xlsx(
  table_one = table1,
  path = export_path,
  nonnormal = nonnormal_vars
)


export_table_one_docx <- function(table_one, path, title, nonnormal = character()) {
  # tableone -> matrix/data.frame
  m <- print(
    table_one,
    nonnormal = nonnormal,
    showAllLevels = FALSE,
    printToggle = FALSE
  )

  df_out <- data.frame(
    Variable = rownames(m),
    m,
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(df_out)
  ft <- flextable::autofit(ft)

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- flextable::body_add_flextable(doc, ft)

  print(doc, target = path)
  invisible(path)
}

docx_path <- file.path(getwd(), "results", "Table1.docx")

export_table_one_docx(
  table_one = table1,
  path = docx_path,
  title = "Table 1: Demographics of the PwPD taking part in the survey",
  nonnormal = nonnormal_vars
)
