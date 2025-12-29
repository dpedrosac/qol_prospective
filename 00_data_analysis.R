#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Quality of Life Analyses in Parkinson’s Disease (Prospective Study)
#
# Description:
#   This script runs the statistical analyses for a prospective study
#   investigating quality of life in subjects with Parkinson’s disease.
#
# Authors:
#   Leonie Moormann
#   David Pedrosa
#
# Last updated:
#   2025-12-28
#
# R version:
#   >= 4.2.0
#
# Notes:
#   - This script is intended to be run as part of the analysis pipeline.
#   - Major methodological changes are tracked via git commit history.
#   - Earlier exploratory analyses (e.g., PCA) have been removed.
#
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

flag_check <- FALSE

# Prefer USER on Unix, USERNAME on Windows
username <- Sys.getenv("USER")
if (username == "") username <- Sys.getenv("USERNAME")

if (username == "Leonie") {
  project_root <- "C:/Users/Leonie/Nextcloud/Moormann@students.uni-marburg.de/Daten/analysis_R"
} else if (username == "david") {
  project_root <- "/media/storage/qol_prospective/"
}

setwd(project_root)

results_dir <- file.path(project_root, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# -------------------------------------------------------------------------
# Dependencies and paths
# -------------------------------------------------------------------------

source("packages.R")
source("directories.R")

# -------------------------------------------------------------------------
# Data import
# -------------------------------------------------------------------------

df_raw <- readxl::read_xlsx(
  path = file.path(data_dir, "data_QoL_fin.xlsx"),
  sheet = "QoL_Parkinson",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 1,
  .name_repair = "unique"
)

# -------------------------------------------------------------------------
# Preprocessing
# -------------------------------------------------------------------------

source("recode_variables.R")

if (flag_check && interactive()) {
  view_df(df_recoded)
}


# ==================================================================================================
## Part 1: Create TableOne for participants
source("CreateTableOne.R")


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

source("exploratory_factor_analysis.R")


## =================================================================================================
# Part 3: Odds Ratios: 
# How much more likely (or less likely) is an 'extremely or very negative' rating of the impact of Parkinson's disease on quality of life (QoL) across groups defined by different demographic and health-related characteristics?

source("odds_ratio_analysis.R")

