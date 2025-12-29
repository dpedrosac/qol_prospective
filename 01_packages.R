#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Package Setup for QoL in Parkinson’s Disease (Prospective Study)
#
# Description:
#   Defines and loads the R package dependencies required for the analyses
#   If a package is missing, it will be installed and then loaded.
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
#   - Installing packages during execution improves convenience but can reduce
#     reproducibility. For publication/archival use, consider pinning versions
#     (e.g., renv) and installing dependencies ahead of time.
#   - This script is typically sourced by analysis entry scripts.
# -------------------------------------------------------------------------

packages <- c(
  "brant",
  "car",
  "descr",
  "dplyr",
  "easystats",
  "ggpubr",
  "gofcat",
  "gplots",
  "lemon",
  "MASS",
  "mice",
  "openxlsx",
  "ordinal",
  "psych",
  "pscl",
  "sjlabelled",
  "sjmisc",
  "sjPlot",
  "sjstats",
  "tableone",
  "tidyverse",
  "VIM"
)

load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
  invisible(TRUE)
}

invisible(lapply(packages, load_or_install))

