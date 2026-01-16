#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Directory Configuration for QoL in Parkinson’s Disease Study
#
# Description:
#   Defines working and data directories based on the current system user.
#   This script is sourced by analysis entry scripts to configure paths.
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
#
# Notes:
#   - Directory mappings are user-specific and intended for local execution.
#   - For collaborative or CI use, prefer setting a project root via an
#     environment variable (e.g., QOL_PROJECT_ROOT).
# -------------------------------------------------------------------------

username <- Sys.info()[["login"]]

dir_map <- list(
  Leonie = list(
    wdir = "C:/Users/Leonie/Nextcloud/Moormann@students.uni-marburg.de/Daten/github",
    data_dir = "C:/Users/Leonie/Nextcloud/Moormann@students.uni-marburg.de/Daten/github"
  ),
  dpedrosac = list(
    wdir = "D:/qol_prospective",
    data_dir = "D:/qol_prospective/data"
  ),
  david = list(
    wdir = "/media/storage/qol_prospective",
    data_dir = "/media/storage/qol_prospective/data"
  )
)

if (!username %in% names(dir_map)) {
  stop(
    sprintf(
      "Unknown user '%s'. Please configure directories in directories.R.",
      username
    ),
    call. = FALSE
  )
}

wdir <- dir_map[[username]]$wdir
data_dir <- dir_map[[username]]$data_dir

if (!dir.exists(wdir)) {
  stop(sprintf("Working directory does not exist: %s", wdir), call. = FALSE)
}

setwd(wdir)

