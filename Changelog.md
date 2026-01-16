# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and versioning reflects analytical milestones rather than semantic releases.

## [2.7] - 2026-01-03
### Changed

- Unnecessary code chunks removed, minor explanatory adjustments 


---

## [2.6] - 2025-12-28
### Changed
- Updated labels of factors
- Revised export of analysis results
- Formatted everything uniformly and changed files' nomenclature
- revised correlation heatmaps (before EFA) and appearances of other plots

---

## [2.5] - 2024-11-25
### Changed
- Minor adjustments to plots

---

## [2.4] - 2024-11-24
### Changed
- Minor adjustments to plots
- Added comments and clarifications in scripts

---

## [2.3] - 2024-11-19
### Changed
- Adjustments to biplot code for exploratory factor analysis (EFA)
- Adjustments to x-axis handling in correlation matrix for EFA
- Minor adjustments in odds ratio (OR) analyses

---

## [2.2] - 2024-11-12
### Added
- Odds ratio estimation using binomial generalized linear models (GLM)
- Visualization of odds ratios

---

## [2.1] - 2024-11-11
### Changed
- Minor adjustments to `TableOne`
- Visualization of health-care conditions
- Updates related to Parts D and E

---

## [2.0] - 2024-10-11
### Added
- Visualization of `TableOne`
- Exploratory factor analysis (EFA)

---

## [1.9] - 2024-11-06
### Changed
- Minor code adjustments
- Changes in nomenclature (e.g., `df_recoded` renamed to `df_recoded`)

---

## [1.8] - 2024-11-03
### Changed
- Removed principal component analysis (PCA)
- Recoded variables
- Implemented moderated regression to evaluate predictor interactions
- Revised ordinal logistic regression (OLR; model m1)

---

## [1.7] - 2024-10-18
### Added
- PCA of factors
- Investigation of interaction effects among predictors
- Draft implementation of ordinal logistic regression (OLR)

---

## [1.6] - 2024-06-20
### Changed
- Corrected `recode_string` in `recode_variables.R`
- Extracted factor scores from EFA
- Draft implementation of OLR and bootstrapping procedures

---

## [1.5] - 2024-07-30
### Added
- Correlation matrix
- Cronbach’s alpha reliability analysis (EFA context)
- Draft implementation of GLM
- Working ordinal logistic regression
- Distance-based donor selection in multiple imputation

### Changed
- Code cleanup: removed discarded analyses and added explanations
- Minor adjustments to recoding of `comorb_r` in `recode_variables.R`

---

## [1.4] - 2024-07-21
### Changed
- Adjustments in multiple imputation procedures
- Draft implementation of exploratory factor analysis (EFA)

---

## [1.3] - 2024-07-15
### Added
- Draft implementation of PCA
- Additional information included in `TableOne`

### Changed
- Updates in `recode_values.R`
- Minor adjustments
- First working draft of multiple imputation

---

## [1.2] - 2024-07-09
### Added
- `TableOne`
- Basis for multiple imputation

