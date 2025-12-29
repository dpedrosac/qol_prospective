#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Variable Recoding for QoL in Parkinson’s Disease (Prospective Study)
#
# Description:
#   Cleans and recodes raw questionnaire data into analysis-ready variables.
#   Steps include:
#   - exclusion of questionnaires without responses in Part C (and Part D/E),
#   - recoding of Likert-type QoL items (Part C),
#   - recoding of sociodemographic variables (Part A),
#   - recoding of clinical and WHO-5 variables (Part B),
#   - targeted NA reduction for filter-question dependent items.
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
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Helper utilities
# -------------------------------------------------------------------------

to_numeric_safe <- function(x) as.numeric(as.character(x))

coerce_numeric_cols <- function(data, cols) {
  cols <- intersect(cols, names(data))
  data[cols] <- lapply(data[cols], to_numeric_safe)
  data
}

apply_recodes <- function(data, mapping, recode_string) {
  # mapping: named character vector/list; names are variables, values are labels
  for (var in names(mapping)) {
    data <- data %>%
      rec(!!rlang::sym(var), rec = recode_string, var.label = mapping[[var]], to.factor = TRUE)
  }
  data
}

fill_na_if_filter_no <- function(data, target, filter_col, filter_no_value = 2, fill_value = 4) {
  data %>%
    dplyr::mutate(
      !!rlang::sym(target) := dplyr::case_when(
        !!rlang::sym(filter_col) == filter_no_value & is.na(!!rlang::sym(target)) ~ fill_value,
        TRUE ~ !!rlang::sym(target)
      )
    )
}

# -------------------------------------------------------------------------
# Step 1: Exclusion based on Part C missingness
# -------------------------------------------------------------------------

columns2check <- c(
  "qol_ms", "qol_nms", "qol_effect_medication", "qol_taking_medication",
  "qol_family", "qol_friends", "qol_affectp", "qol_support", "qol_stigma",
  "qol_adl", "qol_leisure", "qol_finances", "qol_mob", "qol_feeling_needed",
  "qol_self_image", "qol_thoughts_future", "qol_religion", "qol_conditions",
  "qol_sum_disease", "qol_gain", "qol_unpredict", "qol_loss_independence",
  "qol_sum_ident", "qol_invasive_therapy", "qol_other_therapy"
)

df_recoded <- df_raw %>%
  dplyr::filter(rowSums(is.na(dplyr::across(dplyr::all_of(columns2check)))) < length(columns2check))

# Optional diagnostics (keep off by default)
if (interactive()) {
  excluded_cases <- setdiff(df_raw$CASE, df_recoded$CASE)
}

# -------------------------------------------------------------------------
# Step 2: Recode variables in Part C
# -------------------------------------------------------------------------

recode_str_common <- paste(
  "1=1[extremely negative]; 2=2[very negative]; 3=3[somewhat negative];",
  "4=4[not at all]; 5=5[somewhat positive]; 6=6[very positive];",
  "7=7[extremely positive]; else=copy; -9=NA"
)

vars_labels_common <- list(
  qol_ms = "influence ms on qol",
  qol_nms = "influence nms on qol",
  qol_sum_disease = "influence disease on qol",
  qol_conditions = "influence conditions health-care on qol",
  qol_gain = "influence gain that resulted from PD on qol",
  qol_effect_medication = "influence effect of medication on qol",
  qol_taking_medication = "influence of the process of taking medication on qol",
  qol_loss_independence = "influence loss of indenpendence on qol",
  qol_unpredict = "influence of unpredictability of PD on qol",
  qol_family_contact = "influence contact with family on qol",
  qol_friends_contact = "influence contact with friends on qol",
  qol_affectp_contact = "influence contact with other affected people on qol",
  qol_support_yes = "influence support on qol",
  qol_badadl = "influence of restrictions in the performance of everyday activities on qol",
  qol_goodadl = "influence of the ability to carry out everyday activities on qol",
  qol_leisure_yes = "influence pursuing leisure activities on qol",
  qol_finances = "influence finances on qol",
  qol_badmob = "influence of lack of mobility on qol",
  qol_goodmob = "influence of good mobility on qol",
  qol_sum_ident = "influence identity on qol",
  qol_feeling_needed = "influence feeling needed on qol",
  qol_self_image = "influence self image on qol",
  qol_thoughts_future = "influence thoughts about future on qol",
  qol_religion = "influence religion on qol"
)

df_recoded <- apply_recodes(df_recoded, vars_labels_common, recode_str_common)

recode_str_alt <- paste(
  "1=1[extremely negative]; 2=2[very negative]; 3=3[somewhat negative];",
  "-1, 4=4[not at all]; 5=5[somewhat positive]; 6=6[very positive];",
  "7=7[extremely positive]; else=copy; -9=NA"
)

vars_labels_alt <- list(
  qol_invasive_therapy = "influence invasive therapy on qol",
  qol_other_therapy = "influence other therapy on qol",
  qol_stigma = "influence stigmatisation on qol"
)

df_recoded <- apply_recodes(df_recoded, vars_labels_alt, recode_str_alt)

numeric_vars_partC <- c(
  "qol_sum_disease_r", "qol_ms_r", "qol_nms_r", "qol_conditions_r", "qol_gain_r",
  "qol_invasive_therapy_r", "qol_other_therapy_r", "qol_effect_medication_r",
  "qol_taking_medication_r", "qol_loss_independence_r", "qol_unpredict_r",
  "qol_family_contact_r", "qol_friends_contact_r", "qol_affectp_contact_r",
  "qol_support_yes_r", "qol_stigma_r", "qol_badadl_r", "qol_goodadl_r",
  "qol_leisure_yes_r", "qol_finances_r", "qol_badmob_r", "qol_goodmob_r",
  "qol_sum_ident_r", "qol_feeling_needed_r", "qol_self_image_r",
  "qol_thoughts_future_r", "qol_religion_r"
)

df_recoded <- coerce_numeric_cols(df_recoded, numeric_vars_partC)

# -------------------------------------------------------------------------
# Step 3: Reduce NA for filter-dependent items
# -------------------------------------------------------------------------

columns_to_mutate <- list(
  qol_badmob_r = "qol_mob",
  qol_badadl_r = "qol_adl",
  qol_affectp_contact_r = "qol_affectp",
  qol_family_contact_r = "qol_family",
  qol_friends_contact_r = "qol_friends",
  qol_support_yes_r = "qol_support",
  qol_leisure_yes_r = "qol_leisure"
)

for (target in names(columns_to_mutate)) {
  df_recoded <- fill_na_if_filter_no(df_recoded, target, columns_to_mutate[[target]])
}

# -------------------------------------------------------------------------
# Step 4: Recode variables in Part A
# -------------------------------------------------------------------------

df_recoded <- df_recoded %>%
  dplyr::mutate(
    age_r = rec(age, rec = "0:60=1[≤ 60 years];61:69=2[61 - 69 years];69:100=3[≥ 70 years];-9=NA"),
    sex_r = rec(sex, rec = "1=1[male];2=2[female];3,-9=NA; else=copy", var.label = "gender", to.factor = TRUE),
    living_area_r = rec(living_area, rec = "1=1[living in a large city];2,3=2[living in a medium-small city];4,5=3[living in a rural area];-9=NA"),
    education_r = rec(education, rec = "1=1[no education];2=2[primary school level];3=3[up to lower secondary school];4=4[up to higher secondary school];5=5[university degree]; 6=6[doctoral degree and higher]"),
    education_r_r = rec(education_r, rec = "2,3=1[primary-intermediate school degree]; 4=2[higher secondary school degree]; 5,6=3[university degree or higher]; else=copy; 1,-9=NA",
                        var.label = "educational status categorized", to.factor = TRUE),
    financial_stability_r = rec(financial_stability, rec = "1=1[all the time];2=2[most of the time];3=3[some of the time];4=4[rarely];5=5[never]; 6=6[don't know]"),
    financial_stability_r_r = rec(financial_stability_r, rec = "1,2=1[always, most of the time]; 3,4,5=2[some of the time or less]; else=copy; 6,-9=NA",
                                  var.label = "financial stability categorized", to.factor = TRUE),
    family_status_r = rec(family_status, rec = "1=1[single, never married];2=2[married/ in a partnership];3=3[widowed];4=4[divorced];5=5[separated]"),
    family_status_r_r = rec(family_status_r, rec = "1,3,4,5=1[living without a partner]; 2=2[living with a partner]; else=copy; -9=NA",
                            var.label = "family status categorized", to.factor = TRUE),
    living_situation_r = rec(living_situation, rec = "1=1[living independently alone];2=2[living independently with one/ more people];3=3[living in assisted/ retirement home];4=4[living in a nursing home];6=6[living alone with outpatient assistance]"),
    living_situation_r_r = rec(living_situation_r, rec = "1,2=1[living independently]; 3,4,6=2[living with professional support]; else=copy; -9=NA",
                               var.label = "living situation categorized", to.factor = TRUE)
  )

numeric_vars_partA <- c(
  "age_r", "sex_r", "living_area_r", "education_r_r",
  "financial_stability_r_r", "family_status_r_r", "living_situation_r_r"
)
df_recoded <- coerce_numeric_cols(df_recoded, numeric_vars_partA)

# -------------------------------------------------------------------------
# Step 5: Recode variables in Part B (WHO-5 + clinical variables)
# -------------------------------------------------------------------------

recode_str_who5 <- paste(
  "1=5[All the time];2=4[Most of the time]; 3=3[A little more than half the time];",
  "4=2[A little less than half the time]; 5=1[Now and then]; 6=0[At no time];",
  "else=copy; -9=NA"
)

vars_labels_who5 <- list(
  who5_1 = "WHO5: dimension 1: cheerful and in a good spirit",
  who5_2 = "WHO5: dimension 2: calm and relaxed",
  who5_3 = "WHO5: dimension 3: active and vigorous",
  who5_4 = "WHO5: dimension 4: fresh and rested",
  who5_5 = "WHO5: dimension 5: my daily life has been filled with things that interest me"
)

df_recoded <- apply_recodes(df_recoded, vars_labels_who5, recode_str_who5)

numeric_vars_who5 <- c("who5_1_r", "who5_2_r", "who5_3_r", "who5_4_r", "who5_5_r")
df_recoded <- coerce_numeric_cols(df_recoded, numeric_vars_who5)

df_recoded$who5_depr_score <- rowSums(df_recoded[, numeric_vars_who5], na.rm = FALSE)

df_recoded <- df_recoded %>%
  rec(
    who5_depr_score,
    rec = "13:25=0[no signs for depression]; 0:12=1[significantly impaired quality of life]; else=copy; -9=NA",
    var.label = "sign for depression - WHO 5",
    to.factor = TRUE
  ) %>%
  dplyr::rename(who5_depr = who5_depr_score_r)

df_recoded <- df_recoded %>%
  dplyr::mutate(
    comorb_r = to_numeric_safe(rec(comorb, rec = "1=2[Present];2=1[Not present];-9=NA", var.label = "Comorbidity yes or no", to.factor = TRUE)),
    time_from_diagnosis_r = rec(time_from_diagnosis, rec = "1=1[<2 years ago];2=2[2-5 years ago];3=3[5-10 years ago];4=4[10-15 years ago];5=5[>15 years ago];else=copy; -9=NA",
                                var.label = "time from diagnosis", to.factor = TRUE),
    age_at_diagnosis_r = rec(age_at_diagnosis, rec = "1=1[<50 years old];2=2[>50 years old];else=copy; -9=NA",
                             var.label = "age at diagnosis", to.factor = TRUE),
    disturbances_sleep_APD = rec(B012_10, rec = "1=0[No];2=1[Yes]; else=copy; -9=NA",
                                 var.label = "disturbances of night sleep", to.factor = TRUE),
    falls_r = rec(falls, rec = "1=2[Yes];2=1[No]"),
    rollator_r = rec(rollator, rec = "2=0[not selected];1=1[selected]"),
    wheelchair_r = rec(wheelchair, rec = "2=0[not selected];1=1[selected]"),
    assist_mov = rowSums(dplyr::across(dplyr::all_of(c("rollator_r", "wheelchair_r")))),
    ms_fluctuation_APD = rec(B012_01, rec = "1=0[No];2=1[Yes];else=copy; -9=NA",
                             var.label = "motor symptoms fluctuation APD criteria", to.factor = TRUE)
  ) %>%
  rec(
    assist_mov,
    rec = "0=0[no assisted movement]; 1:2=1[assisted movement]; else=copy; -9=NA",
    var.label = "assisted movement - rollator or wheelchair",
    to.factor = TRUE
  ) %>%
  dplyr::rename(assist_mov_r = assist_mov_r)

numeric_vars_partB <- c(
  "who5_depr", "comorb_r", "time_from_diagnosis_r", "age_at_diagnosis_r",
  "disturbances_sleep_APD", "falls_r", "assist_mov_r", "ms_fluctuation_APD"
)
df_recoded <- coerce_numeric_cols(df_recoded, numeric_vars_partB)

