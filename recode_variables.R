# Excluding all questionnaires with no answers in Part C (and in PartD and E)
## ==============================================================================================
## First step: remove rows with only NA
columns2check <- c("qol_ms", "qol_nms", "qol_effect_medication", "qol_taking_medication",
                      "qol_family", "qol_friends", "qol_affectp", "qol_support", "qol_stigma", 
                      "qol_adl", "qol_leisure", "qol_finances", "qol_mob", "qol_feeling_needed", 
                      "qol_self_image", "qol_thoughts_future", "qol_religion", "qol_conditions", 
                      "qol_sum_disease", "qol_gain", "qol_unpredict", "qol_loss_independence", 
                      "qol_sum_ident", "qol_invasive_therapy", "qol_other_therapy")

# Count number of NAs in selected columns and filter those with only NA
na_counts 		<- colSums(is.na(df_raw[, columns2check]))
columns_with_all_na 	<- names(na_counts[na_counts == nrow(df_raw)])

# Filter participants who have NAs in all the specified columns
df_recoded <- df_raw[rowSums(is.na(df_raw[, columns2check])) < length(columns2check), ]

setdiff(df_raw$CASE, df_recoded$CASE)
t(df_raw[12,])

## ==============================================================================================
## Second step: recode variables in Part C

# Define function for recoding and converting to numeric
recode_and_convert <- function(data, var, recode_str, var_label) {
  data <- data %>%
    rec(var, rec = recode_str, var.label = var_label, to.factor = TRUE)
  new_var <- paste0(var, "_r")
  return(data)
}

# Common recode string for most variables:
recode_str 	<- "1=1[extremely negative]; 2=2[very negative]; 3=3[somewhat negative]; 4=4[not at all]; 5=5[somewhat positive]; 6=6[very positive]; 7=7[extremely positive]; else=copy; -9=NA"

# List of variables and their labels
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

# Start recoding and converting variables with the common recode string
for (var in names(vars_labels_common)) {
  df_recoded <- recode_and_convert(df_recoded, var, recode_str, vars_labels_common[[var]])
}

# Alternative recode string:
recode_str_alternative <- "1=1[extremely negative]; 2=2[very negative]; 3=3[somewhat negative]; -1, 4=4[not at all]; 5=5[somewhat positive]; 6=6[very positive]; 7=7[extremely positive]; else=copy; -9=NA"

var_labels_alternative <- list(
  qol_invasive_therapy = "influence invasive therapy on qol",
  qol_other_therapy = "influence other therapy on qol",
  qol_stigma = "influence stigmatisation on qol"
)

for (col in names(var_labels_alternative)) {
  df_recoded <- recode_and_convert(df_recoded, col, recode_str_alternative, var_labels_alternative[[col]])
}

# Labeling newly created variables as numeric
numeric_vars <- c("qol_sum_disease_r", "qol_ms_r", "qol_nms_r", "qol_conditions_r", "qol_gain_r", "qol_invasive_therapy_r", "qol_other_therapy_r",
                  "qol_effect_medication_r", "qol_taking_medication_r","qol_loss_independence_r", "qol_unpredict_r",
                  "qol_family_contact_r", "qol_friends_contact_r", "qol_affectp_contact_r", "qol_support_yes_r", "qol_stigma_r", "qol_badadl_r",
                  "qol_goodadl_r", "qol_leisure_yes_r", "qol_finances_r", "qol_badmob_r", "qol_goodmob_r",
                  "qol_sum_ident_r", "qol_feeling_needed_r", "qol_self_image_r", "qol_thoughts_future_r","qol_religion_r")

df_recoded[numeric_vars] <- lapply(df_recoded[numeric_vars], function(x) as.numeric(as.character(x)))

view_df(df_recoded) # does not work for me

## ==============================================================================================
## Third step: recoding to reduce NA, that is: 
# a) NA resulting from unanswered filter questions remain NA. 
# b) NA resulting from questions being answered as a consequence of the filter question will be recoded to 4 ("no influence at all").

# General function to apply mutations:
mutate_filter_questions <- function(data, target_col, condition_col) {
  data %>%
    mutate(
      !!target_col := case_when(
        !!sym(condition_col) == 2 & is.na(!!sym(target_col)) ~ 4,
        TRUE ~ !!sym(target_col)
      )
    )
}

columns_to_mutate <- list(
  qol_badmob_r = "qol_mob", 			# Mobility
  qol_badadl_r = "qol_adl", 			# Restrictions in ADL
  qol_affectp_contact_r = "qol_affectp", 	# Contact with other affected people
  qol_family_contact_r = "qol_family", 		# Contact with family
  qol_friends_contact_r = "qol_friends", 	# Contact with friends 
  qol_support_yes_r = "qol_support",		# Support
  qol_leisure_yes_r = "qol_leisure"		# Leisure activity
)

#  Apply function to each pair of target and condition columns
for (cols in names(columns_to_mutate)) {
  df_recoded <- mutate_filter_questions(df_recoded, cols, columns_to_mutate[[cols]])
}

## ==============================================================================================
## Fourth step: recode variables in Part A

df_recoded <- df_recoded %>%
         mutate(
         # Age
         age_r = rec(age, rec = "0:60=1[≤ 60 years];61:69=2[61 - 69 years];69:100=3[≥ 70 years];-9=NA"),
         # Gender
         sex_r = rec(sex, rec = "1=1[male];2=2[female];3,-9=NA; else=copy", var.label = "gender", to.factor = T),
         # Living Area
         living_area_r = rec(living_area, rec = "1=1[living in a large city];2,3=2[living in a medium-small city];4,5=3[living in a rural area];-9=NA"),
         # Educational status
         education_r = rec(education, rec = "1=1[no education];2=2[primary school level];3=3[up to lower secondary school];4=4[up to higher secondary school];5=5[university degree]; 6=6[doctoral degree and higher]"),
         education_r_r = rec(education_r, rec = "2,3=1[primary-intermediate school degree]; 4=2[higher secondary school degree]; 5,6=3[university degree or higher]; else=copy; 1,-9=NA", var.label = "educational status categorized", to.factor = T),
         # Financial stability
         financial_stability_r = rec(financial_stability, rec = "1=1[all the time];2=2[most of the time];3=3[some of the time];4=4[rarely];5=5[never]; 6=6[don't know]"),
         financial_stability_r_r = rec(financial_stability_r, rec = "1,2=1[always, most of the time]; 3,4,5=2[some of the time or less]; else=copy; 6,-9=NA", var.label = "financial stability categorized", to.factor = T),
         # Family status
         family_status_r = rec(family_status, rec = "1=1[single, never married];2=2[married/ in a partnership];3=3[widowed];4=4[divorced];5=5[separated]"),
         family_status_r_r = rec(family_status_r, rec = "1,3,4,5=1[living without a partner]; 2=2[living with a partner]; else=copy; -9=NA", var.label = "family status categorized", to.factor = T),
         # Living situation
         living_situation_r = rec(living_situation, rec = "1=1[living independently alone];2=2[living independently with one/ more people];3=3[living in assisted/ retirement home];4=4[living in a nursing home];6=6[living alone with outpatient assistance]"),
         living_situation_r_r = rec(living_situation_r, rec = "1,2=1[living independently]; 3,4,6=2[living with professional support]; else=copy; -9=NA", var.label = "living situation categorized", to.factor = T)
  )

numeric_vars_partA <- c("age_r", "sex_r", "living_area_r", "education_r_r", "financial_stability_r_r", "family_status_r_r", "living_situation_r_r")
df_recoded[numeric_vars_partA] <- lapply(df_recoded[numeric_vars_partA], function(x) as.numeric(as.character(x)))

view_df(df_recoded) # Does not work for me


## ==============================================================================================
## Fifth step: recode variables in Part B

# who5
# Define a function for recoding and converting to numeric (who5)
recode_and_convert_who5 <- function(data, var, recode_str, var_label) {
  data <- data %>%
    rec(var, rec = recode_str, var.label = var_label, to.factor = TRUE)
  new_var <- paste0(var, "_r")
  return(data)
}

# List of variables and their labels (who5)
vars_labels_who5 <- list(
  who5_1 = "WHO5: dimension 1: cheerful and in a good spirit",
  who5_2 = "WHO5: dimension 2: calm and relaxed",
  who5_3 = "WHO5: dimension 3: active and vigorous",
  who5_4 = "WHO5: dimension 4: fresh and rested",
  who5_5 = "WHO5: dimension 5: my daily life has been filled with things that interest me"
)

# Recode string (who5)
recode_str_who5 <- "1=5[All the time];2=4[Most of the time]; 3=3[A little more than half the time];4=2[A little less than half the time]; 5=1[Now and then]; 6=0[At no time]; 
else=copy; -9=NA"

# Recode and convert variables (who5)
for (var in names(vars_labels_who5)) {
  df_recoded <- recode_and_convert(df_recoded, var, recode_str_who5, vars_labels_who5[[var]])
}

# Labeling as numeric (who5)
numeric_vars_who5 <- c("who5_1_r", "who5_2_r", "who5_3_r", "who5_4_r", "who5_5_r")
df_recoded[numeric_vars_who5] <- lapply(df_recoded[numeric_vars_who5], function(x) as.numeric(as.character(x)))

# Creating a new variable for depression (cut off </= 12)
df_recoded$who5_depr <- rowSums(df_recoded[,c("who5_1_r", "who5_2_r", "who5_3_r","who5_4_r","who5_5_r")]) 
df_recoded <- df_recoded %>% rec(who5_depr, rec = "13:25=0[no signs for depression]; 0:12=1[significantly impaired quality of life]; else=copy; -9=NA", var.label = "sign for depression - WHO 5", to.factor = T)


# Comorbidity (comorb)
df_recoded <- df_recoded %>%
  mutate(comorb_r = as.numeric(rec(comorb, rec = "1=2[Present];2=1[Not present];-9=NA", var.label = "Comorbidity yes or no", to.factor = T)))


# Time from diagnosis (time_from_diagnosis)
df_recoded <- df_recoded %>%
  mutate(time_from_diagnosis_r = rec(time_from_diagnosis, rec = "1=1[<2 years ago];2=2[2-5 years ago];3=3[5-10 years ago];4=4[10-15 years ago];5=5[>15 years ago];else=copy; -9=NA",
                                     var.label = "time from diagnosis", to.factor = T))

# Age at diagnosis (age_at_diagnosis)
df_recoded <- df_recoded %>%
  mutate(age_at_diagnosis_r = rec(age_at_diagnosis, rec = "1=1[<50 years old];2=2[>50 years old];else=copy; -9=NA",
                                  var.label = "age at diagnosis", to.factor = T))
 
# Disturbances of night sleep (criteria advanced PD B012_10)
df_recoded <- df_recoded %>% mutate(disturbances_sleep_APD = rec(B012_10, rec = "1=0[No];2=1[Yes]; else =copy; -9=NA",
                                                           var.label = "disturbances of night sleep", to.factor = T))

# Repeated falls (falls)
df_recoded <- df_recoded %>% 
  mutate(falls_r = rec(falls, rec = "1=2[Yes];2=1[No]"))

# Assisted moving
df_recoded <- df_recoded %>%
  mutate(rollator_r = rec(rollator, rec = "2=0[not selected];1=1[selected]"))
df_recoded <- df_recoded %>%
  mutate(wheelchair_r = rec(wheelchair, rec = "2=0[not selected];1=1[selected]"))
df_recoded$assist_mov <- rowSums(df_recoded[, c("rollator_r", "wheelchair_r")])
df_recoded <- df_recoded %>% rec(assist_mov, rec = "0=0[no assisted movement]; 1:2=1[assisted movement]; else=copy; -9=NA", var.label = "assisted movement - rollator or wheelchair", to.factor = T)

# Fluctuations of motor symptoms (most important motor symptom criteria for APD (Atonini et al.))
df_recoded <- df_recoded %>%
  mutate(ms_fluctuation_APD = rec(B012_01, rec = "1=0[No];2=1[Yes];else=copy; -9=NA", var.label = "motor symptoms fluctuation APD criteria", to.factor = T))

# Labeling as numeric: 
# who5_depr_r, comorb_r, time_from_diagnosis_r, age_at_diagnosis_r, disturbances_sleep_ADP, falls_r, assist_mov_r, ms_fluctuation_APD

numeric_vars_partB <- c("who5_depr_r", "comorb_r", "time_from_diagnosis_r", "age_at_diagnosis_r", "disturbances_sleep_APD", 
                        "falls_r", "assist_mov_r", "ms_fluctuation_APD")
df_recoded[numeric_vars_partB] <- lapply(df_recoded[numeric_vars_partB], function(x) as.numeric(as.character(x)))

