# Create TableOne (QoL-study of subjects with PD (data_analysis.R))

## Part 1: Create TableOne for participants

# 1.1: Helper function: recodes variables as factors
recode_factors <- function(df, vars, levels_labels = NULL) {
  df[vars] <- lapply(df[vars], function(x) factor(as.character(x)))
  if (!is.null(levels_labels)) {
    for (var in names(levels_labels)) {
      df[[var]] <- factor(df[[var]], levels = levels_labels[[var]]$levels, labels = levels_labels[[var]]$labels)
    }
  }
  return(df)
}

# Duplicates df_recoded as df_tableone for clarity
df_tableone <- df_recoded

# 1.2: Recode Part A and Part B variables to factors
factor_vars_partA <- c(	"age_r", "sex_r", "living_area_r", 
                        "education_r_r", "financial_stability_r_r", "family_status_r_r", 
                        "living_situation_r_r")
factor_vars_partB <- c(	"who5_depr_r", "comorb_r", "time_from_diagnosis_r", 
                        "age_at_diagnosis_r", "disturbances_sleep_APD", "falls_r", 
                        "assist_mov_r", "ms_fluctuation_APD")

df_tableone <- recode_factors(df_tableone, factor_vars_partA)
df_tableone <- recode_factors(df_tableone, factor_vars_partB)

# 1.3: Define specific factor levels and labels for Part A and Part B
levels_labels <- list(
  sex = list(levels = c("1", "2", "3"), labels = c("Male", "Female", "Divers")),
  age_r = list(levels = c("1", "2", "3"), labels = c("≤ 60 years", "61 - 69 years", "≥ 70 years")),
  education_r_r = list(levels = c("1", "2", "3"), labels = c("Primary school", "Higher secondary school", "University degree or higher")),
  financial_stability_r_r = list(levels = c("1", "2"), labels = c("Most of the time", "Some of the time or less")),
  family_status_r_r = list(levels = c("1", "2"), labels = c("Without a partner", "With a partner")),
  living_situation_r_r = list(levels = c("1", "2"), labels = c("Independently", "With professional support")),
  living_area_r = list(levels = c("1", "2", "3"), labels = c("Large city", "Medium-small city", "Rural area")),
  
  who5_depr_r = list(levels = c("0", "1"), labels = c("No depression", "Impaired quality of life")),
  comorb_r = list(levels = c("1", "2"), labels = c("Not Present", "Present")),
  time_from_diagnosis_r = list(levels = c("1", "2", "3", "4", "5"), labels = c("<2 years", "2-5 years", "5-10 years", "10-15 years", ">15 years")),
  age_at_diagnosis_r = list(levels = c("1", "2"), labels = c("<50 years", "≥50 years")),
  disturbances_sleep_APD = list(levels = c("0", "1"), labels = c("No", "Yes")),
  ms_fluctuation_APD = list(levels = c("0", "1"), labels = c("No", "Yes")),
  assist_mov_r = list(levels = c("0", "1"), labels = c("No assisted movement", "Assisted movement")),
  falls_r = list(levels = c("1", "2"), labels = c("No", "Yes"))
)

df_tableone <- recode_factors(df_tableone, names(levels_labels), levels_labels)

# 1.4: Select variables and create labels for TableOne
Vars <- c("age", "age_r", "sex", "education_r_r", "financial_stability_r_r", "family_status_r_r", 
          "living_situation_r_r", "living_area_r", "who5_depr_r", "comorb_r", "age_at_diagnosis_r", 
          "time_from_diagnosis_r", "disturbances_sleep_APD", "ms_fluctuation_APD", "assist_mov_r", "falls_r")

colnames_Vars <- c("Age", "Age categorized", "Gender", "Education level", "Financial stability", 
                   "Family status", "Household situation", "Living area", "Depression", "Comorbidity", 
                   "Age at diagnosis", "Time since diagnosis", "Sleep disturbances", 
                   "Motor symptoms fluctuation", "Assisted movement", "Falls")

# 1.5: TableOne function
create_table_one <- function(df, vars, colnames_vars, factor_vars) {
  df_tableOne <- df %>% dplyr::select(all_of(vars))
  colnames(df_tableOne) <- colnames_vars
  results.tableOne <- CreateTableOne(vars = colnames_vars, factorVars = factor_vars, data = df_tableOne)
  print(results.tableOne, nonnormal = c("Time since diagnosis", "Education level"), showAllLevels = TRUE)
  return(results.tableOne)
}

# Create the TableOne and display
factVars <- colnames_Vars[-c(1, 2)]  # Exclude continuous variables
results.tableOne <- create_table_one(df_tableone, Vars, colnames_Vars, factVars)
print(head(df_tableone))

# 1.6: Export TableOne to MS Excel file
export_table_one <- function(results, filename = "Table1.xlsx") {
  tableOne_matrix <- print(results, nonnormal = c("Time since diagnosis", "Education level"), showAllLevels = TRUE)
  tableOne_with_rowname <- cbind(Variable = rownames(tableOne_matrix), tableOne_matrix)
  
  wb1 <- createWorkbook()
  addWorksheet(wb1, "Table1")
  writeData(wb1, "Table1", tableOne_with_rowname)
  setColWidths(wb1, "Table1", cols = 1:ncol(tableOne_with_rowname), widths = "auto")
  saveWorkbook(wb1, file = file.path(getwd(), "results", filename), overwrite = TRUE)
}

export_table_one(results.tableOne)
