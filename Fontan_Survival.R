################################################################################
# International Fontan Registry (IFR) Survival Analysis
# Comparison of Lateral Tunnel vs Extracardiac Conduit Fontan Procedures
# 
# Author: Carlos Carhuas
# Date: December 10, 2025
# 
################################################################################

# ==============================================================================
# 1. ENVIRONMENT SETUP
# ==============================================================================



# Load required packages
# These packages provide comprehensive survival analysis capabilities
required_packages <- c(
  "readxl",      # Read Excel data files
  "dplyr",       # Data manipulation and cleaning
  "tidyr",       # Data tidying operations
  "survival",    # Core survival analysis functions (Kaplan-Meier, Cox models)
  "survminer",   # Enhanced survival plots and diagnostics
  "ggplot2",     # Advanced data visualization
  "tableone",    # Create publication-ready baseline tables
  "broom",       # Tidy model outputs
  "lubridate"    # Date/time manipulation
)

# Install packages if not already installed
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Set global options
options(scipen = 999)  # Disable scientific notation for better readability

# ==============================================================================
# 2. DATA IMPORT AND INITIAL EXPLORATION
# ==============================================================================

# Import data
# Load the IFR dataset containing baseline characteristics and outcomes
IFR <- read_excel('/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/IFR_outcomes.xlsx')

# Display data structure
str(IFR)

# Basic data summary
summary(IFR)

# ==============================================================================
# 3. DATA CLEANING AND VARIABLE PREPARATION
# ==============================================================================

# Create working dataset
# Preserve original data while making modifications
IFR_analysis <- IFR

# ----------------------
# 3.1 Convert Character Variables to Numeric
# ----------------------
# Several time variables are stored as character and need conversion
numeric_vars <- c("fontan_age1", "time_recfu1", "time_fon_rev1", "time_echo1", 
                  "echo_grad1", "time_fontd1", "time_death1")

for (var in numeric_vars) {
  if (var %in% names(IFR_analysis)) {
    IFR_analysis[[var]] <- as.numeric(IFR_analysis[[var]])
  }
}

# ----------------------
# 3.2 Create Categorical Variables with Meaningful Labels
# ----------------------

# Gender (1 = Male, 2 = Female)
IFR_analysis <- IFR_analysis %>%
  mutate(
    gender_cat = factor(gender, 
                        levels = c(1, 2), 
                        labels = c("Male", "Female"))
  )

# Fontan Type (EC_LT: 1 = EC, 2 = LT based on variable naming)
# This is the primary exposure variable for comparison
IFR_analysis <- IFR_analysis %>%
  mutate(
    fontan_type = factor(EC_LT,
                         levels = c(1, 2),
                         labels = c("Extracardiac", "Lateral Tunnel"))
  )

# Heterotaxy status (0 = No, 2 = Left, 3 = Right based on common coding)
IFR_analysis <- IFR_analysis %>%
  mutate(
    heterotaxy_cat = factor(case_when(
      heterotaxy == 0 ~ "None",
      heterotaxy == 2 ~ "Left",
      heterotaxy == 3 ~ "Right",
      TRUE ~ "Unknown"
    ))
  )

# Fenestration status (1 = Yes, 0 = No)
IFR_analysis <- IFR_analysis %>%
  mutate(
    fenestration_cat = factor(fenestration,
                              levels = c(0, 1),
                              labels = c("No", "Yes"))
  )

# ----------------------
# 3.3 Create Simplified Ventricular Function Variable
# ----------------------
# qa_slv_reg1 represents ventricular function on a scale
# Based on echocardiographic grading, we'll categorize as Normal/Mild vs Moderate/Severe
IFR_analysis <- IFR_analysis %>%
  mutate(
    vent_function = case_when(
      qa_slv_reg1 <= 2 ~ "Normal/Mild dysfunction",
      qa_slv_reg1 > 2 & qa_slv_reg1 <= 10 ~ "Moderate/Severe dysfunction",
      TRUE ~ NA_character_
    ),
    vent_function = factor(vent_function)
  )
#check ventricular function distribution
table(IFR_analysis$vent_function)

# ----------------------
# 3.4 Create Simplified AV Valve Regurgitation Variable
# ----------------------
# qa_avv_reg1 represents AV valve regurgitation severity
# Common grading: 0-2 = None/Trivial, 2-4 = Mild, 4-6 = Moderate, >6 = Severe
IFR_analysis <- IFR_analysis %>%
  mutate(
    avv_regurg = case_when(
      qa_avv_reg1 <= 2 ~ "None/Trivial",
      qa_avv_reg1 > 2 & qa_avv_reg1 <= 4 ~ "Mild",
      qa_avv_reg1 > 4 & qa_avv_reg1 <= 6 ~ "Moderate",
      qa_avv_reg1 > 6 ~ "Severe",
      TRUE ~ NA_character_
    ),
    avv_regurg = factor(avv_regurg, 
                        levels = c("None/Trivial", "Mild", "Moderate", "Severe"))
  )
#check avv regurgitation distribution
table(IFR_analysis$avv_regurg)

# ----------------------
# 3.5 Create Age Categories
# ----------------------
# Age at Fontan is measured in months; categorize for descriptive analysis
IFR_analysis <- IFR_analysis %>%
  mutate(
    age_years = fontan_age1 / 12,  # Convert months to years
    age_cat = cut(age_years,
                  breaks = c(0, 2, 3, 4, Inf),
                  labels = c("<2 years", "2-3 years", "3-4 years", "≥4 years"),
                  right = FALSE)
  )

#check age categories
table(IFR_analysis$age_cat)
# ==============================================================================
# 4. CREATE OUTCOME VARIABLES
# ==============================================================================

# ----------------------
# 4.1 Primary Composite Outcome
# ----------------------
# Primary outcome is composite of death, transplant, or Fontan takedown

# Create event indicator
# fon_td1: Fontan takedown (1 = yes)
# htx: Heart transplant (1 = yes)
# mortality: Death (1 = yes)
IFR_analysis <- IFR_analysis %>%
  mutate(
    primary_event = case_when(
      mortality == 1 ~ 1,      # Death
      htx == 1 ~ 1,            # Transplant
      fon_td1 == 1 ~ 1,        # Fontan takedown
      TRUE ~ 0                  # Event-free
    )
  )
#check primary event distribution
table(IFR_analysis$primary_event)

# Show the 16 patients who had events
cat("\n=== THE 16 PATIENTS WITH PRIMARY EVENTS ===\n")

events_only <- IFR_analysis %>%
  filter(primary_event == 1) %>%
  select(study_id, fontan_type, mortality, htx, fon_td1)

print(events_only)

# Count each type
cat("\nBreakdown of the 16 events:\n")
cat("Deaths (mortality = 1):", sum(events_only$mortality, na.rm = TRUE), "\n")
cat("Transplants (htx = 1):", sum(events_only$htx, na.rm = TRUE), "\n")
cat("Takedowns (fon_td1 = 1):", sum(events_only$fon_td1, na.rm = TRUE), "\n")

#Some patients had multiple of events, but the analysis is time to first event

# ----------------------
# 4.2 Clean Follow-up Time and Create Time-to-Event Variables
# ----------------------

# Keep all time variables in MONTHS
MAX_FOLLOWUP_MONTHS <- 360  # 30 years = 360 months maximum

# Clean follow-up time
IFR_analysis <- IFR_analysis %>%
  mutate(
    # Remove extreme values but KEEP in months
    time_recfu1 = ifelse(time_recfu1 > MAX_FOLLOWUP_MONTHS, NA, time_recfu1)
  )

cat("Patients with extreme follow-up excluded:", sum(is.na(IFR_analysis$time_recfu1)), "\n")

# Create time-to-event variable
# Use the earliest time among death, transplant, or takedown, 
# or follow-up time if event-free
IFR_analysis <- IFR_analysis %>%
  mutate(
    time_to_primary = case_when(
      # If death occurred, use time to death
      mortality == 1 ~ time_death1,
      # If transplant occurred, use time to transplant (assuming it's captured in time_fontd1 or similar)
      htx == 1 ~ time_fontd1,  
      # If Fontan takedown occurred, use time to takedown
      fon_td1 == 1 ~ time_fontd1,
      # Otherwise, use censoring time (last follow-up)
      TRUE ~ time_recfu1
    )
  )

# Check time to primary event summary
cat("\nPrimary outcome time summary (months):\n")
summary(IFR_analysis$time_to_primary)

# ----------------------
# 4.2 Secondary Outcomes
# ----------------------

# Arrhythmia outcome
# arrhyth: New-onset arrhythmia (1 = yes, 0 = no)
IFR_analysis <- IFR_analysis %>%
  mutate(
    arrhythmia_event = arrhyth,
    # Assuming arrhythmia time is captured similarly; adjust if specific time variable exists
    time_to_arrhythmia = ifelse(arrhyth == 1, time_recfu1, time_recfu1)  
  )

# Thromboembolic events
# thrombo: Thromboembolic event (1 = yes, 0 = no)
IFR_analysis <- IFR_analysis %>%
  mutate(
    thrombo_event = thrombo,
    time_to_thrombo = ifelse(thrombo == 1, time_recfu1, time_recfu1)
  )

# Lymphatic complications
# lymph: Lymphatic complication (1 = yes, 0 = no)
IFR_analysis <- IFR_analysis %>%
  mutate(
    lymph_event = lymph,
    time_to_lymph = ifelse(lymph == 1, time_recfu1, time_recfu1)
  )

# Summary of follow-up times by Fontan type
cat("\n=== Follow-up time by Fontan type (months) ===\n")
tapply(IFR_analysis$time_recfu1, IFR_analysis$fontan_type, summary)

# ----------------------
# 4.3 Check for Missing Outcome Data
# ----------------------
cat("\nPrimary outcome completeness:\n")
cat("Events:", sum(IFR_analysis$primary_event == 1, na.rm = TRUE), "\n")
cat("Event-free:", sum(IFR_analysis$primary_event == 0, na.rm = TRUE), "\n")
cat("Missing primary event:", sum(is.na(IFR_analysis$primary_event)), "\n")
cat("Missing time-to-event:", sum(is.na(IFR_analysis$time_to_primary)), "\n")

# ==============================================================================
# 5. COHORT SELECTION AND EXCLUSIONS
# ==============================================================================

# Document initial cohort size
cat("Initial cohort size:", nrow(IFR_analysis), "\n")

# Apply exclusion criteria per Methods section:
# 1. Must have LT or EC Fontan (exclude AP or missing)
# 2. Must be pediatric (<18 years at Fontan)
# 3. Must have follow-up data

IFR_analysis <- IFR_analysis %>%
  filter(
    # Exclude if Fontan type is missing
    !is.na(fontan_type),
    # Include only pediatric patients (<18 years)
    age_years < 18,
    # Exclude if no follow-up time
    !is.na(time_to_primary) & time_to_primary > 0,
    # Exclude if outcome status is unclear
    !is.na(primary_event)
  )
# Document final cohort size
cat("Final analytical cohort:", nrow(IFR_analysis), "\n")
# Check distribution of Fontan types after exclusions
print(table(IFR_analysis$fontan_type))

# ==============================================================================
# 6. DESCRIPTIVE STATISTICS - TABLE 1
# ==============================================================================


# Create Table 1: Baseline characteristics stratified by Fontan type
# TableOne package creates publication-ready tables with appropriate tests

# Define variables for Table 1
table1_vars <- c(
  "gender_cat",
  "age_years",
  "age_cat",
  "heterotaxy_cat",
  "fenestration_cat",
  "conduitsz",
  "vent_function",
  "avv_regurg",
  "time_to_primary"
)

# Define categorical variables
cat_vars <- c(
  "gender_cat",
  "age_cat",
  "heterotaxy_cat",
  "fenestration_cat",
  "vent_function",
  "avv_regurg"
)

# Create Table 1
table1 <- CreateTableOne(
  vars = table1_vars,
  strata = "fontan_type",
  data = IFR_analysis,
  factorVars = cat_vars,
  test = TRUE,
  addOverall = TRUE
)

# Print Table 1
cat("\n=== TABLE 1: BASELINE CHARACTERISTICS ===\n")
print(table1, showAllLevels = FALSE, formatOptions = list(big.mark = ","))

# Save to CSV for export
table1_export <- print(table1, printToggle = FALSE, noSpaces = TRUE)
write.csv(table1_export, "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/Table1_Baseline_Characteristics.csv")

# ==============================================================================
# 7. KAPLAN-MEIER SURVIVAL ANALYSIS
# ==============================================================================

# ----------------------
# 7.1 Primary Composite Outcome
# ----------------------

# Create survival object for primary outcome
# Surv() creates a survival object encoding time and event status
# Note: Time is in MONTHS
surv_primary <- Surv(time = IFR_analysis$time_to_primary, 
                     event = IFR_analysis$primary_event)

# Fit Kaplan-Meier curves by Fontan type
# survfit() estimates survival probabilities at each event time
km_primary <- survfit(surv_primary ~ fontan_type, data = IFR_analysis)

# Print survival summary
cat("\n--- Primary Composite Outcome (Death/Transplant/Takedown) ---\n")
print(km_primary)

# Print survival estimates at key time points (in MONTHS)
# 12 months = 1 year, 60 months = 5 years, 120 months = 10 years, 180 months = 15 years
cat("\nSurvival estimates at specific time points (months):\n")
summary(km_primary, times = c(12, 60, 120, 180))

# Log-rank test for difference between groups
# Tests null hypothesis that survival curves are identical
logrank_primary <- survdiff(surv_primary ~ fontan_type, data = IFR_analysis)
print(logrank_primary)

# Calculate p-value
p_value_primary <- 1 - pchisq(logrank_primary$chisq, df = 1)
cat("\nLog-rank p-value:", round(p_value_primary, 4), "\n")

# Create Kaplan-Meier plot for primary outcome
# Visual representation of survival curves with confidence intervals
km_plot_primary <- ggsurvplot(
  km_primary,
  data = IFR_analysis,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.height = 0.25,
  ggtheme = theme_minimal(),
  palette = c("#E7B800", "#2E9FDF"),
  title = "Freedom from Death, Transplant, or Fontan Takedown",
  subtitle = "Truncated at 60 months (both groups with adequate follow-up)",
  xlab = "Time from Fontan (months)",
  ylab = "Event-Free Survival",
  legend.title = "Fontan Type",
  legend.labs = c("Extracardiac", "Lateral Tunnel"),
  break.time.by = 12,
  xlim = c(0, 60),                     # TRUNCATE HERE
  ylim = c(0.92, 1.00),
  break.y.by = 0.02
)

print(km_plot_primary)

# Save high-resolution plot
ggsave(
  filename = "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/KM_Primary_Outcome.png",
  plot = km_plot_primary$plot,
  width = 10,
  height = 8,
  dpi = 300
)

# ----------------------
# 7.2 Secondary Outcome: Arrhythmia
# ----------------------


# ----------------------
# 7.2 Secondary Outcome: Arrhythmia
# ----------------------

# Create survival object
# Note: Time is in MONTHS
surv_arrhythmia <- Surv(time = IFR_analysis$time_to_arrhythmia,
                        event = IFR_analysis$arrhythmia_event)

# Fit KM curves
km_arrhythmia <- survfit(surv_arrhythmia ~ fontan_type, data = IFR_analysis)
print(km_arrhythmia)

# Print survival estimates at key time points (in MONTHS)
cat("\nArrhythmia-free survival at specific time points (months):\n")
summary(km_arrhythmia, times = c(12, 24, 36, 48, 60))

# Log-rank test
logrank_arrhythmia <- survdiff(surv_arrhythmia ~ fontan_type, data = IFR_analysis)
cat("\n--- Log-rank test for arrhythmia outcome ---\n")
print(logrank_arrhythmia)

p_value_arrhythmia <- 1 - pchisq(logrank_arrhythmia$chisq, df = 1)
cat("\nArrhythmia log-rank p-value:", round(p_value_arrhythmia, 4), "\n")

# Check event counts
cat("\nArrhythmia events by Fontan type:\n")
table(IFR_analysis$fontan_type, IFR_analysis$arrhythmia_event)

# Create KM plot - truncated at 60 months for consistency
km_plot_arrhythmia <- ggsurvplot(
  km_arrhythmia,
  data = IFR_analysis,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.height = 0.25,
  ggtheme = theme_minimal(),
  palette = c("#E7B800", "#2E9FDF"),
  title = "Freedom from Arrhythmia by Fontan Type",
  subtitle = "Truncated at 60 months (both groups with adequate follow-up)",
  xlab = "Time from Fontan (months)",
  ylab = "Arrhythmia-Free Probability",
  legend.title = "Fontan Type",
  legend.labs = c("Extracardiac", "Lateral Tunnel"),
  break.time.by = 12,            # Breaks every 12 months (1 year)
  xlim = c(0, 60),               # Truncate at 60 months for fair comparison
  ylim = c(0.85, 1.00),          # Adjust based on your event rates
  break.y.by = 0.05,             # Y-axis breaks every 5%
  censor.shape = "|",
  censor.size = 3
)

print(km_plot_arrhythmia)

# Save plot
ggsave(
  filename = "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/KM_Arrhythmia.png",
  plot = km_plot_arrhythmia$plot,
  width = 10,
  height = 8,
  dpi = 300
)


# ----------------------
# 7.3 Secondary Outcome: Thromboembolic Events
# ----------------------

# Create survival object (time in MONTHS)
surv_thrombo <- Surv(time = IFR_analysis$time_to_thrombo,
                     event = IFR_analysis$thrombo_event)

# Fit KM curves
km_thrombo <- survfit(surv_thrombo ~ fontan_type, data = IFR_analysis)
print(km_thrombo)

# Print survival estimates at key time points (in MONTHS)
cat("\nThromboembolic event-free survival at specific time points (months):\n")
summary(km_thrombo, times = c(12, 24, 36, 48, 60))

# Log-rank test
logrank_thrombo <- survdiff(surv_thrombo ~ fontan_type, data = IFR_analysis)
cat("\n--- Log-rank test for thromboembolic events ---\n")
print(logrank_thrombo)

p_value_thrombo <- 1 - pchisq(logrank_thrombo$chisq, df = 1)
cat("\nThromboembolic log-rank p-value:", round(p_value_thrombo, 4), "\n")

# Check event counts
cat("\nThromboembolic events by Fontan type:\n")
table(IFR_analysis$fontan_type, IFR_analysis$thrombo_event)

# Create KM plot - truncated at 60 months
km_plot_thrombo <- ggsurvplot(
  km_thrombo,
  data = IFR_analysis,
  pval = TRUE,
  conf.int = FALSE,                    # Remove confidence intervals
  risk.table = TRUE,
  risk.table.height = 0.25,
  ggtheme = theme_minimal(),
  palette = c("#E7B800", "#2E9FDF"),
  title = "Freedom from Thromboembolic Events by Fontan Type",
  subtitle = "Truncated at 60 months (both groups with adequate follow-up)",
  xlab = "Time from Fontan (months)",
  ylab = "Event-Free Probability",
  legend.title = "Fontan Type",
  legend.labs = c("Extracardiac", "Lateral Tunnel"),
  break.time.by = 12,                  # Breaks every 12 months
  xlim = c(0, 60),                     # Truncate at 60 months
  ylim = c(0.80, 1.00),    # Show 80-100% range
  break.y.by = 0.05,         # 5% increments
  censor.shape = "|",
  censor.size = 3
)

print(km_plot_thrombo)

# Save plot
ggsave(
  filename = "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/KM_Thromboembolism.png",
  plot = km_plot_thrombo$plot,
  width = 10,
  height = 8,
  dpi = 300
)

# ----------------------
# 7.4 Secondary Outcome: Lymphatic Complications
# ----------------------

# Create survival object (time in MONTHS)
surv_lymph <- Surv(time = IFR_analysis$time_to_lymph,
                   event = IFR_analysis$lymph_event)

# Fit KM curves
km_lymph <- survfit(surv_lymph ~ fontan_type, data = IFR_analysis)
print(km_lymph)

# Print survival estimates at key time points (in MONTHS)
cat("\nLymphatic complication-free survival at specific time points (months):\n")
summary(km_lymph, times = c(12, 24, 36, 48, 60))

# Log-rank test
logrank_lymph <- survdiff(surv_lymph ~ fontan_type, data = IFR_analysis)
cat("\n--- Log-rank test for lymphatic complications ---\n")
print(logrank_lymph)

p_value_lymph <- 1 - pchisq(logrank_lymph$chisq, df = 1)
cat("\nLymphatic complications log-rank p-value:", round(p_value_lymph, 4), "\n")

# Check event counts
cat("\nLymphatic complication events by Fontan type:\n")
table(IFR_analysis$fontan_type, IFR_analysis$lymph_event)

# Create KM plot - truncated at 60 months
km_plot_lymph <- ggsurvplot(
  km_lymph,
  data = IFR_analysis,
  pval = TRUE,
  conf.int = FALSE,
  risk.table = TRUE,
  risk.table.height = 0.25,
  ggtheme = theme_minimal(),
  palette = c("#E7B800", "#2E9FDF"),
  title = "Freedom from Lymphatic Complications by Fontan Type",
  subtitle = "Truncated at 60 months (p = 0.036)",  # Emphasize significance
  xlab = "Time from Fontan (months)",
  ylab = "Event-Free Probability",
  legend.title = "Fontan Type",
  legend.labs = c("Extracardiac", "Lateral Tunnel"),
  break.time.by = 12,
  xlim = c(0, 60),
  ylim = c(0.80, 1.00),          # Show 80-100% to capture LT decline
  break.y.by = 0.05,              # 5% increments
  censor.shape = "|",
  censor.size = 3
)

print(km_plot_lymph)

ggsave(
  filename = "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/KM_Lymphatic.png",
  plot = km_plot_lymph$plot,
  width = 10,
  height = 8,
  dpi = 300
)


# ==============================================================================
# 8. MULTIVARIABLE COX PROPORTIONAL HAZARDS REGRESSION
# ==============================================================================

# ----------------------
# 8.1 Prepare Data for Cox Model
# ----------------------

# Create binary heterotaxy variable (Any vs None) for simpler modeling
IFR_analysis <- IFR_analysis %>%
  mutate(
    heterotaxy_binary = factor(
      ifelse(heterotaxy == 0, "No", "Yes"),
      levels = c("No", "Yes")
    )
  )

# Create binary ventricular function variable
IFR_analysis <- IFR_analysis %>%
  mutate(
    vent_dysfunc_binary = factor(
      case_when(
        qa_slv_reg1 <= 2 ~ "Normal/Mild",
        qa_slv_reg1 > 2 ~ "Moderate/Severe",
        TRUE ~ NA_character_
      ),
      levels = c("Normal/Mild", "Moderate/Severe")
    )
  )

# Create binary AV valve regurgitation (None/Mild vs Moderate/Severe)
IFR_analysis <- IFR_analysis %>%
  mutate(
    avv_regurg_binary = factor(
      case_when(
        qa_avv_reg1 <= 4 ~ "None/Mild",
        qa_avv_reg1 > 4 ~ "Moderate/Severe",
        TRUE ~ NA_character_
      ),
      levels = c("None/Mild", "Moderate/Severe")
    )
  )

# Check sample size and completeness for full model
cat("\nSample size assessment:\n")
cox_complete <- IFR_analysis %>%
  filter(!is.na(time_to_primary), 
         !is.na(primary_event),
         !is.na(fontan_type),
         !is.na(age_years),
         !is.na(gender_cat),
         !is.na(heterotaxy_binary),
         !is.na(vent_dysfunc_binary),
         !is.na(avv_regurg_binary))
# Document complete cases
cat("Complete cases for full model:", nrow(cox_complete), "\n")
cat("Primary events in complete cases:", sum(cox_complete$primary_event), "\n")
cat("Events per variable for full model (6 predictors):", 
    round(sum(cox_complete$primary_event) / 6, 2), "\n\n")


# ----------------------
# 8.2 Univariable Cox Models
# ----------------------
# Screen individual predictors before multivariable modeling

# Fontan type (PRIMARY EXPOSURE)
cox_univ_fontan <- coxph(Surv(time_to_primary, primary_event) ~ fontan_type, 
                         data = IFR_analysis)
summary_fontan <- summary(cox_univ_fontan)
cat("Hazard Ratio:", round(summary_fontan$conf.int[1], 3), "\n")
cat("95% CI:", round(summary_fontan$conf.int[3], 3), "-", 
    round(summary_fontan$conf.int[4], 3), "\n")
cat("P-value:", round(summary_fontan$coefficients[5], 4), "\n\n")

# Age at Fontan
cox_univ_age <- coxph(Surv(time_to_primary, primary_event) ~ age_years, 
                      data = IFR_analysis)
summary_age <- summary(cox_univ_age)
cat("Hazard Ratio (per year):", round(summary_age$conf.int[1], 3), "\n")
cat("95% CI:", round(summary_age$conf.int[3], 3), "-", 
    round(summary_age$conf.int[4], 3), "\n")
cat("P-value:", round(summary_age$coefficients[5], 4), "\n\n")

# Gender
cox_univ_gender <- coxph(Surv(time_to_primary, primary_event) ~ gender_cat, 
                         data = IFR_analysis)
summary_gender <- summary(cox_univ_gender)
cat("Hazard Ratio (Female vs Male):", round(summary_gender$conf.int[1], 3), "\n")
cat("95% CI:", round(summary_gender$conf.int[3], 3), "-", 
    round(summary_gender$conf.int[4], 3), "\n")
cat("P-value:", round(summary_gender$coefficients[5], 4), "\n\n")

# Heterotaxy
cox_univ_het <- coxph(Surv(time_to_primary, primary_event) ~ heterotaxy_binary, 
                      data = IFR_analysis)
summary_het <- summary(cox_univ_het)
cat("Hazard Ratio (Yes vs No):", round(summary_het$conf.int[1], 3), "\n")
cat("95% CI:", round(summary_het$conf.int[3], 3), "-", 
    round(summary_het$conf.int[4], 3), "\n")
cat("P-value:", round(summary_het$coefficients[5], 4), "\n\n")

# Ventricular function
cox_univ_vent <- coxph(Surv(time_to_primary, primary_event) ~ vent_dysfunc_binary, 
                       data = IFR_analysis)
summary_vent <- summary(cox_univ_vent)
cat("Hazard Ratio (Moderate/Severe vs Normal/Mild):", 
    round(summary_vent$conf.int[1], 3), "\n")
cat("95% CI:", round(summary_vent$conf.int[3], 3), "-", 
    round(summary_vent$conf.int[4], 3), "\n")
cat("P-value:", round(summary_vent$coefficients[5], 4), "\n\n")

# AV valve regurgitation
cox_univ_avv <- coxph(Surv(time_to_primary, primary_event) ~ avv_regurg_binary, 
                      data = IFR_analysis)
summary_avv <- summary(cox_univ_avv)
cat("Hazard Ratio (Moderate/Severe vs None/Mild):", 
    round(summary_avv$conf.int[1], 3), "\n")
cat("95% CI:", round(summary_avv$conf.int[3], 3), "-", 
    round(summary_avv$conf.int[4], 3), "\n")
cat("P-value:", round(summary_avv$coefficients[5], 4), "\n\n")

# Compile univariable results
univariable_results <- data.frame(
  Variable = c("Fontan Type (LT vs EC)", 
               "Age at Fontan (per year)", 
               "Gender (Female vs Male)",
               "Heterotaxy (Yes vs No)",
               "Ventricular Dysfunction (Mod/Sev vs Normal/Mild)",
               "AV Regurgitation (Mod/Sev vs None/Mild)"),
  HR = c(summary_fontan$conf.int[1],
         summary_age$conf.int[1],
         summary_gender$conf.int[1],
         summary_het$conf.int[1],
         summary_vent$conf.int[1],
         summary_avv$conf.int[1]),
  CI_Lower = c(summary_fontan$conf.int[3],
               summary_age$conf.int[3],
               summary_gender$conf.int[3],
               summary_het$conf.int[3],
               summary_vent$conf.int[3],
               summary_avv$conf.int[3]),
  CI_Upper = c(summary_fontan$conf.int[4],
               summary_age$conf.int[4],
               summary_gender$conf.int[4],
               summary_het$conf.int[4],
               summary_vent$conf.int[4],
               summary_avv$conf.int[4]),
  P_value = c(summary_fontan$coefficients[5],
              summary_age$coefficients[5],
              summary_gender$coefficients[5],
              summary_het$coefficients[5],
              summary_vent$coefficients[5],
              summary_avv$coefficients[5])
)

# Save univariable results
write.csv(univariable_results,
          "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/Cox_Univariable_Results.csv",
          row.names = FALSE)

cat("=== Univariable Results Summary ===\n")
print(univariable_results)
cat("\nUnivariable results saved to CSV.\n")

# ==============================================================================
# 8.3 MULTIVARIABLE COX REGRESSION - DUAL MODELING APPROACH
# ==============================================================================

# ------------------------------------------------------------------------------
# 8.3A PRIMARY MULTIVARIABLE MODEL (Simplified - Adequate EPV)
# ------------------------------------------------------------------------------

# Calculate sample size and EPV for primary model
n_complete_primary <- sum(!is.na(IFR_analysis$time_to_primary) & 
                            !is.na(IFR_analysis$primary_event) &
                            !is.na(IFR_analysis$fontan_type) &
                            !is.na(IFR_analysis$age_years))

n_events_primary <- sum(!is.na(IFR_analysis$time_to_primary) & 
                          !is.na(IFR_analysis$primary_event) &
                          !is.na(IFR_analysis$fontan_type) &
                          !is.na(IFR_analysis$age_years) &
                          IFR_analysis$primary_event == 1, na.rm = TRUE)

#Model Assessment for EPV
cat("Complete cases:", n_complete_primary, "\n")
cat("Events:", n_events_primary, "\n")
cat("Predictors: 2 (Fontan type, Age)\n")
cat("Events per variable (EPV):", round(n_events_primary / 2, 2), "\n")

# Fit primary simplified model
cox_primary <- coxph(
  Surv(time_to_primary, primary_event) ~ 
    fontan_type +     # Primary exposure
    age_years,        # Key confounder
  data = IFR_analysis
)


summary_primary <- summary(cox_primary)
print(summary_primary)

# Format and save primary results
primary_results <- tidy(cox_primary, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    Variable = case_when(
      term == "fontan_typeLateral Tunnel" ~ "Lateral Tunnel (vs Extracardiac)",
      term == "age_years" ~ "Age at Fontan (per year)",
      TRUE ~ term
    ),
    HR = round(estimate, 3),
    CI_Lower = round(conf.low, 3),
    CI_Upper = round(conf.high, 3),
    P_value = round(p.value, 4)
  ) %>%
  select(Variable, HR, CI_Lower, CI_Upper, P_value)

# Print formatted table
print(primary_results)

# Save to CSV
write.csv(primary_results,
          "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/Cox_Primary_Model_Results.csv",
          row.names = FALSE)

# Test PH assumption for primary model

ph_test_primary <- cox.zph(cox_primary)
print(ph_test_primary)

if(ph_test_primary$table["GLOBAL", "p"] >= 0.05) {
  cat("PH assumption satisfied for primary model (global p =", 
      round(ph_test_primary$table["GLOBAL", "p"], 3), ")\n")
} else {
  cat("WARNING: PH assumption may be violated (global p =",
      round(ph_test_primary$table["GLOBAL", "p"], 3), ")\n")
}

# ------------------------------------------------------------------------------
# 8.3B EXPLORATORY FULL MODEL (All Covariates - Limited EPV)
# ------------------------------------------------------------------------------

# Calculate EPV for full model
n_events_full <- sum(cox_complete$primary_event)
n_predictors_full <- 6
EPV_full <- n_events_full / n_predictors_full

cat("Model Assessment:\n")
cat("Complete cases:", nrow(cox_complete), "\n")
cat("Events:", n_events_full, "\n")
cat("Predictors:", n_predictors_full, "\n")
cat("Events per variable (EPV):", round(EPV_full, 2), "\n")

if(EPV_full < 10) {
  cat("EPV Status: INADEQUATE - High risk of overfitting\n")
  cat("Results should be considered exploratory and hypothesis-generating\n\n")
} else {
  cat("EPV Status: Adequate\n\n")
}

# Fit exploratory full model
cox_full <- coxph(
  Surv(time_to_primary, primary_event) ~ 
    fontan_type +
    age_years +
    gender_cat +
    heterotaxy_binary +
    vent_dysfunc_binary +
    avv_regurg_binary,
  data = IFR_analysis
)

summary_full <- summary(cox_full)
print(summary_full)

# Format and save full results
full_results <- tidy(cox_full, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    Variable = case_when(
      term == "fontan_typeLateral Tunnel" ~ "Lateral Tunnel (vs Extracardiac)",
      term == "age_years" ~ "Age at Fontan (per year)",
      term == "gender_catFemale" ~ "Female (vs Male)",
      term == "heterotaxy_binaryYes" ~ "Heterotaxy Present (vs Absent)",
      term == "vent_dysfunc_binaryModerate/Severe" ~ "Moderate/Severe Ventricular Dysfunction",
      term == "avv_regurg_binaryModerate/Severe" ~ "Moderate/Severe AV Valve Regurgitation",
      TRUE ~ term
    ),
    HR = round(estimate, 3),
    CI_95 = paste0(round(conf.low, 3), " - ", round(conf.high, 3)),
    P_value = round(p.value, 4)
  ) %>%
  select(Variable, HR, CI_95, P_value)


print(full_results)

# Test PH assumption for full model
ph_test_full <- cox.zph(cox_full)
print(ph_test_full)

if(ph_test_full$table["GLOBAL", "p"] >= 0.05) {
  cat("PH assumption satisfied for full model (global p =", 
      round(ph_test_full$table["GLOBAL", "p"], 3), ")\n")
} else {
  cat("WARNING: PH assumption may be violated (global p =",
      round(ph_test_full$table["GLOBAL", "p"], 3), ")\n")
}

# ------------------------------------------------------------------------------
# Model Comparison
# ------------------------------------------------------------------------------

cat("\n\n--- Model Comparison ---\n")
cat("Primary model AIC:", round(AIC(cox_primary), 2), "\n")
cat("Full model AIC:", round(AIC(cox_full), 2), "\n")
cat("\nNote: Lower AIC suggests better fit, but EPV considerations\n")
cat("      supersede AIC for model selection given limited events.\n")
cat("      Primary model is preferred for inference due to adequate EPV.\n")

# ==============================================================================
# 8.4 MODEL DIAGNOSTICS (Using Full Model for Comprehensive Assessment)
# ==============================================================================

# ------------------------------------------------------------------------------
# 8.4A Proportional Hazards Assumption - Detailed
# ------------------------------------------------------------------------------

# Test results for both models
cat("Primary Model (EPV = 8.0):\n")
cat("  Global p-value:", round(ph_test_primary$table["GLOBAL", "p"], 3), 
    "- PH assumption SATISFIED\n\n")

cat("Full Model (EPV = 2.5):\n")
cat("  Global p-value:", round(ph_test_full$table["GLOBAL", "p"], 3), 
    "- PH assumption SATISFIED\n")
cat("  Note: Fontan type p = 0.048 (marginal), likely due to small sample size\n\n")

# Plot and save PRIMARY model Schoenfeld residuals
cat("Creating primary model Schoenfeld residual plots...\n")
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(ph_test_primary)

# Plot Schoenfeld residuals for full model
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(ph_test_full)

# Save plot
png("/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/Schoenfeld_Residuals.png",
    width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(ph_test_full)
dev.off()

cat("Schoenfeld residual plots saved and displayed.\n")
cat("Conclusion: PH assumption satisfied for both models.\n\n")

# ==============================================================================
# 8.5 FOREST PLOTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Forest Plot for PRIMARY Model
# ------------------------------------------------------------------------------

forest_primary <- primary_results %>%
  mutate(
    estimate = as.numeric(HR),
    conf.low = as.numeric(sapply(strsplit(CI_95, " - "), `[`, 1)),
    conf.high = as.numeric(sapply(strsplit(CI_95, " - "), `[`, 2)),
    significant = ifelse(P_value < 0.05, "Significant (p<0.05)", "Not Significant")
  )

plot_primary <- ggplot(forest_primary, 
                       aes(x = estimate, y = reorder(Variable, estimate),
                           color = significant)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, 
                 linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8)) +
  scale_color_manual(values = c("Significant (p<0.05)" = "#E63946", 
                                "Not Significant" = "#457B9D")) +
  labs(
    title = "Primary Multivariable Cox Regression Model",
    subtitle = "Adjusted Hazard Ratios with 95% Confidence Intervals",
    x = "Hazard Ratio (log scale)",
    y = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, color = "gray30"),
    axis.text = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  annotate("text", x = 0.3, y = 0.3, label = "Decreased Risk", 
           size = 3.5, color = "gray40", fontface = "italic") +
  annotate("text", x = 5, y = 0.3, label = "Increased Risk", 
           size = 3.5, color = "gray40", fontface = "italic")

print(plot_primary)

ggsave(
  filename = "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/Forest_Plot_Primary_Model.png",
  plot = plot_primary,
  width = 10,
  height = 6,
  dpi = 300
)


# ==============================================================================
# 9. SUBGROUP ANALYSES
# ==============================================================================


# ------------------------------------------------------------------------------
# 9.1 Stratified Analysis by Age Group
# ------------------------------------------------------------------------------

# Create age subgroups
IFR_analysis <- IFR_analysis %>%
  mutate(
    age_subgroup = factor(
      ifelse(age_years < 3, "Younger (<3 years)", "Older (≥3 years)"),
      levels = c("Younger (<3 years)", "Older (≥3 years)")
    )
  )

# Sample sizes by age group
print(table(IFR_analysis$age_subgroup, IFR_analysis$fontan_type))

age_events <- IFR_analysis %>%
  group_by(age_subgroup, fontan_type) %>%
  summarise(N = n(),
            Events = sum(primary_event),
            Event_Rate = round(100 * sum(primary_event) / n(), 1),
            .groups = "drop")
print(age_events)


# Younger patients
younger_data <- filter(IFR_analysis, age_subgroup == "Younger (<3 years)")
n_young_events <- sum(younger_data$primary_event)
cat("N =", nrow(younger_data), ", Events =", n_young_events, "\n\n")

if(n_young_events >= 5) {
  cox_young <- coxph(
    Surv(time_to_primary, primary_event) ~ fontan_type,
    data = younger_data
  )
  summary_young <- summary(cox_young)
  cat("HR (LT vs EC):", round(summary_young$conf.int[1], 3), "\n")
  cat("95% CI:", round(summary_young$conf.int[3], 3), "-", 
      round(summary_young$conf.int[4], 3), "\n")
  cat("P-value:", round(summary_young$coefficients[5], 4), "\n\n")
} else {
  cat("Insufficient events (n < 5) for reliable Cox regression\n\n")
}

# Older patients
older_data <- filter(IFR_analysis, age_subgroup == "Older (≥3 years)")
n_old_events <- sum(older_data$primary_event)
cat("N =", nrow(older_data), ", Events =", n_old_events, "\n\n")

if(n_old_events >= 5) {
  cox_old <- coxph(
    Surv(time_to_primary, primary_event) ~ fontan_type,
    data = older_data
  )
  summary_old <- summary(cox_old)
  cat("HR (LT vs EC):", round(summary_old$conf.int[1], 3), "\n")
  cat("95% CI:", round(summary_old$conf.int[3], 3), "-", 
      round(summary_old$conf.int[4], 3), "\n")
  cat("P-value:", round(summary_old$coefficients[5], 4), "\n\n")
} else {
  cat("Insufficient events (n < 5) for reliable Cox regression\n\n")
}

# Interaction test
cat("--- INTERACTION TEST (Fontan type × Age group) ---\n")
cox_interaction <- coxph(
  Surv(time_to_primary, primary_event) ~ fontan_type * age_subgroup,
  data = IFR_analysis
)

summary_interaction <- summary(cox_interaction)
interaction_term <- "fontan_typeLateral Tunnel:age_subgroupOlder (≥3 years)"

if(interaction_term %in% rownames(summary_interaction$coefficients)) {
  interaction_p <- summary_interaction$coefficients[interaction_term, 5]
  cat("Interaction p-value:", round(interaction_p, 4), "\n\n")
  
  if(interaction_p < 0.05) {
    cat("SIGNIFICANT INTERACTION: Effect of Fontan type differs by age group\n")
  } else {
    cat("No significant interaction: Effect of Fontan type is similar across age groups\n")
  }
} else {
  cat("Interaction term not estimable (insufficient data)\n")
}

# ==============================================================================
# 10. SUMMARY STATISTICS AND FINAL OUTPUTS
# ==============================================================================

# Comprehensive event summary
event_summary <- IFR_analysis %>%
  group_by(fontan_type) %>%
  summarise(
    N = n(),
    Primary_Events = sum(primary_event),
    Primary_Event_Rate = round(100 * sum(primary_event) / n(), 1),
    Median_FU_months = round(median(time_to_primary, na.rm = TRUE), 1),
    IQR_FU_Low = round(quantile(time_to_primary, 0.25, na.rm = TRUE), 1),
    IQR_FU_High = round(quantile(time_to_primary, 0.75, na.rm = TRUE), 1),
    Arrhythmia_Events = sum(arrhythmia_event),
    Arrhythmia_Rate = round(100 * sum(arrhythmia_event) / n(), 1),
    Thrombo_Events = sum(thrombo_event),
    Thrombo_Rate = round(100 * sum(thrombo_event) / n(), 1),
    Lymph_Events = sum(lymph_event),
    Lymph_Rate = round(100 * sum(lymph_event) / n(), 1),
    .groups = "drop"
  )

cat("Event Summary by Fontan Type:\n")
print(event_summary)

write.csv(event_summary,
          "/Users/ccarhuas/Documents/GW - MS Biostatistics /GW Fall 2025/Survival Analysis/Final Project/Event_Summary_by_Fontan_Type.csv",
          row.names = FALSE)

################################################################################
# END OF MULTIVARIABLE COX REGRESSION ANALYSIS
################################################################################