# Creates table with multivariate meta-regression for PEESE

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing
library(patchwork) # For combining plots

# Source required functions ----
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/create_mmr_coefficient_plot.R"))

# Data prep ----

## Consolidate estimation methods ---- 
d_no_qc <- d_no_qc %>%
  mutate(group_est_broad = case_when(
    # Group 1: var, standard VAR
    (var == 1 & lp == 0 & vecm == 0 & dyn_ols == 0 & fvar == 0 & tvar == 0 & gvar == 0 & dsge == 0 & varother == 0) ~ "standard_var",
    
    # Group 2: lp_ardl
    ((lp == 1 | dyn_ols == 1) & fvar == 0) ~ "lp_ardl",
    
    # Group 3: favar
    (fvar == 1) ~ "favar",
    
    # Group 4: other_var
    ((varother == 1 | tvar == 1 | gvar == 1 | vecm == 1) & lp == 0 & dyn_ols == 0 & fvar == 0 & dsge == 0) ~ "other_var",
    
    # Group 5: dsge
    (dsge == 1) ~ "dsge",
    
    # Default case
    TRUE ~ "Other"
    
  ))

# Transform into factor and make "standard_var" the reference 
d_no_qc$group_est_broad <- factor(d_no_qc$group_est_broad, 
                               levels = c("standard_var", 
                                          "lp_ardl", 
                                          "favar", 
                                          "other_var", 
                                          "dsge"))
levels(d_no_qc$group_est_broad)

# Transform group_ident_broad into factor and make "chol" the reference 
d_no_qc$group_ident_broad <- factor(d_no_qc$group_ident_broad, 
                                    levels = c("chol", 
                                               "hf", 
                                               "nr", 
                                               "signr", 
                                               "idother"))
levels(d_no_qc$group_ident_broad)

## Create long run interest rate dummy ----
d_no_qc$interest_rate_long <- 1 - d_no_qc$interest_rate_short

## Consolidate interest rates more granuarly ----  
### --- Step 1: Define Classification Rules ----
# Function to classify inttypes based on keywords like "month", "day", "year", etc.
classify_inttype <- function(inttype) {
  if (grepl("overnight|a_money_market|callrate|call_rate|cashrate|cash_rate|q_fed_funds|a_fed_funds|a_fedfunds|a_eonia", inttype, ignore.case = TRUE)) {
    return("overnight")
  } else if (grepl("7_day|14_day|month|short_term|short_rate|a_repo|a_discount|a_interbank|a_MRO|a_policy_rate|a_target_rate|a_pribor|a_wibor|a_bubor|a_bank_rate", inttype, ignore.case = TRUE)) {
    return("week_month")
  } else if (grepl("year|long_term|2_year|1_year|ssr|a_shadow_rate|government_bond|mibor|shadow_fed_funds", inttype, ignore.case = TRUE)) {
    return("year")
  } else {
    return("unclear")  # Mark cases that do not match any rule
  }
}

### --- Step 2: Extract Unique `inttypes` ----
all_inttypes <- unique(d_no_qc$inttype)  

### --- Step 3: Apply Classification ----
classified_inttypes <- sapply(all_inttypes, classify_inttype)

# Convert the result into a dataframe for reference
classification_lookup <- data.frame(
  inttype = all_inttypes,
  group_inttype = classified_inttypes,
  stringsAsFactors = FALSE
)
# Print
print(classification_lookup, row.names = FALSE)

### --- Step 4: Map Classification Back to Original Dataframe ----
d_no_qc <- merge(
  d_no_qc,
  classification_lookup,
  by.x = "inttype",
  by.y = "inttype",
  all.x = TRUE
)
# Make group_inttype a factor with "overnight" as reference
d_no_qc$group_inttype <- factor(d_no_qc$group_inttype, levels = c("overnight", "week_month", "year", "unclear"))

## Create consolidated `outcome_measure_output_cons` for output ----
## transform into factor and make "gdp" (now including "gnp") the reference (only a very small number of observations uses "gnp")
d_no_qc <- d_no_qc %>%
  mutate(outcome_measure_output_cons = case_when(
    outcome == "output" & outcome_measure == "gnp" ~ "gdp",
    outcome == "output" ~ outcome_measure,
    TRUE ~ NA_character_  # For non-output rows
  )) %>%
  mutate(outcome_measure_output_cons = factor(outcome_measure_output_cons, 
                                              levels = c("gdp", "ip", "gap")))
levels(d_no_qc$outcome_measure_output_cons)

## Create consolidated `outcome_measure_pricelevel` for price level ----
## transform `outcome_measure_pricelevel` into factor and make "cpi" the reference
d_no_qc <- d_no_qc %>%
  mutate(outcome_measure_pricelevel = case_when(
    outcome == "inflation" ~ outcome_measure,
    TRUE ~ NA_character_  # For non-pricelevel rows
  )) %>% 
  mutate(outcome_measure_pricelevel = factor(outcome_measure_pricelevel, 
                                             levels = c("cpi", "deflator", "wpi", "core")))

# Define periods for estimation ----
# chosen_periods <- seq(3, 60, by = 3)
chosen_periods <- c(
  3, 
  6, 
  12, 
  18, 
  24, 
  30,
  36,
  42,
  48,
  54,
  60
  )

# For output ----
out_var <- "output"

## Baseline PEESE ----
# Define baseline moderators 
baseline_mods <- c("group_ident_broad",
  "top_5_or_tier",
  "cbanker")
mmr_output_baseline <- meta_analysis(d_no_qc, 
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = chosen_periods,
                                     wins = wins_para, 
                                     prec_weighted = TRUE, 
                                     estimation = "PEESE", 
                                     cluster_se = TRUE, 
                                     mods = baseline_mods)

coef_names_mmr_output_baseline <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation ',
  'top_5_or_tier' = 'Top tier publication',
  'cbanker' = 'Central bank related')

# Create html output
modelsummary::modelsummary(mmr_output_baseline, 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Baseline", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_baseline)

# Save as png
modelsummary::modelsummary(mmr_output_baseline, 
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_baseline", ".png"),
                           stars = TRUE, 
                           fmt = 2,
                           statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_output_baseline)

# Coefficient plots
y_lims <- c(-2.5, 0.5)
(p1 <- create_mmr_coefficient_plot(mmr_output_baseline, "(Intercept)", 
                              custom_title = "P-bias corrected (PEESE) output response for Cholesky/SVAR, no top journal, no CB affiliation") +
  coord_cartesian(ylim = c(-0.25, 0)))

(p2 <- create_mmr_coefficient_plot(mmr_output_baseline, "variance_winsor", 
                              custom_title = "P-bias coefficient (variance)") +
  coord_cartesian(ylim = c(-1.5, 0)))

p3 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadhf", 
                              custom_title = "HF") +
  coord_cartesian(ylim = y_lims)

p4 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadnr", 
                              custom_title = "NR") +
  coord_cartesian(ylim = y_lims)

p5 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadsignr", 
                              custom_title = "SignR") +
  coord_cartesian(ylim = y_lims)

p6 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadidother", 
                              custom_title = "Other") +
  coord_cartesian(ylim = y_lims)

# Combine plots on identification methods
combined_plot <- (p3 + p4) / (p5 + p6)
combined_plot +
  plot_annotation(
    title = "MMR coefficient estimates for identification methods (differential to MMR intercept)",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

# Plot on top journal 
(p7 <- create_mmr_coefficient_plot(mmr_output_baseline, "top_5_or_tier", 
                              custom_title = "MMR coefficient estimates for top journal article (differential to MMR intercept)") +
  coord_cartesian(ylim = y_lims))

### Corrected effects for top journals for different identification methods ----
# Create prediction data with variance_winsor = 0
pred_data <- data.frame(
  group_ident_broad = factor("hf", 
                             levels = c("chol", "hf", "nr", "signr", "idother")),
  top_5_or_tier = 1,
  cbanker = 0,
  variance_winsor = 0
)
mmr_output_baseline_pred <- meta_analysis(d_no_qc, 
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = chosen_periods,
                                     wins = wins_para, 
                                     prec_weighted = TRUE, 
                                     estimation = "PEESE", 
                                     cluster_se = TRUE, 
                                     mods = baseline_mods,
                                     pred_data = pred_data)
predictions_hf <- mmr_output_baseline_pred$predictions
predictions_hf <- do.call(rbind, predictions_hf)
# Plot the predictions
ggplot(predictions_hf, aes(x = period, ymin = ci_lower, ymax = ci_upper)) +
  geom_errorbar(width = 0.2) +
  labs(title = "Predicted values with CI", x = "Period", y = "Predicted value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Set y axis limits
  coord_cartesian(ylim = c(-1.5, 0.2)) 


ggplot(predictions_hf, aes(x = period, ymin = ci_lower, ymax = ci_upper)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                  fill = factor(conflevel)), alpha = 0.2) +
  # Add the point estimates
  geom_line(aes(y = predicted_value), color = "black", linewidth = 1) +
  geom_point(aes(y = predicted_value), color = "black", size = 2) +
  # Zero line for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Corrected response - High frequency, top journal, no CB", x = "Month", y = "Effect") +
  theme_minimal() +
  # Set y axis limits
  coord_cartesian(ylim = c(-1.5, 0.2)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


## Robustness: + estimation methods, outcome_measure, long run rate ----
mmr_output_robust <- meta_analysis(d_no_qc,
                            outvar = out_var, 
                            se_option = "upper", 
                            periods = chosen_periods,
                            wins = wins_para, 
                            prec_weighted = TRUE, 
                            estimation = "PEESE", 
                            cluster_se = TRUE, 
                            mods = c(
                              # Main interest
                              "group_ident_broad",
                              "top_5_or_tier",
                              "cbanker",
                              # Other controls
                              "group_est_broad",
                              "outcome_measure_output_cons",
                              "group_inttype"
                              ))

coef_names_mmr_output_robust <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation ',
  'top_5_or_tier' = 'Top tier publication',
  'cbanker' = 'Central bank related',
  'group_est_broadlp_ardl' = 'LP and ARDL',
  'group_est_broadfavar' = 'FAVAR', 
  'group_est_broadother_var' = 'Other VAR',
  'group_est_broaddsge' = 'DSGE',
  'outcome_measure_output_consgap' = 'Output gap',
  'outcome_measure_output_consip' = 'IP',
  'group_inttypeweek_month' = 'weekly/monthly rate',
  'group_inttypeyear' = 'yearly rate'
  )

# Create html output
modelsummary::modelsummary(mmr_output_robust, 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Robustness", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_robust)


modelsummary::modelplot(mmr_output_robust, facet = TRUE,
          # coef_omit = omit,
          conf_level = conflevel,
          # title = "Meta-Analysis Plot", 
          # background = b
          ) +
  aes(color = ifelse(p.value < 1-conflevel, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "black"))

# Y lims
y_lims <- c(-2.5, 0.5)
# Create individual plots
p1 <- create_mmr_coefficient_plot(mmr_output_robust, "(Intercept)", 
                              custom_title = "Intercept") +
  coord_cartesian(ylim = y_lims)

p2 <- create_mmr_coefficient_plot(mmr_output_robust, "variance_winsor", 
                              custom_title = "Variance") +
  coord_cartesian(ylim = y_lims)

p3 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadhf", 
                              custom_title = "HF") +
  coord_cartesian(ylim = y_lims)

p4 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadnr", 
                              custom_title = "NR") +
  coord_cartesian(ylim = y_lims)

p5 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadsignr", 
                              custom_title = "SignR") +
  coord_cartesian(ylim = y_lims)

p6 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadidother", 
                              custom_title = "Other") +
  coord_cartesian(ylim = y_lims)

# Combine plots vertically
combined_plot <- (p3 + p4) / (p5 + p6)
combined_plot +
  plot_annotation(
    title = "Coefficient Estimates Across Time",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

  
# Save as png
modelsummary::modelsummary(mmr_output_robust, 
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_robust", ".png"),
                           # output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_robust", ".tex"),
                           stars = TRUE, 
                           fmt = 2,
                           statistic = 'conf.int',
                           gof_map = c("nobs", "r.squared"), 
                           coef_map = coef_names_mmr_output_robust)

# For price level ----
out_var <- "inflation"

d_no_qc_pricelevel <- d_no_qc %>%
  filter(period.month %in% c(0, chosen_periods), outcome == out_var)

## Baseline PEESE ----
mmr_pricelevel_baseline <- meta_analysis(d_no_qc, 
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = chosen_periods,
                                     wins = wins_para, 
                                     prec_weighted = TRUE, 
                                     estimation = "PEESE", 
                                     cluster_se = TRUE, 
                                     mods = c("group_ident_broad",
                                              "top_5_or_tier",
                                              "cbanker")) 

coef_names_mmr_pricelevel_baseline <- c(
  "(Intercept)" = "Intercept",
  "variance_winsor" = "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation ',
  'top_5_or_tier' = 'Top tier publication',
  'cbanker' = 'Central bank related')

# Create html output
modelsummary::modelsummary(mmr_pricelevel_baseline, 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Baseline", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_pricelevel_baseline)

# Save as png
modelsummary::modelsummary(mmr_pricelevel_baseline, 
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_baseline", ".png"),
                           stars = TRUE, 
                           fmt = 2,
                           statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_pricelevel_baseline)

## Robustness: + estimation methods, outcome_measure, long run rate ----
mmr_pricelevel_robust <- meta_analysis(d_no_qc,
                            outvar = out_var, 
                            se_option = "upper", 
                            periods = chosen_periods,
                            wins = wins_para, 
                            prec_weighted = TRUE, 
                            estimation = "PEESE", 
                            cluster_se = TRUE, 
                            mods = c("group_ident_broad",
                                     "group_est_broad",
                                     "outcome_measure_pricelevel",
                                     "interest_rate_long",
                                     "top_5_or_tier",
                                     "cbanker"))

coef_names_mmr_pricelevel_robust <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation ',
  'top_5_or_tier' = 'Top tier publication',
  'cbanker' = 'Central bank related',
  'group_est_broadlp_ardl' = 'LP and ARDL',
  'group_est_broadfavar' = 'FAVAR', 
  'group_est_broadother_var' = 'Other VAR',
  'group_est_broaddsge' = 'DSGE',
  'bayes' = 'Bayesian',
  'outcome_measure_pricelevelcore' = 'Core',
  'outcome_measure_priceleveldeflator' = 'Deflator',
  'outcome_measure_pricelevelwpi' = 'WPI',
  'interest_rate_long' = 'Long-term rate'
  )

# Create html output
modelsummary::modelsummary(mmr_pricelevel_robust, 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Robustness", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_pricelevel_robust)

# Save as png
modelsummary::modelsummary(mmr_pricelevel_robust, 
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_robust", ".png"),
                           stars = TRUE, 
                           fmt = 2,
                           statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_pricelevel_robust)











