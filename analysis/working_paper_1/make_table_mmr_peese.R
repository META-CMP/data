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
source(here::here("analysis/R/plot_average_irfs.R"))

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
chosen_periods <- c(1, seq(3, 60, by = 3))
chosen_periods_tables <- c(
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

# Exclude study SBUKV3GN due to extreme effects on results for high frequency identification
d_no_qc_with_SBUKV3GN <- d_no_qc # Storing for comparison
d_no_qc <- d_no_qc %>%
  filter(!(key %in% c("SBUKV3GN")))

## Only identification methods ----

# For output ----
out_var <- "output"

# Define moderators 
ident_mods <- c("group_ident_broad")
mmr_output_ident <- meta_analysis(d_no_qc, 
                                  outvar = out_var, 
                                  se_option = "upper", 
                                  periods = chosen_periods,
                                  wins = wins_para, 
                                  prec_weighted = TRUE, 
                                  estimation = "PEESE", 
                                  cluster_se = TRUE, 
                                  mods = ident_mods)

coef_names_mmr_output_ident <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation '
  )

# Create html output
modelsummary::modelsummary(mmr_output_ident[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Identification", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_ident)

### Coefficient plots ----
y_lims <- c(-0.25, 0)
#### Corrected effect ----
(p1 <- create_mmr_coefficient_plot(mmr_output_ident, "(Intercept)", 
                              custom_title = "P-bias corrected (PEESE) output response for Cholesky/SVAR") +
  coord_cartesian(ylim = y_lims))
#### P-bias coefficient ----
(p2 <- create_mmr_coefficient_plot(mmr_output_ident, "variance_winsor", 
                              custom_title = "P-bias coefficient (variance)") +
  coord_cartesian(ylim = c(-1.5, 0)))
#### Identification methods ----
y_lims <- c(-2.5, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_ident, "group_ident_broadhf", 
                              custom_title = "HF") +
  coord_cartesian(ylim = y_lims)

p4 <- create_mmr_coefficient_plot(mmr_output_ident, "group_ident_broadnr", 
                              custom_title = "NR") +
  coord_cartesian(ylim = y_lims)

p5 <- create_mmr_coefficient_plot(mmr_output_ident, "group_ident_broadsignr", 
                              custom_title = "SignR") +
  coord_cartesian(ylim = y_lims)

p6 <- create_mmr_coefficient_plot(mmr_output_ident, "group_ident_broadidother", 
                              custom_title = "Other") +
  coord_cartesian(ylim = y_lims)

# Combine plots on identification methods
combined_plot <- (p3 + p4) / (p5 + p6)
combined_plot +
  plot_annotation(
    title = "MMR coefficient estimates for identification methods (differential to MMR intercept)",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

### Corrected effects for different identification methods ----
# Function to generate predictions for a single identification method
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_mods,
    pred_data = pred_data
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

predictions_combined <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Plot corrected effects
ggplot(predictions_combined, 
       aes(x = period, 
           color = source, 
           fill = source)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2) +
  geom_line(aes(y = predicted_value), 
            linewidth = 1) +
  geom_point(aes(y = predicted_value), 
             size = 2) +
  scale_color_manual(values = c(
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"
  )) +
  scale_fill_manual(values = c(
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"
  )) +
  labs(
    title = "Corrected effects",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-2.1, 0.2)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

## Only identification methods with se_option = "avg" ----
# Define moderators 
ident_mods_avg_se <- c("group_ident_broad")
mmr_output_ident_avg_se <- meta_analysis(d_no_qc, 
                                  outvar = out_var, 
                                  se_option = "avg", 
                                  periods = chosen_periods,
                                  wins = wins_para, 
                                  prec_weighted = TRUE, 
                                  estimation = "PEESE", 
                                  cluster_se = TRUE, 
                                  mods = ident_mods_avg_se)

coef_names_mmr_output_ident_avg_se <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation '
)

# Create html output
modelsummary::modelsummary(mmr_output_ident_avg_se[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Identification", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_ident_avg_se)

### Coefficient plots ----
y_lims <- c(-0.25, 0)
#### Corrected effect ----
(p1 <- create_mmr_coefficient_plot(mmr_output_ident_avg_se, "(Intercept)", 
                                   custom_title = "P-bias corrected (PEESE) output response for Cholesky/SVAR") +
   coord_cartesian(ylim = y_lims))
#### P-bias coefficient ----
(p2 <- create_mmr_coefficient_plot(mmr_output_ident_avg_se, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
   coord_cartesian(ylim = c(-1.5, 0)))
#### Identification methods ----
y_lims <- c(-2.5, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_ident_avg_se, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims)

p4 <- create_mmr_coefficient_plot(mmr_output_ident_avg_se, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims)

p5 <- create_mmr_coefficient_plot(mmr_output_ident_avg_se, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims)

p6 <- create_mmr_coefficient_plot(mmr_output_ident_avg_se, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims)

# Combine plots on identification methods
combined_plot_avg_se <- (p3 + p4) / (p5 + p6)
combined_plot_avg_se +
  plot_annotation(
    title = "MMR coefficient estimates for identification methods (differential to MMR intercept)",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

### Corrected effects for different identification methods ----
# Function to generate predictions for a single identification method
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "avg",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_mods,
    pred_data = pred_data
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

predictions_combined <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Plot corrected effects
ggplot(predictions_combined, 
       aes(x = period, 
           color = source, 
           fill = source)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2) +
  geom_line(aes(y = predicted_value), 
            linewidth = 1) +
  geom_point(aes(y = predicted_value), 
             size = 2) +
  scale_color_manual(values = c(
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"
  )) +
  scale_fill_manual(values = c(
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"
  )) +
  labs(
    title = "Corrected effects",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-2.1, 0.2)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

## Only top journals with se_option = "upper" ----
# Define moderators 
top_journals_mods <- c("top_5_or_tier")
mmr_output_top_journals <- meta_analysis(d_no_qc, 
                                         outvar = out_var, 
                                         se_option = "upper", 
                                         periods = chosen_periods,
                                         wins = wins_para, 
                                         prec_weighted = TRUE, 
                                         estimation = "PEESE", 
                                         cluster_se = TRUE, 
                                         mods = top_journals_mods)

coef_names_mmr_output_top_journals <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'top_5_or_tier' = 'Top tier publication'
  )

# Create html output
modelsummary::modelsummary(mmr_output_top_journals[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Top journals", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_top_journals)

### Coefficient plots ----
y_lims <- c(-0.25, 0)
#### Corrected effect ----
(p1 <- create_mmr_coefficient_plot(mmr_output_top_journals, "(Intercept)", 
                                   custom_title = "P-bias corrected (PEESE) output response for other publication") +
   coord_cartesian(ylim = y_lims))
#### P-bias coefficient ----
(p2 <- create_mmr_coefficient_plot(mmr_output_top_journals, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
   coord_cartesian(ylim = c(-1.5, 0)))
#### Top journals ----
y_lims <- c(-2.5, 0.5)
(p3 <- create_mmr_coefficient_plot(mmr_output_top_journals, "top_5_or_tier", 
                                  custom_title = "Top tier publication") +
  coord_cartesian(ylim = y_lims))

### Corrected effects for top tier ----
pred_data <- data.frame(
  top_5_or_tier = 1,
  variance_winsor = 0
)
mmr_output_top_journals_pred <- meta_analysis(d_no_qc, 
                                              outvar = out_var, 
                                              se_option = "upper", 
                                              periods = chosen_periods,
                                              wins = wins_para, 
                                              prec_weighted = TRUE, 
                                              estimation = "PEESE", 
                                              cluster_se = TRUE, 
                                              mods = top_journals_mods,
                                              pred_data = pred_data)

predictions_top_journals <- mmr_output_top_journals_pred$predictions
predictions_top_journals <- do.call(rbind, predictions_top_journals)
# Plot the predictions
ggplot(predictions_top_journals, aes(x = period, ymin = ci_lower, ymax = ci_upper)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                  fill = factor(conflevel)), alpha = 0.2) +
  # Add the point estimates
  geom_line(aes(y = predicted_value), color = "black", linewidth = 1) +
  geom_point(aes(y = predicted_value), color = "black", size = 2) +
  # Zero line for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Corrected response - Top tier publication", x = "Month", y = "Effect") +
  theme_minimal() +
  # Set y axis limits
  coord_cartesian(ylim = c(-1.5, 0.2)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## Only central bank related with se_option = "upper" ----
# Define moderators
cb_mods <- c("cbanker")
mmr_output_cb <- meta_analysis(d_no_qc, 
                               outvar = out_var, 
                               se_option = "avg", 
                               periods = chosen_periods,
                               wins = wins_para, 
                               prec_weighted = TRUE, 
                               estimation = "PEESE", 
                               cluster_se = TRUE, 
                               mods = cb_mods)

coef_names_mmr_output_cb <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'cbanker' = 'Central bank related'
  )

# Create html output
modelsummary::modelsummary(mmr_output_cb[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Central bank related", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_cb)

### Coefficient plots ----
y_lims <- c(-0.25, 0)
#### Corrected effect ----
(p1 <- create_mmr_coefficient_plot(mmr_output_cb, "(Intercept)", 
                                   custom_title = "P-bias corrected (PEESE) output response for non-CB") +
   coord_cartesian(ylim = y_lims))
#### P-bias coefficient ----
(p2 <- create_mmr_coefficient_plot(mmr_output_cb, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
   coord_cartesian(ylim = c(-1.5, 0)))
#### Central bank related ----
y_lims <- c(-2.5, 0.5)
(p3 <- create_mmr_coefficient_plot(mmr_output_cb, "cbanker", 
                                  custom_title = "Central bank related") +
  coord_cartesian(ylim = y_lims))

### Corrected effects for central bank related ----
pred_data <- data.frame(
  cbanker = 1,
  variance_winsor = 0
)
mmr_output_cb_pred <- meta_analysis(d_no_qc, 
                                    outvar = out_var, 
                                    se_option = "avg", 
                                    periods = chosen_periods,
                                    wins = wins_para, 
                                    prec_weighted = TRUE, 
                                    estimation = "PEESE", 
                                    cluster_se = TRUE, 
                                    mods = cb_mods,
                                    pred_data = pred_data)

predictions_cb <- mmr_output_cb_pred$predictions
predictions_cb <- do.call(rbind, predictions_cb)
# Plot the predictions
ggplot(predictions_cb, aes(x = period, ymin = ci_lower, ymax = ci_upper)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                  fill = factor(conflevel)), alpha = 0.2) +
  # Add the point estimates
  geom_line(aes(y = predicted_value), color = "black", linewidth = 1) +
  geom_point(aes(y = predicted_value), color = "black", size = 2) +
  # Zero line for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Corrected response - Central bank related", x = "Month", y = "Effect") +
  theme_minimal() +
  # Set y axis limits
  coord_cartesian(ylim = c(-1.5, 0.2)) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

## Identification, top journals, CB ----
# Higher winsorization
wins_para <- 0.05
# Define moderators 
ident_top_cb_mods <- c("group_ident_broad",
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
                                     mods = ident_top_cb_mods)

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
modelsummary::modelsummary(mmr_output_baseline[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Baseline", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_baseline)

# Save as png
modelsummary::modelsummary(mmr_output_baseline[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_baseline", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_output_baseline)

#### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_output_baseline, "(Intercept)",
                                   custom_title = "Corrected reference response") +
  coord_cartesian(ylim = c(-0.4, 0)) +
  # No subtitle
  # theme(plot.subtitle = element_blank()) +
  # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
  labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_output_baseline, "variance_winsor", 
                              custom_title = "P-bias coefficient (variance)") +
  coord_cartesian(ylim = c(-1.3, 0)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_output_baseline_intercept_p_bias <- (p1 + p2) +
  plot_annotation(
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_intercept_p_bias.pdf"),
       plot = figure_mmr_output_baseline_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadhf", 
                              custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadnr", 
                              custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadsignr", 
                              custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_output_baseline, "group_ident_broadidother", 
                              custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_output_baseline, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_output_baseline, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5)),
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

#### Corrected effects for top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.5,
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_top_cb_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_output_baseline_corrected_effects_ident <- ggplot(prediction, 
       aes(x = period, 
           color = source, 
           fill = source)) +
  # Add confidence intervals without outer lines
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.1,
              color = NA) +
  geom_line(aes(y = predicted_value), 
            linewidth = 1) +
  geom_point(aes(y = predicted_value), 
             size = 2) +
  scale_color_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3",
    "Uncorrected" = "black"
  )) +
  scale_fill_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3",
    "Uncorrected" = "black"
  )) +
  labs(
    title = "P-bias corrected effects, top journal, 50 % CB affiliation",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-1, 0.25)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  # Add uncorrected
  geom_line(data = prediction %>% filter(source == "Uncorrected"), 
            aes(y = predicted_value), 
            color = "black", 
            linetype = "dashed") +
  geom_point(data = prediction %>% filter(source == "Uncorrected"), 
             aes(y = predicted_value), 
             color = "black", 
             size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_corrected_effects_ident.pdf"),
       plot = mmr_output_baseline_corrected_effects_ident,
       device = "pdf",
       width = 7,
       height = 5)

#### Results with outlier SBUKV3GN ----
mmr_output_baseline_with_SBUKV3GN <- meta_analysis(d_no_qc_with_SBUKV3GN, 
                                                   outvar = out_var, 
                                                   se_option = "upper", 
                                                   periods = chosen_periods,
                                                   wins = wins_para, 
                                                   prec_weighted = TRUE, 
                                                   estimation = "PEESE", 
                                                   cluster_se = TRUE, 
                                                   mods = ident_top_cb_mods)

# Save table as png
modelsummary::modelsummary(mmr_output_baseline_with_SBUKV3GN[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_baseline_with_SBUKV3GN", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_output_baseline)

##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "(Intercept)",
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.4, 0)) +
   # No subtitle
   # theme(plot.subtitle = element_blank()) +
   # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-1.3, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_output_baseline_intercept_p_bias_with_SBUKV3GN <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_intercept_p_bias_with_SBUKV3GN.pdf"),
       plot = figure_mmr_output_baseline_intercept_p_bias_with_SBUKV3GN,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_output_baseline_with_SBUKV3GN, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_moderators_with_SBUKV3GN.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

##### Corrected effects for top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.5,
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc_with_SBUKV3GN,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_top_cb_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc_with_SBUKV3GN %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_output_baseline_corrected_effects_ident_with_SBUKV3GN <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "#377EB8",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "black"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "#377EB8",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "black"
    )) +
    labs(
      title = "P-bias corrected effects, top journal, 50 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-1, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "black", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "black", 
               size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_corrected_effects_ident_with_SBUKV3GN.pdf"),
       plot = mmr_output_baseline_corrected_effects_ident_with_SBUKV3GN,
       device = "pdf",
       width = 7,
       height = 5)

#### Results with lower winsorization ----
# Lower winsorization
wins_para <- 0.02
mmr_output_baseline_lower_wins <- meta_analysis(d_no_qc, 
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = chosen_periods,
                                     wins = wins_para, 
                                     prec_weighted = TRUE, 
                                     estimation = "PEESE", 
                                     cluster_se = TRUE, 
                                     mods = ident_top_cb_mods)

# Save as png
modelsummary::modelsummary(mmr_output_baseline_lower_wins[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_baseline_lower_wins", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_output_baseline)

#### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "(Intercept)",
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.4, 0)) +
   # No subtitle
   # theme(plot.subtitle = element_blank()) +
   # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-1.3, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_output_baseline_intercept_p_bias_lower_wins <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_intercept_p_bias_lower_wins.pdf"),
       plot = figure_mmr_output_baseline_intercept_p_bias_lower_wins,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_output_baseline_lower_wins, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_moderators_lower_wins.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

#### Corrected effects for top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.5,
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_top_cb_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_output_baseline_corrected_effects_ident_lower_wins <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "#377EB8",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "black"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "#377EB8",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "black"
    )) +
    labs(
      title = "P-bias corrected effects, top journal, 50 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-1, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "black", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "black", 
               size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_corrected_effects_ident_lower_wins.pdf"),
       plot = mmr_output_baseline_corrected_effects_ident_lower_wins,
       device = "pdf",
       width = 7,
       height = 5)



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
modelsummary::modelsummary(mmr_output_robust[as.character(chosen_periods_tables)],
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Robustness", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_output_robust)
# Save as png
modelsummary::modelsummary(mmr_output_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_robust", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_output_robust)

# Coefficient plots
(p1 <- create_mmr_coefficient_plot(mmr_output_robust, "(Intercept)",
                                   custom_title = "Corrected reference response") +
  coord_cartesian(ylim = c(-0.4, 0)) +
  # No subtitle
  # theme(plot.subtitle = element_blank()) +
  # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
  labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_output_robust, "variance_winsor", 
                              custom_title = "P-bias coefficient (variance)") +
  coord_cartesian(ylim = c(-1.3, 0)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_output_robust_intercept_p_bias <- (p1 + p2) +
  plot_annotation(
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_robust_intercept_p_bias.pdf"),
       plot = figure_mmr_output_robust_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadhf", 
                              custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadnr", 
                              custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadsignr", 
                              custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_output_robust, "group_ident_broadidother", 
                              custom_title = "Other") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p7 <- create_mmr_coefficient_plot(mmr_output_robust, "top_5_or_tier", 
                              custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p8 <- create_mmr_coefficient_plot(mmr_output_robust, "cbanker", 
                              custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p9 <- create_mmr_coefficient_plot(mmr_output_robust, "group_est_broadlp_ardl", 
                              custom_title = "LP and ARDL") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p10 <- create_mmr_coefficient_plot(mmr_output_robust, "group_est_broadfavar", 
                              custom_title = "FAVAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p11 <- create_mmr_coefficient_plot(mmr_output_robust, "group_est_broadother_var", 
                              custom_title = "Other VAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p12 <- create_mmr_coefficient_plot(mmr_output_robust, "group_est_broaddsge", 
                              custom_title = "DSGE") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p13 <- create_mmr_coefficient_plot(mmr_output_robust, "outcome_measure_output_consgap", 
                              custom_title = "Output gap") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p14 <- create_mmr_coefficient_plot(mmr_output_robust, "outcome_measure_output_consip", 
                              custom_title = "IP") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p15 <- create_mmr_coefficient_plot(mmr_output_robust, "group_inttypeweek_month", 
                              custom_title = "weekly/monthly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p16 <- create_mmr_coefficient_plot(mmr_output_robust, "group_inttypeyear", 
                              custom_title = "yearly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots as before 
combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) 
(combined_plot +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5)),
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_robust_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

# Combine other moderators
(combined_plot_est <- (p9 + p10) / (p11 + p12) / (p13 + p14) / (p15 + p16) +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5)),
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_robust_moderators_est.pdf"),
       plot = combined_plot_est,
       device = "pdf",
       width = 7,
       height = 12)

# For price level ----
out_var <- "inflation"

## Identification, top journals, CB ----
# Define moderators 
ident_top_cb_mods <- c("group_ident_broad",
                       "top_5_or_tier",
                       "cbanker")
mmr_pricelevel_baseline <- meta_analysis(d_no_qc, 
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = chosen_periods,
                                     wins = wins_para, 
                                     prec_weighted = TRUE, 
                                     estimation = "PEESE", 
                                     cluster_se = TRUE, 
                                     mods = ident_top_cb_mods)

coef_names_mmr_pricelevel_baseline <- c(
  "(Intercept)"="Intercept",
  "variance_winsor"= "Variance",
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation ',
  'top_5_or_tier' = 'Top tier publication',
  'cbanker' = 'Central bank related')

# Create html pricelevel
modelsummary::modelsummary(mmr_pricelevel_baseline[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Baseline", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_pricelevel_baseline)

# Save as png
modelsummary::modelsummary(mmr_pricelevel_baseline[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_baseline", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_pricelevel_baseline)

# Coefficient plots
(p1 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "(Intercept)", 
                                   custom_title = "Corrected reference response") +
    coord_cartesian(ylim = c(-0.2, 0.1)) +
    # No subtitle
    # theme(plot.subtitle = element_blank()) +
    # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
    labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-2.5, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_pricelevel_baseline_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_intercept_p_bias.pdf"),
       plot = figure_mmr_pricelevel_baseline_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

### Corrected effects for top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.5,
    variance_winsor = 0
  )
  
  mmr_pricelevel <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_top_cb_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_pricelevel$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_pricelevel <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_pricelevel$data$period.month,
  predicted_value = avg_irf_pricelevel$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_pricelevel_baseline_corrected_effects_ident <- ggplot(prediction, 
       aes(x = period, 
           color = source, 
           fill = source)) +
  # Add confidence intervals without outer lines
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.1,
              color = NA) +
  geom_line(aes(y = predicted_value), 
            linewidth = 1) +
  geom_point(aes(y = predicted_value), 
             size = 2) +
  scale_color_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"
  )) +
  scale_fill_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "#377EB8",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"
  )) +
  labs(
    title = "P-bias corrected effects, top journals, 50 % CB affiliation",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-0.75, 0.25)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  # Add uncorrected
  geom_line(data = prediction %>% filter(source == "Uncorrected"), 
            aes(y = predicted_value), 
            color = "black", 
            linetype = "dashed") +
  geom_point(data = prediction %>% filter(source == "Uncorrected"), 
             aes(y = predicted_value), 
             color = "black", 
             size = 2))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_corrected_effects_ident.pdf"),
       plot = mmr_pricelevel_baseline_corrected_effects_ident,
       device = "pdf",
       width = 7,
       height = 5)

#### Results with outlier SBUKV3GN ----
mmr_pricelevel_baseline_with_SBUKV3GN <- meta_analysis(d_no_qc_with_SBUKV3GN, 
                                                   outvar = out_var, 
                                                   se_option = "upper", 
                                                   periods = chosen_periods,
                                                   wins = wins_para, 
                                                   prec_weighted = TRUE, 
                                                   estimation = "PEESE", 
                                                   cluster_se = TRUE, 
                                                   mods = ident_top_cb_mods)

# Save table as png
modelsummary::modelsummary(mmr_pricelevel_baseline_with_SBUKV3GN[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_baseline_with_SBUKV3GN", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_pricelevel_baseline)

##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "(Intercept)",
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.4, 0)) +
   # No subtitle
   # theme(plot.subtitle = element_blank()) +
   # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-1.3, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_pricelevel_baseline_intercept_p_bias_with_SBUKV3GN <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_intercept_p_bias_with_SBUKV3GN.pdf"),
       plot = figure_mmr_pricelevel_baseline_intercept_p_bias_with_SBUKV3GN,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_pricelevel_baseline_with_SBUKV3GN, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_moderators_with_SBUKV3GN.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

##### Corrected effects for top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.5,
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc_with_SBUKV3GN,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = ident_top_cb_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc_with_SBUKV3GN %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_pricelevel_baseline_corrected_effects_ident_with_SBUKV3GN <- ggplot(prediction, 
                                                                     aes(x = period, 
                                                                         color = source, 
                                                                         fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "#377EB8",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "black"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "#377EB8",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "black"
    )) +
    labs(
      title = "P-bias corrected effects, top journal, 50 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-0.75, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "black", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "black", 
               size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_corrected_effects_ident_with_SBUKV3GN.pdf"),
       plot = mmr_pricelevel_baseline_corrected_effects_ident_with_SBUKV3GN,
       device = "pdf",
       width = 7,
       height = 5)

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
                                                "group_inttype",
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
modelsummary::modelsummary(mmr_pricelevel_robust[as.character(chosen_periods_tables)],
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Robustness", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = coef_names_mmr_pricelevel_robust)

# Save as png
modelsummary::modelsummary(mmr_pricelevel_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_robust", ".png"),
                           stars = FALSE, 
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel, 
                           gof_map = "nobs", 
                           coef_map = coef_names_mmr_pricelevel_robust)

# Coefficient plots
(p1 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "(Intercept)",
                                  custom_title = "Corrected reference response") +
  coord_cartesian(ylim = c(-0.2, 0.1)) +
  # No subtitle
  # theme(plot.subtitle = element_blank()) +
  # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
  labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "variance_winsor", 
                                  custom_title = "P-bias coefficient (variance)") +
  coord_cartesian(ylim = c(-2.5, 0)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_pricelevel_robust_intercept_p_bias <- (p1 + p2) +
  plot_annotation(
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_robust_intercept_p_bias.pdf"),
       plot = figure_mmr_pricelevel_robust_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p7 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p8 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p9 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_est_broadlp_ardl", 
                                  custom_title = "LP and ARDL") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p10 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_est_broadfavar", 
                                   custom_title = "FAVAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p11 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_est_broadother_var", 
                                   custom_title = "Other VAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p12 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_est_broaddsge", 
                                   custom_title = "DSGE") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p13 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "outcome_measure_pricelevelcore", 
                                   custom_title = "Core") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p14 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "outcome_measure_priceleveldeflator", 
                                   custom_title = "Deflator") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p15 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "outcome_measure_pricelevelwpi", 
                                   custom_title = "WPI") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p16 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_inttypeweek_month", 
                                   custom_title = "weekly/monthly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p17 <- create_mmr_coefficient_plot(mmr_pricelevel_robust, "group_inttypeyear", 
                                   custom_title = "yearly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots as before
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5)),
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_robust_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

# Create one empty plot
empty_plot <- ggplot() +
  theme_minimal()

# Combine other moderators
(combined_plot_est <- (p9 + p10) / (p11 + p12) / (p17 + p16) / (p13 + p14) / (p15 + empty_plot) +
  plot_annotation(
    theme = theme(plot.title = element_text(hjust = 0.5)),
    caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
  ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_robust_moderators_est.pdf"),
       plot = combined_plot_est,
       device = "pdf",
       width = 7,
       height = 12)
