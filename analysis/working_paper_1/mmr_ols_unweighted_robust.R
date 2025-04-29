# Creates figures for comparison between BMA and MMR with unweighted PEESE

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing
library(patchwork) # For combining plots
library(viridis) # For color palette in funnel plots

# Source required functions ----
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/create_mmr_coefficient_plot.R"))
source((here::here("analysis/R/funnel_plot.R")))
source(here::here("analysis/R/plot_average_irfs.R"))

# Define periods for estimation ----
chosen_periods <- seq(0, 60, by = 3)
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

# Define coefficient names and moderator variables ----
## Baseline model ----
### PEESE coefficient names 
peese_coef_name <- c(
  "(Intercept)" = "Intercept",
  "variance_winsor"= "Variance"
)
### Consolidated identification methods
ident_mod <- "group_ident_broad"
ident_coef_name <- c(
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation '
)
### Top journal dummy 
top_journal_mod <- c("top_5_or_tier") 
top_journal_coef_name <- c('top_5_or_tier' = 'Top tier publication')
### CB affiliation dummy
cb_mod <- c("cbanker")
cb_coef_name <- c('cbanker' = 'Central bank related')
### Baseline model combination 
baseline_mods <- c(ident_mod, top_journal_mod, cb_mod)
baseline_coef_names <- c(peese_coef_name, ident_coef_name, top_journal_coef_name, cb_coef_name)
## Robustness models ----
### Consolidated estimation methods
est_mod <- "group_est_broad"
est_coef_name <- c(
  'group_est_broadlp_ardl' = 'LP and ARDL',
  'group_est_broadfavar' = 'FAVAR', 
  'group_est_broadother_var' = 'Other VAR',
  'group_est_broaddsge' = 'DSGE'
)
### Consolidated outcome measures
output_measure <- "outcome_measure_output_cons"
output_measure_coef_name <- c(
  'outcome_measure_output_consip' = 'Ind. prod.',
  'outcome_measure_output_consgap' = 'Output gap')
pricelevel_measure <- "outcome_measure_pricelevel_cons"
pricelevel_measure_coef_name <- c(
  'outcome_measure_pricelevel_consdeflator' = 'Deflator',
  'outcome_measure_pricelevel_conswpi' = 'WPI',
  'outcome_measure_pricelevel_conscore' = 'Core')
### Consolidated interest rate types
int_rate_mod <- "group_inttype"
int_rate_coef_name <- c(
  'group_inttypeweek_month' = 'weekly/monthly rate',
  'group_inttypeyear' = 'yearly rate'
)
### Publication year and number of citations
#### Outcome- and horizon-specific de-meaning of pub_year and num_cit 
d_no_qc <- d_no_qc %>%
  group_by(outcome, period.month) %>%
  mutate(
    pub_year_dm =  pub_year - mean(pub_year, na.rm = TRUE),
    num_cit_dm = num_cit - mean(num_cit, na.rm = TRUE)
  ) %>%
  ungroup()
pub_year_mod <- "pub_year_dm"
pub_year_coef_name <- c("pub_year_dm" = "Publication year")
num_cit_mod <- "num_cit_dm"
num_cit_coef_name <- c("num_cit_dm" = "# citations")
### Preferred estimate
prefer_mod <- "prefer"
prefer_coef_name <- c("prefer" = "Preferred estimate")
### Byproduct 
byproduct_mod <- "byproduct"
byproduct_coef_name <- c("byproduct" = "By-product")
### Data frequency 
freq_mod <- "freq"
freq_coef_name <- c("freqmonth" = "Monthly data",
                    "freqannual" = "Yearly data")
### Panel (vs time series)
panel_mod <- "panel"
panel_coef_name <- c("panel" = "Panel")

### Robustness model combinations
robustness_mods_output <- c(est_mod, 
                            output_measure, 
                            int_rate_mod, 
                            pub_year_mod,
                            num_cit_mod,
                            prefer_mod,
                            byproduct_mod,
                            freq_mod,
                            panel_mod)
robusntess_coef_names_output <- c(est_coef_name, 
                                  output_measure_coef_name, 
                                  int_rate_coef_name, 
                                  pub_year_coef_name,
                                  num_cit_coef_name,
                                  prefer_coef_name,
                                  byproduct_coef_name,
                                  freq_coef_name,
                                  panel_coef_name)
robustness_mods_pricelevel <- c(est_mod, 
                                pricelevel_measure, 
                                int_rate_mod,
                                pub_year_mod,
                                num_cit_mod,
                                prefer_mod,
                                byproduct_mod,
                                freq_mod,
                                panel_mod)
robusntess_coef_names_pricelevel <- c(est_coef_name, 
                                      int_rate_coef_name, 
                                      pricelevel_measure_coef_name,
                                      pub_year_coef_name,
                                      num_cit_coef_name,
                                      prefer_coef_name,
                                      byproduct_coef_name,
                                      freq_coef_name,
                                      panel_coef_name)

# Hardcode the maximum of period 0 precision to allow estimation ---- 
## Create vectors of the columns we want to process ----

se_cols <- c("SE.avg", "SE.upper", "SE.lower")
precision_cols <- c("precision.avg", "precision.upper", "precision.lower")

for (outcome in unique(d_no_qc$outcome)) {
  # Subset data for this outcome where period.month == 1
  subset_data <- d_no_qc[d_no_qc$period.month == 1 & d_no_qc$outcome == outcome, ]
  
  # Process SE columns (check against minimums)
  for (col in se_cols) {
    winsorized_values <- winsorizor(subset_data[[col]], percentile = wins_para)
    min_value <- min(winsorized_values)
    
    # Only overwrite values that are below the minimum
    mask <- d_no_qc$period.month == 0 &
      d_no_qc$outcome == outcome &
      d_no_qc[[col]] < 2*min_value
    
    d_no_qc[mask, col] <- 2*min_value
  }
  
  # Process precision columns (check against maximums)
  for (col in precision_cols) {
    winsorized_values <- winsorizor(subset_data[[col]], percentile = wins_para)
    max_value <- max(winsorized_values)
    
    # Only overwrite values that are above the maximum
    mask <- d_no_qc$period.month == 0 &
      d_no_qc$outcome == outcome &
      d_no_qc[[col]] > 2*max_value
    
    d_no_qc[mask, col] <- 2*max_value
  }
}

# Estimation ----

## For output ----
out_var <- "output"

### Corrected effects for OLS version of PEESE (no robustness mods) ----
prediction_conf_level = 0.67
#### For different identification methods (top_5_or_tier and cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0.14, # See table 1 in working paper
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = FALSE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = prediction_conf_level
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
# avg_irf_output <- plot_average_irfs(
#   d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
#   period_limit = 60,
#   winsor = TRUE,
#   wins_par = wins_para,
#   corrected_irf = NULL,
#   show_legend = TRUE,
#   show_median = FALSE,
#   return_data = TRUE
# )
# uncorrected_irf <- data.frame(
#   method_id = 0,
#   period = avg_irf_output$data$period.month,
#   predicted_value = avg_irf_output$data$avg.effect,
#   std_error = NA,
#   ci_lower = NA,
#   ci_upper = NA,
#   source = "Uncorrected"
# )
# Join uncorrected_irf to prediction df with source = uncorrected
# prediction <- rbind(prediction, uncorrected_irf)
# Get data for uncorrected Chol/SVAR
avg_irf_output_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
chol <- data.frame(
  period = avg_irf_output_chol$data$period.month,
  average = avg_irf_output_chol$data$avg.effect
)
# Get data for uncorrected High Frequency
avg_irf_output_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
hf <- data.frame(
  period = avg_irf_output_hf$data$period.month,
  average = avg_irf_output_hf$data$avg.effect
)
# Get data for uncorrected Narrative
avg_irf_output_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
nr <- data.frame(
  period = avg_irf_output_nr$data$period.month,
  average = avg_irf_output_nr$data$avg.effect
)
# Get data for uncorrected SignR
avg_irf_output_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
SignR <- data.frame(
  period = avg_irf_output_signr$data$period.month,
  average = avg_irf_output_signr$data$avg.effect
)
# Get data for uncorrected Other
avg_irf_output_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
other <- data.frame(
  period = avg_irf_output_idother$data$period.month,
  average = avg_irf_output_idother$data$avg.effect
)

#### Plot corrected effects ----
mmr_output_baseline_corrected_effects_ident_OLS <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # Add confidence intervals without outer lines
    # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
    #             alpha = 0.1,
    #             color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3"#,
      # "Uncorrected" = "#1f77b4"
    )) +
    # scale_fill_manual(values = c(
    #   "Chol/SVAR" = "#112EB8",
    #   "High Frequency" = "#E41A1C",
    #   "Narrative" = "orange",
    #   "SignR" = "#4DAF4A",
    #   "Other" = "#984EA3",
    #   "Uncorrected" = "#1f77b4"
    # )) +
    labs(
      title = "P-bias corrected effects, 14 % top journal, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-3, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) #+
    # Add uncorrected
    # geom_line(data = prediction %>% filter(source == "Uncorrected"), 
    #           aes(y = predicted_value), 
    #           color = "#1f77b4", 
    #           linetype = "dashed") +
    # geom_point(data = prediction %>% filter(source == "Uncorrected"), 
    #            aes(y = predicted_value), 
    #            color = "#1f77b4", 
    #            size = 2)

# Add uncorrected sub-sample lines to plot
mmr_output_baseline_corrected_effects_ident_OLS <- mmr_output_baseline_corrected_effects_ident_OLS +
  geom_line(data = chol, aes(x = period, y = average), color = "#112EB8", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = hf, aes(x = period, y = average), color = "#E41A1C", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = nr, aes(x = period, y = average), color = "orange", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) + 
  geom_line(data = SignR, aes(x = period, y = average), color = "#4DAF4A", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = other, aes(x = period, y = average), color = "#984EA3", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75)

mmr_output_baseline_corrected_effects_ident_OLS

#### Save plot as pdf ----
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_corrected_effects_ident_OLS.pdf"),
       plot = mmr_output_baseline_corrected_effects_ident_OLS,
       device = "pdf",
       width = 7,
       height = 5)

### Robustness model estimation for BMA comparison ----
mmr_output_robust_OLS <- meta_analysis(d_no_qc,
                                   outvar = out_var, 
                                   se_option = "upper", 
                                   periods = chosen_periods,
                                   wins = wins_para,
                                   prec_weighted = FALSE, 
                                   estimation = "PEESE", 
                                   cluster_se = TRUE, 
                                   mods = c(
                                     baseline_mods,
                                     robustness_mods_output)
)

## For price level ----
out_var <- "inflation"

### Corrected effects ----

#### For different identification methods (top_5_or_tier and cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0.14, # See table 1 in working paper
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_pricelevel <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = FALSE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
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

# # Get data for uncorrected sample average 
# avg_irf_pricelevel <- plot_average_irfs(
#   d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
#   period_limit = 60,
#   winsor = TRUE,
#   wins_par = wins_para,
#   corrected_irf = NULL,
#   show_legend = TRUE,
#   show_median = FALSE,
#   return_data = TRUE
# )
# uncorrected_irf <- data.frame(
#   method_id = 0,
#   period = avg_irf_pricelevel$data$period.month,
#   predicted_value = avg_irf_pricelevel$data$avg.effect,
#   std_error = NA,
#   ci_lower = NA,
#   ci_upper = NA,
#   source = "Uncorrected"
# )
# # Join uncorrected_irf to prediction df with source = uncorrected
# prediction <- rbind(prediction, uncorrected_irf)

# Get data for uncorrected Chol/SVAR
avg_irf_pricelevel_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
chol <- data.frame(
  period = avg_irf_pricelevel_chol$data$period.month,
  average = avg_irf_pricelevel_chol$data$avg.effect
)
# Get data for uncorrected High Frequency
avg_irf_pricelevel_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
hf <- data.frame(
  period = avg_irf_pricelevel_hf$data$period.month,
  average = avg_irf_pricelevel_hf$data$avg.effect
)
# Get data for uncorrected Narrative
avg_irf_pricelevel_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
nr <- data.frame(
  period = avg_irf_pricelevel_nr$data$period.month,
  average = avg_irf_pricelevel_nr$data$avg.effect
)
# Get data for uncorrected SignR
avg_irf_pricelevel_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
SignR <- data.frame(
  period = avg_irf_pricelevel_signr$data$period.month,
  average = avg_irf_pricelevel_signr$data$avg.effect
)
# Get data for uncorrected Other
avg_irf_pricelevel_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
other <- data.frame(
  period = avg_irf_pricelevel_idother$data$period.month,
  average = avg_irf_pricelevel_idother$data$avg.effect
)

#### Plot corrected effects ----
mmr_pricelevel_baseline_corrected_effects_ident_OLS <- ggplot(prediction, 
                                                           aes(x = period, 
                                                               color = source, 
                                                               fill = source)) +
    # # Add confidence intervals without outer lines
    # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
    #             alpha = 0.1,
    #             color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3"#,
      # "Uncorrected" = "#1f77b4"
    )) +
    # scale_fill_manual(values = c(
    #   "Chol/SVAR" = "#112EB8",
    #   "High Frequency" = "#E41A1C",
    #   "Narrative" = "orange",
    #   "SignR" = "#4DAF4A",
    #   "Other" = "#984EA3",
    #   "Uncorrected" = "#1f77b4"
    # )) +
    labs(
      title = "P-bias corrected effects, 14 % top journals, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-2, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) #+
    # # Add uncorrected
    # geom_line(data = prediction %>% filter(source == "Uncorrected"), 
    #           aes(y = predicted_value), 
    #           color = "#1f77b4", 
    #           linetype = "dashed") +
    # geom_point(data = prediction %>% filter(source == "Uncorrected"), 
    #            aes(y = predicted_value), 
    #            color = "#1f77b4", 
    #            size = 2)

# Add uncorrected sub-sample lines to plot
mmr_pricelevel_baseline_corrected_effects_ident_OLS <- mmr_pricelevel_baseline_corrected_effects_ident_OLS +
  geom_line(data = chol, aes(x = period, y = average), color = "#112EB8", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = hf, aes(x = period, y = average), color = "#E41A1C", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = nr, aes(x = period, y = average), color = "orange", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) + 
  geom_line(data = SignR, aes(x = period, y = average), color = "#4DAF4A", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = other, aes(x = period, y = average), color = "#984EA3", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75)

mmr_pricelevel_baseline_corrected_effects_ident_OLS

#### Save plot as pdf ----
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_corrected_effects_ident_OLS.pdf"),
       plot = mmr_pricelevel_baseline_corrected_effects_ident_OLS,
       device = "pdf",
       width = 7,
       height = 5)

### Robustness model estimation ----
mmr_pricelevel_robust_OLS <- meta_analysis(d_no_qc,
                                       outvar = out_var, 
                                       se_option = "upper", 
                                       periods = chosen_periods,
                                       wins = wins_para, 
                                       prec_weighted = FALSE, 
                                       estimation = "PEESE", 
                                       cluster_se = TRUE, 
                                       mods = c(
                                         baseline_mods,
                                         robustness_mods_pricelevel)
)


