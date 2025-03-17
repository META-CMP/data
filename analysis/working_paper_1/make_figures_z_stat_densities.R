# Creates the plots with counterfactual densities of z-statistics

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Source required functions ----
source(here::here("analysis/R/data_prep_counterfactual.R"))
source(here::here("analysis/R/calibrate_counterfactual.R"))
source(here::here("analysis/R/plot_counterfactual.R"))

# Set significance thresholds ----
sig_thresholds <- c(-1, -1.645, -1.96, -2.576)
calibration_threshold <- -5

# For output ----
out_var <- "output"

## Data prep ----
d_z_stat_output <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   # only_negative = TRUE,
                                   only_negative = FALSE,
                                   only_positive = FALSE
                                   )

## Add stylized horizons ----
d_z_stat_output <- d_z_stat_output %>% 
  mutate(horizon = case_when(
    period.month < vsr ~ horizons[1],
    (period.month >= vsr & period.month <= smr) ~ horizons[2],
    (period.month > smr & period.month <= mlr) ~ horizons[3],
    period.month > mlr ~ horizons[4]
  ),
  horizon = factor(horizon, levels = horizons)
  )

## Create 2x2 plot for all horizons ----
par(mfrow = c(2, 2))

for(m in horizons) {
  # Calibrate
  cf <- calibrate_counterfactual(d_z_stat_output,
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot
  plot_counterfactual(d_z_stat_output,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      xlims = c(-6, 6),
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1000, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1000),
                      significance = sig_thresholds,
                      ylim = c(0, 0.7),
                      show_params = FALSE,
                      omit_cf = FALSE,
                      add_legend = ifelse(m == horizons[1], TRUE, FALSE),
                      add_significance_stars = TRUE,
                      star_spacing = 0.02,
                      star_buffer = -0.02,
                      star_cex = 1
                      )
  
  # Add vertical zero line
  abline(v = 0, col = "black", lty = 2)
  
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_output.pdf", 
         width = 10, 
         height = 10)
dev.off()

par(mfrow = c(1, 1))

# For price level ----
out_var <- "inflation"

d_z_stat_neg_pricelevel <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   # only_negative = TRUE,
                                   only_negative = FALSE,
                                   only_positive = FALSE
                                   )

## Add stylized horizons ----
d_z_stat_neg_pricelevel <- d_z_stat_neg_pricelevel %>% 
  mutate(horizon = case_when(
    period.month < vsr ~ horizons[1],
    (period.month >= vsr & period.month <= smr) ~ horizons[2],
    (period.month > smr & period.month <= mlr) ~ horizons[3],
    period.month > mlr ~ horizons[4]
  ),
  horizon = factor(horizon, levels = horizons)
)

## Create 2x2 plot for all horizons ----
par(mfrow = c(2, 2))

for(m in horizons) {
  # Calibrate
  cf <- calibrate_counterfactual(d_z_stat_neg_pricelevel,
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot
  plot_counterfactual(d_z_stat_neg_pricelevel,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      xlims = c(-6, 6),
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1000, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1000),
                      significance = sig_thresholds,
                      ylim = c(0, 1),
                      show_params = FALSE,
                      omit_cf = FALSE,
                      add_legend = ifelse(m == horizons[1], TRUE, FALSE),
                      add_significance_stars = TRUE,
                      star_spacing = 0.02,
                      star_buffer = -0.02,
                      star_cex = 1
                      )
  
  # Add vertical zero line
  abline(v = 0, col = "black", lty = 2)
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_pricelevel.pdf", 
         width = 10, 
         height = 10)
dev.off()

par(mfrow = c(1, 1))

# For interest rate ----
out_var <- "rate"

d_z_stat_neg_rate <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   # only_negative = TRUE,
                                   only_negative = FALSE,
                                   only_positive = FALSE)

## Add stylized horizons ----
d_z_stat_neg_rate <- d_z_stat_neg_rate %>% 
  mutate(horizon = case_when(
    period.month < vsr ~ horizons[1],
    (period.month >= vsr & period.month <= smr) ~ horizons[2],
    (period.month > smr & period.month <= mlr) ~ horizons[3],
    period.month > mlr ~ horizons[4]
  ),
  horizon = factor(horizon, levels = horizons)
)

## Create 2x2 plot for all horizons ----
par(mfrow = c(2, 2))

for(m in horizons) {
  # Calibrate
  cf <- calibrate_counterfactual(d_z_stat_neg_rate,
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = -calibration_threshold)
  
  # Plot
  plot_counterfactual(d_z_stat_neg_rate,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      xlims = c(-6, 6),
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1000, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.6),
                      show_params = FALSE,
                      omit_cf = FALSE,
                      add_legend = ifelse(m == horizons[1], TRUE, FALSE),
                      add_significance_stars = TRUE,
                      star_spacing = 0.02,
                      star_buffer = -0.02,
                      star_cex = 1
                      )
  
  # Add vertical zero line
  abline(v = 0, col = "black", lty = 2)
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_rate.pdf", 
         width = 10, 
         height = 10)
dev.off()

par(mfrow = c(1, 1))
