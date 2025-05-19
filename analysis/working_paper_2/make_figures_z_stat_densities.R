# Creates the plots with counterfactual densities of z-statistics

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/setup_wp_2.R"))

# Source required functions ----
source(here::here("analysis/R/data_prep_counterfactual.R"))
source(here::here("analysis/R/calibrate_counterfactual.R"))
source(here::here("analysis/R/plot_counterfactual.R"))

# Set significance thresholds ----
sig_thresholds <- c(-1, -1.645, -1.96, -2.576)
calibration_threshold <- -5

# wins_para <- 0 # To check for influence of winsorization

# For employment ----
out_var <- "emp"

## Data prep ----
d_z_stat_emp <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   # only_negative = TRUE,
                                   only_negative = FALSE,
                                   only_positive = FALSE
                                   )

## Add stylized horizons ----
d_z_stat_emp <- d_z_stat_emp %>% 
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

# Define horizon specific ylim to be used in loop
ylim_horizons <- list(
  "impact (0m)" = c(0, 0.8),
  "short run (1m - 12m)" = c(0, 0.4),
  "medium run (13m - 36m)" = c(0, 0.4),
  "long run (> 36m)" = c(0, 0.4)
)
  

for(m in horizons) {
  # Calibrate
  cf <- calibrate_counterfactual(d_z_stat_emp,
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot
  plot_counterfactual(d_z_stat_emp,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      xlims = c(-6, 6),
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      ylim = ylim_horizons[[m]],
                      show_params = FALSE,
                      omit_cf = FALSE,
                      add_legend = ifelse(m == horizons[1], TRUE, FALSE),
                      add_significance_stars = TRUE,
                      star_spacing = ifelse(m == horizons[1], 0.02, 0.01),
                      star_buffer = ifelse(m == horizons[1], -0.02, -0.01),
                      star_cex = 1
                      )
  
  # Add vertical zero line
  abline(v = 0, col = "black", lty = 2)
  
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_emp.pdf", 
         width = 10, 
         height = 10)
dev.off()

par(mfrow = c(1, 1))

# For unemployment ----
out_var <- "unemp"

d_z_stat_neg_unemp <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "lower",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   # only_negative = TRUE,
                                   only_negative = FALSE,
                                   only_positive = FALSE
                                   )

## Add stylized horizons ----
d_z_stat_neg_unemp <- d_z_stat_neg_unemp %>% 
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

# Define horizon specific ylim to be used in loop
ylim_horizons <- list(
  "impact (0m)" = c(0, 0.8),
  "short run (1m - 12m)" = c(0, 0.45),
  "medium run (13m - 36m)" = c(0, 0.45),
  "long run (> 36m)" = c(0, 0.45)
)

for(m in horizons) {
  # Calibrate
  cf <- calibrate_counterfactual(d_z_stat_neg_unemp,
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = -calibration_threshold)
  
  # Plot
  plot_counterfactual(d_z_stat_neg_unemp,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      xlims = c(-6, 6),
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = -sig_thresholds,
                      ylim = ylim_horizons[[m]],
                      show_params = FALSE,
                      omit_cf = FALSE,
                      add_legend = ifelse(m == horizons[1], TRUE, FALSE),
                      add_significance_stars = TRUE,
                      star_spacing = ifelse(m == horizons[1], 0.02, 0.01),
                      star_buffer = ifelse(m == horizons[1], -0.02, -0.01),
                      star_cex = 1
                      )
  
  # Add vertical zero line
  abline(v = 0, col = "black", lty = 2)
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_unemp.pdf", 
         width = 10, 
         height = 10)
dev.off()

par(mfrow = c(1, 1))
