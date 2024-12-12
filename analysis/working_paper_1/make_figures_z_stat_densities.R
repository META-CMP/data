# Creates the plots with counterfactual densities of z-statistics

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Source required functions ----
source(here::here("analysis/R/data_prep_counterfactual.R"))
source(here::here("analysis/R/calibrate_counterfactual.R"))
source(here::here("analysis/R/plot_counterfactual.R"))

# For output ----
out_var <- "output"

## Data prep ----
d_z_stat_neg_output <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   only_negative = TRUE,
                                   only_positive = FALSE)

## Add stylized horizons ----
d_z_stat_neg_output <- d_z_stat_neg_output %>% 
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
  cf <- calibrate_counterfactual(d_z_stat_neg_output,
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot
  plot_counterfactual(d_z_stat_neg_output,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
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
                                   only_negative = TRUE,
                                   only_positive = FALSE)

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
                                 threshold = 5)
  
  # Plot
  plot_counterfactual(d_z_stat_neg_pricelevel,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 1))
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_pricelevel.pdf", 
         width = 10, 
         height = 10)
dev.off()

# For interest rate ----
out_var <- "rate"

d_z_stat_neg_rate <- data_prep_counterfactual(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper",
                                   periods = seq(0, max(d_no_qc$period.month), 1),
                                   wins = wins_para,
                                   only_negative = FALSE,
                                   only_positive = TRUE)

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
                                 threshold = 5)
  
  # Plot
  plot_counterfactual(d_z_stat_neg_rate,
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 1.8),
                      kernel_type = "gaussian")
}

## Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_rate.pdf", 
         width = 10, 
         height = 10)
dev.off()

