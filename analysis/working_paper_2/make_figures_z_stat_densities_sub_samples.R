# Counterfactuals for sub-samples: creates the plots with counterfactual 
# densities of z-statistics for sub-samples

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/make_figures_z_stat_densities.R"))

# For employment ----
out_var <- "emp"

## Only for impact: signr vs rest ----
dev.off()
par(mfrow = c(1, 2))
cf_all <- calibrate_counterfactual(d_z_stat_emp %>% filter(horizon == "impact (0m)"),
                                     statistic = "z_stat",
                                     group = "horizon",
                                     group_value = "impact (0m)",
                                     threshold = calibration_threshold)
plot_counterfactual(d_z_stat_emp %>% filter(horizon == "impact (0m)"),
                    statistic = "z_stat", 
                    group = "horizon",
                    group_value = "impact (0m)",
                    calibration = cf_all,
                    show_params = FALSE,
                    omit_cf = TRUE,
                    # breaks = c(seq(0, 10, 0.05), 1000),
                    breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                    significance = sig_thresholds,
                    add_legend = FALSE,
                    xlims = c(-6, 6),
                    ylim = c(0, 0.9))
abline(v = 0, col = "black", lty = 2)
mtext("With sign restrictions", side = 3, line = 3)
cf_other <- calibrate_counterfactual(d_z_stat_emp %>% filter(!(group_ident_broad %in% c("signr")), horizon == "impact (0m)"),
                                     statistic = "z_stat",
                                     group = "horizon",
                                     group_value = "impact (0m)",
                                     threshold = calibration_threshold)
plot_counterfactual(d_z_stat_emp %>% filter(!(group_ident_broad %in% c("signr")), horizon == "impact (0m)"),
                    statistic = "z_stat", 
                    group = "horizon",
                    group_value = "impact (0m)",
                    calibration = cf_other,
                    show_params = FALSE,
                    omit_cf = TRUE,
                    # breaks = c(seq(0, 10, 0.05), 1000),
                    breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                    significance = sig_thresholds,
                    add_legend = FALSE,
                    xlims = c(-6, 6),
                    ylim = c(0, 0.9))
abline(v = 0, col = "black", lty = 2)
mtext("Without sign restrictions", side = 3, line = 3)
### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_emp_impact_signr_vs_rest.pdf",
         width = 10,
         height = 5)
dev.off()

## Top journals vs other publications ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

# Define horizon specific ylim to be used in loop
ylim_horizons <- list(
  "impact (0m)" = c(0, 0.8),
  "short run (1m - 12m)" = c(0, 0.5),
  "medium run (13m - 36m)" = c(0, 0.8),
  "long run (> 36m)" = c(0, 0.8)
)

for(m in horizons) {
  
  # Calibrate for other publications
  cf_other <- calibrate_counterfactual(d_z_stat_emp %>% filter(top_5_or_tier == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_emp %>% filter(top_5_or_tier == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("Other publications", side = 3, line = 3)
  }
  
  # Calibrate for top journals
  cf_top <- calibrate_counterfactual(d_z_stat_emp %>% filter(top_5_or_tier == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot other publications
  plot_counterfactual(d_z_stat_emp %>% filter(top_5_or_tier == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_top,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("Top journals", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_emp_top_journals_vs_other.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))

## For central bank affiliation ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

# Define horizon specific ylim to be used in loop
ylim_horizons <- list(
  "impact (0m)" = c(0, 0.8),
  "short run (1m - 12m)" = c(0, 0.4),
  "medium run (13m - 36m)" = c(0, 0.4),
  "long run (> 36m)" = c(0, 0.4)
)

for(m in horizons) {
  
  # Calibrate for no central bank affiliation
  cf_other <- calibrate_counterfactual(d_z_stat_emp %>% filter(cbanker == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot for no central bank affiliation
  plot_counterfactual(d_z_stat_emp %>% filter(cbanker == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("non-CB", side = 3, line = 3)
  }
  
  # Calibrate for other publications
  cf_cb <- calibrate_counterfactual(d_z_stat_emp %>% filter(cbanker == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_emp %>% filter(cbanker == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_cb,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("CB", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_emp_cbanker_vs_non_cbanker.pdf",
         width = 10,
         height = 10)
dev.off()

# For unemployment rate ----
out_var <- "unemp"

## Only for impact: signr vs rest ----
par(mfrow = c(1, 2))
cf_all <- calibrate_counterfactual(d_z_stat_neg_unemp %>% filter(horizon == "impact (0m)"),
                                     statistic = "z_stat",
                                     group = "horizon",
                                     group_value = "impact (0m)",
                                     threshold = calibration_threshold)
plot_counterfactual(d_z_stat_neg_unemp %>% filter(horizon == "impact (0m)"),
                    statistic = "z_stat", 
                    group = "horizon",
                    group_value = "impact (0m)",
                    calibration = cf_all,
                    show_params = FALSE,
                    omit_cf = TRUE,
                    # breaks = c(seq(0, 10, 0.05), 1000),
                    breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                    significance = sig_thresholds,
                    add_legend = FALSE,
                    xlims = c(-6, 6),
                    ylim = c(0, 0.9))
abline(v = 0, col = "black", lty = 2)
mtext("With sign restrictions", side = 3, line = 3)
cf_other <- calibrate_counterfactual(d_z_stat_neg_unemp %>% filter(!(group_ident_broad %in% c("signr")), horizon == "impact (0m)"),
                                     statistic = "z_stat",
                                     group = "horizon",
                                     group_value = "impact (0m)",
                                     threshold = calibration_threshold)
plot_counterfactual(d_z_stat_neg_unemp %>% filter(!(group_ident_broad %in% c("signr")), horizon == "impact (0m)"),
                    statistic = "z_stat", 
                    group = "horizon",
                    group_value = "impact (0m)",
                    calibration = cf_other,
                    show_params = FALSE,
                    omit_cf = TRUE,
                    # breaks = c(seq(0, 10, 0.05), 1000),
                    breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                    significance = sig_thresholds,
                    add_legend = FALSE,
                    xlims = c(-6, 6),
                    ylim = c(0, 1))
abline(v = 0, col = "black", lty = 2)
mtext("Without sign restrictions", side = 3, line = 3)
### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_unemp_impact_signr_vs_rest.pdf",
         width = 10,
         height = 5)
dev.off()

## Top journals vs other publications ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

# Define horizon specific ylim to be used in loop
ylim_horizons <- list(
  "impact (0m)" = c(0, 1),
  "short run (1m - 12m)" = c(0, 0.5),
  "medium run (13m - 36m)" = c(0, 0.5),
  "long run (> 36m)" = c(0, 0.5)
)

for(m in horizons) {
  
  # Calibrate for other publications
  cf_other <- calibrate_counterfactual(d_z_stat_neg_unemp %>% filter(top_5_or_tier == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_unemp %>% filter(top_5_or_tier == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("Other publications", side = 3, line = 3)
  }
  
  # Calibrate for top journals
  cf_top <- calibrate_counterfactual(d_z_stat_neg_unemp %>% filter(top_5_or_tier == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot other publications
  plot_counterfactual(d_z_stat_neg_unemp %>% filter(top_5_or_tier == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_top,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("Top journals", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_unemp_top_journals_vs_other.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))

## For central bank affiliation ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

# Define horizon specific ylim to be used in loop
ylim_horizons <- list(
  "impact (0m)" = c(0, 1),
  "short run (1m - 12m)" = c(0, 0.4),
  "medium run (13m - 36m)" = c(0, 0.5),
  "long run (> 36m)" = c(0, 0.5)
)

for(m in horizons) {
  
  # Calibrate for no central bank affiliation
  cf_other <- calibrate_counterfactual(d_z_stat_neg_unemp %>% filter(cbanker == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot for no central bank affiliation
  plot_counterfactual(d_z_stat_neg_unemp %>% filter(cbanker == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("non-CB", side = 3, line = 3)
  }
  
  # Calibrate for other publications
  cf_cb <- calibrate_counterfactual(d_z_stat_neg_unemp %>% filter(cbanker == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = calibration_threshold)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_unemp %>% filter(cbanker == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_cb,
                      # breaks = c(seq(0, 10, 0.05), 1000),
                      breaks = c(-1e15, seq(-10, 0.05, 0.05), seq(0, 10, 0.05), 1e15),
                      significance = sig_thresholds,
                      add_legend = FALSE,
                      xlims = c(-6, 6),
                      ylim = ylim_horizons[[m]]
                      )
  if (m == horizons[1]) {
    mtext("CB", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_2/figures/z_stat_densities/figure_z_density_and_counterfactual_unemp_cbanker_vs_non_cbanker.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))

