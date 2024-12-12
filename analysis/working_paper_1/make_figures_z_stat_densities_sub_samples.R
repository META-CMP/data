# Counterfactuals for sub-samples: creates the plots with counterfactual 
# densities of z-statistics for sub-samples

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/make_figures_z_stat_densities.R"))

# For output ----
out_var <- "output"

## Top journals vs other publications ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

for(m in horizons) {
  
  # Calibrate for other publications
  cf_other <- calibrate_counterfactual(d_z_stat_neg_output %>% filter(top_5_or_tier == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_output %>% filter(top_5_or_tier == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.9))
  if (m == horizons[1]) {
    mtext("Other publications", side = 3, line = 3)
  }
  
  # Calibrate for top journals
  cf_top <- calibrate_counterfactual(d_z_stat_neg_output %>% filter(top_5_or_tier == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot other publications
  plot_counterfactual(d_z_stat_neg_output %>% filter(top_5_or_tier == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_top,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.9))
  if (m == horizons[1]) {
    mtext("Top journals", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_output_top_journals_vs_other.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))

## For central bank affiliation ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

for(m in horizons) {
  
  # Calibrate for no central bank affiliation
  cf_other <- calibrate_counterfactual(d_z_stat_neg_output %>% filter(cbanker == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for no central bank affiliation
  plot_counterfactual(d_z_stat_neg_output %>% filter(cbanker == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("non-CB", side = 3, line = 3)
  }
  
  # Calibrate for other publications
  cf_cb <- calibrate_counterfactual(d_z_stat_neg_output %>% filter(cbanker == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_output %>% filter(cbanker == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_cb,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("CB", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_output_cbanker_vs_non_cbanker.pdf",
         width = 10,
         height = 10)
dev.off()

# For price level ----
out_var <- "inflation"

## Top journals vs other publications ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

for(m in horizons) {
  
  # Calibrate for other publications
  cf_other <- calibrate_counterfactual(d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("Other publications", side = 3, line = 3)
  }
  
  # Calibrate for top journals
  cf_top <- calibrate_counterfactual(d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot other publications
  plot_counterfactual(d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_top,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("Top journals", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_pricelevel_top_journals_vs_other.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))

## For central bank affiliation ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

for(m in horizons) {
  
  # Calibrate for no central bank affiliation
  cf_other <- calibrate_counterfactual(d_z_stat_neg_pricelevel %>% filter(cbanker == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for no central bank affiliation
  plot_counterfactual(d_z_stat_neg_pricelevel %>% filter(cbanker == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("non-CB", side = 3, line = 3)
  }
  
  # Calibrate for other publications
  cf_cb <- calibrate_counterfactual(d_z_stat_neg_pricelevel %>% filter(cbanker == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_pricelevel %>% filter(cbanker == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_cb,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("CB", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_pricelevel_cbanker_vs_non_cbanker.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))

# For interest rate ----
out_var <- "rate"

## Top journals vs other publications ----

### Create 4x2 plot for all horizons ----
par(mfrow = c(4, 2))

for(m in horizons) {
  
  # Calibrate for other publications
  cf_other <- calibrate_counterfactual(d_z_stat_neg_rate %>% filter(top_5_or_tier == 0),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot for other publications
  plot_counterfactual(d_z_stat_neg_rate %>% filter(top_5_or_tier == 0),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_other,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("Other publications", side = 3, line = 3)
  }
  
  # Calibrate for top journals
  cf_top <- calibrate_counterfactual(d_z_stat_neg_rate %>% filter(top_5_or_tier == 1),
                                 statistic = "z_stat",
                                 group = "horizon",
                                 group_value = m,
                                 threshold = 5)
  
  # Plot other publications
  plot_counterfactual(d_z_stat_neg_rate %>% filter(top_5_or_tier == 1),
                      statistic = "z_stat", 
                      group = "horizon",
                      group_value = m,
                      calibration = cf_top,
                      breaks = c(seq(0, 10, 0.05), 1000),
                      significance = c(1, 1.645, 1.96, 2.576),
                      ylim = c(0, 0.7))
  if (m == horizons[1]) {
    mtext("Top journals", side = 3, line = 3)
  }  
}

### Save as PDF ----
dev.copy(pdf, "analysis/working_paper_1/figures/z_stat_densities/figure_z_density_and_counterfactual_rate_top_journals_vs_other.pdf",
         width = 10,
         height = 10)
dev.off()

par(mfrow = c(1, 1))


