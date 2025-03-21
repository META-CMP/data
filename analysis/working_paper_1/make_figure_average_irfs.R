# Creates average IRF plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing

# Source required functions ----
source(here::here("analysis/R/plot_average_irfs.R"))
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/kasy_MetaStudiesFunctions.R"))
source(here::here("analysis/R/kasy_RobustVariance.R"))
source(here::here("analysis/R/kasy_MetaStudiesPlots.R"))

# Define general folder path to save figures and their data
save_path <- "analysis/working_paper_1/figures/average_irfs/"

# Hardcode the maximum of period 0 precision to allow estimation in period 0 ---- 
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

# Redefining top_5_or_tier and cbanker as factors
d_no_qc$top_5_or_tier <- factor(d_no_qc$top_5_or_tier, levels = c(0, 1), labels = c("other publication", "top journal"))
d_no_qc$cbanker <- factor(d_no_qc$cbanker, levels = c(0, 1), labels = c("non-central bank affiliated", "central bank affiliated"))

# For output ----
out_var <- "output"

## Without median ----
avg_irf_output <- plot_average_irfs(
  d_no_qc %>% plotly::filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = FALSE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output$data, here::here(save_path, "avg_irf_output.csv"))
# Change plot title
avg_irf_output <- avg_irf_output$plot %>% plotly::layout(
  title = "Average IRF for Output"
)

## With median ----
avg_irf_output_median <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_median$data, here::here(save_path, "avg_irf_output_median.csv"))
# Change plot title
avg_irf_output_median <- avg_irf_output_median$plot %>% plotly::layout(
  title = "Average and median IRF for Output"
)

## With median and different publication bias corrections ----
#### Define helper function to extract intercepts ----  
extract_intercepts <- function(results) {
  
  # For other estimation methods, use confint
  intercepts <- lapply(results, function(model) {
    ci <- confint(model, level = conflevel)
    c(estimate = model[[1]][1],
      estimate_se = model[1,2],
      lower = ci[1, 1],
      upper = ci[1, 2])
  })
  
  data.frame(
    period = as.numeric(names(results)),
    estimate = sapply(intercepts, function(x) x["estimate"]),
    estimate_se = sapply(intercepts, function(x) x["estimate_se"]),
    lower = sapply(intercepts, function(x) x["lower"]),
    upper = sapply(intercepts, function(x) x["upper"])
  )
}
#### Define helper function to p-bias coefficients (only for FAT-PET, PEESE) ----  
extract_pbias <- function(results) {
  
  # For other estimation methods, use confint
  pbias <- lapply(results, function(model) {
    ci <- confint(model, level = conflevel)
    c(estimate = model[[2]][1],
      estimate_se = model[2,2],
      lower = ci[2, 1],
      upper = ci[2, 2])
  })
  
  data.frame(
    period = as.numeric(names(results)),
    estimate = sapply(pbias, function(x) x["estimate"]),
    estimate_se = sapply(pbias, function(x) x["estimate_se"]),
    lower = sapply(pbias, function(x) x["lower"]),
    upper = sapply(pbias, function(x) x["upper"])
  )
}
#### Define helper function to extract AK intercepts ----  
extract_intercepts_AK <- function(results) {
    # For AK estimation method, extract the precomputed confidence intervals
    intercepts <- lapply(results, function(model) {
      # Assuming the first row contains the intercept estimates
      c(estimate = model$tidy$estimate[1],
        estimate_se = model$tidy$std.error[1],
        lower = model$tidy$conf.low[1],
        upper = model$tidy$conf.high[1])
    })
  
  data.frame(
    period = as.numeric(names(results)),
    estimate = sapply(intercepts, function(x) x["estimate"]),
    estimate_se = sapply(intercepts, function(x) x["estimate_se"]),
    lower = sapply(intercepts, function(x) x["lower"]),
    upper = sapply(intercepts, function(x) x["upper"])
  )
}
### Estimate PEESE ----
peese_output <- meta_analysis(d_no_qc,
                              outvar = out_var,
                              se_option = "upper", 
                              periods = seq(0, 60, by = 3),
                              wins = wins_para,
                              prec_weighted = TRUE,
                              estimation = "PEESE", 
                              cluster_se = TRUE)
peese_output_pbias <- extract_pbias(peese_output)
peese_output <- extract_intercepts(peese_output)
### Estimate FAT-PET ----
fatpet_output <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_output_pbias <- extract_pbias(fatpet_output)
fatpet_output <- extract_intercepts(fatpet_output)
### Estimate FAT-PET without weights ----
fatpet_uw_output <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = FALSE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_uw_output_pbias <- extract_pbias(fatpet_uw_output)
fatpet_uw_output <- extract_intercepts(fatpet_uw_output)
### Estimate PEESE without weights ----
peese_uw_output <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper", 
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = FALSE,
                               estimation = "PEESE", 
                               cluster_se = TRUE)
peese_uw_output_pbias <- extract_pbias(peese_uw_output)
peese_uw_output <- extract_intercepts(peese_uw_output)
### Estimate WAAP with model selection based on horizon ----
model_waap_output <- meta_analysis(d_no_qc,
                                   outvar = out_var,
                                   se_option = "upper", 
                                   periods = seq(0, 60, by = 3),
                                   wins = wins_para,
                                   prec_weighted = FALSE,
                                   ap = TRUE,
                                   ap_horizon = 12,
                                   ap_prec_weighted = TRUE,
                                   ap_parameter = 2.8,
                                   estimation = "UWLS", 
                                   cluster_se = TRUE)
model_waap_output <- extract_intercepts(model_waap_output)
### Estimate AAP (unweigthed WAAP) with model selection based on horizon ----
model_waap_uw_output <- meta_analysis(d_no_qc,
                                      outvar = out_var,
                                      se_option = "upper", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = FALSE,
                                      ap = TRUE,
                                      ap_horizon = 12,
                                      ap_prec_weighted = FALSE,
                                      ap_parameter = 2.8,
                                      estimation = "UWLS", 
                                      cluster_se = TRUE)
model_waap_uw_output <- extract_intercepts(model_waap_uw_output)
### Estimate AK ----
ak_output <- meta_analysis(d_no_qc,
                           outvar = out_var,
                           se_option = "upper", 
                           periods = seq(0, 60, by = 3),
                           wins = wins_para,
                           prec_weighted = FALSE,
                           estimation = "AK", 
                           cluster_se = TRUE,
                           cutoff_val = 1,
                           AK_modelmu = "t",
                           AK_symmetric = FALSE,
                           AK_conf_level = conflevel,
                           ak_plot = "both")
ak_output <- extract_intercepts_AK(ak_output)
### Plot corrected effects ----
avg_irf_output_corrections <- avg_irf_output %>% 
  add_lines(
    data = peese_output,
    x = ~period,
    y = ~estimate,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_lines(
    data = fatpet_output,
    x = ~period,
    y = ~estimate,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>% 
  add_lines(
    data = fatpet_uw_output,
    x = ~period,
    y = ~estimate,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>%
  add_lines(
    data = peese_uw_output,
    x = ~period,
    y = ~estimate,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_trace(
    data = model_waap_output,
    x = ~period,
    y = ~estimate,
    name = "WAAP",
    showlegend = FALSE,
    marker = list(color = "darkgreen", size = 5)
  ) %>%
  add_trace(
    data = model_waap_uw_output,
    x = ~period,
    y = ~estimate,
    name = "UAAP",
    showlegend = FALSE,
    marker = list(color = "white", size = 5,
                  line = list(
                    color = 'darkgreen',
                    width = 1
                  ))
  ) %>% 
  add_lines(
    data = ak_output,
    x = ~period,
    y = ~estimate,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  )
avg_irf_output_corrections
### Save as PDF
orca(avg_irf_output_corrections,
     file = "analysis/working_paper_1/figures/average_irfs/avg_irf_output_corrections.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot corrected effects with bounds ----
avg_irf_output_corrections_with_bounds <- avg_irf_output %>% 
  add_ribbons(
    data = peese_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_ribbons(
    data = fatpet_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>%
  add_ribbons(
    data = fatpet_uw_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>% 
  add_ribbons(
    data = peese_uw_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_ribbons(
    data = model_waap_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>%
  add_ribbons(
    data = model_waap_uw_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "UAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>% 
  add_ribbons(
    data = ak_output,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  ) %>% 
  layout(
    yaxis = list(
      range = c(-1, 0.2)
    )
  )
avg_irf_output_corrections_with_bounds
### Save as PDF
orca(avg_irf_output_corrections_with_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/avg_irf_output_corrections_with_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot p-bias coefficients ----
output_pbias_plot <- plot_ly() %>%
  add_ribbons(
    data = fatpet_output_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3)
  ) %>% 
  add_ribbons(
    data = fatpet_uw_output_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3, dash = "dot")
  ) %>%
  add_ribbons(
    data = peese_output_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3, dash = "solid")
  ) %>%
  add_ribbons(
    data = peese_uw_output_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3, dash = "dash")
  )
output_pbias_plot
### Save as PDF
orca(output_pbias_plot,
     file = "analysis/working_paper_1/figures/average_irfs/output_pbias_plot.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
## With median, with SE bounds ----
avg_irf_output_median_se_bounds <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  ci_method = "avg.se",
  se_multiplier = 2,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_median_se_bounds$data, here::here(save_path, "avg_irf_output_median_se_bounds.csv"))
# Change plot title
avg_irf_output_median_se_bounds <- avg_irf_output_median_se_bounds$plot %>% plotly::layout(
  title = "Average and median IRF for Output"
)
# Comparison plot avg_irf_output_median and avg_irf_output_median_se_bounds with same ylim and next to each other
figure_avg_irf_output_median_se_bounds <- subplot(avg_irf_output_median, 
                                                  avg_irf_output_median_se_bounds, 
                                                  nrows = 1, 
                                                  margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "With approx. CI bounds", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "With SE bounds", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(-2.8,0.8)
    ),
    yaxis2 = list(range = list(-2.8,0.8)
    ),
    hovermode = "compare"
  )
# Save as pdf
orca(figure_avg_irf_output_median_se_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/figure_avg_irf_output_median_se_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

# For price level ----
out_var <- "inflation"

## Without median ----
avg_irf_pricelevel <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = FALSE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel$data, here::here(save_path, "avg_irf_pricelevel.csv"))
# Change plot title
avg_irf_pricelevel <- avg_irf_pricelevel$plot %>% plotly::layout(
  title = "Average IRF for Inflation"
)

## With median ----
avg_irf_pricelevel_median <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_median$data, here::here(save_path, "avg_irf_pricelevel_median.csv"))
# Change plot title
avg_irf_pricelevel_median <- avg_irf_pricelevel_median$plot %>% plotly::layout(
  title = "Average and median IRF for Inflation"
)

## With median and different publication bias corrections ----
### Estimate PEESE ----
peese_pricelevel <- meta_analysis(d_no_qc,
                              outvar = out_var,
                              se_option = "upper", 
                              periods = seq(0, 60, by = 3),
                              wins = wins_para,
                              prec_weighted = TRUE,
                              estimation = "PEESE", 
                              cluster_se = TRUE)
peese_pricelevel_pbias <- extract_pbias(peese_pricelevel)
peese_pricelevel <- extract_intercepts(peese_pricelevel)
### Estimate FAT-PET ----
fatpet_pricelevel <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_pricelevel_pbias <- extract_pbias(fatpet_pricelevel)
fatpet_pricelevel <- extract_intercepts(fatpet_pricelevel)
### Estimate FAT-PET without weights ----
fatpet_uw_pricelevel <- meta_analysis(d_no_qc,
                                  outvar = out_var,
                                  se_option = "upper",
                                  periods = seq(0, 60, by = 3),
                                  wins = wins_para,
                                  prec_weighted = FALSE,
                                  estimation = "FAT-PET",
                                  cluster_se = TRUE)
fatpet_uw_pricelevel_pbias <- extract_pbias(fatpet_uw_pricelevel)
fatpet_uw_pricelevel <- extract_intercepts(fatpet_uw_pricelevel)
### Estimate PEESE without weights ----
peese_uw_pricelevel <- meta_analysis(d_no_qc,
                                 outvar = out_var,
                                 se_option = "upper", 
                                 periods = seq(0, 60, by = 3),
                                 wins = wins_para,
                                 prec_weighted = FALSE,
                                 estimation = "PEESE", 
                                 cluster_se = TRUE)
peese_uw_pricelevel_pbias <- extract_pbias(peese_uw_pricelevel)
peese_uw_pricelevel <- extract_intercepts(peese_uw_pricelevel)
### Estimate WAAP with model selection based on horizon ----
model_waap_pricelevel <- meta_analysis(d_no_qc,
                                      outvar = out_var,
                                      se_option = "upper", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = FALSE,
                                      ap = TRUE,
                                      ap_horizon = 36,
                                      ap_prec_weighted = TRUE,
                                      ap_parameter = 2.8,
                                      estimation = "UWLS", 
                                      cluster_se = TRUE)
model_waap_pricelevel <- extract_intercepts(model_waap_pricelevel)
### Estimate AAP (unweigthed WAAP) with model selection based on horizon ----
model_waap_uw_pricelevel <- meta_analysis(d_no_qc,
                                         outvar = out_var,
                                         se_option = "upper", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = FALSE,
                                         ap = TRUE,
                                         ap_horizon = 36,
                                         ap_prec_weighted = FALSE,
                                         ap_parameter = 2.8,
                                         estimation = "UWLS", 
                                         cluster_se = TRUE)
model_waap_uw_pricelevel <- extract_intercepts(model_waap_uw_pricelevel)
### Estimate AK ----
ak_pricelevel <- meta_analysis(d_no_qc,
                           outvar = out_var,
                           se_option = "upper", 
                           periods = seq(0, 60, by = 3),
                           wins = wins_para,
                           prec_weighted = FALSE,
                           estimation = "AK", 
                           cluster_se = TRUE,
                           cutoff_val = 1,
                           AK_modelmu = "t",
                           AK_symmetric = FALSE,
                           AK_conf_level = conflevel,
                           ak_plot = "both")
ak_pricelevel <- extract_intercepts_AK(ak_pricelevel)
### Plot corrected effects ----
avg_irf_pricelevel_corrections <- avg_irf_pricelevel %>% 
  add_lines(
    data = peese_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_lines(
    data = fatpet_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>% 
  add_lines(
    data = fatpet_uw_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>%
  add_lines(
    data = peese_uw_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_trace(
    data = model_waap_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "WAAP",
    showlegend = FALSE,
    marker = list(color = "darkgreen", size = 5)
  ) %>%
  add_trace(
    data = model_waap_uw_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "UAAP",
    showlegend = FALSE,
    marker = list(color = "white", size = 5,
                  line = list(
                    color = 'darkgreen',
                    width = 1
                  ))
  ) %>% 
  add_lines(
    data = ak_pricelevel,
    x = ~period,
    y = ~estimate,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  )
avg_irf_pricelevel_corrections
### Save as PDF
orca(avg_irf_pricelevel_corrections,
     file = "analysis/working_paper_1/figures/average_irfs/avg_irf_pricelevel_corrections.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot corrected effects with bounds ----
avg_irf_pricelevel_corrections_with_bounds <- avg_irf_pricelevel %>% 
  add_ribbons(
    data = peese_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>%
  add_ribbons(
    data = fatpet_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>%
  add_ribbons(
    data = fatpet_uw_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>% 
  add_ribbons(
    data = peese_uw_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_ribbons(
    data = model_waap_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>%
  add_ribbons(
    data = model_waap_uw_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "UAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>% 
  add_ribbons(
    data = ak_pricelevel,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  ) %>% 
  layout(
    yaxis = list(
      range = c(-0.7, 0.15)
    )
  )
avg_irf_pricelevel_corrections_with_bounds
### Save as PDF
orca(avg_irf_pricelevel_corrections_with_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/avg_irf_pricelevel_corrections_with_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot p-bias coefficients ----
pricelevel_pbias_plot <- plot_ly() %>%
  add_ribbons(
    data = fatpet_pricelevel_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3)
  ) %>% 
  add_ribbons(
    data = fatpet_uw_pricelevel_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3, dash = "dot")
  ) %>%
  add_ribbons(
    data = peese_pricelevel_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3, dash = "solid")
  ) %>%
  add_ribbons(
    data = peese_uw_pricelevel_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = 'darkgreen', width = 3, dash = "dash")
  )
pricelevel_pbias_plot
### Save as PDF
orca(pricelevel_pbias_plot,
     file = "analysis/working_paper_1/figures/average_irfs/pricelevel_pbias_plot.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

## With median, with SE bounds ----
avg_irf_pricelevel_median_se_bounds <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  ci_method = "avg.se",
  se_multiplier = 2,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_median_se_bounds$data, here::here(save_path, "avg_irf_pricelevel_median_se_bounds.csv"))
# Change plot title
avg_irf_pricelevel_median_se_bounds <- avg_irf_pricelevel_median_se_bounds$plot %>% plotly::layout(
  title = "Mean and median IRF for Inflation"
)
# Comparison plot avg_irf_pricelevel_median and avg_irf_pricelevel_median_se_bounds with same ylim and next to each other
figure_avg_irf_pricelevel_median_se_bounds <- subplot(avg_irf_pricelevel_median, 
                                                      avg_irf_pricelevel_median_se_bounds, 
                                                      nrows = 1, 
                                                      margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "With approx. CI bounds", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "With SE bounds", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(-2.8,0.8)
    ),
    yaxis2 = list(range = list(-2.8,0.8)
    ),
    hovermode = "compare"
  )
# Save as pdf
orca(figure_avg_irf_pricelevel_median_se_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/figure_avg_irf_pricelevel_median_se_bounds.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

# For the interest rate ----
out_var <- "rate"

## Without median ----
avg_irf_rate <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate$data, here::here(save_path, "avg_irf_rate.csv"))
# Change plot title
avg_irf_rate <- avg_irf_rate$plot %>% plotly::layout(
  title = "Average IRF for Interest Rate"
)

## With median ----
avg_irf_rate_median <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_median$data, here::here(save_path, "avg_irf_rate_median.csv"))
# Change plot title
avg_irf_rate_median <- avg_irf_rate_median$plot %>% plotly::layout(
  title = "Average and median IRF for Interest Rate"
)

## With median and different publication bias corrections ----
### Estimate PEESE ----
peese_rate <- meta_analysis(d_no_qc,
                              outvar = out_var,
                              se_option = "avg", 
                              periods = seq(0, 60, by = 3),
                              wins = wins_para,
                              prec_weighted = TRUE,
                              estimation = "PEESE", 
                              cluster_se = TRUE)
peese_rate_pbias <- extract_pbias(peese_rate)
peese_rate <- extract_intercepts(peese_rate)
### Estimate FAT-PET ----
fatpet_rate <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "avg",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_rate_pbias <- extract_pbias(fatpet_rate)
fatpet_rate <- extract_intercepts(fatpet_rate)
### Estimate FAT-PET without weights ----
fatpet_uw_rate <- meta_analysis(d_no_qc,
                                  outvar = out_var,
                                  se_option = "avg",
                                  periods = seq(0, 60, by = 3),
                                  wins = wins_para,
                                  prec_weighted = FALSE,
                                  estimation = "FAT-PET",
                                  cluster_se = TRUE)
fatpet_uw_rate_pbias <- extract_pbias(fatpet_uw_rate)
fatpet_uw_rate <- extract_intercepts(fatpet_uw_rate)
### Estimate PEESE without weights ----
peese_uw_rate <- meta_analysis(d_no_qc,
                                 outvar = out_var,
                                 se_option = "avg", 
                                 periods = seq(0, 60, by = 3),
                                 wins = wins_para,
                                 prec_weighted = FALSE,
                                 estimation = "PEESE", 
                                 cluster_se = TRUE)
peese_uw_rate_pbias <- extract_pbias(peese_uw_rate)
peese_uw_rate <- extract_intercepts(peese_uw_rate)
### Estimate WAAP with model selection based on horizon ----
model_waap_rate <- meta_analysis(d_no_qc,
                                      outvar = out_var,
                                      se_option = "avg", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = FALSE,
                                      ap = TRUE,
                                      ap_horizon = 12,
                                      ap_prec_weighted = TRUE,
                                      ap_parameter = 2.8,
                                      estimation = "UWLS", 
                                      cluster_se = TRUE)
model_waap_rate <- extract_intercepts(model_waap_rate)
### Estimate AAP (unweigthed WAAP) with model selection based on horizon ----
model_waap_uw_rate <- meta_analysis(d_no_qc,
                                         outvar = out_var,
                                         se_option = "avg", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = FALSE,
                                         ap = TRUE,
                                         ap_horizon = 12,
                                         ap_prec_weighted = FALSE,
                                         ap_parameter = 2.8,
                                         estimation = "UWLS", 
                                         cluster_se = TRUE)
model_waap_uw_rate <- extract_intercepts(model_waap_uw_rate)
### Estimate AK ----
ak_rate <- meta_analysis(d_no_qc,
                           outvar = out_var,
                           se_option = "avg", 
                           periods = seq(0, 60, by = 3),
                           wins = wins_para,
                           prec_weighted = FALSE,
                           estimation = "AK", 
                           cluster_se = TRUE,
                           cutoff_val = 1,
                           AK_modelmu = "t",
                           AK_symmetric = FALSE,
                           AK_conf_level = conflevel,
                           ak_plot = "both")
ak_rate <- extract_intercepts_AK(ak_rate)
### Plot corrected effects ----
avg_irf_rate_corrections <- avg_irf_rate %>% 
  add_lines(
    data = peese_rate,
    x = ~period,
    y = ~estimate,
    name = "PEESE",
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_lines(
    data = fatpet_rate,
    x = ~period,
    y = ~estimate,
    name = "FAT-PET",
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>% 
  add_lines(
    data = fatpet_uw_rate,
    x = ~period,
    y = ~estimate,
    name = "OLS",
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>%
  add_lines(
    data = peese_uw_rate,
    x = ~period,
    y = ~estimate,
    name = "OLS with SE<sup>2</sup>",
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_trace(
    data = model_waap_rate,
    x = ~period,
    y = ~estimate,
    name = "WAAP",
    marker = list(color = "darkgreen", size = 5)
  ) %>%
  add_trace(
    data = model_waap_uw_rate,
    x = ~period,
    y = ~estimate,
    name = "UAAP",
    marker = list(color = "white", size = 5,
                  line = list(
                    color = 'darkgreen',
                    width = 1
                  ))
  ) %>% 
  add_lines(
    data = ak_rate,
    x = ~period,
    y = ~estimate,
    name = "AK",
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  )
avg_irf_rate_corrections
### Save as PDF
orca(avg_irf_rate_corrections,
     file = "analysis/working_paper_1/figures/average_irfs/avg_irf_rate_corrections.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot corrected effects with bounds ----
avg_irf_rate_corrections_with_bounds <- avg_irf_rate %>% 
  add_ribbons(
    data = peese_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_ribbons(
    data = fatpet_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>%
  add_ribbons(
    data = fatpet_uw_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>% 
  add_ribbons(
    data = peese_uw_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_ribbons(
    data = model_waap_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP",
    line = list(color = "darkgreen", width = 1)
  ) %>%
  add_ribbons(
    data = model_waap_uw_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "UAAP",
    line = list(color = "darkgreen", width = 1)
  ) %>% 
  add_ribbons(
    data = ak_rate,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK",
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  ) %>% 
  layout(
    yaxis = list(
      range = c(-0.12, 1)
    )
  )
avg_irf_rate_corrections_with_bounds
### Save as PDF
orca(avg_irf_rate_corrections_with_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/avg_irf_rate_corrections_with_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot p-bias coefficients ----
rate_pbias_plot <- plot_ly() %>%
  add_ribbons(
    data = fatpet_rate_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    line = list(color = 'darkgreen', width = 3)
  ) %>% 
  add_ribbons(
    data = fatpet_uw_rate_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    line = list(color = 'darkgreen', width = 3, dash = "dot")
  ) %>%
  add_ribbons(
    data = peese_rate_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    line = list(color = 'darkgreen', width = 3, dash = "solid")
  ) %>%
  add_ribbons(
    data = peese_uw_rate_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    line = list(color = 'darkgreen', width = 3, dash = "dash")
  )
rate_pbias_plot
### Save as PDF
orca(rate_pbias_plot,
     file = "analysis/working_paper_1/figures/average_irfs/rate_pbias_plot.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

## With median, with SE bounds ----
avg_irf_rate_median_se_bounds <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  ci_method = "avg.se",
  se_multiplier = 2,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_median_se_bounds$data, here::here(save_path, "avg_irf_rate_median_se_bounds.csv"))
# Change plot title
avg_irf_rate_median_se_bounds <- avg_irf_rate_median_se_bounds$plot %>% plotly::layout(
  title = "Average and median IRF for Interest Rate",
  yaxis = list(title = "Effect (%-points)")
)
# Comparison plot avg_irf_rate_median and avg_irf_rate_median_se_bounds with same ylim and next to each other
figure_avg_irf_rate_median_se_bounds <- subplot(avg_irf_rate_median, 
                                                avg_irf_rate_median_se_bounds, 
                                                nrows = 1, 
                                                margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on the interest rate',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "With approx. CI bounds", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "With SE bounds", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%-points)",
                 range = list(-1,1.5)
    ),
    yaxis2 = list(range = list(-1,1.5)
    ),
    hovermode = "compare"
  )
# Save as pdf
orca(figure_avg_irf_rate_median_se_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/figure_avg_irf_rate_median_se_bounds.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

# Joint figure of average IRFs for output and price level ----
figure_average_irfs_output_pricelevel <- subplot(avg_irf_output, 
                                                 avg_irf_pricelevel, 
                                                 nrows = 1, 
                                                 margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Output", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Price level", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
),
margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(-2.8,0.8)
    ),
    yaxis2 = list(range = list(-2.8,0.8)
    ),
    hovermode = "compare"
  )

# Display figure
figure_average_irfs_output_pricelevel

# Save as pdf
orca(figure_average_irfs_output_pricelevel,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_pricelevel.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

# Joint figure of average IRFs for output, price level and interest rate with median ----
figure_average_irfs_output_pricelevel_rate_with_median <- subplot(subplot(avg_irf_output_median,
                                                                          avg_irf_pricelevel_median,
                                                                          shareY = TRUE),
                                                                  avg_irf_rate_median, widths = c(.66, .33)
) %>%
  layout(legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>%        # put legend in center of x-axis
  layout(title = "",
         xaxis2 = list(title = "Months")
  ) %>%
  layout(
    yaxis = list(#title = "Effect (%)",
      range = list(y_lims[1], y_lims[2])),
    yaxis2 = list(#title = "Effect (%-points)",
      range = list(-1, 1.5))
  ) %>%
  layout(annotations = list(
    list(x = 30, y = y_lims[2], text = "Output response (%)", showarrow = FALSE, xref = "x", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = y_lims[2], text = "Price level response (%)", showarrow = FALSE, xref = "x2", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = 1.5, text = "Interest rate response (%-points)", showarrow = FALSE, xref = "x3", yref = "y2",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)
  )
# Display figure
figure_average_irfs_output_pricelevel_rate_with_median

# Save as pdf
orca(figure_average_irfs_output_pricelevel_rate_with_median,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_pricelevel_rate_with_median.pdf",
     scale = NULL,
     width = 1034,
     height = 486 * 0.8
)

# Joint figure of average IRFs for output, price level and interest rate with different publication bias corrections ----
figure_average_irfs_output_pricelevel_rate_corrections <- subplot(subplot(avg_irf_output_corrections,
                                                                  avg_irf_pricelevel_corrections,
                                                                  shareY = TRUE),
                                                          avg_irf_rate_corrections, widths = c(.66, .33)
) %>%
  layout(legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>%        # put legend in center of x-axis
  layout(title = "",
         xaxis2 = list(title = "Months")
  ) %>%
  layout(
    yaxis = list(#title = "Effect (%)",
      range = list(y_lims[1], y_lims[2])),
    yaxis2 = list(#title = "Effect (%-points)",
      range = list(-1, 1.5))
  ) %>%
  layout(annotations = list(
    list(x = 30, y = y_lims[2], text = "Output response (%)", showarrow = FALSE, xref = "x", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = y_lims[2], text = "Price level response (%)", showarrow = FALSE, xref = "x2", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = 1.5, text = "Interest rate response (%-points)", showarrow = FALSE, xref = "x3", yref = "y2",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)
  )
# Display figure
figure_average_irfs_output_pricelevel_rate_corrections
# Save as pdf
orca(figure_average_irfs_output_pricelevel_rate_corrections,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_pricelevel_rate_corrections.pdf",
     scale = NULL,
     width = 1034,
     height = 486 * 0.8
)

# For sub-samples ----
## Set PEESE color and legend ----
peese_color <- "darkgreen"
peese_legend <- paste0("PEESE ", 100*conflevel, "% CI")
## For output ----
out_var <- "output"
### For top journals vs other publications ----
y_lims <- c(-4.7, 1.1)
#### Top journals
##### Corrected IRF PEESE
peese_top_journals <- meta_analysis(d_no_qc %>% filter(top_5_or_tier == "top journal"),
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = seq(0, 60, by = 3),
                                     wins = wins_para,
                                     prec_weighted = TRUE,
                                     estimation = "PEESE", 
                                     cluster_se = TRUE)
peese_top_journals <- extract_intercepts(peese_top_journals)
##### Plot
avg_irf_output_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_top_journals$data, here::here(save_path, "avg_irf_output_top_journals.csv"))
# Change plot title
avg_irf_output_top_journals <- avg_irf_output_top_journals$plot %>% plotly::layout(
  title = "Average and median IRF for output in top journals"
)

#### Other publications
##### Corrected IRF PEESE
peese_other_publications <- meta_analysis(d_no_qc %>% filter(top_5_or_tier == "other publication"),
                                         outvar = out_var, 
                                         se_option = "upper", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = TRUE,
                                         estimation = "PEESE", 
                                         cluster_se = TRUE)
peese_other_publications <- extract_intercepts(peese_other_publications)
##### Plot
avg_irf_output_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_other_publications$data, here::here(save_path, "avg_irf_output_other_publications.csv"))
# Change plot title
avg_irf_output_other_publications <- avg_irf_output_other_publications$plot %>% plotly::layout(
  title = "Average and median IRF for output in other publications"
)

#### Joint figure, top journals right plot, other left
figure_average_irfs_output_top_journals_other_publications <- subplot(avg_irf_output_other_publications, 
                                                                      avg_irf_output_top_journals, 
                                                                      nrows = 1, 
                                                                      margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Other publications", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Top journals", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Output - Top journals vs other publications"
  )
# Display figure
figure_average_irfs_output_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_output_top_journals_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_top_journals_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
#### Sample size plot of top journals vs other
top_other_prop <- d_no_qc %>% # Proportions by period
  filter(period.month %in% seq(0,60,by=3)) %>%
  count(period.month, top_5_or_tier) %>%
  group_by(period.month) %>%
  mutate(proportion = n / sum(n))
print(top_other_prop, n = 42)
# Get average proportion for top journals across periods
top_prop <- top_other_prop %>%
  filter(top_5_or_tier == "top journal") %>%
  select(period.month, top_5_or_tier, proportion)
print(top_prop, n = 42)
mean(top_prop$proportion)  

d_no_qc %>%
  filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
  ggplot(aes(x = period.month, fill = top_5_or_tier)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Journals by Period",
       x = "Period",
       y = "Count",
       fill = "Publication Type") +
  theme_minimal()

### For cbanker vs non-cbanker ----
#### For central bank affiliated
##### Corrected IRF PEESE
peese_cbanker <- meta_analysis(d_no_qc %>% filter(cbanker == "central bank affiliated"),
                               outvar = out_var,
                               se_option = "upper", 
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "PEESE", 
                               cluster_se = TRUE)
peese_cbanker <- extract_intercepts(peese_cbanker)
##### Plot
avg_irf_output_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_cbanker,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_cbanker$data, here::here(save_path, "avg_irf_output_cbanker.csv"))
# Change plot title
avg_irf_output_cbanker <- avg_irf_output_cbanker$plot %>% plotly::layout(
  title = "Average and median IRF for output in central bank affiliated publications"
)

#### For non-central bank affiliated
##### Corrected IRF PEESE
peese_non_cbanker <- meta_analysis(d_no_qc %>% filter(cbanker == "non-central bank affiliated"),
                                   outvar = out_var,
                                   se_option = "upper", 
                                   periods = seq(0, 60, by = 3),
                                   wins = wins_para,
                                   prec_weighted = TRUE,
                                   estimation = "PEESE", 
                                   cluster_se = TRUE)
peese_non_cbanker <- extract_intercepts(peese_non_cbanker)
##### Plot
avg_irf_output_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_non_cbanker,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_non_cbanker$data, here::here(save_path, "avg_irf_output_non_cbanker.csv"))
# Change plot title
avg_irf_output_non_cbanker <- avg_irf_output_non_cbanker$plot %>% plotly::layout(
  title = "Average and median IRF for output in non-central bank affiliated publications"
)

# Joint figure, cbanker right plot, non-cbanker left
y_lims <- c(-2.5, 1)
figure_average_irfs_output_cbanker_non_cbanker <- subplot(avg_irf_output_non_cbanker, 
                                                           avg_irf_output_cbanker, 
                                                           nrows = 1, 
                                                           margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Non-central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
) %>% layout(
  title = "Average and median IRFs - Output - CB vs no CB"
)
# Display figure
figure_average_irfs_output_cbanker_non_cbanker
# Save as pdf
orca(figure_average_irfs_output_cbanker_non_cbanker,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_cbanker_non_cbanker.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

# Sample size plot for cbanker vs non-cbanker
d_no_qc %>%
  filter(period.month %in% seq(0, 60, by = 3), outcome == out_var) %>%
  ggplot(aes(x = period.month, fill = cbanker)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Central Bank Affiliation by Period",
       x = "Period",
       y = "Count",
       fill = "Central Bank Affiliation") +
  theme_minimal()
# Get average proportion for central bank affiliated across periods
cbanker_prop <- d_no_qc %>% # Proportions by period
  filter(period.month %in% seq(0,60,by=3)) %>%
  count(period.month, cbanker) %>%
  group_by(period.month) %>%
  mutate(proportion = n / sum(n))
print(cbanker_prop, n = 42)
cbanker_prop <- cbanker_prop %>% 
       filter(cbanker == "central bank affiliated") %>%
       select(period.month, proportion)
print(cbanker_prop, n = 42)
mean(cbanker_prop$proportion)

### For identifaction methods ("chol", "signr", "hf", "idother", "nr") ----
### For all publications
#### For Cholesky
##### Corrected IRF PEESE
peese_chol <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol"),
                            outvar = out_var,
                            se_option = "upper", 
                            periods = seq(0, 60, by = 3),
                            wins = wins_para,
                            prec_weighted = TRUE,
                            estimation = "PEESE", 
                            cluster_se = TRUE)
peese_chol <- extract_intercepts(peese_chol)
##### Plot
avg_irf_output_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_chol$data, here::here(save_path, "avg_irf_output_chol.csv"))
# Change plot title
avg_irf_output_chol <- avg_irf_output_chol$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr"),
                             outvar = out_var,
                             se_option = "upper", 
                             periods = seq(0, 60, by = 3),
                             wins = wins_para,
                             prec_weighted = TRUE,
                             estimation = "PEESE", 
                             cluster_se = TRUE)
peese_signr <- extract_intercepts(peese_signr)
##### Plot
avg_irf_output_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_signr$data, here::here(save_path, "avg_irf_output_signr.csv"))
# Change plot title
avg_irf_output_signr <- avg_irf_output_signr$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Corrected IRF PEESE
peese_hf <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf"),
                          outvar = out_var,
                          se_option = "upper", 
                          periods = seq(0, 60, by = 3),
                          wins = wins_para,
                          prec_weighted = TRUE,
                          estimation = "PEESE", 
                          cluster_se = TRUE)
peese_hf <- extract_intercepts(peese_hf)
##### Plot
avg_irf_output_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_hf$data, here::here(save_path, "avg_irf_output_hf.csv"))
# Change plot title
avg_irf_output_hf <- avg_irf_output_hf$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother"),
                               outvar = out_var,
                               se_option = "upper", 
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "PEESE", 
                               cluster_se = TRUE)
peese_idother <- extract_intercepts(peese_idother)
##### Plot
avg_irf_output_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_idother$data, here::here(save_path, "avg_irf_output_idother.csv"))
# Change plot title
avg_irf_output_idother <- avg_irf_output_idother$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Corrected IRF PEESE
peese_nr <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr"),
                          outvar = out_var,
                          se_option = "upper", 
                          periods = seq(0, 60, by = 3),
                          wins = wins_para,
                          prec_weighted = TRUE,
                          estimation = "PEESE", 
                          cluster_se = TRUE)
peese_nr <- extract_intercepts(peese_nr)
##### Plot
avg_irf_output_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_nr$data, here::here(save_path, "avg_irf_output_nr.csv"))
# Change plot title
avg_irf_output_nr <- avg_irf_output_nr$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-5, 2)
figure_average_irfs_output_identification_methods <- subplot(avg_irf_output_chol, 
                                                             avg_irf_output_signr, 
                                                             avg_irf_output_hf, 
                                                             avg_irf_output_idother, 
                                                             avg_irf_output_nr, 
                                                             nrows = 1, 
                                                             margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Output - By identification method"
  )
# Display figure
figure_average_irfs_output_identification_methods
# Save as pdf
orca(figure_average_irfs_output_identification_methods,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_identification_methods.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

# Sample size plot for identification methods
d_no_qc %>%
  filter(period.month %in% seq(0, 60, by = 3), outcome == out_var) %>%
  ggplot(aes(x = period.month, fill = group_ident_broad)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Identification Methods by Period",
       x = "Period",
       y = "Count",
       fill = "Identification Method") +
  theme_minimal()

### By identifcation method for top journals ----
#### For Cholesky
##### Corrected IRF PEESE
peese_chol_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol", top_5_or_tier == "top journal"),
                                        outvar = out_var,
                                        se_option = "upper", 
                                        periods = seq(0, 60, by = 3),
                                        wins = wins_para,
                                        prec_weighted = TRUE,
                                        estimation = "PEESE", 
                                        cluster_se = TRUE)
peese_chol_top_journals <- extract_intercepts(peese_chol_top_journals)
##### Plot
avg_irf_output_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_chol_top_journals$data, here::here(save_path, "avg_irf_output_chol_top_journals.csv"))
# Change plot title
avg_irf_output_chol_top_journals <- avg_irf_output_chol_top_journals$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr", top_5_or_tier == "top journal"),
                                         outvar = out_var,
                                         se_option = "upper", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = TRUE,
                                         estimation = "PEESE", 
                                         cluster_se = TRUE)
peese_signr_top_journals <- extract_intercepts(peese_signr_top_journals)
##### Plot
avg_irf_output_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_signr_top_journals$data, here::here(save_path, "avg_irf_output_signr_top_journals.csv"))
# Change plot title
avg_irf_output_signr_top_journals <- avg_irf_output_signr_top_journals$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Corrected IRF PEESE
peese_hf_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf", top_5_or_tier == "top journal"),
                                      outvar = out_var,
                                      se_option = "upper", 
                                      periods = seq(0, 57, by = 3), # Estimation fails for period 60, probably due to small sample issues
                                      wins = wins_para,
                                      prec_weighted = TRUE,
                                      estimation = "PEESE", 
                                      cluster_se = TRUE)
peese_hf_top_journals <- extract_intercepts(peese_hf_top_journals)
##### Plot
avg_irf_output_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_hf_top_journals$data, here::here(save_path, "avg_irf_output_hf_top_journals.csv"))
# Change plot title
avg_irf_output_hf_top_journals <- avg_irf_output_hf_top_journals$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother", top_5_or_tier == "top journal"),
                                           outvar = out_var,
                                           se_option = "upper", 
                                           periods = seq(0, 60, by = 3),
                                           wins = wins_para,
                                           prec_weighted = TRUE,
                                           estimation = "PEESE", 
                                           cluster_se = TRUE)
peese_idother_top_journals <- extract_intercepts(peese_idother_top_journals)
##### Plot
avg_irf_output_idother_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_idother_top_journals$data, here::here(save_path, "avg_irf_output_idother_top_journals.csv"))
# Change plot title
avg_irf_output_idother_top_journals <- avg_irf_output_idother_top_journals$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Corrected IRF PEESE
peese_nr_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr", top_5_or_tier == "top journal"),
                                      outvar = out_var,
                                      se_option = "upper", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = TRUE,
                                      estimation = "PEESE", 
                                      cluster_se = TRUE)
peese_nr_top_journals <- extract_intercepts(peese_nr_top_journals)
##### Plot
avg_irf_output_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_nr_top_journals$data, here::here(save_path, "avg_irf_output_nr_top_journals.csv"))
# Change plot title
avg_irf_output_nr_top_journals <- avg_irf_output_nr_top_journals$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-10, 3)
figure_average_irfs_output_identification_methods_top_journals <- subplot(avg_irf_output_chol_top_journals, 
                                                                          avg_irf_output_signr_top_journals, 
                                                                          avg_irf_output_hf_top_journals, 
                                                                          avg_irf_output_idother_top_journals, 
                                                                          avg_irf_output_nr_top_journals, 
                                                                          nrows = 1, 
                                                                          margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>% 
  layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Output - By identification method - Top journals"
  )
# Display figure
figure_average_irfs_output_identification_methods_top_journals
# Save as pdf
orca(figure_average_irfs_output_identification_methods_top_journals,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_identification_methods_top_journals.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
### By identifcation method for other publications ----
#### For Cholesky
##### Corrected IRF PEESE
peese_chol_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol", top_5_or_tier == "other publication"),
                                             outvar = out_var,
                                             se_option = "upper", 
                                             periods = seq(0, 60, by = 3),
                                             wins = wins_para,
                                             prec_weighted = TRUE,
                                             estimation = "PEESE", 
                                             cluster_se = TRUE)
peese_chol_other_publications <- extract_intercepts(peese_chol_other_publications)
avg_irf_output_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_chol_other_publications$data, here::here(save_path, "avg_irf_output_chol_other_publications.csv"))
# Change plot title
avg_irf_output_chol_other_publications <- avg_irf_output_chol_other_publications$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr", top_5_or_tier == "other publication"),
                                              outvar = out_var,
                                              se_option = "upper", 
                                              periods = seq(0, 60, by = 3),
                                              wins = wins_para,
                                              prec_weighted = TRUE,
                                              estimation = "PEESE", 
                                              cluster_se = TRUE)
peese_signr_other_publications <- extract_intercepts(peese_signr_other_publications)
avg_irf_output_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_signr_other_publications$data, here::here(save_path, "avg_irf_output_signr_other_publications.csv"))
# Change plot title
avg_irf_output_signr_other_publications <- avg_irf_output_signr_other_publications$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Corrected IRF PEESE
peese_hf_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf", top_5_or_tier == "other publication"),
                                           outvar = out_var,
                                           se_option = "upper", 
                                           periods = seq(0, 60, by = 3),
                                           wins = wins_para,
                                           prec_weighted = TRUE,
                                           estimation = "PEESE", 
                                           cluster_se = TRUE)
peese_hf_other_publications <- extract_intercepts(peese_hf_other_publications)
##### Plot
avg_irf_output_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_hf_other_publications$data, here::here(save_path, "avg_irf_output_hf_other_publications.csv"))
# Change plot title
avg_irf_output_hf_other_publications <- avg_irf_output_hf_other_publications$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother", top_5_or_tier == "other publication"),
                                                outvar = out_var,
                                                se_option = "upper", 
                                                periods = seq(0, 60, by = 3),
                                                wins = wins_para,
                                                prec_weighted = TRUE,
                                                estimation = "PEESE", 
                                                cluster_se = TRUE)
peese_idother_other_publications <- extract_intercepts(peese_idother_other_publications)
##### Plot
avg_irf_output_idother_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_idother_other_publications$data, here::here(save_path, "avg_irf_output_idother_other_publications.csv"))
# Change plot title
avg_irf_output_idother_other_publications <- avg_irf_output_idother_other_publications$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Corrected IRF PEESE
peese_nr_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr", top_5_or_tier == "other publication"),
                                           outvar = out_var,
                                           se_option = "upper", 
                                           periods = seq(0, 60, by = 3),
                                           wins = wins_para,
                                           prec_weighted = TRUE,
                                           estimation = "PEESE", 
                                           cluster_se = TRUE)
peese_nr_other_publications <- extract_intercepts(peese_nr_other_publications)
##### Plot
avg_irf_output_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_nr_other_publications$data, here::here(save_path, "avg_irf_output_nr_other_publications.csv"))
# Change plot title
avg_irf_output_nr_other_publications <- avg_irf_output_nr_other_publications$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-10, 3)
figure_average_irfs_output_identification_methods_other_publications <- subplot(avg_irf_output_chol_other_publications, 
                                                                              avg_irf_output_signr_other_publications, 
                                                                              avg_irf_output_hf_other_publications, 
                                                                              avg_irf_output_idother_other_publications, 
                                                                              avg_irf_output_nr_other_publications, 
                                                                              nrows = 1, 
                                                                              margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>% 
  layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Output - By identification method - Other publications"
  )
# Display figure
figure_average_irfs_output_identification_methods_other_publications
# Save as pdf
orca(figure_average_irfs_output_identification_methods_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_identification_methods_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
# Joint figure top journals vs other publications by identification method, two rows, first other, then top
y_lims <- c(-10, 3)
figure_average_irfs_output_identification_methods_top_journals_other_publications <- subplot(figure_average_irfs_output_identification_methods_other_publications, 
                                                                                           figure_average_irfs_output_identification_methods_top_journals, 
                                                                                           nrows = 2, 
                                                                                           margin = 0.05) %>% layout(
  showlegend = FALSE,
  title = 'Average effects of conventional monetary policy shocks on output') %>%
  layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis6 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Output - By identification method - Other publications (top row) vs top journals (bottom row)"
  )
# Display figure
figure_average_irfs_output_identification_methods_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_output_identification_methods_top_journals_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_identification_methods_top_journals_other_publications.pdf",
     scale = NULL,
     width = 2000,
     height = 1000
)

### By country/region ----
#### US ----
##### Corrected IRF PEESE
peese_us <- meta_analysis(d_no_qc %>% filter(us == 1),
                          outvar = out_var,
                          se_option = "upper", 
                          periods = seq(0, 60, by = 3),
                          wins = wins_para,
                          prec_weighted = TRUE,
                          estimation = "PEESE", 
                          cluster_se = TRUE)
peese_us <- extract_intercepts(peese_us)
##### Plot
avg_irf_output_us <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, us == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_us,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_us$data, here::here(save_path, "avg_irf_output_us.csv"))
# Change plot title
avg_irf_output_us <- avg_irf_output_us$plot %>% plotly::layout(
  title = "US"
)

#### EA12 ----
##### Corrected IRF PEESE
peese_ea12 <- meta_analysis(d_no_qc %>% filter(ea12 == 1),
                            outvar = out_var,
                            se_option = "upper", 
                            periods = seq(0, 60, by = 3),
                            wins = wins_para,
                            prec_weighted = TRUE,
                            estimation = "PEESE", 
                            cluster_se = TRUE)
peese_ea12 <- extract_intercepts(peese_ea12)
##### Plot
avg_irf_output_ea12 <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, ea12 == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_ea12,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_ea12$data, here::here(save_path, "avg_irf_output_ea12.csv"))
# Change plot title
avg_irf_output_ea12 <- avg_irf_output_ea12$plot %>% plotly::layout(
  title = "EA12"
)

#### Upper middle ----
##### Corrected IRF PEESE
peese_upper_middle <- meta_analysis(d_no_qc %>% filter(upper_middle_income == 1),
                                   outvar = out_var,
                                   se_option = "upper", 
                                   periods = seq(0, 60, by = 3),
                                   wins = wins_para,
                                   prec_weighted = TRUE,
                                   estimation = "PEESE", 
                                   cluster_se = TRUE)
peese_upper_middle <- extract_intercepts(peese_upper_middle)
##### Plot
avg_irf_output_upper_middle <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, upper_middle_income == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_upper_middle,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_upper_middle$data, here::here(save_path, "avg_irf_output_upper_middle.csv"))
# Change plot title
avg_irf_output_upper_middle <- avg_irf_output_upper_middle$plot %>% plotly::layout(
  title = "Emerging Economies"
)

#### Other high_income  ----
##### Corrected IRF PEESE
peese_other_high_income <- meta_analysis(d_no_qc %>% filter(high_income == 1, ea12 != 1, us != 1),
                                      outvar = out_var,
                                      se_option = "upper", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = TRUE,
                                      estimation = "PEESE", 
                                      cluster_se = TRUE)
peese_other_high_income <- extract_intercepts(peese_other_high_income)
##### Plot
avg_irf_output_other_high_income <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, high_income == 1, ea12 != 1, us != 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_other_high_income,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_other_high_income$data, here::here(save_path, "avg_irf_output_other_high_income.csv"))
# Change plot title
avg_irf_output_other_high_income <- avg_irf_output_other_high_income$plot %>% plotly::layout(
  title = "Other high_income"
)

#### Joint plot 
y_lims <- c(-3, 1.5)
figure_average_irfs_output_country_region <- subplot(avg_irf_output_us, 
                                                     avg_irf_output_ea12, 
                                                     avg_irf_output_upper_middle, 
                                                     avg_irf_output_other_high_income, 
                                                     nrows = 1, 
                                                     margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on output',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "US", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.4, y = 1, text = "Euro area", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.65, y = 1, text = "Emerging Economies", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Other Advanced", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2]) 
    ), 
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare" 
) %>% layout(
  title = "Average and median IRFs - Output - By country/group"
)
# Display figure
figure_average_irfs_output_country_region
# Save as pdf
orca(figure_average_irfs_output_country_region,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_country_region.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
#### Sample size plot for country/region
print(d_no_qc %>% # Proportions by period
        filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
        count(period.month, us, ea12, upper_middle_income, high_income) %>%
        group_by(period.month) %>%
        mutate(proportion = n / sum(n)), n = 105)

ggplot(d_no_qc %>% 
         filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>% 
         count(period.month, us, ea12, upper_middle_income, high_income) %>%
         group_by(period.month) %>%
         mutate(proportion = n / sum(n)), 
       aes(x = period.month, y = proportion, fill = interaction(us, ea12, upper_middle_income, high_income))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Other", "Advanced", "Upper Middle", "EA12", "US"),
                    name = "Region") +
  labs(x = "Period (Months)", 
       y = "Share",
       title = "Regional Shares Over Time") +
  theme_minimal() +
  theme(legend.position = "bottom")

d_plot <- d_no_qc %>% 
  filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
  count(period.month, us, ea12, upper_middle_income, high_income)

ggplot(d_plot, aes(x = period.month, y = n, 
                   fill = interaction(us, ea12, upper_middle_income, high_income))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Other", "Advanced", "Upper Middle", "EA12", "US"),
                    name = "Region") +
  labs(x = "Period (Months)", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom")

## For price level ----
out_var <- "inflation"
### For top journals vs other publications ----
#### Top journals
##### Corrected IRF PEESE
peese_top_journals <- meta_analysis(d_no_qc %>% filter(top_5_or_tier == "top journal"),
                                   outvar = out_var,
                                   se_option = "upper", 
                                   periods = seq(0, 60, by = 3),
                                   wins = wins_para,
                                   prec_weighted = TRUE,
                                   estimation = "PEESE", 
                                   cluster_se = TRUE)
peese_top_journals <- extract_intercepts(peese_top_journals)
##### Plot
avg_irf_pricelevel_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_top_journals$data, here::here(save_path, "avg_irf_pricelevel_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_top_journals <- avg_irf_pricelevel_top_journals$plot %>% plotly::layout(
  title = "Top journals"
)

#### Other publications
##### Corrected IRF PEESE
peese_other_publications <- meta_analysis(d_no_qc %>% filter(top_5_or_tier == "other publication"),
                                        outvar = out_var,
                                        se_option = "upper", 
                                        periods = seq(0, 60, by = 3),
                                        wins = wins_para,
                                        prec_weighted = TRUE,
                                        estimation = "PEESE", 
                                        cluster_se = TRUE)
peese_other_publications <- extract_intercepts(peese_other_publications)
##### Plot
avg_irf_pricelevel_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_other_publications$data, here::here(save_path, "avg_irf_pricelevel_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_other_publications <- avg_irf_pricelevel_other_publications$plot %>% plotly::layout(
  title = "Other publications"
)

#### Joint figure, top journals right plot, other left
y_lims <- c(-4.2, 0.9)
figure_average_irfs_pricelevel_top_journals_other_publications <- subplot(avg_irf_pricelevel_other_publications, 
                                                                         avg_irf_pricelevel_top_journals, 
                                                                         nrows = 1, 
                                                                         margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Other publications", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Top journals", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
) %>% layout(
  title = "Average and median IRFs - Price level - Top journals vs other publications"
)
# Display figure
figure_average_irfs_pricelevel_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_pricelevel_top_journals_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_top_journals_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
#### Sample size plot of top journals vs other
print(d_no_qc %>% # Proportions by period
        filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
        count(period.month, top_5_or_tier) %>%
        group_by(period.month) %>%
        mutate(proportion = n / sum(n)), n = 42)
d_no_qc %>%
  filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
  ggplot(aes(x = period.month, fill = top_5_or_tier)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Journals by Period",
       x = "Period",
       y = "Count",
       fill = "Publication Type") +
  theme_minimal()

### For cbanker vs non-cbanker ----
#### Central bank affiliated
##### Corrected IRF PEESE
peese_cbanker <- meta_analysis(d_no_qc %>% filter(cbanker == "central bank affiliated"),
                             outvar = out_var,
                             se_option = "upper", 
                             periods = seq(0, 60, by = 3),
                             wins = wins_para,
                             prec_weighted = TRUE,
                             estimation = "PEESE", 
                             cluster_se = TRUE)
peese_cbanker <- extract_intercepts(peese_cbanker)
##### Plot
avg_irf_pricelevel_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_cbanker,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_cbanker$data, here::here(save_path, "avg_irf_pricelevel_cbanker.csv"))
# Change plot title
avg_irf_pricelevel_cbanker <- avg_irf_pricelevel_cbanker$plot %>% plotly::layout(
  title = "Central bank affiliated"
)

### For non-cbanker
##### Corrected IRF PEESE
peese_non_cbanker <- meta_analysis(d_no_qc %>% filter(cbanker == "non-central bank affiliated"),
                                outvar = out_var,
                                se_option = "upper", 
                                periods = seq(0, 60, by = 3),
                                wins = wins_para,
                                prec_weighted = TRUE,
                                estimation = "PEESE", 
                                cluster_se = TRUE)
peese_non_cbanker <- extract_intercepts(peese_non_cbanker)
##### Plot
avg_irf_pricelevel_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_non_cbanker,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_non_cbanker$data, here::here(save_path, "avg_irf_pricelevel_non_cbanker.csv"))
# Change plot title
avg_irf_pricelevel_non_cbanker <- avg_irf_pricelevel_non_cbanker$plot %>% plotly::layout(
  title = "Non-central bank affiliated"
)

# Joint figure, cbanker right plot, non-cbanker left
y_lims <- c(-2, 0.5)
figure_average_irfs_pricelevel_cbanker_non_cbanker <- subplot(avg_irf_pricelevel_non_cbanker, 
                                                              avg_irf_pricelevel_cbanker, 
                                                              nrows = 1, 
                                                              margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Non-central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
) %>% layout(
  title = "Average and median IRFs - Price level - CB vs no CB"
)
# Display figure
figure_average_irfs_pricelevel_cbanker_non_cbanker
# Save as pdf
orca(figure_average_irfs_pricelevel_cbanker_non_cbanker,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_cbanker_non_cbanker.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

### By identifcation method ("chol", "signr", "hf", "idother", "nr") ----
### For all publications
#### For Cholesky
##### Corrected IRF PEESE
peese_chol <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol"),
                          outvar = out_var,
                          se_option = "upper", 
                          periods = seq(0, 60, by = 3),
                          wins = wins_para,
                          prec_weighted = TRUE,
                          estimation = "PEESE", 
                          cluster_se = TRUE)
peese_chol <- extract_intercepts(peese_chol)
##### Plot
avg_irf_pricelevel_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_chol$data, here::here(save_path, "avg_irf_pricelevel_chol.csv"))
# Change plot title
avg_irf_pricelevel_chol <- avg_irf_pricelevel_chol$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr"),
                           outvar = out_var,
                           se_option = "upper", 
                           periods = seq(0, 60, by = 3),
                           wins = wins_para,
                           prec_weighted = TRUE,
                           estimation = "PEESE", 
                           cluster_se = TRUE)
peese_signr <- extract_intercepts(peese_signr)
##### Plot
avg_irf_pricelevel_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_signr$data, here::here(save_path, "avg_irf_pricelevel_signr.csv"))
# Change plot title
avg_irf_pricelevel_signr <- avg_irf_pricelevel_signr$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Corrected IRF PEESE
peese_hf <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf"),
                        outvar = out_var,
                        se_option = "upper", 
                        periods = seq(0, 60, by = 3),
                        wins = wins_para,
                        prec_weighted = TRUE,
                        estimation = "PEESE", 
                        cluster_se = TRUE)
peese_hf <- extract_intercepts(peese_hf)
##### Plot
avg_irf_pricelevel_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_hf$data, here::here(save_path, "avg_irf_pricelevel_hf.csv"))
# Change plot title
avg_irf_pricelevel_hf <- avg_irf_pricelevel_hf$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother"),
                             outvar = out_var,
                             se_option = "upper", 
                             periods = seq(0, 60, by = 3),
                             wins = wins_para,
                             prec_weighted = TRUE,
                             estimation = "PEESE", 
                             cluster_se = TRUE)
peese_idother <- extract_intercepts(peese_idother)
##### Plot
avg_irf_pricelevel_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_idother$data, here::here(save_path, "avg_irf_pricelevel_idother.csv"))
# Change plot title
avg_irf_pricelevel_idother <- avg_irf_pricelevel_idother$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Corrected IRF PEESE
peese_nr <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr"),
                      outvar = out_var,
                      se_option = "upper", 
                      periods = seq(0, 60, by = 3),
                      wins = wins_para,
                      prec_weighted = TRUE,
                      estimation = "PEESE", 
                      cluster_se = TRUE)
peese_nr <- extract_intercepts(peese_nr)
##### Plot
avg_irf_pricelevel_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_nr$data, here::here(save_path, "avg_irf_pricelevel_nr.csv"))
# Change plot title
avg_irf_pricelevel_nr <- avg_irf_pricelevel_nr$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-2.5, 1)
figure_average_irfs_pricelevel_identification_methods <- subplot(avg_irf_pricelevel_chol, 
                                                                 avg_irf_pricelevel_signr, 
                                                                 avg_irf_pricelevel_hf, 
                                                                 avg_irf_pricelevel_idother, 
                                                                 avg_irf_pricelevel_nr, 
                                                                 nrows = 1, 
                                                                 margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Price level - By identification method"
  )
# Display figure
figure_average_irfs_pricelevel_identification_methods
# Save as pdf
orca(figure_average_irfs_pricelevel_identification_methods,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_identification_methods.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

### By identifcation method for top journals ----
#### For Cholesky
##### Corrected IRF PEESE
peese_chol_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol", top_5_or_tier == "top journal"),
                                      outvar = out_var,
                                      se_option = "upper", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = TRUE,
                                      estimation = "PEESE", 
                                      cluster_se = TRUE)
peese_chol_top_journals <- extract_intercepts(peese_chol_top_journals)
##### Plot
avg_irf_pricelevel_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_chol_top_journals$data, here::here(save_path, "avg_irf_pricelevel_chol_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_chol_top_journals <- avg_irf_pricelevel_chol_top_journals$plot %>% plotly::layout(
  title = "Cholesky"
)
  
#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr", top_5_or_tier == "top journal"),
                                       outvar = out_var,
                                       se_option = "upper", 
                                       periods = seq(0, 60, by = 3),
                                       wins = wins_para,
                                       prec_weighted = TRUE,
                                       estimation = "PEESE", 
                                       cluster_se = TRUE)
peese_signr_top_journals <- extract_intercepts(peese_signr_top_journals)
##### Plot
avg_irf_pricelevel_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_signr_top_journals$data, here::here(save_path, "avg_irf_pricelevel_signr_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_signr_top_journals <- avg_irf_pricelevel_signr_top_journals$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Corrected IRF PEESE
peese_hf_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf", top_5_or_tier == "top journal"),
                                    outvar = out_var,
                                    se_option = "upper", 
                                    periods = seq(0, 60, by = 3),
                                    wins = wins_para,
                                    prec_weighted = TRUE,
                                    estimation = "PEESE", 
                                    cluster_se = TRUE)
peese_hf_top_journals <- extract_intercepts(peese_hf_top_journals)
##### Plot
avg_irf_pricelevel_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_hf_top_journals$data, here::here(save_path, "avg_irf_pricelevel_hf_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_hf_top_journals <- avg_irf_pricelevel_hf_top_journals$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother", top_5_or_tier == "top journal"),
                                         outvar = out_var,
                                         se_option = "upper", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = TRUE,
                                         estimation = "PEESE", 
                                         cluster_se = TRUE)
peese_idother_top_journals <- extract_intercepts(peese_idother_top_journals)
##### Plot
avg_irf_pricelevel_idother_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_idother_top_journals$data, here::here(save_path, "avg_irf_pricelevel_idother_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_idother_top_journals <- avg_irf_pricelevel_idother_top_journals$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Corrected IRF PEESE
peese_nr_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr", top_5_or_tier == "top journal"),
                                   outvar = out_var,
                                   se_option = "upper", 
                                   periods = seq(0, 60, by = 3),
                                   wins = wins_para,
                                   prec_weighted = TRUE,
                                   estimation = "PEESE", 
                                   cluster_se = TRUE)
peese_nr_top_journals <- extract_intercepts(peese_nr_top_journals)
##### Plot
avg_irf_pricelevel_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_nr_top_journals$data, here::here(save_path, "avg_irf_pricelevel_nr_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_nr_top_journals <- avg_irf_pricelevel_nr_top_journals$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-5, 0.75)
figure_average_irfs_pricelevel_identification_methods_top_journals <- subplot(avg_irf_pricelevel_chol_top_journals, 
                                                                             avg_irf_pricelevel_signr_top_journals, 
                                                                             avg_irf_pricelevel_hf_top_journals, 
                                                                             avg_irf_pricelevel_idother_top_journals, 
                                                                             avg_irf_pricelevel_nr_top_journals, 
                                                                             nrows = 1, 
                                                                             margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Price level - By identification method - Top journals"
  )
# Display figure
figure_average_irfs_pricelevel_identification_methods_top_journals
# Save as pdf
orca(figure_average_irfs_pricelevel_identification_methods_top_journals,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_identification_methods_top_journals.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
### By identifcation method for other publications ----
#### For Cholesky
##### Corrected IRF PEESE
peese_chol_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol", top_5_or_tier == "other publication"),
                                           outvar = out_var,
                                           se_option = "upper", 
                                           periods = seq(0, 60, by = 3),
                                           wins = wins_para,
                                           prec_weighted = TRUE,
                                           estimation = "PEESE", 
                                           cluster_se = TRUE)
peese_chol_other_publications <- extract_intercepts(peese_chol_other_publications)
##### Plot
avg_irf_pricelevel_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_chol_other_publications$data, here::here(save_path, "avg_irf_pricelevel_chol_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_chol_other_publications <- avg_irf_pricelevel_chol_other_publications$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr", top_5_or_tier == "other publication"),
                                            outvar = out_var,
                                            se_option = "upper", 
                                            periods = seq(0, 60, by = 3),
                                            wins = wins_para,
                                            prec_weighted = TRUE,
                                            estimation = "PEESE", 
                                            cluster_se = TRUE)
peese_signr_other_publications <- extract_intercepts(peese_signr_other_publications)
##### Plot
avg_irf_pricelevel_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_signr_other_publications$data, here::here(save_path, "avg_irf_pricelevel_signr_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_signr_other_publications <- avg_irf_pricelevel_signr_other_publications$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Corrected IRF PEESE
peese_hf_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf", top_5_or_tier == "other publication"),
                                          outvar = out_var,
                                          se_option = "upper", 
                                          periods = seq(0, 60, by = 3),
                                          wins = wins_para,
                                          prec_weighted = TRUE,
                                          estimation = "PEESE", 
                                          cluster_se = TRUE)
peese_hf_other_publications <- extract_intercepts(peese_hf_other_publications)
##### Plot
avg_irf_pricelevel_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_hf_other_publications$data, here::here(save_path, "avg_irf_pricelevel_hf_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_hf_other_publications <- avg_irf_pricelevel_hf_other_publications$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother", top_5_or_tier == "other publication"),
                                             outvar = out_var,
                                             se_option = "upper", 
                                             periods = seq(0, 60, by = 3),
                                             wins = wins_para,
                                             prec_weighted = TRUE,
                                             estimation = "PEESE", 
                                             cluster_se = TRUE)
peese_idother_other_publications <- extract_intercepts(peese_idother_other_publications)
##### Plot
avg_irf_pricelevel_idother_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_idother_other_publications$data, here::here(save_path, "avg_irf_pricelevel_idother_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_idother_other_publications <- avg_irf_pricelevel_idother_other_publications$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Corrected IRF PEESE
peese_nr_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr", top_5_or_tier == "other publication"),
                                         outvar = out_var,
                                         se_option = "upper", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = TRUE,
                                         estimation = "PEESE", 
                                         cluster_se = TRUE)
peese_nr_other_publications <- extract_intercepts(peese_nr_other_publications)
##### Plot
avg_irf_pricelevel_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_nr_other_publications$data, here::here(save_path, "avg_irf_pricelevel_nr_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_nr_other_publications <- avg_irf_pricelevel_nr_other_publications$plot %>% plotly::layout(
  title = "Narrative"
)  

# Joint figure, one row
y_lims <- c(-3, 0.5)
figure_average_irfs_pricelevel_identification_methods_other_publications <- subplot(avg_irf_pricelevel_chol_other_publications, 
                                                                                  avg_irf_pricelevel_signr_other_publications, 
                                                                                  avg_irf_pricelevel_hf_other_publications, 
                                                                                  avg_irf_pricelevel_idother_other_publications, 
                                                                                  avg_irf_pricelevel_nr_other_publications, 
                                                                                  nrows = 1, 
                                                                                  margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Price level - By identification method - Other publications"
  )
# Display figure
figure_average_irfs_pricelevel_identification_methods_other_publications
# Save as pdf
orca(figure_average_irfs_pricelevel_identification_methods_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_identification_methods_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
# Joint figure top journals vs other publications by identification method, two rows, first other, then top
y_lims <- c(-8, 1)
figure_average_irfs_pricelevel_identification_methods_top_journals_other_publications <- subplot(figure_average_irfs_pricelevel_identification_methods_other_publications, 
                                                                                                 figure_average_irfs_pricelevel_identification_methods_top_journals, 
                                                                                                 nrows = 2, 
                                                                                                 margin = 0.05) %>% layout(
  showlegend = FALSE,
  title = 'Average effects of conventional monetary policy shocks on inflation') %>%
  layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis6 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Price level - By identification method - Other publications (top row) vs top journals (bottom row)"
  )
# Display figure
figure_average_irfs_pricelevel_identification_methods_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_pricelevel_identification_methods_top_journals_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_identification_methods_top_journals_other_publications.pdf",
     scale = NULL,
     width = 2000,
     height = 1000
)

### By country/region ----
#### US ----
##### Corrected IRF PEESE
peese_us <- meta_analysis(d_no_qc %>% filter(us == 1),
                      outvar = out_var,
                      se_option = "upper", 
                      periods = seq(0, 60, by = 3),
                      wins = wins_para,
                      prec_weighted = TRUE,
                      estimation = "PEESE", 
                      cluster_se = TRUE)
peese_us <- extract_intercepts(peese_us)
##### Plot
avg_irf_pricelevel_us <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, us == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_us,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_us$data, here::here(save_path, "avg_irf_pricelevel_us.csv"))
# Change plot title
avg_irf_pricelevel_us <- avg_irf_pricelevel_us$plot %>% plotly::layout(
  title = "US"
)

#### EA12 ----
##### Corrected IRF PEESE
peese_ea12 <- meta_analysis(d_no_qc %>% filter(ea12 == 1),
                        outvar = out_var,
                        se_option = "upper", 
                        periods = seq(0, 60, by = 3),
                        wins = wins_para,
                        prec_weighted = TRUE,
                        estimation = "PEESE", 
                        cluster_se = TRUE)
peese_ea12 <- extract_intercepts(peese_ea12)
##### Plot
avg_irf_pricelevel_ea12 <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, ea12 == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_ea12,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_ea12$data, here::here(save_path, "avg_irf_pricelevel_ea12.csv"))
# Change plot title
avg_irf_pricelevel_ea12 <- avg_irf_pricelevel_ea12$plot %>% plotly::layout(
  title = "EA12"
)

#### Upper middle ----
##### Corrected IRF PEESE
peese_upper_middle <- meta_analysis(d_no_qc %>% filter(upper_middle_income == 1),
                                outvar = out_var,
                                se_option = "upper", 
                                periods = seq(0, 60, by = 3),
                                wins = wins_para,
                                prec_weighted = TRUE,
                                estimation = "PEESE", 
                                cluster_se = TRUE)
peese_upper_middle <- extract_intercepts(peese_upper_middle)
##### Plot
avg_irf_pricelevel_upper_middle <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, upper_middle_income == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_upper_middle,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_upper_middle$data, here::here(save_path, "avg_irf_pricelevel_upper_middle.csv"))
# Change plot title
avg_irf_pricelevel_upper_middle <- avg_irf_pricelevel_upper_middle$plot %>% plotly::layout(
  title = "Upper middle income countries"
)

#### Other high_income ----
##### Corrected IRF PEESE
peese_other_high_income <- meta_analysis(d_no_qc %>% filter(high_income == 1, ea12 != 1, us != 1),
                                    outvar = out_var,
                                    se_option = "upper", 
                                    periods = seq(0, 60, by = 3),
                                    wins = wins_para,
                                    prec_weighted = TRUE,
                                    estimation = "PEESE", 
                                    cluster_se = TRUE)
peese_other_high_income <- extract_intercepts(peese_other_high_income)
##### Plot
avg_irf_pricelevel_other_high_income <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, high_income == 1, ea12 != 1, us != 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_other_high_income,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_other_high_income$data, here::here(save_path, "avg_irf_pricelevel_other_high_income.csv"))
# Change plot title
avg_irf_pricelevel_other_high_income <- avg_irf_pricelevel_other_high_income$plot %>% plotly::layout(
  title = "Other high income economies"
)

#### Joint plot 
y_lims <- c(-2, 0.75)
figure_average_irfs_pricelevel_country_region <- subplot(avg_irf_pricelevel_us, 
                                                     avg_irf_pricelevel_ea12, 
                                                     avg_irf_pricelevel_upper_middle, 
                                                     avg_irf_pricelevel_other_high_income, 
                                                     nrows = 1, 
                                                     margin = 0.03) %>% layout(
                                                       showlegend=FALSE,
                                                       title = 'Average effects of conventional monetary policy shocks on output',
                                                       xaxis3 = list(title = "Month"), # x-axis for plot 3
                                                       xaxis4 = list(title = "Month"), # x-axis for plot 4
                                                       xaxis5 = list(title = "Month"), # x-axis for plot 5
                                                       xaxis6 = list(title = "Month")  # x-axis for plot 6
                                                     ) %>% layout(annotations = list(
                                                       list(x = 0.1, y = 1, text = "US", showarrow = FALSE, xref = "paper", yref = "paper",
                                                            xanchor = "center", yanchor = "bottom"),
                                                       list(x = 0.4, y = 1, text = "Euro area", showarrow = FALSE, xref = "paper", yref = "paper",
                                                            xanchor = "center", yanchor = "bottom"),
                                                       list(x = 0.65, y = 1, text = "Emerging economies", showarrow = FALSE, xref = "paper", yref = "paper",
                                                            xanchor = "center", yanchor = "bottom"),
                                                       list(x = 0.9, y = 1, text = "Other advanced", showarrow = FALSE, xref = "paper", yref = "paper",
                                                            xanchor = "center", yanchor = "bottom")
                                                     ), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2]) 
    ), 
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare" 
  ) %>% layout(
    title = "Average and median IRFs - Price level - By country/group"
  )
# Display figure
figure_average_irfs_pricelevel_country_region
# Save as pdf
orca(figure_average_irfs_pricelevel_country_region,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_pricelevel_country_region.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

#### Sample size plot for country/region
print(d_no_qc %>% # Proportions by period
        filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
        count(period.month, us, ea12, upper_middle_income, high_income) %>%
        group_by(period.month) %>%
        mutate(proportion = n / sum(n)), n = 105)

ggplot(d_no_qc %>% 
         filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>% 
         count(period.month, us, ea12, upper_middle_income, high_income) %>%
         group_by(period.month) %>%
         mutate(proportion = n / sum(n)), 
       aes(x = period.month, y = proportion, fill = interaction(us, ea12, upper_middle_income, high_income))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Other", "Advanced", "Upper Middle", "EA12", "US"),
                    name = "Region") +
  labs(x = "Period (Months)", 
       y = "Share",
       title = "Regional Shares Over Time") +
  theme_minimal() +
  theme(legend.position = "bottom")

d_plot <- d_no_qc %>% 
  filter(period.month %in% seq(0,60,by=3), outcome == out_var) %>%
  count(period.month, us, ea12, upper_middle_income, high_income)

ggplot(d_plot, aes(x = period.month, y = n, 
                   fill = interaction(us, ea12, upper_middle_income, high_income))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Other", "Advanced", "Upper Middle", "EA12", "US"),
                    name = "Region") +
  labs(x = "Period (Months)", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom")


## For interest rate ----
out_var <- "rate"

### For top journals vs other publications ----
#### Top journals
##### Corrected IRF PEESE
peese_top_journals <- meta_analysis(d_no_qc %>% filter(top_5_or_tier == "top journal"),
                               outvar = out_var,
                               se_option = "avg", 
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "PEESE", 
                               cluster_se = TRUE)
peese_top_journals <- extract_intercepts(peese_top_journals)
##### Plot
avg_irf_rate_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_top_journals$data, here::here(save_path, "avg_irf_rate_top_journals.csv"))
# Change plot title
avg_irf_rate_top_journals <- avg_irf_rate_top_journals$plot %>% plotly::layout(
  title = "Top journals"
)

#### Other publications
##### Corrected IRF PEESE
peese_other_publications <- meta_analysis(d_no_qc %>% filter(top_5_or_tier == "other publication"),
                                     outvar = out_var,
                                     se_option = "avg", 
                                     periods = seq(0, 60, by = 3),
                                     wins = wins_para,
                                     prec_weighted = TRUE,
                                     estimation = "PEESE", 
                                     cluster_se = TRUE)
peese_other_publications <- extract_intercepts(peese_other_publications)
##### Plot
avg_irf_rate_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_other_publications$data, here::here(save_path, "avg_irf_rate_other_publications.csv"))
# Change plot title
avg_irf_rate_other_publications <- avg_irf_rate_other_publications$plot %>% plotly::layout(
  title = "Other publications"
)

#### Joint figure, top journals right plot, other left
figure_average_irfs_rate_top_journals_other_publications <- subplot(avg_irf_rate_other_publications, 
                                                                   avg_irf_rate_top_journals, 
                                                                   nrows = 1, 
                                                                   margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on interest rate',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Other publications", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Top journals", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%-points)",
                 range = list(-1,1.6)
    ),
    yaxis2 = list(range = list(-1,1.6)
    ),
    hovermode = "compare"
) %>% layout(
  title = "Average and median IRFs - Interest rate - Top journals vs other publications"
)
# Display figure
figure_average_irfs_rate_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_rate_top_journals_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_rate_top_journals_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

### For cbanker vs non-cbanker ----
#### Central bank affiliated
##### Corrected IRF PEESE
peese_cbanker <- meta_analysis(d_no_qc %>% filter(cbanker == "central bank affiliated"),
                           outvar = out_var,
                           se_option = "avg", 
                           periods = seq(0, 60, by = 3),
                           wins = wins_para,
                           prec_weighted = TRUE,
                           estimation = "PEESE", 
                           cluster_se = TRUE)
peese_cbanker <- extract_intercepts(peese_cbanker)
##### Plot
avg_irf_rate_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_cbanker,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_cbanker$data, here::here(save_path, "avg_irf_rate_cbanker.csv"))
# Change plot title
avg_irf_rate_cbanker <- avg_irf_rate_cbanker$plot %>% plotly::layout(
  title = "Central bank affiliated"
)

### Non-central bank affiliated
##### Corrected IRF PEESE
peese_non_cbanker <- meta_analysis(d_no_qc %>% filter(cbanker == "non-central bank affiliated"),
                               outvar = out_var,
                               se_option = "avg", 
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "PEESE", 
                               cluster_se = TRUE)
peese_non_cbanker <- extract_intercepts(peese_non_cbanker)
##### Plot
avg_irf_rate_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_non_cbanker,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_non_cbanker$data, here::here(save_path, "avg_irf_rate_non_cbanker.csv"))
# Change plot title
avg_irf_rate_non_cbanker <- avg_irf_rate_non_cbanker$plot %>% plotly::layout(
  title = "Non-central bank affiliated"
)

# Joint figure, cbanker right plot, non-cbanker left
y_lims <- c(-1, 1.6)
figure_average_irfs_rate_cbanker_non_cbanker <- subplot(avg_irf_rate_non_cbanker, 
                                                        avg_irf_rate_cbanker, 
                                                        nrows = 1, 
                                                        margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on interest rate',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Non-central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%-points)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
) %>% layout(
  title = "Average and median IRFs - Interest rate - CB vs no CB"
)
# Display figure
figure_average_irfs_rate_cbanker_non_cbanker
# Save as pdf
orca(figure_average_irfs_rate_cbanker_non_cbanker,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_rate_cbanker_non_cbanker.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

### By identifcation method ("chol", "signr", "hf", "idother", "nr") ----
### For all publications
#### For Cholesky
##### Corrected IRF PEESE
peese_chol <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol"),
                        outvar = out_var,
                        se_option = "avg", 
                        periods = seq(0, 60, by = 3),
                        wins = wins_para,
                        prec_weighted = TRUE,
                        estimation = "PEESE", 
                        cluster_se = TRUE)
peese_chol <- extract_intercepts(peese_chol)
##### Plot
avg_irf_rate_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_chol$data, here::here(save_path, "avg_irf_rate_chol.csv"))

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr"),
                         outvar = out_var,
                         se_option = "avg", 
                         periods = seq(0, 60, by = 3),
                         wins = wins_para,
                         prec_weighted = TRUE,
                         estimation = "PEESE", 
                         cluster_se = TRUE)
peese_signr <- extract_intercepts(peese_signr)
##### Plot
avg_irf_rate_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_signr$data, here::here(save_path, "avg_irf_rate_signr.csv"))

#### For High frequency
##### Corrected IRF PEESE
peese_hf <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf"),
                      outvar = out_var,
                      se_option = "avg", 
                      periods = seq(0, 60, by = 3),
                      wins = wins_para,
                      prec_weighted = TRUE,
                      estimation = "PEESE", 
                      cluster_se = TRUE)
peese_hf <- extract_intercepts(peese_hf)
##### Plot
avg_irf_rate_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_hf$data, here::here(save_path, "avg_irf_rate_hf.csv"))

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother"),
                           outvar = out_var,
                           se_option = "avg", 
                           periods = seq(0, 60, by = 3),
                           wins = wins_para,
                           prec_weighted = TRUE,
                           estimation = "PEESE", 
                           cluster_se = TRUE)
peese_idother <- extract_intercepts(peese_idother)
##### Plot
avg_irf_rate_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_idother$data, here::here(save_path, "avg_irf_rate_idother.csv"))

#### For narrative
##### Corrected IRF PEESE
peese_nr <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr"),
                      outvar = out_var,
                      se_option = "avg", 
                      periods = seq(0, 60, by = 3),
                      wins = wins_para,
                      prec_weighted = TRUE,
                      estimation = "PEESE", 
                      cluster_se = TRUE)
peese_nr <- extract_intercepts(peese_nr)
##### Plot
avg_irf_rate_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_nr$data, here::here(save_path, "avg_irf_rate_nr.csv"))

# Joint figure, one row
y_lims <- c(-1.5, 2.2)
figure_average_irfs_rate_identification_methods <- subplot(avg_irf_rate_chol$plot, 
                                                           avg_irf_rate_signr$plot, 
                                                           avg_irf_rate_hf$plot, 
                                                           avg_irf_rate_idother$plot, 
                                                           avg_irf_rate_nr$plot, 
                                                           nrows = 1, 
                                                           margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on interest rate',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%-points)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Interest rate - By identification method"
  )
# Display figure
figure_average_irfs_rate_identification_methods
# Save as pdf
orca(figure_average_irfs_rate_identification_methods,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_rate_identification_methods.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

### By identifcation method for top journals ----
#### For Cholesky
##### Corrected IRF PEESE
peese_chol_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol", top_5_or_tier == "top journal"),
                                    outvar = out_var,
                                    se_option = "avg", 
                                    periods = seq(0, 60, by = 3),
                                    wins = wins_para,
                                    prec_weighted = TRUE,
                                    estimation = "PEESE", 
                                    cluster_se = TRUE)
peese_chol_top_journals <- extract_intercepts(peese_chol_top_journals)
##### Plot
avg_irf_rate_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_chol_top_journals$data, here::here(save_path, "avg_irf_rate_chol_top_journals.csv"))

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr", top_5_or_tier == "top journal"),
                                     outvar = out_var,
                                     se_option = "avg", 
                                     periods = seq(0, 60, by = 3),
                                     wins = wins_para,
                                     prec_weighted = TRUE,
                                     estimation = "PEESE", 
                                     cluster_se = TRUE)
peese_signr_top_journals <- extract_intercepts(peese_signr_top_journals)
##### Plot
avg_irf_rate_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_signr_top_journals$data, here::here(save_path, "avg_irf_rate_signr_top_journals.csv"))

#### For High frequency
##### Corrected IRF PEESE
peese_hf_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf", top_5_or_tier == "top journal"),
                                  outvar = out_var,
                                  se_option = "avg", 
                                  periods = seq(0, 60, by = 3),
                                  wins = wins_para,
                                  prec_weighted = TRUE,
                                  estimation = "PEESE", 
                                  cluster_se = TRUE)
peese_hf_top_journals <- extract_intercepts(peese_hf_top_journals)
##### Plot
avg_irf_rate_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_hf_top_journals$data, here::here(save_path, "avg_irf_rate_hf_top_journals.csv"))

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother", top_5_or_tier == "top journal"),
                                      outvar = out_var,
                                      se_option = "avg", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = TRUE,
                                      estimation = "PEESE", 
                                      cluster_se = TRUE)
peese_idother_top_journals <- extract_intercepts(peese_idother_top_journals)
##### Plot
avg_irf_rate_idother_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_idother_top_journals$data, here::here(save_path, "avg_irf_rate_idother_top_journals.csv"))

#### For narrative
##### Corrected IRF PEESE
peese_nr_top_journals <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr", top_5_or_tier == "top journal"),
                                outvar = out_var,
                                se_option = "avg", 
                                periods = seq(0, 60, by = 3),
                                wins = wins_para,
                                prec_weighted = TRUE,
                                estimation = "PEESE", 
                                cluster_se = TRUE)
peese_nr_top_journals <- extract_intercepts(peese_nr_top_journals)
##### Plot
avg_irf_rate_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr_top_journals,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_nr_top_journals$data, here::here(save_path, "avg_irf_rate_nr_top_journals.csv"))

# Joint figure, one row
y_lims <- c(-1.6, 2.2)
figure_average_irfs_rate_identification_methods_top_journals <- subplot(avg_irf_rate_chol_top_journals$plot, 
                                                                       avg_irf_rate_signr_top_journals$plot, 
                                                                       avg_irf_rate_hf_top_journals$plot, 
                                                                       avg_irf_rate_idother_top_journals$plot, 
                                                                       avg_irf_rate_nr_top_journals$plot, 
                                                                       nrows = 1, 
                                                                       margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on interest rate',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%-points)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Interest rate - By identification method - Top journals"
  )
# Display figure
figure_average_irfs_rate_identification_methods_top_journals
# Save as pdf
orca(figure_average_irfs_rate_identification_methods_top_journals,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_rate_identification_methods_top_journals.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)

### By identifcation method for other publications ----
#### For Cholesky
##### Corrected IRF PEESE
peese_chol_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "chol", top_5_or_tier == "other publication"),
                                          outvar = out_var,
                                          se_option = "avg", 
                                          periods = seq(0, 60, by = 3),
                                          wins = wins_para,
                                          prec_weighted = TRUE,
                                          estimation = "PEESE", 
                                          cluster_se = TRUE)
peese_chol_other_publications <- extract_intercepts(peese_chol_other_publications)
##### Plot
avg_irf_rate_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_chol_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_chol_other_publications$data, here::here(save_path, "avg_irf_rate_chol_other_publications.csv"))

#### For Sign restrictions
##### Corrected IRF PEESE
peese_signr_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "signr", top_5_or_tier == "other publication"),
                                           outvar = out_var,
                                           se_option = "avg", 
                                           periods = seq(0, 60, by = 3),
                                           wins = wins_para,
                                           prec_weighted = TRUE,
                                           estimation = "PEESE", 
                                           cluster_se = TRUE)
peese_signr_other_publications <- extract_intercepts(peese_signr_other_publications)
##### Plot
avg_irf_rate_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_signr_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_signr_other_publications$data, here::here(save_path, "avg_irf_rate_signr_other_publications.csv"))

#### For High frequency
##### Corrected IRF PEESE
peese_hf_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "hf", top_5_or_tier == "other publication"),
                                        outvar = out_var,
                                        se_option = "avg", 
                                        periods = seq(0, 60, by = 3),
                                        wins = wins_para,
                                        prec_weighted = TRUE,
                                        estimation = "PEESE", 
                                        cluster_se = TRUE)
peese_hf_other_publications <- extract_intercepts(peese_hf_other_publications)
##### Plot
avg_irf_rate_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_hf_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_hf_other_publications$data, here::here(save_path, "avg_irf_rate_hf_other_publications.csv"))

#### For Other identification methods
##### Corrected IRF PEESE
peese_idother_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "idother", top_5_or_tier == "other publication"),
                                            outvar = out_var,
                                            se_option = "avg", 
                                            periods = seq(0, 60, by = 3),
                                            wins = wins_para,
                                            prec_weighted = TRUE,
                                            estimation = "PEESE", 
                                            cluster_se = TRUE)
peese_idother_other_publications <- extract_intercepts(peese_idother_other_publications)
##### Plot
avg_irf_rate_idother_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_idother_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_idother_other_publications$data, here::here(save_path, "avg_irf_rate_idother_other_publications.csv"))

#### For narrative
##### Corrected IRF PEESE
peese_nr_other_publications <- meta_analysis(d_no_qc %>% filter(group_ident_broad == "nr", top_5_or_tier == "other publication"),
                                        outvar = out_var,
                                        se_option = "avg", 
                                        periods = seq(0, 60, by = 3),
                                        wins = wins_para,
                                        prec_weighted = TRUE,
                                        estimation = "PEESE", 
                                        cluster_se = TRUE)
peese_nr_other_publications <- extract_intercepts(peese_nr_other_publications)
##### Plot
avg_irf_rate_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  # corrected_irf = peese_nr_other_publications,
  # corrected_irf_color = peese_color,
  # corrected_irf_name = peese_legend,
  # corrected_irf_show_CIs = FALSE,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_nr_other_publications$data, here::here(save_path, "avg_irf_rate_nr_other_publications.csv"))

# Joint figure, one row
y_lims <- c(-1.5, 2.2)
figure_average_irfs_rate_identification_methods_other_publications <- subplot(avg_irf_rate_chol_other_publications$plot, 
                                                                            avg_irf_rate_signr_other_publications$plot, 
                                                                            avg_irf_rate_hf_other_publications$plot, 
                                                                            avg_irf_rate_idother_other_publications$plot, 
                                                                            avg_irf_rate_nr_other_publications$plot, 
                                                                            nrows = 1, 
                                                                            margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on interest rate',
  xaxis3 = list(title = "Month"), # x-axis for plot 3
  xaxis4 = list(title = "Month"), # x-axis for plot 4
  xaxis5 = list(title = "Month"), # x-axis for plot 5
  xaxis6 = list(title = "Month")  # x-axis for plot 6
) %>% layout(annotations = list(
  list(x = 0.1, y = 1, text = "Cholesky", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.7, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.9, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months"),
    xaxis5 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%-points)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Interest rate - By identification method - Other publications"
  )
# Display figure
figure_average_irfs_rate_identification_methods_other_publications
# Save as pdf
orca(figure_average_irfs_rate_identification_methods_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_rate_identification_methods_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
# Joint figure top journals vs other publications by identification method, two rows, first other, then top
y_lims <- c(-1.5, 2.2)
figure_average_irfs_rate_identification_methods_top_journals_other_publications <- subplot(figure_average_irfs_rate_identification_methods_other_publications, 
                                                                                         figure_average_irfs_rate_identification_methods_top_journals, 
                                                                                         nrows = 2, 
                                                                                         margin = 0.05) %>% layout(
  showlegend = FALSE,
  title = 'Average effects of conventional monetary policy shocks on interest rate') %>%
  layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    yaxis2 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis3 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis4 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis5 = list(range = list(y_lims[1], y_lims[2])
    ),
    yaxis6 = list(range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  ) %>% layout(
    title = "Average and median IRFs - Interest rate - By identification method - Other publications (top row) vs top journals (bottom row)"
  )
# Display figure
figure_average_irfs_rate_identification_methods_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_rate_identification_methods_top_journals_other_publications,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_rate_identification_methods_top_journals_other_publications.pdf",
     scale = NULL,
     width = 2000,
     height = 1000
)

beepr::beep()
