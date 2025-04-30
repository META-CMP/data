# Creates average IRF plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/setup_wp_2.R"))

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
save_path <- "analysis/working_paper_2/figures/average_irfs/"

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

# For employment ----
out_var <- "emp"

## Without median ----
avg_irf_emp <- plot_average_irfs(
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
write_csv(avg_irf_emp$data, here::here(save_path, "avg_irf_emp.csv"))
# Change plot title
avg_irf_emp <- avg_irf_emp$plot %>% plotly::layout(
  title = "Average IRF for employment"
)

## With median ----
avg_irf_emp_median <- plot_average_irfs(
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
write_csv(avg_irf_emp_median$data, here::here(save_path, "avg_irf_emp_median.csv"))
# Change plot title
avg_irf_emp_median <- avg_irf_emp_median$plot %>% plotly::layout(
  title = "Average and median IRF for emplyoment"
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
peese_emp <- meta_analysis(d_no_qc,
                              outvar = out_var,
                              se_option = "upper", 
                              periods = seq(0, 60, by = 3),
                              wins = wins_para,
                              prec_weighted = TRUE,
                              estimation = "PEESE", 
                              cluster_se = TRUE)
peese_emp_pbias <- extract_pbias(peese_emp)
peese_emp <- extract_intercepts(peese_emp)
### Estimate FAT-PET ----
fatpet_emp <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_emp_pbias <- extract_pbias(fatpet_emp)
fatpet_emp <- extract_intercepts(fatpet_emp)
### Estimate FAT-PET without weights ----
fatpet_uw_emp <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = FALSE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_uw_emp_pbias <- extract_pbias(fatpet_uw_emp)
fatpet_uw_emp <- extract_intercepts(fatpet_uw_emp)
### Estimate PEESE without weights ----
peese_uw_emp <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "upper", 
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = FALSE,
                               estimation = "PEESE", 
                               cluster_se = TRUE)
peese_uw_emp_pbias <- extract_pbias(peese_uw_emp)
peese_uw_emp <- extract_intercepts(peese_uw_emp)
### Estimate WAAP with model selection based on horizon ----
model_waap_emp <- meta_analysis(d_no_qc,
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
model_waap_emp <- extract_intercepts(model_waap_emp)
### Estimate AAP (unweigthed WAAP) with model selection based on horizon ----
model_waap_uw_emp <- meta_analysis(d_no_qc,
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
model_waap_uw_emp <- extract_intercepts(model_waap_uw_emp)
### Estimate AK ----
ak_emp <- meta_analysis(d_no_qc,
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
ak_emp <- extract_intercepts_AK(ak_emp)
### Plot corrected effects ----
avg_irf_emp_corrections <- avg_irf_emp %>% 
  add_lines(
    data = peese_emp,
    x = ~period,
    y = ~estimate,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_lines(
    data = fatpet_emp,
    x = ~period,
    y = ~estimate,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>% 
  add_lines(
    data = fatpet_uw_emp,
    x = ~period,
    y = ~estimate,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>%
  add_lines(
    data = peese_uw_emp,
    x = ~period,
    y = ~estimate,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_trace(
    data = model_waap_emp,
    x = ~period,
    y = ~estimate,
    name = "WAAP",
    showlegend = FALSE,
    marker = list(color = "darkgreen", size = 5)
  ) %>%
  add_trace(
    data = model_waap_uw_emp,
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
    data = ak_emp,
    x = ~period,
    y = ~estimate,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  )
avg_irf_emp_corrections
### Save as PDF
orca(avg_irf_emp_corrections,
     file = "analysis/working_paper_2/figures/average_irfs/avg_irf_emp_corrections.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot corrected effects with bounds ----
avg_irf_emp_corrections_with_bounds <- avg_irf_emp %>% 
  add_ribbons(
    data = peese_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_ribbons(
    data = fatpet_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>%
  add_ribbons(
    data = fatpet_uw_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>% 
  add_ribbons(
    data = peese_uw_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_ribbons(
    data = model_waap_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>%
  add_ribbons(
    data = model_waap_uw_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "UAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>% 
  add_ribbons(
    data = ak_emp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  ) %>% 
  layout(
    yaxis = list(
      # range = c(-1, 0.2)
    )
  )
avg_irf_emp_corrections_with_bounds
### Save as PDF
orca(avg_irf_emp_corrections_with_bounds,
     file = "analysis/working_paper_2/figures/average_irfs/avg_irf_emp_corrections_with_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

### Plot p-bias coefficients ----
emp_pbias_plot <- plot_ly() %>%
  add_ribbons(
    data = fatpet_emp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(width = 3)
  ) %>% 
  add_ribbons(
    data = fatpet_uw_emp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(width = 3, dash = "dot")
  ) %>%
  add_ribbons(
    data = peese_emp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(width = 3, dash = "solid")
  ) %>%
  add_ribbons(
    data = peese_uw_emp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(width = 3, dash = "dash")
  )
emp_pbias_plot
### Save as PDF
orca(emp_pbias_plot,
     file = "analysis/working_paper_2/figures/average_irfs/emp_pbias_plot.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
## With median, with SE bounds ----
avg_irf_emp_median_se_bounds <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  ci_method = "avg.se",
  se_multiplier = 2,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_median_se_bounds$data, here::here(save_path, "avg_irf_emp_median_se_bounds.csv"))
# Change plot title
avg_irf_emp_median_se_bounds <- avg_irf_emp_median_se_bounds$plot %>% plotly::layout(
  title = "Average and median IRF for employment"
)
# Comparison plot avg_irf_emp_median and avg_irf_emp_median_se_bounds with same ylim and next to each other
figure_avg_irf_emp_median_se_bounds <- subplot(avg_irf_emp_median, 
                                                  avg_irf_emp_median_se_bounds, 
                                                  nrows = 1, 
                                                  margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on employment',
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
orca(figure_avg_irf_emp_median_se_bounds,
     file = "analysis/working_paper_2/figures/average_irfs/figure_avg_irf_emp_median_se_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

# For unemployment ----
out_var <- "unemp"

## Without median ----
avg_irf_unemp <- plot_average_irfs(
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
write_csv(avg_irf_unemp$data, here::here(save_path, "avg_irf_unemp.csv"))
# Change plot title
avg_irf_unemp <- avg_irf_unemp$plot %>% plotly::layout(
  title = "Average IRF for unemployment"
)

## With median ----
avg_irf_unemp_median <- plot_average_irfs(
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
write_csv(avg_irf_unemp_median$data, here::here(save_path, "avg_irf_unemp_median.csv"))
# Change plot title
avg_irf_unemp_median <- avg_irf_unemp_median$plot %>% plotly::layout(
  title = "Average and median IRF for unemployment"
)

## With median and different publication bias corrections ----
### Estimate PEESE ----
peese_unemp <- meta_analysis(d_no_qc,
                              outvar = out_var,
                              se_option = "lower", 
                              periods = seq(0, 60, by = 3),
                              wins = wins_para,
                              prec_weighted = TRUE,
                              estimation = "PEESE", 
                              cluster_se = TRUE)
peese_unemp_pbias <- extract_pbias(peese_unemp)
peese_unemp <- extract_intercepts(peese_unemp)
### Estimate FAT-PET ----
fatpet_unemp <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "lower",
                               periods = seq(0, 60, by = 3),
                               wins = wins_para,
                               prec_weighted = TRUE,
                               estimation = "FAT-PET",
                               cluster_se = TRUE)
fatpet_unemp_pbias <- extract_pbias(fatpet_unemp)
fatpet_unemp <- extract_intercepts(fatpet_unemp)
### Estimate FAT-PET without weights ----
fatpet_uw_unemp <- meta_analysis(d_no_qc,
                                  outvar = out_var,
                                  se_option = "lower",
                                  periods = seq(0, 60, by = 3),
                                  wins = wins_para,
                                  prec_weighted = FALSE,
                                  estimation = "FAT-PET",
                                  cluster_se = TRUE)
fatpet_uw_unemp_pbias <- extract_pbias(fatpet_uw_unemp)
fatpet_uw_unemp <- extract_intercepts(fatpet_uw_unemp)
### Estimate PEESE without weights ----
peese_uw_unemp <- meta_analysis(d_no_qc,
                                 outvar = out_var,
                                 se_option = "lower", 
                                 periods = seq(0, 60, by = 3),
                                 wins = wins_para,
                                 prec_weighted = FALSE,
                                 estimation = "PEESE", 
                                 cluster_se = TRUE)
peese_uw_unemp_pbias <- extract_pbias(peese_uw_unemp)
peese_uw_unemp <- extract_intercepts(peese_uw_unemp)
### Estimate WAAP with model selection based on horizon ----
model_waap_unemp <- meta_analysis(d_no_qc,
                                      outvar = out_var,
                                      se_option = "lower", 
                                      periods = seq(0, 60, by = 3),
                                      wins = wins_para,
                                      prec_weighted = FALSE,
                                      ap = TRUE,
                                      ap_horizon = 36,
                                      ap_prec_weighted = TRUE,
                                      ap_parameter = 2.8,
                                      estimation = "UWLS", 
                                      cluster_se = TRUE)
model_waap_unemp <- extract_intercepts(model_waap_unemp)
### Estimate AAP (unweigthed WAAP) with model selection based on horizon ----
model_waap_uw_unemp <- meta_analysis(d_no_qc,
                                         outvar = out_var,
                                         se_option = "lower", 
                                         periods = seq(0, 60, by = 3),
                                         wins = wins_para,
                                         prec_weighted = FALSE,
                                         ap = TRUE,
                                         ap_horizon = 36,
                                         ap_prec_weighted = FALSE,
                                         ap_parameter = 2.8,
                                         estimation = "UWLS", 
                                         cluster_se = TRUE)
model_waap_uw_unemp <- extract_intercepts(model_waap_uw_unemp)
### Estimate AK ----
ak_unemp <- meta_analysis(d_no_qc,
                           outvar = out_var,
                           se_option = "lower", 
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
ak_unemp <- extract_intercepts_AK(ak_unemp)
### Plot corrected effects ----
avg_irf_unemp_corrections <- avg_irf_unemp %>% 
  add_lines(
    data = peese_unemp,
    x = ~period,
    y = ~estimate,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>% 
  add_lines(
    data = fatpet_unemp,
    x = ~period,
    y = ~estimate,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>% 
  add_lines(
    data = fatpet_uw_unemp,
    x = ~period,
    y = ~estimate,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>%
  add_lines(
    data = peese_uw_unemp,
    x = ~period,
    y = ~estimate,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_trace(
    data = model_waap_unemp,
    x = ~period,
    y = ~estimate,
    name = "WAAP",
    showlegend = FALSE,
    marker = list(color = "darkgreen", size = 5)
  ) %>%
  add_trace(
    data = model_waap_uw_unemp,
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
    data = ak_unemp,
    x = ~period,
    y = ~estimate,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  )
avg_irf_unemp_corrections
### Save as PDF
orca(avg_irf_unemp_corrections,
     file = "analysis/working_paper_2/figures/average_irfs/avg_irf_unemp_corrections.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot corrected effects with bounds ----
avg_irf_unemp_corrections_with_bounds <- avg_irf_unemp %>% 
  add_ribbons(
    data = peese_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'solid')
  ) %>%
  add_ribbons(
    data = fatpet_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 2, dash = "dot")
  ) %>%
  add_ribbons(
    data = fatpet_uw_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "dot")
  ) %>% 
  add_ribbons(
    data = peese_uw_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 4, dash = "solid")
  ) %>%
  add_ribbons(
    data = model_waap_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>%
  add_ribbons(
    data = model_waap_uw_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "UAAP",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1)
  ) %>% 
  add_ribbons(
    data = ak_unemp,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK",
    showlegend = FALSE,
    line = list(color = "darkgreen", width = 1, dash = 'longdashdot')
  ) %>% 
  layout(
    yaxis = list(
      # range = c(-0.7, 0.15)
    )
  )
avg_irf_unemp_corrections_with_bounds
### Save as PDF
orca(avg_irf_unemp_corrections_with_bounds,
     file = "analysis/working_paper_2/figures/average_irfs/avg_irf_unemp_corrections_with_bounds.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)
### Plot p-bias coefficients ----
unemp_pbias_plot <- plot_ly() %>%
  add_ribbons(
    data = fatpet_unemp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "FAT-PET",
    showlegend = FALSE,
    line = list(width = 3)
  ) %>% 
  add_ribbons(
    data = fatpet_uw_unemp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS",
    showlegend = FALSE,
    line = list(width = 3, dash = "dot")
  ) %>%
  add_ribbons(
    data = peese_unemp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE",
    showlegend = FALSE,
    line = list(width = 3, dash = "solid")
  ) %>%
  add_ribbons(
    data = peese_uw_unemp_pbias,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "OLS with SE<sup>2</sup>",
    showlegend = FALSE,
    line = list(width = 3, dash = "dash")
  )
unemp_pbias_plot
### Save as PDF
orca(unemp_pbias_plot,
     file = "analysis/working_paper_2/figures/average_irfs/unemp_pbias_plot.pdf",
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

## With median, with SE bounds ----
avg_irf_unemp_median_se_bounds <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  ci_method = "avg.se",
  se_multiplier = 2,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_median_se_bounds$data, here::here(save_path, "avg_irf_unemp_median_se_bounds.csv"))
# Change plot title
avg_irf_unemp_median_se_bounds <- avg_irf_unemp_median_se_bounds$plot %>% plotly::layout(
  title = "Mean and median IRF for unemployment"
)
# Comparison plot avg_irf_unemp_median and avg_irf_unemp_median_se_bounds with same ylim and next to each other
figure_avg_irf_unemp_median_se_bounds <- subplot(avg_irf_unemp_median, 
                                                      avg_irf_unemp_median_se_bounds, 
                                                      nrows = 1, 
                                                      margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on unemployment',
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
orca(figure_avg_irf_unemp_median_se_bounds,
     file = "analysis/working_paper_2/figures/average_irfs/figure_avg_irf_unemp_median_se_bounds.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)


# Joint figure of average IRFs for emp and Unemployment ----
figure_average_irfs_emp_unemp <- subplot(avg_irf_emp, 
                                                 avg_irf_unemp, 
                                                 nrows = 1, 
                                                 margin = 0.03) %>% layout(
                                                   showlegend=FALSE,
                                                   title = 'Average effects of conventional monetary policy shocks',
                                                   xaxis3 = list(title = "Month"), # x-axis for plot 3
                                                   xaxis4 = list(title = "Month")  # x-axis for plot 4
                                                 ) %>% layout(annotations = list(
                                                   list(x = 0.25, y = 1, text = "emp", showarrow = FALSE, xref = "paper", yref = "paper",
                                                        xanchor = "center", yanchor = "bottom"),
                                                   list(x = 0.75, y = 1, text = "Unemployment", showarrow = FALSE, xref = "paper", yref = "paper",
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
figure_average_irfs_emp_unemp

# Save as pdf
orca(figure_average_irfs_emp_unemp,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_unemp.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)


# Joint figure of average IRFs for emp, Unemployment with median ----
figure_average_irfs_emp_unemp_with_median <- subplot(avg_irf_emp_median,
                                                                          avg_irf_unemp_median) %>%
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
    list(x = 30, y = y_lims[2], text = "Employment response (%)", showarrow = FALSE, xref = "x", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = y_lims[2], text = "Unemployment response (%)", showarrow = FALSE, xref = "x2", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)
  )
# Display figure
figure_average_irfs_emp_unemp_with_median

# Save as pdf
orca(figure_average_irfs_emp_unemp_with_median,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_unemp_with_median.pdf",
     scale = NULL,
     width = 1034,
     height = 486 * 0.8
)

# Joint figure of average IRFs for emp, Unemployment with different publication bias corrections ----
figure_average_irfs_emp_unemp_corrections <- subplot(avg_irf_emp_corrections,
                                                          avg_irf_unemp_corrections) %>%
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
    list(x = 30, y = y_lims[2], text = "emp response (%)", showarrow = FALSE, xref = "x", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = y_lims[2], text = "Unemployment response (%)", showarrow = FALSE, xref = "x2", yref = "y",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)
  )
# Display figure
figure_average_irfs_emp_unemp_corrections
# Save as pdf
orca(figure_average_irfs_emp_unemp_corrections,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_unemp_corrections.pdf",
     scale = NULL,
     width = 1034,
     height = 486 * 0.8
)

# For sub-samples ----
## For emp ----
out_var <- "emp"
### For top journals vs other publications ----
y_lims <- c(-3.5, 1.1)
#### Top journals
##### Plot
avg_irf_emp_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_top_journals$data, here::here(save_path, "avg_irf_emp_top_journals.csv"))
# Change plot title
avg_irf_emp_top_journals <- avg_irf_emp_top_journals$plot %>% plotly::layout(
  title = ""
)

#### Other publications
##### Plot
avg_irf_emp_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_other_publications$data, here::here(save_path, "avg_irf_emp_other_publications.csv"))
# Change plot title
avg_irf_emp_other_publications <- avg_irf_emp_other_publications$plot %>% plotly::layout(
  title = ""
)

#### Joint figure, top journals right plot, other left
figure_average_irfs_emp_top_journals_other_publications <- subplot(avg_irf_emp_other_publications, 
                                                                      avg_irf_emp_top_journals, 
                                                                      nrows = 1,
                                                                      shareY = TRUE) %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>% 
  layout(annotations = list(
    list(x = 0.25, y = 1, text = "Other publications", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.75, y = 1, text = "Top journals", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  )
# Display figure
figure_average_irfs_emp_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_emp_top_journals_other_publications,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_top_journals_other_publications.pdf",
     scale = NULL,
     width = 1034 * 2/3,
     height = 486 * 0.8
)
#### Sample shares of top journals vs other
# Get proportions by period
top_other_prop <- d_no_qc %>%
  filter(period.month %in% seq(0, 60, by=3)) %>%
  count(period.month, top_5_or_tier) %>%
  group_by(period.month) %>%
  mutate(proportion = n / sum(n))
print(top_other_prop, n = 42)
# Get average proportion for top journals across periods
top_prop <- top_other_prop %>%
  filter(top_5_or_tier == "top journal") %>%
  select(period.month, top_5_or_tier, proportion)
print(top_prop, n = 21)
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
##### Plot
avg_irf_emp_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_cbanker$data, here::here(save_path, "avg_irf_emp_cbanker.csv"))
# Change plot title
avg_irf_emp_cbanker <- avg_irf_emp_cbanker$plot %>% plotly::layout(
  title = ""
)

#### For non-central bank affiliated
##### Corrected IRF PEESE
##### Plot
avg_irf_emp_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_non_cbanker$data, here::here(save_path, "avg_irf_emp_non_cbanker.csv"))
# Change plot title
avg_irf_emp_non_cbanker <- avg_irf_emp_non_cbanker$plot %>% plotly::layout(
  title = ""
)

# Joint figure, cbanker right plot, non-cbanker left
y_lims <- c(-2.5, 1)
figure_average_irfs_emp_cbanker_non_cbanker <- subplot(avg_irf_emp_non_cbanker, 
                                                           avg_irf_emp_cbanker, 
                                                           nrows = 1,
                                                           shareY = TRUE) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5, y = -0.3,
                       font = list(size = titles_size))) %>%
  layout(annotations = list(
    list(x = 0.25, y = 1, text = "Non-central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.75, y = 1, text = "Central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  )
# Display figure
figure_average_irfs_emp_cbanker_non_cbanker
# Save as pdf
orca(figure_average_irfs_emp_cbanker_non_cbanker,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_cbanker_non_cbanker.pdf",
     scale = NULL,
     width = 1034 * 2/3,
     height = 486 * 0.8
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
##### Plot
avg_irf_emp_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_chol$data, here::here(save_path, "avg_irf_emp_chol.csv"))
# Change plot title
avg_irf_emp_chol <- avg_irf_emp_chol$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Plot
avg_irf_emp_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_signr$data, here::here(save_path, "avg_irf_emp_signr.csv"))
# Change plot title
avg_irf_emp_signr <- avg_irf_emp_signr$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Plot
avg_irf_emp_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_hf$data, here::here(save_path, "avg_irf_emp_hf.csv"))
# Change plot title
avg_irf_emp_hf <- avg_irf_emp_hf$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Plot
avg_irf_emp_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_idother$data, here::here(save_path, "avg_irf_emp_idother.csv"))
# Change plot title
avg_irf_emp_idother <- avg_irf_emp_idother$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Plot
avg_irf_emp_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_nr$data, here::here(save_path, "avg_irf_emp_nr.csv"))
# Change plot title
avg_irf_emp_nr <- avg_irf_emp_nr$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-5, 2)
figure_average_irfs_emp_identification_methods <- subplot(avg_irf_emp_chol, 
                                                             avg_irf_emp_signr, 
                                                             avg_irf_emp_hf, 
                                                             avg_irf_emp_nr, 
                                                             avg_irf_emp_idother, 
                                                             nrows = 1,
                                                             shareY = TRUE) %>% layout(
  title = ''
) %>% layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>% 
  layout(annotations = list(
    list(x = 0.1, y = 1, text = "Cholesky & SVAR", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.7, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.9, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
), margin = list(t = 60)) %>%
  layout(
    xaxis3 = list(title = "Month")
  ) %>% layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  )
# Display figure
figure_average_irfs_emp_identification_methods
# Save as pdf
orca(figure_average_irfs_emp_identification_methods,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_identification_methods.pdf",
     scale = NULL,
     width = 1034 * 5/3,
     height = 486 * 1.1
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
##### Plot
avg_irf_emp_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_chol_top_journals$data, here::here(save_path, "avg_irf_emp_chol_top_journals.csv"))
# Change plot title
avg_irf_emp_chol_top_journals <- avg_irf_emp_chol_top_journals$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Plot
avg_irf_emp_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_signr_top_journals$data, here::here(save_path, "avg_irf_emp_signr_top_journals.csv"))
# Change plot title
avg_irf_emp_signr_top_journals <- avg_irf_emp_signr_top_journals$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Plot
avg_irf_emp_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_hf_top_journals$data, here::here(save_path, "avg_irf_emp_hf_top_journals.csv"))
# Change plot title
avg_irf_emp_hf_top_journals <- avg_irf_emp_hf_top_journals$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Plot
avg_irf_emp_idother_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_idother_top_journals$data, here::here(save_path, "avg_irf_emp_idother_top_journals.csv"))
# Change plot title
avg_irf_emp_idother_top_journals <- avg_irf_emp_idother_top_journals$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Plot
avg_irf_emp_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_nr_top_journals$data, here::here(save_path, "avg_irf_emp_nr_top_journals.csv"))
# Change plot title
avg_irf_emp_nr_top_journals <- avg_irf_emp_nr_top_journals$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-10, 3)
figure_average_irfs_emp_identification_methods_top_journals <- subplot(avg_irf_emp_chol_top_journals, 
                                                                          avg_irf_emp_signr_top_journals, 
                                                                          avg_irf_emp_hf_top_journals, 
                                                                          avg_irf_emp_idother_top_journals, 
                                                                          avg_irf_emp_nr_top_journals, 
                                                                          nrows = 1, 
                                                                          margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on employment',
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
  )
# Display figure
figure_average_irfs_emp_identification_methods_top_journals
# Save as pdf
orca(figure_average_irfs_emp_identification_methods_top_journals,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_identification_methods_top_journals.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
### By identifcation method for other publications ----
#### For Cholesky
avg_irf_emp_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_chol_other_publications$data, here::here(save_path, "avg_irf_emp_chol_other_publications.csv"))
# Change plot title
avg_irf_emp_chol_other_publications <- avg_irf_emp_chol_other_publications$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
avg_irf_emp_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_signr_other_publications$data, here::here(save_path, "avg_irf_emp_signr_other_publications.csv"))
# Change plot title
avg_irf_emp_signr_other_publications <- avg_irf_emp_signr_other_publications$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Plot
avg_irf_emp_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_hf_other_publications$data, here::here(save_path, "avg_irf_emp_hf_other_publications.csv"))
# Change plot title
avg_irf_emp_hf_other_publications <- avg_irf_emp_hf_other_publications$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Plot
avg_irf_emp_idother_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_idother_other_publications$data, here::here(save_path, "avg_irf_emp_idother_other_publications.csv"))
# Change plot title
avg_irf_emp_idother_other_publications <- avg_irf_emp_idother_other_publications$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Plot
avg_irf_emp_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_nr_other_publications$data, here::here(save_path, "avg_irf_emp_nr_other_publications.csv"))
# Change plot title
avg_irf_emp_nr_other_publications <- avg_irf_emp_nr_other_publications$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-10, 3)
figure_average_irfs_emp_identification_methods_other_publications <- subplot(avg_irf_emp_chol_other_publications, 
                                                                              avg_irf_emp_signr_other_publications, 
                                                                              avg_irf_emp_hf_other_publications, 
                                                                              avg_irf_emp_idother_other_publications, 
                                                                              avg_irf_emp_nr_other_publications, 
                                                                              nrows = 1, 
                                                                              margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on employment',
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
  )
# Display figure
figure_average_irfs_emp_identification_methods_other_publications
# Save as pdf
orca(figure_average_irfs_emp_identification_methods_other_publications,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_identification_methods_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
# Joint figure top journals vs other publications by identification method, two rows, first other, then top
y_lims <- c(-10, 3)
figure_average_irfs_emp_identification_methods_top_journals_other_publications <- subplot(figure_average_irfs_emp_identification_methods_other_publications, 
                                                                                           figure_average_irfs_emp_identification_methods_top_journals, 
                                                                                           nrows = 2, 
                                                                                           margin = 0.05) %>% layout(
  showlegend = FALSE,
  title = 'Average effects of conventional monetary policy shocks on employment') %>%
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
  )
# Display figure
figure_average_irfs_emp_identification_methods_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_emp_identification_methods_top_journals_other_publications,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_identification_methods_top_journals_other_publications.pdf",
     scale = NULL,
     width = 2000,
     height = 1000
)

### By country/region ----
#### US ----
##### Plot
avg_irf_emp_us <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, us == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_us$data, here::here(save_path, "avg_irf_emp_us.csv"))
# Change plot title
avg_irf_emp_us <- avg_irf_emp_us$plot %>% plotly::layout(
  title = "US"
)

#### EA12 ----
##### Corrected IRF PEESE
##### Plot
avg_irf_emp_ea12 <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, ea12 == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_ea12$data, here::here(save_path, "avg_irf_emp_ea12.csv"))
# Change plot title
avg_irf_emp_ea12 <- avg_irf_emp_ea12$plot %>% plotly::layout(
  title = "EA12"
)

#### Upper middle ----
##### Plot
avg_irf_emp_upper_middle <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, upper_middle_income == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_upper_middle$data, here::here(save_path, "avg_irf_emp_upper_middle.csv"))
# Change plot title
avg_irf_emp_upper_middle <- avg_irf_emp_upper_middle$plot %>% plotly::layout(
  title = "Emerging Economies"
)

#### Other high_income  ----
##### Plot
avg_irf_emp_other_high_income <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, high_income == 1, ea12 != 1, us != 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_emp_other_high_income$data, here::here(save_path, "avg_irf_emp_other_high_income.csv"))
# Change plot title
avg_irf_emp_other_high_income <- avg_irf_emp_other_high_income$plot %>% plotly::layout(
  title = "Other high_income"
)

#### Joint plot 
y_lims <- c(-3, 1.5)
figure_average_irfs_emp_country_region <- subplot(avg_irf_emp_us, 
                                                     avg_irf_emp_ea12, 
                                                     avg_irf_emp_upper_middle, 
                                                     avg_irf_emp_other_high_income, 
                                                     nrows = 1,
                                                     shareY = TRUE) %>% layout(
  title = ''
) %>% 
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>% 
  layout(annotations = list(
          list(x = 30, y = 1, text = "US", showarrow = FALSE, xref = "x", yref = "paper",
               xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
          list(x = 30, y = 1, text = "Euro area", showarrow = FALSE, xref = "x2", yref = "paper",
               xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
          list(x = 30, y = 1, text = "Emerging economies", showarrow = FALSE, xref = "x3", yref = "paper",
               xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
          list(x = 30, y = 1, text = "Other advanced", showarrow = FALSE, xref = "x4", yref = "paper",
               xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
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
)
# Display figure
figure_average_irfs_emp_country_region
# Save as pdf
orca(figure_average_irfs_emp_country_region,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_emp_country_region.pdf",
     scale = NULL,
     width = 1034 * 4/3,
     height = 486 * 0.8
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

## For unemployment ----
out_var <- "unemp"
### For top journals vs other publications ----
#### Top journals
##### Plot
avg_irf_unemp_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_top_journals$data, here::here(save_path, "avg_irf_unemp_top_journals.csv"))
# Change plot title
avg_irf_unemp_top_journals <- avg_irf_unemp_top_journals$plot %>% plotly::layout(
  title = "Top journals"
)

#### Other publications
##### Plot
avg_irf_unemp_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_other_publications$data, here::here(save_path, "avg_irf_unemp_other_publications.csv"))
# Change plot title
avg_irf_unemp_other_publications <- avg_irf_unemp_other_publications$plot %>% plotly::layout(
  title = "Other publications"
)

#### Joint figure, top journals right plot, other left
y_lims <- c(-3, 0.9)
figure_average_irfs_unemp_top_journals_other_publications <- subplot(avg_irf_unemp_other_publications, 
                                                                         avg_irf_unemp_top_journals, 
                                                                         nrows = 1,
                                                                         shareY = TRUE) %>% 
  layout(title = "") %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>% 
  layout(annotations = list(
    list(x = 0.25, y = 1, text = "Other publications", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.75, y = 1, text = "Top journals", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  )
# Display figure
figure_average_irfs_unemp_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_unemp_top_journals_other_publications,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_top_journals_other_publications.pdf",
     scale = NULL,
     width = 1034 * 2/3,
     height = 486 * 0.8
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
##### Plot
avg_irf_unemp_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_cbanker$data, here::here(save_path, "avg_irf_unemp_cbanker.csv"))
# Change plot title
avg_irf_unemp_cbanker <- avg_irf_unemp_cbanker$plot %>% plotly::layout(
  title = "Central bank affiliated"
)

### For non-cbanker
##### Plot
avg_irf_unemp_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_non_cbanker$data, here::here(save_path, "avg_irf_unemp_non_cbanker.csv"))
# Change plot title
avg_irf_unemp_non_cbanker <- avg_irf_unemp_non_cbanker$plot %>% plotly::layout(
  title = "Non-central bank affiliated"
)

# Joint figure, cbanker right plot, non-cbanker left
y_lims <- c(-2, 0.5)
figure_average_irfs_unemp_cbanker_non_cbanker <- subplot(avg_irf_unemp_non_cbanker, 
                                                              avg_irf_unemp_cbanker, 
                                                              nrows = 1,
                                                              shareY = TRUE) %>%
  layout(title = "") %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>%
  layout(annotations = list(
    list(x = 0.25, y = 1, text = "Non-central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.75, y = 1, text = "Central bank affiliated", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  )
# Display figure
figure_average_irfs_unemp_cbanker_non_cbanker
# Save as pdf
orca(figure_average_irfs_unemp_cbanker_non_cbanker,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_cbanker_non_cbanker.pdf",
     scale = NULL,
     width = 1034 * 2/3,
     height = 486 * 0.8
)

### By identifcation method ("chol", "signr", "hf", "idother", "nr") ----
### For all publications
#### For Cholesky
##### Plot
avg_irf_unemp_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_chol$data, here::here(save_path, "avg_irf_unemp_chol.csv"))
# Change plot title
avg_irf_unemp_chol <- avg_irf_unemp_chol$plot %>% plotly::layout(
  title = "Cholesky & SVAR"
)

#### For Sign restrictions
##### Plot
avg_irf_unemp_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_signr$data, here::here(save_path, "avg_irf_unemp_signr.csv"))
# Change plot title
avg_irf_unemp_signr <- avg_irf_unemp_signr$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Plot
avg_irf_unemp_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_hf$data, here::here(save_path, "avg_irf_unemp_hf.csv"))
# Change plot title
avg_irf_unemp_hf <- avg_irf_unemp_hf$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Plot
avg_irf_unemp_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_idother$data, here::here(save_path, "avg_irf_unemp_idother.csv"))
# Change plot title
avg_irf_unemp_idother <- avg_irf_unemp_idother$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Plot
avg_irf_unemp_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_nr$data, here::here(save_path, "avg_irf_unemp_nr.csv"))
# Change plot title
avg_irf_unemp_nr <- avg_irf_unemp_nr$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-2.5, 1)
figure_average_irfs_unemp_identification_methods <- subplot(avg_irf_unemp_chol, 
                                                                 avg_irf_unemp_signr, 
                                                                 avg_irf_unemp_hf, 
                                                                 avg_irf_unemp_nr, 
                                                                 avg_irf_unemp_idother, 
                                                                 nrows = 1, 
                                                                 shareY = TRUE) %>%
  layout(title = "") %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>%
  layout(annotations = list(
    list(x = 0.1, y = 1, text = "Cholesky & SVAR", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.3, y = 1, text = "Sign restrictions", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.5, y = 1, text = "High frequency", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.7, y = 1, text = "Narrative", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 0.9, y = 1, text = "Other", showarrow = FALSE, xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)) %>%
  layout(
    xaxis3 = list(title = "Months"),
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
  )
# Display figure
figure_average_irfs_unemp_identification_methods
# Save as pdf
orca(figure_average_irfs_unemp_identification_methods,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_identification_methods.pdf",
     scale = NULL,
     width = 1034 * 5/3,
     height = 486 * 1.1
)

### By identifcation method for top journals ----
#### For Cholesky
##### Plot
avg_irf_unemp_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_chol_top_journals$data, here::here(save_path, "avg_irf_unemp_chol_top_journals.csv"))
# Change plot title
avg_irf_unemp_chol_top_journals <- avg_irf_unemp_chol_top_journals$plot %>% plotly::layout(
  title = "Cholesky"
)
  
#### For Sign restrictions
##### Plot
avg_irf_unemp_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_signr_top_journals$data, here::here(save_path, "avg_irf_unemp_signr_top_journals.csv"))
# Change plot title
avg_irf_unemp_signr_top_journals <- avg_irf_unemp_signr_top_journals$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Plot
avg_irf_unemp_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_hf_top_journals$data, here::here(save_path, "avg_irf_unemp_hf_top_journals.csv"))
# Change plot title
avg_irf_unemp_hf_top_journals <- avg_irf_unemp_hf_top_journals$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Plot
avg_irf_unemp_idother_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_idother_top_journals$data, here::here(save_path, "avg_irf_unemp_idother_top_journals.csv"))
# Change plot title
avg_irf_unemp_idother_top_journals <- avg_irf_unemp_idother_top_journals$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Plot
avg_irf_unemp_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_nr_top_journals$data, here::here(save_path, "avg_irf_unemp_nr_top_journals.csv"))
# Change plot title
avg_irf_unemp_nr_top_journals <- avg_irf_unemp_nr_top_journals$plot %>% plotly::layout(
  title = "Narrative"
)

# Joint figure, one row
y_lims <- c(-5, 0.75)
figure_average_irfs_unemp_identification_methods_top_journals <- subplot(avg_irf_unemp_chol_top_journals, 
                                                                             avg_irf_unemp_signr_top_journals, 
                                                                             avg_irf_unemp_hf_top_journals, 
                                                                             avg_irf_unemp_idother_top_journals, 
                                                                             avg_irf_unemp_nr_top_journals, 
                                                                             nrows = 1, 
                                                                             margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on unemployment',
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
  )
# Display figure
figure_average_irfs_unemp_identification_methods_top_journals
# Save as pdf
orca(figure_average_irfs_unemp_identification_methods_top_journals,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_identification_methods_top_journals.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
### By identifcation method for other publications ----
#### For Cholesky
##### Plot
avg_irf_unemp_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_chol_other_publications$data, here::here(save_path, "avg_irf_unemp_chol_other_publications.csv"))
# Change plot title
avg_irf_unemp_chol_other_publications <- avg_irf_unemp_chol_other_publications$plot %>% plotly::layout(
  title = "Cholesky"
)

#### For Sign restrictions
##### Plot
avg_irf_unemp_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_signr_other_publications$data, here::here(save_path, "avg_irf_unemp_signr_other_publications.csv"))
# Change plot title
avg_irf_unemp_signr_other_publications <- avg_irf_unemp_signr_other_publications$plot %>% plotly::layout(
  title = "Sign restrictions"
)

#### For High frequency
##### Plot
avg_irf_unemp_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_hf_other_publications$data, here::here(save_path, "avg_irf_unemp_hf_other_publications.csv"))
# Change plot title
avg_irf_unemp_hf_other_publications <- avg_irf_unemp_hf_other_publications$plot %>% plotly::layout(
  title = "High frequency"
)

#### For Other identification methods
##### Plot
avg_irf_unemp_idother_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_idother_other_publications$data, here::here(save_path, "avg_irf_unemp_idother_other_publications.csv"))
# Change plot title
avg_irf_unemp_idother_other_publications <- avg_irf_unemp_idother_other_publications$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
##### Plot
avg_irf_unemp_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_nr_other_publications$data, here::here(save_path, "avg_irf_unemp_nr_other_publications.csv"))
# Change plot title
avg_irf_unemp_nr_other_publications <- avg_irf_unemp_nr_other_publications$plot %>% plotly::layout(
  title = "Narrative"
)  

# Joint figure, one row
y_lims <- c(-3, 0.5)
figure_average_irfs_unemp_identification_methods_other_publications <- subplot(avg_irf_unemp_chol_other_publications, 
                                                                                  avg_irf_unemp_signr_other_publications, 
                                                                                  avg_irf_unemp_hf_other_publications, 
                                                                                  avg_irf_unemp_idother_other_publications, 
                                                                                  avg_irf_unemp_nr_other_publications, 
                                                                                  nrows = 1, 
                                                                                  margin = 0.03) %>% layout(
  showlegend=FALSE,
  title = 'Average effects of conventional monetary policy shocks on unemployment',
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
  )
# Display figure
figure_average_irfs_unemp_identification_methods_other_publications
# Save as pdf
orca(figure_average_irfs_unemp_identification_methods_other_publications,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_identification_methods_other_publications.pdf",
     scale = NULL,
     width = 1500,
     height = NULL
)
# Joint figure top journals vs other publications by identification method, two rows, first other, then top
y_lims <- c(-8, 1)
figure_average_irfs_unemp_identification_methods_top_journals_other_publications <- subplot(figure_average_irfs_unemp_identification_methods_other_publications, 
                                                                                                 figure_average_irfs_unemp_identification_methods_top_journals, 
                                                                                                 nrows = 2, 
                                                                                                 margin = 0.05) %>% layout(
  showlegend = FALSE,
  title = 'Average effects of conventional monetary policy shocks on unemployment') %>%
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
  )
# Display figure
figure_average_irfs_unemp_identification_methods_top_journals_other_publications
# Save as pdf
orca(figure_average_irfs_unemp_identification_methods_top_journals_other_publications,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_identification_methods_top_journals_other_publications.pdf",
     scale = NULL,
     width = 2000,
     height = 1000
)

### By country/region ----
#### US ----
##### Plot
avg_irf_unemp_us <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, us == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_us$data, here::here(save_path, "avg_irf_unemp_us.csv"))
# Change plot title
avg_irf_unemp_us <- avg_irf_unemp_us$plot %>% plotly::layout(
  title = "US"
)

#### EA12 ----
##### Plot
avg_irf_unemp_ea12 <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, ea12 == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_ea12$data, here::here(save_path, "avg_irf_unemp_ea12.csv"))
# Change plot title
avg_irf_unemp_ea12 <- avg_irf_unemp_ea12$plot %>% plotly::layout(
  title = "EA12"
)

#### Upper middle ----
##### Plot
avg_irf_unemp_upper_middle <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, upper_middle_income == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_upper_middle$data, here::here(save_path, "avg_irf_unemp_upper_middle.csv"))
# Change plot title
avg_irf_unemp_upper_middle <- avg_irf_unemp_upper_middle$plot %>% plotly::layout(
  title = "Upper middle income countries"
)

#### Other high_income ----
##### Plot
avg_irf_unemp_other_high_income <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, high_income == 1, ea12 != 1, us != 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_unemp_other_high_income$data, here::here(save_path, "avg_irf_unemp_other_high_income.csv"))
# Change plot title
avg_irf_unemp_other_high_income <- avg_irf_unemp_other_high_income$plot %>% plotly::layout(
  title = "Other high income economies"
)

#### Joint plot 
y_lims <- c(-2, 0.75)
figure_average_irfs_unemp_country_region <- subplot(avg_irf_unemp_us, 
                                                     avg_irf_unemp_ea12, 
                                                     avg_irf_unemp_upper_middle, 
                                                     avg_irf_unemp_other_high_income, 
                                                     nrows = 1,
                                                     shareY = TRUE) %>%
  layout(title = "") %>%
  layout(legend = list(orientation = "h",   
                       xanchor = "center",  
                       x = 0.5, y = -0.3, 
                       font = list(size = titles_size))) %>%
  layout(annotations = list(
    list(x = 30, y = 1, text = "US", showarrow = FALSE, xref = "x", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = 1, text = "Euro area", showarrow = FALSE, xref = "x2", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = 1, text = "Emerging economies", showarrow = FALSE, xref = "x3", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
    list(x = 30, y = 1, text = "Other advanced", showarrow = FALSE, xref = "x4", yref = "paper",
         xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
  ), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    xaxis4 = list(title = "Months")
  ) %>% layout(
    yaxis = list(title = "Effect (%)",
                 range = list(y_lims[1], y_lims[2])
    ),
    hovermode = "compare"
  )
# Display figure
figure_average_irfs_unemp_country_region
# Save as pdf
orca(figure_average_irfs_unemp_country_region,
     file = "analysis/working_paper_2/figures/average_irfs/figure_average_irfs_unemp_country_region.pdf",
     scale = NULL,
     width = 1034 * 5/3,
     height = 486 * 1.1
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


beepr::beep()
