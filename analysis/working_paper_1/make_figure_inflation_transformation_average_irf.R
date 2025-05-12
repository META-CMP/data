# Inflation transformation

## Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing

## Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

### Source required functions ----
source(here::here("analysis/R/plot_average_irfs.R"))

### Save path for figures ----
save_path <- "analysis/working_paper_1/figures/average_irfs/"

## Filter just for price level data ----
d_pricelevel_all <- d_no_qc %>% 
  filter(outcome == "inflation") # The naming in MetaExtractR is misleading, it actually is price level data

## Apply winsorization to the d_pricelevel_all, grouped by period.month ----
d_pricelevel_all <- d_pricelevel_all %>%
  group_by(period.month) %>%
  mutate(
    approx.CI.lower_68 = winsorizor(approx.CI.lower_68, percentile = wins_para),
    approx.CI.upper_68 = winsorizor(approx.CI.upper_68, percentile = wins_para),
    approx.CI.lower_90 = winsorizor(approx.CI.lower_90, percentile = wins_para),
    approx.CI.upper_90 = winsorizor(approx.CI.upper_90, percentile = wins_para),
    approx.CI.lower_95 = winsorizor(approx.CI.lower_95, percentile = wins_para),
    approx.CI.upper_95 = winsorizor(approx.CI.upper_95, percentile = wins_para),
    mean.effect = winsorizor(mean.effect, percentile = wins_para),
    SE.upper = winsorizor(SE.upper, percentile = wins_para),
    SE.lower = winsorizor(SE.lower, percentile = wins_para)
  ) %>%
  ungroup()

## ECB version with simple differences both for effect and bounds and replacement of initial values ----

# # TEST: Select only models covering exactly 0 to 60 months, not less. If this is activated,
# # the difference with the version doing the transformation after averaging (see end of script) disappears.
# d_pricelevel_all <- d_pricelevel_all %>%
#   group_by(key, model_id) %>%
#   filter(all(c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60) %in% period.month)) %>%
#   ungroup()

### Group by key and model_id and transform the data ----
d_inflation_ecb <- d_pricelevel_all %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(12) to get 12-month differences
    # - Quarterly data: lag(4) to get 4-quarter differences
    # - Annual data: lag(1) to get year-over-year differences
    inflation_effect = case_when(
      month == TRUE ~ mean.effect - lag(mean.effect, 12),
      quarter == TRUE ~ mean.effect - lag(mean.effect, 4),
      annual == TRUE ~ mean.effect - lag(mean.effect, 1),
      TRUE ~ NA_real_ # Fallback for any unexpected cases
    ),
    
    # 68% confidence intervals
    ci_lower_inflation_68 = case_when(
      month == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 12),
      quarter == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 4),
      annual == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_68 = case_when(
      month == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 12),
      quarter == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 4),
      annual == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 1),
      TRUE ~ NA_real_
    ),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = case_when(
      month == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 12),
      quarter == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 4),
      annual == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_90 = case_when(
      month == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 12),
      quarter == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 4),
      annual == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 1),
      TRUE ~ NA_real_
    ),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = case_when(
      month == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 12),
      quarter == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 4),
      annual == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_95 = case_when(
      month == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 12),
      quarter == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 4),
      annual == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 1),
      TRUE ~ NA_real_
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      month == TRUE ~ SE.upper - lag(SE.upper, 12),
      quarter == TRUE ~ SE.upper - lag(SE.upper, 4),
      annual == TRUE ~ SE.upper - lag(SE.upper, 1),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      month == TRUE ~ SE.lower - lag(SE.lower, 12),
      quarter == TRUE ~ SE.lower - lag(SE.lower, 4),
      annual == TRUE ~ SE.lower - lag(SE.lower, 1),
      TRUE ~ NA_real_
    ),
    
    # For the first periods in each group, use original values
    inflation_effect = if_else(is.na(inflation_effect), mean.effect, inflation_effect),
    
    # 68% confidence intervals
    ci_lower_inflation_68 = if_else(is.na(ci_lower_inflation_68), approx.CI.lower_68, ci_lower_inflation_68),
    ci_upper_inflation_68 = if_else(is.na(ci_upper_inflation_68), approx.CI.upper_68, ci_upper_inflation_68),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = if_else(is.na(ci_lower_inflation_90), approx.CI.lower_90, ci_lower_inflation_90),
    ci_upper_inflation_90 = if_else(is.na(ci_upper_inflation_90), approx.CI.upper_90, ci_upper_inflation_90),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = if_else(is.na(ci_lower_inflation_95), approx.CI.lower_95, ci_lower_inflation_95),
    ci_upper_inflation_95 = if_else(is.na(ci_upper_inflation_95), approx.CI.upper_95, ci_upper_inflation_95),
    
    # Standard error bounds
    SE_upper_inflation = if_else(is.na(SE_upper_inflation), SE.upper, SE_upper_inflation),
    SE_lower_inflation = if_else(is.na(SE_lower_inflation), SE.lower, SE_lower_inflation)
    
  ) %>%
  ungroup()


### Delete original columns and rename the transformed ones ----
d_inflation_ecb <- d_inflation_ecb %>%
  select(-mean.effect, 
         -approx.CI.lower_68, 
         -approx.CI.upper_68,
         -approx.CI.lower_90,
         -approx.CI.upper_90,
         -approx.CI.lower_95,
         -approx.CI.upper_95,
         -SE.upper, 
         -SE.lower) %>%
  rename(mean.effect = inflation_effect, 
         approx.CI.lower_68 = ci_lower_inflation_68, 
         approx.CI.upper_68 = ci_upper_inflation_68,
         approx.CI.lower_90 = ci_lower_inflation_90,
         approx.CI.upper_90 = ci_upper_inflation_90,
         approx.CI.lower_95 = ci_lower_inflation_95,
         approx.CI.upper_95 = ci_upper_inflation_95,
         SE.upper = SE_upper_inflation,
         SE.lower = SE_lower_inflation)


### Creating the plots ----

### Create plot ----
avg_irf_inflation_ecb <- plot_average_irfs(
  d_inflation_ecb %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE#,
  # ci_method = "avg.se"
)
avg_irf_inflation_ecb

#### Save data as csv ----
write_csv(avg_irf_inflation_ecb$data, here::here(save_path, "avg_irf_inflation_simple_diff.csv"))

# Change plot title
avg_irf_inflation_ecb <- avg_irf_inflation_ecb$plot %>% plotly::layout(
  title = "Mean and median IRF for Inflation"
)

#### Save plot as pdf ----
orca(avg_irf_inflation_ecb,
     file = "analysis/working_paper_1/figures/average_irfs/figure_avg_irf_inflation_simple_diff.pdf",
     scale = NULL,
     width = 600,
     height = NULL
)

### Comparison with transformation after averaging ----

average_irf <- d_pricelevel_all %>%
  filter(period.month %in% seq(0,60,by=3)) %>%
  group_by(period.month) %>%
  summarise(
    # Mean calculations
    avg.effect = mean(mean.effect),
    avg_CI.lower_68 = mean(approx.CI.lower_68),
    avg_CI.upper_68 = mean(approx.CI.upper_68),
    avg_CI.lower_90 = mean(approx.CI.lower_90),
    avg_CI.upper_90 = mean(approx.CI.upper_90),
    avg_CI.lower_95 = mean(approx.CI.lower_95),
    avg_CI.upper_95 = mean(approx.CI.upper_95),
    avg_SE.upper = mean(SE.upper),
    avg_SE.lower = mean(SE.lower),
    # Median calculations
    median.effect = median(mean.effect),
    median_CI.lower_68 = median(approx.CI.lower_68),
    median_CI.upper_68 = median(approx.CI.upper_68),
    median_CI.lower_90 = median(approx.CI.lower_90),
    median_CI.upper_90 = median(approx.CI.upper_90),
    median_CI.lower_95 = median(approx.CI.lower_95),
    median_CI.upper_95 = median(approx.CI.upper_95),
    median_SE.upper = median(SE.upper),
    median_SE.lower = median(SE.lower),
    # Percentile calculations
    percentile_2.5 = quantile(mean.effect, 0.025),
    percentile_16 = quantile(mean.effect, 0.16),
    percentile_84 = quantile(mean.effect, 0.84),
    percentile_97.5 = quantile(mean.effect, 0.975)
  )

mean_trans <- average_irf$avg.effect - lag(average_irf$avg.effect, 4)
mean_trans[1:4] <- average_irf$avg.effect[1:4]
median_trans <- average_irf$median.effect - lag(average_irf$median.effect, 4)
median_trans[1:4] <- average_irf$median.effect[1:4]

# Add transformation after averaging to the plot
avg_irf_inflation_ecb %>%
  plotly::add_trace(
    x = seq(0, 60, by = 3),
    y = c(mean_trans),
    type = "scatter",
    mode = "lines",
    name = "trans_of_mean",
    line = list(color = "green", width = 2, dash = "dash")
  ) %>%
  add_trace(
    x = seq(0, 60, by = 3),
    y = c(median_trans),
    type = "scatter",
    mode = "lines", 
    name = "trans_of_median",
    line = list(color = "black", width = 2, dash = "dash")
  )

# The transformation after averaging is not the same as the averages of the 
# transformed IRFs. This is due to the different IRF horizons of individual models.
# Models with shorter IRF horizons drop out which has different effects in both 
# transformation approaches. Averaging after transformation is more robust to outliers.


### Sub-sample inflation IRFs ----

#### Original in log-lev specification ----
d_inflation_ecb_log_lev <- d_inflation_ecb %>% 
  filter(transformation == "log")

avg_irf_inflation_ecb_log_lev <- plot_average_irfs(
  d_inflation_ecb_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE#,
  # ci_method = "avg.se"
)
avg_irf_inflation_ecb_log_lev <- avg_irf_inflation_ecb_log_lev$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_ecb_log_lev

#### Original in growth rates or log-diff ---- 
d_inflation_ecb_logdiff_or_gr <- d_inflation_ecb %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_ecb_logdiff_or_gr <- plot_average_irfs(
  d_inflation_ecb_logdiff_or_gr %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE#,
  # ci_method = "avg.se"
)
avg_irf_inflation_ecb_logdiff_or_gr <- avg_irf_inflation_ecb_logdiff_or_gr$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_ecb_logdiff_or_gr

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr <- 
  subplot(
    avg_irf_inflation_ecb,
    avg_irf_inflation_ecb_log_lev,
    avg_irf_inflation_ecb_logdiff_or_gr,
    nrows = 1, 
    shareY = TRUE, 
    titleX = TRUE
  ) %>%
  layout(title = "ECB transformation",
         annotations = list(
           list(x = 30, y = 0.4, text = "All", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log-lev", showarrow = FALSE, xref = "x2", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log_diff & gr", showarrow = FALSE, xref = "x3", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irg_inflation_all_log_logdiff_gr



## ECB version with simple differences for effect but more precise transformation for bounds and replacement of initial values ----

### Group by key and model_id and transform the data ----
d_inflation_ecb_correct_bounds <- d_pricelevel_all %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(12) to get 12-month differences
    # - Quarterly data: lag(4) to get 4-quarter differences
    # - Annual data: lag(1) to get year-over-year differences
    inflation_effect = case_when(
      month == TRUE ~ mean.effect - lag(mean.effect, 12),
      quarter == TRUE ~ mean.effect - lag(mean.effect, 4),
      annual == TRUE ~ mean.effect - lag(mean.effect, 1),
      TRUE ~ NA_real_ # Fallback for any unexpected cases
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      month == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 12)^2),
      quarter == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 4)^2),
      annual == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 1)^2),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      month == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 12)^2),
      quarter == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 4)^2),
      annual == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 1)^2),
      TRUE ~ NA_real_
    ),
    
    # For the first periods in each group, use original values
    inflation_effect = if_else(is.na(inflation_effect), mean.effect, inflation_effect),
    
    # Standard error bounds
    SE_upper_inflation = if_else(is.na(SE_upper_inflation), SE.upper, SE_upper_inflation),
    SE_lower_inflation = if_else(is.na(SE_lower_inflation), SE.lower, SE_lower_inflation)
    
  ) %>%
  ungroup()


### Delete original columns and rename the transformed ones ----
d_inflation_ecb_correct_bounds <- d_inflation_ecb_correct_bounds %>%
  select(-mean.effect,
         -SE.upper, 
         -SE.lower) %>%
  rename(mean.effect = inflation_effect,
         SE.upper = SE_upper_inflation,
         SE.lower = SE_lower_inflation)

### Recalculate approximate 68, 90 and 95 % confidence bounds (assuming normality) ----
crit_val_68 <- qnorm(0.84)  # crit_val for 68% confidence interval
crit_val_90 <- qnorm(0.95)  # crit_val for 90% confidence interval
crit_val_95 <- qnorm(0.975)  # crit_val for 95% confidence interval
d_inflation_ecb_correct_bounds <- d_inflation_ecb_correct_bounds %>%
  mutate(
    approx.CI.lower_68 = mean.effect - crit_val_68 * SE.lower,
    approx.CI.upper_68 = mean.effect + crit_val_68 * SE.upper,
    approx.CI.lower_90 = mean.effect - crit_val_90 * SE.lower,
    approx.CI.upper_90 = mean.effect + crit_val_90 * SE.upper,
    approx.CI.lower_95 = mean.effect - crit_val_95 * SE.lower,
    approx.CI.upper_95 = mean.effect + crit_val_95 * SE.upper
  )

### Creating the plots ----

### Create plot ----
avg_irf_inflation_ecb_correct_bounds <- plot_average_irfs(
  d_inflation_ecb_correct_bounds %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE#,
  # ci_method = "avg.se"
)
avg_irf_inflation_ecb_correct_bounds

#### Save data as csv ----
write_csv(avg_irf_inflation_ecb_correct_bounds$data, here::here(save_path, "avg_irf_inflation_diff_correct_bounds.csv"))

# Change plot title
avg_irf_inflation_ecb_correct_bounds <- avg_irf_inflation_ecb_correct_bounds$plot %>% plotly::layout(
  title = "Mean and median IRF for Inflation"
)

#### Save plot as pdf ----
orca(avg_irf_inflation_ecb_correct_bounds,
     file = "analysis/working_paper_1/figures/average_irfs/figure_avg_irf_inflation_diff_correct_bounds.pdf",
     scale = NULL,
     width = 600,
     height = NULL
)

### Comparison with transformation after averaging ----

average_irf <- d_pricelevel_all %>%
  filter(period.month %in% seq(0,60,by=3)) %>%
  group_by(period.month) %>%
  summarise(
    # Mean calculations
    avg.effect = mean(mean.effect),
    avg_CI.lower_68 = mean(approx.CI.lower_68),
    avg_CI.upper_68 = mean(approx.CI.upper_68),
    avg_CI.lower_90 = mean(approx.CI.lower_90),
    avg_CI.upper_90 = mean(approx.CI.upper_90),
    avg_CI.lower_95 = mean(approx.CI.lower_95),
    avg_CI.upper_95 = mean(approx.CI.upper_95),
    avg_SE.upper = mean(SE.upper),
    avg_SE.lower = mean(SE.lower),
    # Median calculations
    median.effect = median(mean.effect),
    median_CI.lower_68 = median(approx.CI.lower_68),
    median_CI.upper_68 = median(approx.CI.upper_68),
    median_CI.lower_90 = median(approx.CI.lower_90),
    median_CI.upper_90 = median(approx.CI.upper_90),
    median_CI.lower_95 = median(approx.CI.lower_95),
    median_CI.upper_95 = median(approx.CI.upper_95),
    median_SE.upper = median(SE.upper),
    median_SE.lower = median(SE.lower),
    # Percentile calculations
    percentile_2.5 = quantile(mean.effect, 0.025),
    percentile_16 = quantile(mean.effect, 0.16),
    percentile_84 = quantile(mean.effect, 0.84),
    percentile_97.5 = quantile(mean.effect, 0.975)
  )

mean_trans <- average_irf$avg.effect - lag(average_irf$avg.effect, 4)
mean_trans[1:4] <- average_irf$avg.effect[1:4]
median_trans <- average_irf$median.effect - lag(average_irf$median.effect, 4)
median_trans[1:4] <- average_irf$median.effect[1:4]

# Add transformation after averaging to the plot
avg_irf_inflation_ecb_correct_bounds %>%
  plotly::add_trace(
    x = seq(0, 60, by = 3),
    y = c(mean_trans),
    type = "scatter",
    mode = "lines",
    name = "trans_of_mean",
    line = list(color = "green", width = 2, dash = "dash")
  ) %>%
  add_trace(
    x = seq(0, 60, by = 3),
    y = c(median_trans),
    type = "scatter",
    mode = "lines", 
    name = "trans_of_median",
    line = list(color = "black", width = 2, dash = "dash")
  )

# The transformation after averaging is not the same as the averages of the 
# transformed IRFs. This is due to the different IRF horizons of individual models.
# Models with shorter IRF horizons drop out which has different effects in both 
# transformation approaches. Averaging after transformation is more robust to outliers.


### Sub-sample inflation IRFs ----

#### Original in log-lev specification ----
d_inflation_ecb_correct_bounds_log_lev <- d_inflation_ecb_correct_bounds %>% 
  filter(transformation == "log")

avg_irf_inflation_ecb_correct_bounds_log_lev <- plot_average_irfs(
  d_inflation_ecb_correct_bounds_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE#,
  # ci_method = "avg.se"
)
avg_irf_inflation_ecb_correct_bounds_log_lev <- avg_irf_inflation_ecb_correct_bounds_log_lev$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_ecb_correct_bounds_log_lev

#### Original in growth rates or log-diff ---- 
d_inflation_ecb_correct_bounds_logdiff_or_gr <- d_inflation_ecb_correct_bounds %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_ecb_correct_bounds_logdiff_or_gr <- plot_average_irfs(
  d_inflation_ecb_correct_bounds_logdiff_or_gr %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE#,
  # ci_method = "avg.se"
)
avg_irf_inflation_ecb_correct_bounds_logdiff_or_gr <- avg_irf_inflation_ecb_correct_bounds_logdiff_or_gr$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_ecb_correct_bounds_logdiff_or_gr

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr <- 
  subplot(
    avg_irf_inflation_ecb_correct_bounds,
    avg_irf_inflation_ecb_correct_bounds_log_lev,
    avg_irf_inflation_ecb_correct_bounds_logdiff_or_gr,
    nrows = 1, 
    shareY = TRUE, 
    titleX = TRUE
  ) %>%
  layout(title = "ECB transformation",
         annotations = list(
           list(x = 30, y = 0.4, text = "All", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log-lev", showarrow = FALSE, xref = "x2", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log_diff & gr", showarrow = FALSE, xref = "x3", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irg_inflation_all_log_logdiff_gr










