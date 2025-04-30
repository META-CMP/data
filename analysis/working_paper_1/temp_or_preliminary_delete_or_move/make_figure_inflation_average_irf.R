# Plots of average and median IRFs for inflation transformation 

## Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing

## Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

### Source required functions ----
source(here::here("analysis/R/plot_average_irfs.R"))
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/kasy_MetaStudiesFunctions.R"))
source(here::here("analysis/R/kasy_RobustVariance.R"))
source(here::here("analysis/R/kasy_MetaStudiesPlots.R"))

## Transformation of price level IRFs to inflation IRFs (approximation) ----

### Filter just for price level data but keep all time periods ----
d_pricelevel_all <- d_no_qc %>% 
  filter(outcome == "inflation") # The naming in MetaExtractR is misleading, it actually is price level data

# # TEST: Select only models covering exactly 0 to 60 months, not less. If this is activated,
# # the difference with the version doing the transformation after averaging (see end of script) disappears.
# d_pricelevel_all <- d_pricelevel_all %>%
#   group_by(key, model_id) %>%
#   filter(all(c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60) %in% period.month)) %>%
#   ungroup()

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

### Group by key and model_id and transform the data ----
d_inflation <- d_pricelevel_all %>%
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


### View the transformed data ----
View(d_inflation %>% select(key, model_id, period.month, month, quarter, annual, 
                            mean.effect, inflation_effect, 
                            approx.CI.lower_68, ci_lower_inflation_68, 
                            approx.CI.upper_68, ci_upper_inflation_68,
                            approx.CI.lower_90, ci_lower_inflation_90,
                            approx.CI.upper_90, ci_upper_inflation_90,
                            approx.CI.lower_95, ci_lower_inflation_95,
                            approx.CI.upper_95, ci_upper_inflation_95))

### Delete original columns and rename the transformed ones ----
d_inflation <- d_inflation %>%
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

## Creating the plots ----


### Define general folder path to save figures and their data ----
# save_path <- "analysis/working_paper_1/figures/average_irfs/"

### Create plot ----
avg_irf_inflation <- plot_average_irfs(
  d_inflation %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation
# Save data as csv
# write_csv(avg_irf_inflation$data, here::here(save_path, "avg_irf_inflation.csv"))
# Change plot title
avg_irf_inflation <- avg_irf_inflation$plot %>% plotly::layout(
  title = "Mean and median IRF for Inflation"
)
# Save as pdf
# orca(avg_irf_inflation,
#      file = "analysis/working_paper_1/figures/average_irfs/figure_avg_irf_inflation.pdf",
#      scale = NULL,
#      width = 600,
#      height = NULL
# )

## Comparison with transformation after averaging ----

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
avg_irf_inflation %>%
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


# Get unique key model_id combination in the data
length(unique(d_inflation$key))


# Sub-sample inflation IRFs ----

## Original in log-lev specification ----
d_inflation_log_lev <- d_inflation %>% 
  filter(transformation == "log")

avg_irf_inflation_log_lev <- plot_average_irfs(
  d_inflation_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
avg_irf_inflation_log_lev <- avg_irf_inflation_log_lev$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_log_lev

## Original in growth rates or log-diff ---- 
d_inflation_logdiff_or_gr <- d_inflation %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_logdiff_or_gr <- plot_average_irfs(
  d_inflation_logdiff_or_gr %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
avg_irf_inflation_logdiff_or_gr <- avg_irf_inflation_logdiff_or_gr$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_logdiff_or_gr

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr <- 
subplot(
  avg_irf_inflation,
  avg_irf_inflation_log_lev,
  avg_irf_inflation_logdiff_or_gr,
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

# Alternative: period-to-period quarter-to-quarter inflation ----

### Group by key and model_id and transform the data ----
d_inflation_quarter <- d_pricelevel_all %>%
  # Exclude annual
  filter(annual == FALSE) %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(12) to get 12-month differences
    # - Quarterly data: lag(4) to get 4-quarter differences
    # - Annual data: would not work, therefore excluded above
    inflation_effect = case_when(
      month == TRUE ~ mean.effect - lag(mean.effect, 3),
      quarter == TRUE ~ mean.effect - lag(mean.effect, 1),
      TRUE ~ NA_real_ # Fallback for any unexpected cases
    ),
    
    # 68% confidence intervals
    ci_lower_inflation_68 = case_when(
      month == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 3),
      quarter == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_68 = case_when(
      month == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 3),
      quarter == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 1),
      TRUE ~ NA_real_
    ),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = case_when(
      month == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 3),
      quarter == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_90 = case_when(
      month == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 3),
      quarter == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 1),
      TRUE ~ NA_real_
    ),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = case_when(
      month == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 3),
      quarter == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_95 = case_when(
      month == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 3),
      quarter == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 1),
      TRUE ~ NA_real_
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      month == TRUE ~ SE.upper - lag(SE.upper, 3),
      quarter == TRUE ~ SE.upper - lag(SE.upper, 1),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      month == TRUE ~ SE.lower - lag(SE.lower, 3),
      quarter == TRUE ~ SE.lower - lag(SE.lower, 1),
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


### View the transformed data ----
View(d_inflation_quarter %>% select(key, model_id, period.month, month, quarter, annual, 
                            mean.effect, inflation_effect, 
                            approx.CI.lower_68, ci_lower_inflation_68, 
                            approx.CI.upper_68, ci_upper_inflation_68,
                            approx.CI.lower_90, ci_lower_inflation_90,
                            approx.CI.upper_90, ci_upper_inflation_90,
                            approx.CI.lower_95, ci_lower_inflation_95,
                            approx.CI.upper_95, ci_upper_inflation_95))

### Delete original columns and rename the transformed ones ----
d_inflation_quarter <- d_inflation_quarter %>%
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

### Create plot ----
avg_irf_inflation_quarter <- plot_average_irfs(
  d_inflation_quarter %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_quarter <- avg_irf_inflation_quarter$plot %>% 
  # Change title
  layout(title = "ECB transformation (quarter-to-quarter)")
avg_irf_inflation_quarter

### Compare with the year-over-year plot using subplot ----
avg_irf_inflation_year_vs_quarter <- subplot(
  avg_irf_inflation,
  avg_irf_inflation_quarter,
  nrows = 1, 
  shareY = TRUE, 
  titleX = TRUE
) %>%
layout(title = "ECB transformation - year-to-year vs quarter-to-quarter",
       annotations = list(
         list(x = 30, y = 0.4, text = "Year-over-year", showarrow = FALSE, xref = "x", yref = "y",
              xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
         list(x = 30, y = 0.4, text = "Quarter-to-quarter", showarrow = FALSE, xref = "x2", yref = "y",
              xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
       ))
avg_irf_inflation_year_vs_quarter

#### Comparison with price level IRF
avg_irf_pricelevel_median <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE
)
avg_irf_pricelevel_median <- avg_irf_pricelevel_median$plot %>% plotly::layout(
  title = "Average and median IRF for Inflation"
)
avg_irf_pricelevel_median

# Subplot with avg_irf_pricelevel_median in top row, then the avg_irf_inflation$plot in the next row, then the quarterly in the next
avg_irf_pricelevel_median_vs_inflation <- subplot(
  avg_irf_pricelevel_median,
  avg_irf_inflation,
  avg_irf_inflation_quarter,
  nrows = 3, 
  shareY = TRUE, 
  titleX = TRUE
) %>%
  layout(title = "With price level IRF",
         annotations = list(
           list(x = 30, y = 0.4, text = "Price level", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Year-over-year", showarrow = FALSE, xref = "x2", yref = "y2",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Quarter-to-quarter", showarrow = FALSE, xref = "x3", yref = "y3",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irf_pricelevel_median_vs_inflation


# Most correct from my perspective - here only implemented for the SEs ----

### Group by key and model_id and transform the data ----
d_inflation_correctest <- d_pricelevel_all %>%
  # Exclude annual
  filter(annual == FALSE) %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(12) to get 12-month differences
    # - Quarterly data: lag(4) to get 4-quarter differences
    # - Annual data: would not work, therefore excluded above
    inflation_effect = case_when(
      month == TRUE ~ mean.effect - lag(mean.effect, 12),
      quarter == TRUE ~ mean.effect - lag(mean.effect, 4),
      TRUE ~ NA_real_ # Fallback for any unexpected cases
    ),
    
    # 68% confidence intervals
    ci_lower_inflation_68 = case_when(
      month == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 12),
      quarter == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 4),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_68 = case_when(
      month == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 12),
      quarter == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 4),
      TRUE ~ NA_real_
    ),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = case_when(
      month == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 12),
      quarter == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 4),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_90 = case_when(
      month == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 12),
      quarter == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 4),
      TRUE ~ NA_real_
    ),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = case_when(
      month == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 12),
      quarter == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 4),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_95 = case_when(
      month == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 12),
      quarter == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 4),
      TRUE ~ NA_real_
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      month == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 12)^2),
      quarter == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 4)^2),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      month == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 12)^2),
      quarter == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 4)^2),
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


### View the transformed data ----
View(d_inflation_correctest %>% select(key, model_id, period.month, month, quarter, annual, 
                                    mean.effect, inflation_effect,
                                    SE_upper_inflation, SE_lower_inflation, 
                                    approx.CI.lower_68, ci_lower_inflation_68, 
                                    approx.CI.upper_68, ci_upper_inflation_68,
                                    approx.CI.lower_90, ci_lower_inflation_90,
                                    approx.CI.upper_90, ci_upper_inflation_90,
                                    approx.CI.lower_95, ci_lower_inflation_95,
                                    approx.CI.upper_95, ci_upper_inflation_95))

### Delete original columns and rename the transformed ones ----
d_inflation_correctest <- d_inflation_correctest %>%
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

### Create plot ----
avg_irf_inflation_correctest <- plot_average_irfs(
  d_inflation_correctest %>% filter(period.month %in% seq(0,60,by=3), 
                                    outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_correctest <- avg_irf_inflation_correctest$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (quarter-to-quarter)")
avg_irf_inflation_correctest


## Original in log-lev specification ----
d_inflation_log_lev <- d_inflation_correctest %>% 
  filter(transformation == "log")

avg_irf_inflation_log_lev <- plot_average_irfs(
  d_inflation_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_log_lev <- avg_irf_inflation_log_lev$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_log_lev

## Original in growth rates or log-diff ---- 
d_inflation_logdiff_or_gr <- d_inflation_correctest %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_logdiff_or_gr <- plot_average_irfs(
  d_inflation_logdiff_or_gr %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_logdiff_or_gr <- avg_irf_inflation_logdiff_or_gr$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_logdiff_or_gr

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr <- 
  subplot(
    avg_irf_inflation_correctest,
    avg_irf_inflation_log_lev,
    avg_irf_inflation_logdiff_or_gr,
    nrows = 1, 
    shareY = TRUE, 
    titleX = TRUE
  ) %>%
  layout(title = "Transformation using sqrt(SE.lower^2 - lag(SE.lower, 12)^2) for monthly",
         annotations = list(
           list(x = 30, y = 0.4, text = "All", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log-lev", showarrow = FALSE, xref = "x2", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log_diff & gr", showarrow = FALSE, xref = "x3", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irg_inflation_all_log_logdiff_gr

# Alternative: period-to-period quarter-to-quarter inflation ----

### Group by key and model_id and transform the data ----
d_inflation_correctest_quarter <- d_pricelevel_all %>%
  # Exclude annual
  filter(annual == FALSE) %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(3) to get 3-month differences
    # - Quarterly data: lag(1) to get 1-quarter differences
    # - Annual data: would not work, therefore excluded above
    inflation_effect = case_when(
      month == TRUE ~ mean.effect - lag(mean.effect, 3),
      quarter == TRUE ~ mean.effect - lag(mean.effect, 1),
      TRUE ~ NA_real_ # Fallback for any unexpected cases
    ),
    
    # 68% confidence intervals
    ci_lower_inflation_68 = case_when(
      month == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 3),
      quarter == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_68 = case_when(
      month == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 3),
      quarter == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 1),
      TRUE ~ NA_real_
    ),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = case_when(
      month == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 3),
      quarter == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_90 = case_when(
      month == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 3),
      quarter == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 1),
      TRUE ~ NA_real_
    ),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = case_when(
      month == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 3),
      quarter == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_95 = case_when(
      month == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 3),
      quarter == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 1),
      TRUE ~ NA_real_
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      month == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 3)^2),
      quarter == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 1)^2),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      month == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 3)^2),
      quarter == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 1)^2),
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

### View the transformed data ----
View(d_inflation_correctest_quarter %>% select(key, model_id, period.month, month, quarter, annual, 
                                    mean.effect, inflation_effect,
                                    SE_upper_inflation, SE_lower_inflation, 
                                    approx.CI.lower_68, ci_lower_inflation_68, 
                                    approx.CI.upper_68, ci_upper_inflation_68,
                                    approx.CI.lower_90, ci_lower_inflation_90,
                                    approx.CI.upper_90, ci_upper_inflation_90,
                                    approx.CI.lower_95, ci_lower_inflation_95,
                                    approx.CI.upper_95, ci_upper_inflation_95))

### Delete original columns and rename the transformed ones ----
d_inflation_correctest_quarter <- d_inflation_correctest_quarter %>%
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

### Create plot ----
avg_irf_inflation_correctest_quarter <- plot_average_irfs(
  d_inflation_correctest_quarter %>% filter(period.month %in% seq(0,60,by=3), 
                                    outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_correctest_quarter <- avg_irf_inflation_correctest_quarter$plot %>% 
  # Change title
  layout(title = "Transformation using sqrt(SE.lower^2 - lag(SE.lower, 3)) - quarter-to-quarter")
avg_irf_inflation_correctest_quarter

## Original in log-lev specification ----
d_inflation_log_lev <- d_inflation_correctest_quarter %>% 
  filter(transformation == "log")

avg_irf_inflation_log_lev_quarter <- plot_average_irfs(
  d_inflation_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_log_lev_quarter <- avg_irf_inflation_log_lev_quarter$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_log_lev_quarter

## Original in growth rates or log-diff ---- 
d_inflation_logdiff_or_gr_quarter <- d_inflation_correctest_quarter %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_logdiff_or_gr_quarter <- plot_average_irfs(
  d_inflation_logdiff_or_gr_quarter %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_logdiff_or_gr_quarter <- avg_irf_inflation_logdiff_or_gr_quarter$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_logdiff_or_gr_quarter

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr_quarter <- 
  subplot(
    avg_irf_inflation_correctest_quarter,
    avg_irf_inflation_log_lev_quarter,
    avg_irf_inflation_logdiff_or_gr_quarter,
    nrows = 1, 
    shareY = TRUE, 
    titleX = TRUE
  ) %>%
  layout(title = "Transformation using sqrt(SE.lower^2 - lag(SE.lower, 3)^2) for monthly - quarter-to-quarter",
         annotations = list(
           list(x = 30, y = 0.4, text = "All", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log-lev", showarrow = FALSE, xref = "x2", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log_diff & gr", showarrow = FALSE, xref = "x3", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irg_inflation_all_log_logdiff_gr_quarter

# Alternative: month-to-month inflation ----

### Group by key and model_id and transform the data ----
d_inflation_correctest_month <- d_pricelevel_all %>%
  # Exclude annual
  filter(month == TRUE) %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(1) to get 1-month differences
    # - Quarterly data: would not work
    # - Annual data: would not work, therefore excluded above
    inflation_effect = case_when(
      month == TRUE ~ mean.effect - lag(mean.effect, 1),
      TRUE ~ NA_real_ # Fallback for any unexpected cases
    ),
    
    # 68% confidence intervals
    ci_lower_inflation_68 = case_when(
      month == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_68 = case_when(
      month == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 1),
      TRUE ~ NA_real_
    ),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = case_when(
      month == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_90 = case_when(
      month == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 1),
      TRUE ~ NA_real_
    ),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = case_when(
      month == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 1),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_95 = case_when(
      month == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 1),
      TRUE ~ NA_real_
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      month == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 1)^2),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      month == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 1)^2),
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

### View the transformed data ----
View(d_inflation_correctest_month %>% select(key, model_id, period.month, month, quarter, annual, 
                                               mean.effect, inflation_effect,
                                               SE_upper_inflation, SE_lower_inflation, 
                                               approx.CI.lower_68, ci_lower_inflation_68, 
                                               approx.CI.upper_68, ci_upper_inflation_68,
                                               approx.CI.lower_90, ci_lower_inflation_90,
                                               approx.CI.upper_90, ci_upper_inflation_90,
                                               approx.CI.lower_95, ci_lower_inflation_95,
                                               approx.CI.upper_95, ci_upper_inflation_95))

### Delete original columns and rename the transformed ones ----
d_inflation_correctest_month <- d_inflation_correctest_month %>%
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

### Create plot ----
avg_irf_inflation_correctest_month <- plot_average_irfs(
  d_inflation_correctest_month %>% filter(period.month %in% seq(0,60,by=3), 
                                            outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_correctest_month <- avg_irf_inflation_correctest_month$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (quarter-to-quarter)")
avg_irf_inflation_correctest_month

## Original in log-lev specification ----
d_inflation_log_lev <- d_inflation_correctest_month %>% 
  filter(transformation == "log")

avg_irf_inflation_log_lev_month <- plot_average_irfs(
  d_inflation_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_log_lev_month <- avg_irf_inflation_log_lev_month$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_log_lev_month

## Original in growth rates or log-diff ---- 
d_inflation_logdiff_or_gr_month <- d_inflation_correctest_month %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_logdiff_or_gr_month <- plot_average_irfs(
  d_inflation_logdiff_or_gr_month %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_logdiff_or_gr_month <- avg_irf_inflation_logdiff_or_gr_month$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_logdiff_or_gr_month

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr_month <- 
  subplot(
    avg_irf_inflation_correctest_month,
    avg_irf_inflation_log_lev_month,
    avg_irf_inflation_logdiff_or_gr_month,
    nrows = 1, 
    shareY = TRUE, 
    titleX = TRUE
  ) %>%
  layout(title = "Transformation using sqrt(SE.lower^2 - lag(SE.lower, 1)) for monthly only - month-to-month",
         annotations = list(
           list(x = 30, y = 0.4, text = "All", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log-lev", showarrow = FALSE, xref = "x2", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log_diff & gr", showarrow = FALSE, xref = "x3", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irg_inflation_all_log_logdiff_gr_month

# New Alternative: annualized inflation without kink ---- 
#HERE ONLY FOR QUARTERLY

### Group by key and model_id and transform the data ----
d_inflation_correctest_sg_quarter <- d_pricelevel_all %>%
  # Only quarterly for now
  filter(quarter == TRUE) %>%
  # Group by study and model
  group_by(key, model_id) %>%
  # Arrange by period.month within each group
  arrange(key, model_id, period.month) %>%
  # Create the transformation with different lags based on frequency
  mutate(
    # Set appropriate lags based on frequency:
    # - Monthly data: lag(1) to get 1-month differences
    # - Quarterly data: would not work
    # - Annual data: would not work, therefore excluded above
    
    # 68% confidence intervals
    ci_lower_inflation_68 = case_when(
      # month == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 12),
      quarter == TRUE ~ approx.CI.lower_68 - lag(approx.CI.lower_68, 4),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_68 = case_when(
      # month == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 12),
      quarter == TRUE ~ approx.CI.upper_68 - lag(approx.CI.upper_68, 4),
      TRUE ~ NA_real_
    ),
    
    # 90% confidence intervals
    ci_lower_inflation_90 = case_when(
      # month == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 12),
      quarter == TRUE ~ approx.CI.lower_90 - lag(approx.CI.lower_90, 4),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_90 = case_when(
      # month == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 12),
      quarter == TRUE ~ approx.CI.upper_90 - lag(approx.CI.upper_90, 4),
      TRUE ~ NA_real_
    ),
    
    # 95% confidence intervals
    ci_lower_inflation_95 = case_when(
      # month == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 12),
      quarter == TRUE ~ approx.CI.lower_95 - lag(approx.CI.lower_95, 4),
      TRUE ~ NA_real_
    ),
    
    ci_upper_inflation_95 = case_when(
      # month == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 12),
      quarter == TRUE ~ approx.CI.upper_95 - lag(approx.CI.upper_95, 4),
      TRUE ~ NA_real_
    ),
    
    # Standard error bounds
    SE_upper_inflation = case_when(
      # month == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 12)^2),
      quarter == TRUE ~ sqrt(SE.upper^2 - lag(SE.upper, 4)^2),
      TRUE ~ NA_real_
    ),
    
    SE_lower_inflation = case_when(
      # month == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 12)^2),
      quarter == TRUE ~ sqrt(SE.lower^2 - lag(SE.lower, 4)^2),
      TRUE ~ NA_real_
    ),
    
    # For the first periods in each group, use partial annualization of original values
    # inflation_effect = case_when(
    #   period.month == 0 ~ ((1+(log(100 + mean.effect) - log(100)))^(4/4)-1),
    #   period.month == 3 ~ ((1+(log(100 + mean.effect) - log(100)))^(4/1)-1),
    #   period.month == 6 ~ ((1+(log(100 + mean.effect) - log(100)))^(4/2)-1),
    #   period.month == 9 ~ ((1+(log(100 + mean.effect) - log(100)))^(4/3)-1),
    #   period.month == 12 ~ ((1+(log(100 + mean.effect) - log(100)))^(4/4)-1),
    #   quarter == TRUE & period.month > 12 ~ mean.effect - lag(mean.effect, 4)
    # ),
    inflation_effect = case_when(
      period.month == 0 ~ mean.effect*(4/4),
      period.month == 3 ~ mean.effect*(4/1),
      period.month == 6 ~ mean.effect*(4/2),
      period.month == 9 ~ mean.effect*(4/3),
      period.month == 12 ~ mean.effect*(4/4),
      quarter == TRUE & period.month > 12 ~ mean.effect - lag(mean.effect, 4)
    ),
    
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

### View the transformed data ----
View(d_inflation_correctest_sg_quarter %>% select(key, model_id, period.month, month, quarter, annual, 
                                             mean.effect, inflation_effect,
                                             SE_upper_inflation, SE_lower_inflation, 
                                             approx.CI.lower_68, ci_lower_inflation_68, 
                                             approx.CI.upper_68, ci_upper_inflation_68,
                                             approx.CI.lower_90, ci_lower_inflation_90,
                                             approx.CI.upper_90, ci_upper_inflation_90,
                                             approx.CI.lower_95, ci_lower_inflation_95,
                                             approx.CI.upper_95, ci_upper_inflation_95))

### Delete original columns and rename the transformed ones ----
d_inflation_correctest_sg_quarter <- d_inflation_correctest_sg_quarter %>%
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

### Create plot ----
avg_irf_inflation_correctest_sg_quarter <- plot_average_irfs(
  d_inflation_correctest_sg_quarter %>% filter(period.month %in% seq(0,60,by=3), 
                                          outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_correctest_sg_quarter <- avg_irf_inflation_correctest_sg_quarter$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (quarter-to-quarter)")
avg_irf_inflation_correctest_sg_quarter

## Original in log-lev specification ----
d_inflation_log_lev <- d_inflation_correctest_sg_quarter %>% 
  filter(transformation == "log")

avg_irf_inflation_log_lev_sg_quarter <- plot_average_irfs(
  d_inflation_log_lev %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_log_lev_sg_quarter <- avg_irf_inflation_log_lev_sg_quarter$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-level specification)")
avg_irf_inflation_log_lev_sg_quarter

## Original in growth rates or log-diff ---- 
d_inflation_logdiff_or_gr_month <- d_inflation_correctest_sg_quarter %>%
  filter(transformation == "logdiff" | transformation == "gr")

avg_irf_inflation_logdiff_or_gr_sg_quarter <- plot_average_irfs(
  d_inflation_logdiff_or_gr_month %>% filter(period.month %in% seq(0,60,by=3), outcome == "inflation"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = 0, # Already winsorized above
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  ci_method = "avg.se"
)
avg_irf_inflation_logdiff_or_gr_sg_quarter <- avg_irf_inflation_logdiff_or_gr_sg_quarter$plot %>% 
  # Change title
  layout(title = "Mean and median IRF for inflation (original log-diff or growth rate specification)")
avg_irf_inflation_logdiff_or_gr_sg_quarter

## Compare the three plots next to each other using subplot
avg_irg_inflation_all_log_logdiff_gr_month <- 
  subplot(
    avg_irf_inflation_correctest_sg_quarter,
    avg_irf_inflation_log_lev_sg_quarter,
    avg_irf_inflation_logdiff_or_gr_sg_quarter,
    nrows = 1, 
    shareY = TRUE, 
    titleX = TRUE
  ) %>%
  layout(title = "Transformation using sqrt(SE.lower^2 - lag(SE.lower, 4)^2) for quarterly - annualized",
         annotations = list(
           list(x = 30, y = 0.4, text = "All", showarrow = FALSE, xref = "x", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log-lev", showarrow = FALSE, xref = "x2", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size)),
           list(x = 30, y = 0.4, text = "Log_diff & gr", showarrow = FALSE, xref = "x3", yref = "y",
                xanchor = "center", yanchor = "bottom", font = list(size = titles_size))
         ))
avg_irg_inflation_all_log_logdiff_gr_month

d <- d_inflation_correctest_sg_quarter %>% 
  filter(period.month == 3, group_est_broad != "lp_ardl")

hist(d$mean.effect)
