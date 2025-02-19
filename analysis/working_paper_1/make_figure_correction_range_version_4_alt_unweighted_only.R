# Creates range of IRF correction

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

# Sub-folder for figures ----
subfolder <- "version_4_alt_unweighted_only"
# This version:
# Methods:
## FAT-PET unweighted
## PEESE unweighted
## WAAP (1.84, horizon-based model selection) unweighted
# Sampling: no, full sample
# Winsorization: 0, 0.01, 0.02, 0.03, 0.04, 0.05 (only up to 0.03 for interest rate)

# Define additional functions ----

# Function to extract intercepts
extract_intercepts <- function(model, method) {
  if (method == "AK") {
    estimate <- model$tidy[1, 2]
    se <- model$tidy[1, 3]
    ci_lower <- estimate - se * qnorm(1 - (1 - conflevel) / 2)
    ci_upper <- estimate + se * qnorm(1 - (1 - conflevel) / 2)
  } else {
    ci <- confint(model, level = conflevel)
    estimate <- unname(coef(model)[1])
    ci_lower <- unname(ci[1, 1])
    ci_upper <- unname(ci[1, 2])
  }
  return(c(estimate = estimate, lower = ci_lower, upper = ci_upper))
}

# Function to combine results
combine_results <- function(results_list, method_name) {
  lapply(names(results_list), function(wins_name) {
    results <- results_list[[wins_name]][[method_name]]
    intercepts <- lapply(results, extract_intercepts, method = method_name)
    df <- do.call(rbind, intercepts)
    df <- as.data.frame(df)
    df$period <- as.numeric(rownames(df))
    df$wins_para <- wins_name
    df$method <- method_name
    return(df)
  })
}

analyze_fullsample <- function(subsample_id) {
  # Instead of sampling, just use the filtered data directly
  subsample <- filtered_data
  
  results_list <- lapply(wins_para_levels, function(wins) perform_meta_analysis(subsample, wins))
  names(results_list) <- paste0("wins_", wins_para_levels)
  
  # Combine results for all methods
  final_fatpet_uw <- do.call(rbind, combine_results(results_list, "fatpet_uw"))
  final_peese_uw <- do.call(rbind, combine_results(results_list, "peese_uw"))
  final_waap_uw <- do.call(rbind, combine_results(results_list, "waap_uw"))

  # Combine all method results into a single final data frame
  final_df <- rbind(final_fatpet_uw,
                    final_peese_uw,
                    final_waap_uw
  )
  final_df$subsample <- subsample_id
  
  return(final_df)
}


# Set range of winsorization levels for estimations
wins_para_levels <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05)

# Define periods for estimation
chosen_periods <- seq(0, 60, by = 3)

# For output ----
out_var <- "output"

# Function to perform multiple meta-analyses for a given wins level for output
perform_meta_analysis <- function(data, wins, se_opt = "upper", waap_horizon = 12) {
  list(
    fatpet_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt, 
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      prec_weighted = FALSE,
      estimation = "FAT-PET", 
      cluster_se = TRUE),
    peese_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt, 
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      prec_weighted = FALSE,
      estimation = "PEESE", 
      cluster_se = TRUE),
    waap_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt,
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      ap = TRUE,
      ap_horizon = waap_horizon,
      ap_prec_weighted = FALSE,
      ap_parameter = 1.84,
      prec_weighted = FALSE,
      estimation = "UWLS",
      cluster_se = TRUE
    )
    )
}

filtered_data <- d_no_qc %>%
  filter(period.month %in% c(0, chosen_periods), outcome == out_var)

# Initialize a list to store results
all_results <- list()

# Compute results using the functions from above
results <- lapply(1:1, analyze_fullsample)

# Combine results for all subsamples
all_results_df <- do.call(rbind, results)

# Corrections per period
all_results_df %>%
  group_by(period) %>%
  summarise(
    n()
  )

# Use 95% region of estimate per period. 
min_max_per_period_95 <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.025, na.rm = TRUE),
    max_estimate = quantile(estimate,0.975, na.rm = TRUE)
  ) %>% ungroup()

# Use 68% region of estimate per period. 
min_max_per_period_68 <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.16, na.rm = TRUE),
    max_estimate = quantile(estimate,0.84, na.rm = TRUE)
  ) %>% ungroup()

# Mean estimate per period. 
mean_median_per_period <- all_results_df %>%
  group_by(period) %>%
  summarise(
    mean = mean(estimate),
    median = median(estimate)
  ) %>% ungroup()

# Generate the average IRF data and plot
figure_irf_range_correction_output <- plot_average_irfs(
  filtered_data,
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  show_median = TRUE,
  corrected_irf = NULL,
  show_legend = T,
  return_data = TRUE
)

# Join average IRF data with correction data
min_max_per_period_95 <- min_max_per_period_95 %>%
  rename(period.month = period, correction.min_estimate_95 = min_estimate, correction.max_estimate_95 = max_estimate)
min_max_per_period_68 <- min_max_per_period_68 %>%
  rename(period.month = period, correction.min_estimate_68 = min_estimate, correction.max_estimate_68 = max_estimate)
mean_median_per_period <- mean_median_per_period %>%
  rename(period.month = period, correction.mean = mean, correction.median = median)
figure_irf_range_correction_output$data <- figure_irf_range_correction_output$data %>%
  left_join(min_max_per_period_95, by = "period.month") %>%
  left_join(min_max_per_period_68, by = "period.month") %>%
  left_join(mean_median_per_period, by = "period.month")
# Save figure data as csv
write_csv(figure_irf_range_correction_output$data, here::here(paste0("analysis/working_paper_1/figures/irf_range_correction/", subfolder, "/figure_irf_range_correction_output.csv")))

# Add publication bias correction range to plot
figure_irf_range_correction_output <- figure_irf_range_correction_output$plot %>%
  add_ribbons(
    data = min_max_per_period_95,
    x = ~period.month,
    ymin = ~correction.min_estimate_95,
    ymax = ~correction.max_estimate_95,
    name = "P-bias corrections 95% range",
    line = list(color = 'darkblue'),
    fillcolor = 'rgba(0,0,139,0.2)'
  ) %>%
  add_ribbons(
    data = min_max_per_period_68,
    x = ~period.month,
    ymin = ~correction.min_estimate_68,
    ymax = ~correction.max_estimate_68,
    name = "P-bias corrections 68% range",
    line = list(color = 'darkblue'),
    fillcolor = 'rgba(0,0,139,0.3)'
  ) %>% 
  add_lines(
    data = mean_median_per_period,
    x = ~period.month,
    y = ~correction.mean,
    name = "Correction mean",
    line = list(color = 'darkblue',
                dash = 'dash')
  ) %>%
  layout(
    title = "Output response to 100 bp rate shock, average and p-bias corrected range",
    xaxis = list(title = "Month"),
    yaxis = list(title = "Effect in %"),
    hovermode = "compare"
  )
figure_irf_range_correction_output

# Save figure as pdf
orca(figure_irf_range_correction_output,
     file = paste0("analysis/working_paper_1/figures/irf_range_correction/", subfolder, "/figure_irf_range_correction_output.pdf"),
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

# For price level ----
out_var <- "inflation"

# Function to perform multiple meta-analyses for a given wins level for price level 
perform_meta_analysis <- function(data, wins, se_opt = "upper", waap_horizon = 36) {
  list(
    fatpet_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt, 
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      prec_weighted = FALSE,
      estimation = "FAT-PET", 
      cluster_se = TRUE),
    peese_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt, 
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      prec_weighted = FALSE,
      estimation = "PEESE", 
      cluster_se = TRUE),
    waap_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt,
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      ap = TRUE,
      ap_horizon = waap_horizon,
      ap_prec_weighted = FALSE,
      ap_parameter = 1.84,
      prec_weighted = FALSE,
      estimation = "UWLS",
      cluster_se = TRUE
    )
  )
}

## With sampling - this version has unstable results due to drawing samples but much faster estimation. ----
filtered_data <- d_no_qc %>%
  filter(period.month %in% c(0, chosen_periods), outcome == out_var)

# Initialize a list to store results
all_results <- list()

# Compute results using the functions from above
results <- lapply(1:1, analyze_fullsample)

# Combine results for all subsamples
all_results_df <- do.call(rbind, results)

# Corrections per period
all_results_df %>%
  group_by(period) %>%
  summarise(
    n()
  )

# Use 95% region of estimate per period. 
min_max_per_period_95 <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.025, na.rm = TRUE),
    max_estimate = quantile(estimate,0.975, na.rm = TRUE)
  ) %>% ungroup()

# Use 68% region of estimate per period. 
min_max_per_period_68 <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.16, na.rm = TRUE),
    max_estimate = quantile(estimate,0.84, na.rm = TRUE)
  ) %>% ungroup()

# Mean estimate per period. 
mean_median_per_period <- all_results_df %>%
  group_by(period) %>%
  summarise(
    mean = mean(estimate),
    median = median(estimate)
  ) %>% ungroup()

# Generate the average IRF data and plot
figure_irf_range_correction_pricelevel <- plot_average_irfs(
  filtered_data,
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  show_median = TRUE,
  corrected_irf = NULL,
  show_legend = T,
  return_data = TRUE
)

# Join average IRF data with correction data
min_max_per_period_95 <- min_max_per_period_95 %>%
  rename(period.month = period, correction.min_estimate_95 = min_estimate, correction.max_estimate_95 = max_estimate)
min_max_per_period_68 <- min_max_per_period_68 %>%
  rename(period.month = period, correction.min_estimate_68 = min_estimate, correction.max_estimate_68 = max_estimate)
mean_median_per_period <- mean_median_per_period %>%
  rename(period.month = period, correction.mean = mean, correction.median = median)
figure_irf_range_correction_pricelevel$data <- figure_irf_range_correction_pricelevel$data %>%
  left_join(min_max_per_period_95, by = "period.month") %>%
  left_join(min_max_per_period_68, by = "period.month") %>%
  left_join(mean_median_per_period, by = "period.month")
# Save figure data as csv
write_csv(figure_irf_range_correction_pricelevel$data, here::here(paste0("analysis/working_paper_1/figures/irf_range_correction/", subfolder, "/figure_irf_range_correction_pricelevel.csv")))

# Add publication bias correction range to plot
figure_irf_range_correction_pricelevel <- figure_irf_range_correction_pricelevel$plot %>%
  add_ribbons(
    data = min_max_per_period_95,
    x = ~period.month,
    ymin = ~correction.min_estimate_95,
    ymax = ~correction.max_estimate_95,
    name = "P-bias corrections 95% range",
    line = list(color = 'darkblue'),
    fillcolor = 'rgba(0,0,139,0.2)'
  ) %>%
  add_ribbons(
    data = min_max_per_period_68,
    x = ~period.month,
    ymin = ~correction.min_estimate_68,
    ymax = ~correction.max_estimate_68,
    name = "P-bias corrections 68% range",
    line = list(color = 'darkblue'),
    fillcolor = 'rgba(0,0,139,0.3)'
  ) %>% 
  add_lines(
    data = mean_median_per_period,
    x = ~period.month,
    y = ~correction.mean,
    name = "Correction mean",
    line = list(color = 'darkblue',
                dash = 'dash')
  ) %>%
  layout(
    title = "Price level response to 100 bp rate shock, average and p-bias corrected range",
    xaxis = list(title = "Month"),
    yaxis = list(title = "Effect in %"),
    hovermode = "compare"
  )
figure_irf_range_correction_pricelevel

# Save figure as pdf
orca(figure_irf_range_correction_pricelevel,
     file = paste0("analysis/working_paper_1/figures/irf_range_correction/", subfolder, "/figure_irf_range_correction_pricelevel.pdf"),
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

# For interest rate ----
out_var <- "rate"

# Set range of winsorization levels for estimations (higher values fail due to singularity issues)
wins_para_levels <- c(0, 0.01, 0.02, 0.03)

# Function to perform multiple meta-analyses for a given wins level for interest rate
perform_meta_analysis <- function(data, wins, se_opt = "avg", waap_horizon = 12) {
  list(
    fatpet_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt, 
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      prec_weighted = FALSE,
      estimation = "FAT-PET", 
      cluster_se = TRUE),
    peese_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt, 
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      prec_weighted = FALSE,
      estimation = "PEESE", 
      cluster_se = TRUE),
    waap_uw = meta_analysis(
      data = data,
      outvar = out_var,
      se_option = se_opt,
      periods = chosen_periods,
      wins = wins,
      first_period_wins_prec = 0.2,
      first_period_wins_mean = wins,
      ap = TRUE,
      ap_horizon = waap_horizon,
      ap_prec_weighted = FALSE,
      ap_parameter = 1.84,
      prec_weighted = FALSE,
      estimation = "UWLS",
      cluster_se = TRUE
    )
  )
}

filtered_data <- d_no_qc %>%
  filter(period.month %in% c(0, chosen_periods), outcome == out_var)

# Initialize a list to store results
all_results <- list()

# Compute results using the functions from above
results <- lapply(1:1, analyze_fullsample)

# Combine results for all subsamples
all_results_df <- do.call(rbind, results)

# Corrections per period
all_results_df %>%
  group_by(period) %>%
  summarise(
    n()
  )

# Use 95% region of estimate per period. 
min_max_per_period_95 <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.025, na.rm = TRUE),
    max_estimate = quantile(estimate,0.975, na.rm = TRUE)
  ) %>% ungroup()

# Use 68% region of estimate per period. 
min_max_per_period_68 <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.16, na.rm = TRUE),
    max_estimate = quantile(estimate,0.84, na.rm = TRUE)
  ) %>% ungroup()

# Mean estimate per period. 
mean_median_per_period <- all_results_df %>%
  group_by(period) %>%
  summarise(
    mean = mean(estimate),
    median = median(estimate)
  ) %>% ungroup()

# Generate the average IRF data and plot
figure_irf_range_correction_rate <- plot_average_irfs(
  filtered_data,
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  show_median = TRUE,
  corrected_irf = NULL,
  show_legend = T,
  return_data = TRUE
)

# Join average IRF data with correction data
min_max_per_period_95 <- min_max_per_period_95 %>%
  rename(period.month = period, correction.min_estimate_95 = min_estimate, correction.max_estimate_95 = max_estimate)
min_max_per_period_68 <- min_max_per_period_68 %>%
  rename(period.month = period, correction.min_estimate_68 = min_estimate, correction.max_estimate_68 = max_estimate)
mean_median_per_period <- mean_median_per_period %>%
  rename(period.month = period, correction.mean = mean, correction.median = median)
figure_irf_range_correction_rate$data <- figure_irf_range_correction_rate$data %>%
  left_join(min_max_per_period_95, by = "period.month") %>%
  left_join(min_max_per_period_68, by = "period.month") %>%
  left_join(mean_median_per_period, by = "period.month")
# Save figure data as csv
write_csv(figure_irf_range_correction_rate$data, here::here(paste0("analysis/working_paper_1/figures/irf_range_correction/", subfolder, "/figure_irf_range_correction_rate.csv")))

# Add publication bias correction range to plot
figure_irf_range_correction_rate <- figure_irf_range_correction_rate$plot %>%
  add_ribbons(
    data = min_max_per_period_95,
    x = ~period.month,
    ymin = ~correction.min_estimate_95,
    ymax = ~correction.max_estimate_95,
    name = "P-bias corrections 95% range",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(0,0,139,0.2)'
  ) %>%
  add_ribbons(
    data = min_max_per_period_68,
    x = ~period.month,
    ymin = ~correction.min_estimate_68,
    ymax = ~correction.max_estimate_68,
    name = "P-bias corrections 68% range",
    line = list(color = 'darkblue'),
    fillcolor = 'rgba(0,0,139,0.3)'
  ) %>% 
  add_lines(
    data = mean_median_per_period,
    x = ~period.month,
    y = ~correction.mean,
    name = "Correction mean",
    line = list(color = 'darkblue',
                dash = 'dash')
  ) %>%
  layout(
    title = "Interest rate response to 100 bp interest rate shock, average and p-bias corrected range",
    xaxis = list(title = "Month"),
    yaxis = list(title = "Effect in %-points"),
    hovermode = "compare"
  )
figure_irf_range_correction_rate

# Save figure as pdf
orca(figure_irf_range_correction_rate,
     file = paste0("analysis/working_paper_1/figures/irf_range_correction/", subfolder, "/figure_irf_range_correction_rate.pdf"),
     scale = NULL,
     width = 1500 * 0.6,
     height = 1100 * 0.6
)

beepr::beep()
