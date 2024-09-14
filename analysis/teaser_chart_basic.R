rm(list = ls())



# Load necessary libraries
library(dplyr)
library(here)
library(plotly)
library(JWileymisc)

# Load the data 
data_path <- here("data/preliminary_data_test.RData")
load(data_path)
rm(data_path)

# Load required functions
source(here("analysis/R/meta_analysis.R"))
source(here("analysis/R/apply_winsorization.R"))
source(here("analysis/R/plot_average_irfs.R"))
source(here("analysis/R/kasy_MetaStudiesFunctions.R"))
source(here("analysis/R/kasy_RobustVariance.R"))
source(here("analysis/R/kasy_MetaStudiesPlots.R"))

################################################################################



# Set up parameters for PEESE estimation
prd <- seq(3, 60, by = 3)  # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
out <- "output"  # Output variable
wins_para_levels <- c(0,0.01, 0.02, 0.03, 0.04)  # Winsorization levels#,0.05
conf_lev <- 0.89  # Confidence level


# Filter the data
filtered_data <- data %>%
  filter(!quality_concern, period.month %in% seq(0,60, by = 3), outcome ==out)#, us==1

# Function to perform meta-analysis for a given wins_para
perform_meta_analysis <- function(data, wins) {
  list(
    peese = meta_analysis(
      data = data,
      outvar = out,
      se_option = funnel_se_option,
      periods = prd,
      wins = wins,
      ap = FALSE,
      prec_weighted = TRUE,
      estimation = "PEESE",
      cluster_se = TRUE
    ),
    waap = meta_analysis(
      data = data,
      outvar = out,
      se_option = funnel_se_option,
      periods = prd,
      wins = wins,
      ap = TRUE,
      prec_weighted = TRUE,
      estimation = "UWLS",
      cluster_se = TRUE
      ),
      AK = meta_analysis(
        data = data,
        outvar = out,
        se_option = funnel_se_option,
        periods = prd,
        wins = wins,
        prec_weighted = FALSE,
        estimation = "AK",
        cluster_se = TRUE,
        cutoff_val=1,
        AK_modelmu = "t",
        AK_symmetric = FALSE,
        AK_conf_level = 0.68
    )
  )
}

# Function to extract intercepts
extract_intercepts <- function(model, method) {
  if (method == "AK") {
    estimate <- model$tidy[1, 2]
    se <- model$tidy[1, 3]
    ci_lower <- estimate - se * qnorm(1 - (1 - conf_lev) / 2)
    ci_upper <- estimate + se * qnorm(1 - (1 - conf_lev) / 2)
  } else {
    ci <- confint(model, level = conf_lev)
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

analyze_subsample <- function(subsample_id) {#, us_value, group_value
  # Filter data based on 'us' and 'group_ident_broad' values
  subsample <- filtered_data %>%
    #filter(us == us_value, group_ident_broad != group_value) %>%
    group_by(key, period.month) %>%
    sample_n(size = 1, replace = FALSE) %>%
    ungroup()  # Adjust sample size as needed
  
  results_list <- lapply(wins_para_levels, function(wins) perform_meta_analysis(subsample, wins))
  names(results_list) <- paste0("wins_", wins_para_levels)
  
  # Combine results for all methods
  final_peese <- do.call(rbind, combine_results(results_list, "peese"))
  final_waap <- do.call(rbind, combine_results(results_list, "waap"))
  final_ak <- do.call(rbind, combine_results(results_list, "AK"))
  
  # Combine all method results into a single final data frame
  final_df <- rbind(final_peese, final_waap, final_ak)#
  final_df$subsample <- subsample_id
  # final_df$us <- us_value
  # final_df$group_ident_broad <- group_value
  
  return(final_df)
}



# Initialize a list to store results
all_results <- list()


########################## combute results using the functions from above
results <- lapply(1:10, analyze_subsample)
  

# Combine results for all subsamples
all_results_df <- do.call(rbind, results)



#### use 95%  region of estimate per period. 
min_max_per_period <- all_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.025, na.rm = TRUE),
    max_estimate = quantile(estimate,0.975, na.rm = TRUE)
  ) %>% ungroup()



# set winsorizatoin parameter for the average effect
wins_para <- 0.02





############################################################## plot graph

# Generate the average IRF plot with PEESE correction
out_avg_irf_plot <- plot_average_irfs(
  filtered_data %>% filter(outcome==out),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = T
) %>%
  add_ribbons(
    data = min_max_per_period,
    x = ~period,
    ymin = ~min_estimate,
    ymax = ~max_estimate,
    name = "P-bias correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(100,20,200,0.2)'
  ) %>%
  # add_lines(
  #   data = corrected_irf_waap,
  #   x = ~period,
  #   y = ~estimate,
  #   line = list(color = 'rgba(100,20,200,0.2)')
  # ) %>%
  layout(
    title = "Price level response to 100 bp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect in %"),
    hovermode = "compare"
  )

out_avg_irf_plot


