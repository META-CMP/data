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
# Results for output
# Filter the data
filtered_data <- data %>%
  filter(!quality_concern, period.month %in% seq(0,60, by = 3))



# Set up parameters for PEESE estimation
prd <- seq(3, 60, by = 3)  # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
out <- "output"  # Output variable
wins_para_levels <- c(0,0.01, 0.02, 0.03, 0.04,0.05)  # Winsorization levels #,0.05
conf_lev <- 0.89  # Confidence level


# Filter the data
filtered_data <- data %>%
  filter(!quality_concern, period.month %in% seq(0,60, by = 3), outcome ==out)

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
      prec_weighted = FALSE,
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

analyze_subsample <- function(subsample_id, us_value, group_value) {
  # Filter data based on 'us' and 'group_ident_broad' values
  subsample <- filtered_data %>%
    filter(us == us_value, group_ident_broad != group_value) %>%
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
  final_df$us <- us_value
  final_df$group_ident_broad <- group_value
  
  return(final_df)
}

# Create a data frame with all combinations of 'us' and 'group_ident_broad'
combinations <- expand.grid(us_value = c(0, 1), group_value = unique(filtered_data$group_ident_broad))


# Initialize a list to store results
all_results <- list()

# Loop over each combination
for (i in 1:nrow(combinations)) {
  us_value <- combinations$us_value[i]
  group_value <- combinations$group_value[i]
  
  # Perform analysis on 10 subsamples for the current combination of 'us' and 'group_ident_broad'
  results <- lapply(1:10, analyze_subsample, us_value = us_value, group_value = group_value)
  
  # Combine results for all subsamples
  all_results_df <- do.call(rbind, results)
  
  # Add the results to the list
  all_results[[paste(us_value, group_value, sep = "_")]] <- all_results_df
}

# Combine all results into a single data frame if needed
combined_results_df <- do.call(rbind, all_results)


min_max_per_period_country <- combined_results_df %>%
  group_by(period,us) %>%
  summarise(
    min_estimate = quantile(estimate,0.025, na.rm = TRUE),
    max_estimate = quantile(estimate,0.975, na.rm = TRUE)
  ) %>% ungroup()

min_max_per_period <- combined_results_df %>%
  group_by(period) %>%
  summarise(
    min_estimate = quantile(estimate,0.025, na.rm = TRUE),
    max_estimate = quantile(estimate,0.975, na.rm = TRUE)
  ) %>% ungroup()


######################################################################### best practice response #######################################################################

data$transformation<-as.factor(data$transformation)
data <- within(data, transformation <- relevel(transformation, ref = 3))

# as factor transformed
data$transformed<-as.factor(data$transformed)


# Explanatory variables for each formula (excluding "standarderror_winsor")
equation <- list("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q", "transformed", "cum", "lrir", "fx", "foreignir", "inflexp", "eglob", "find", "outpgap", "comprice","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes","convent")

###Output

chosen_periods<- seq(3,60, by = 3)


model <- meta_analysis(data, outvar = "output", se_option = "avg", periods = chosen_periods,
                       wins = 0.02, prec_weighted = TRUE, estimation = "FAT-PET", cluster_se = TRUE, mods = equation)

modelsummary::modelsummary(model, output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("FAT-PET output"), gof_map = NULL)

best_pract<-c(1,0,1,0,0,0,1,0,0,1,0,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,1)


results <- list()

model[[20]]<-NULL

# Loop through each model in the list
for (i in seq_along(model)) {
  # Extract coefficients as a matrix
  betahat <- as.matrix(coef(model[[i]]))
  
  # Perform matrix multiplication
  result <- best_pract %*% betahat
  
  # Store the result in the list
  results[[i]] <- result
}

# Print or return the results as needed
print(results)


df_results <- do.call(rbind, lapply(results, as.data.frame))
df_results$period<-seq(3,57, by = 3)

################################################################################ create graph #####################################################################################


wins_para <- 0.02

# Generate the average IRF plot with PEESE correction
out_avg_irf_plot <- plot_average_irfs(
  filtered_data %>% filter(outcome==out),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  add_ribbons(
    data = min_max_per_period,
    x = ~period,
    ymin = ~min_estimate,
    ymax = ~max_estimate,
    name = "Publication bias correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(100,20,200,0.2)'
  ) %>%
  add_lines(
    data = df_results,
    x = ~period,
    y = ~V1,
    name = "Best practice",
    line = list(color = 'rgba(100,100,100,1)')
  ) %>%
  layout(
    title = "Output response to 1 pp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect"),
    hovermode = "compare"
  )

out_avg_irf_plot



#US plot
out_avg_irf_plot_us <- plot_average_irfs(
  filtered_data %>% filter(outcome==out & us==1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  add_ribbons(
    data = min_max_per_period_country %>% filter(us==1),
    x = ~period,
    ymin = ~min_estimate,
    ymax = ~max_estimate,
    name = "Publication bias correction",
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
    title = "Rate response to 1 pp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect"),
    hovermode = "compare"
  )

out_avg_irf_plot_us


out_avg_irf_plot_non_us <- plot_average_irfs(
  filtered_data %>% filter(outcome==out & us==0),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  add_ribbons(
    data = min_max_per_period_country %>% filter(us==0),
    x = ~period,
    ymin = ~min_estimate,
    ymax = ~max_estimate,
    name = "Publication bias correction",
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
    title = "Rate response to 1 pp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect"),
    hovermode = "compare"
  )

out_avg_irf_plot_non_us




# Generate the average IRF plot with PEESE correction
out_avg_irf_plot <- plot_average_irfs(
  filtered_data %>% filter(outcome==out),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  # add_ribbons(
  #   data = min_max_per_period,
  #   x = ~period,
  #   ymin = ~min_estimate,
  #   ymax = ~max_estimate,
  #   name = "Publication bias correction",
  #   line = list(color = 'rgba(0,0,0,0)'),
  #   fillcolor = 'rgba(100,20,200,0.2)'
  # ) %>%
  # add_lines(
  #   data = df_results,
  #   x = ~period,
  #   y = ~V1,
  #   name = "Best practice",
  #   line = list(color = 'rgba(100,100,100,1)')
  # ) %>%
  layout(
    title = "Output response to 100 bp rate shock, average IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect in %"),
    hovermode = "compare"
  )

out_avg_irf_plot



#data$group_ident_broad
### old code to be added to meta_analysis code ####
# save effect after publication bias correction
# Set column name of the df to current horzion
# new_col_name <- paste0("Horizon_", x)
# 
# coef_df[[new_col_name]]<-coef(reg_result)[1]
# 
# results_list<-list(results_list,coef_df)
# 
# 
# # Create df list for coefficients
# object<-c("coefficient")
# coef_df<-data.frame(object)