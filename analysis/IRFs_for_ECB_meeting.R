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

################################################################################
# Results for output
# Filter the data
filtered_data <- data %>%
  filter(outcome == "output", !quality_concern, period.month %in% seq(0,60, by = 3))

# Set up parameters for PEESE estimation
prd <- seq(0,60, by = 3) # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
wins_para <- 0.05  # 5 % winsorization

# Perform meta-analysis with PEESE estimation
peese_results <- meta_analysis(
  data = filtered_data,
  outvar = "output",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = FALSE,
  prec_weighted = TRUE,
  estimation = "PEESE",
  cluster_se = TRUE
)

# Perform meta-analysis with WAAP estimation
waap_results <- meta_analysis(
  data = filtered_data,
  outvar = "output",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = TRUE,
  prec_weighted = FALSE,
  estimation = "UWLS",
  cluster_se = FALSE
)

# Extract intercept estimates (corrected IRF)
extract_intercepts <- function(results) {
  intercepts <- lapply(results, function(model) {
    ci <- confint(model, level = 0.89)  # Using 89% confidence interval as in the app
    c(estimate = coef(model)[1],
      lower = ci[1, 1],
      upper = ci[1, 2])
  })
  
  data.frame(
    period = as.numeric(names(results)),
    estimate = sapply(intercepts, function(x) x["estimate"]),
    lower = sapply(intercepts, function(x) x["lower"]),
    upper = sapply(intercepts, function(x) x["upper"])
  )
}

corrected_irf_peese <- extract_intercepts(peese_results)
corrected_irf_waap <- extract_intercepts(waap_results)

# Generate the average IRF plot with PEESE correction
out_avg_irf_plot <- plot_average_irfs(
  filtered_data,
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  add_ribbons(
    data = corrected_irf_peese,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(255,0,0,0.2)'
    ) %>%
  add_ribbons(
    data = corrected_irf_waap,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(100,20,200,0.2)'
  ) %>%
  add_lines(
    data = corrected_irf_waap,
    x = ~period,
    y = ~estimate,
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  layout(
    title = "Output response to 1 pp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect"),
    hovermode = "compare"
  )

out_avg_irf_plot

################################################################################
# Results for inflation
# Filter the data
filtered_data <- data %>%
  filter(outcome == "inflation", !quality_concern, period.month %in% seq(0,60, by = 3))

# Set up parameters for PEESE estimation
prd <- seq(0,60, by = 3) # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
wins_para <- 0.05  # 5 % winsorization

# Perform meta-analysis with PEESE estimation
peese_results <- meta_analysis(
  data = filtered_data,
  outvar = "inflation",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = FALSE,
  prec_weighted = TRUE,
  estimation = "PEESE",
  cluster_se = TRUE
)

# Perform meta-analysis with WAAP estimation
waap_results <- meta_analysis(
  data = filtered_data,
  outvar = "inflation",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = TRUE,
  prec_weighted = FALSE,
  estimation = "UWLS",
  cluster_se = FALSE
)

# Extract intercept estimates (corrected IRF)
extract_intercepts <- function(results) {
  intercepts <- lapply(results, function(model) {
    ci <- confint(model, level = 0.89)  # Using 89% confidence interval as in the app
    c(estimate = coef(model)[1],
      lower = ci[1, 1],
      upper = ci[1, 2])
  })
  
  data.frame(
    period = as.numeric(names(results)),
    estimate = sapply(intercepts, function(x) x["estimate"]),
    lower = sapply(intercepts, function(x) x["lower"]),
    upper = sapply(intercepts, function(x) x["upper"])
  )
}

corrected_irf_peese <- extract_intercepts(peese_results)
corrected_irf_waap <- extract_intercepts(waap_results)

# Generate the average IRF plot with PEESE correction
infl_avg_irf_plot <- plot_average_irfs(
  filtered_data,
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  add_ribbons(
    data = corrected_irf_peese,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(255,0,0,0.2)'
  ) %>%
  add_ribbons(
    data = corrected_irf_waap,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "WAAP correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(100,20,200,0.2)'
  ) %>%
  add_lines(
    data = corrected_irf_waap,
    x = ~period,
    y = ~estimate,
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  layout(
    title = "Inflation response to 1 pp rate shock, average and p-bias corrected IRFs"
  )

infl_avg_irf_plot
