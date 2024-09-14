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
prd <- seq(3,60, by = 3) # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
wins_para <- 0.03 # 3 % winsorization
outv<-"output"
conf_lev <- 0.95  # Confidence level

# Perform meta-analysis with PEESE estimation
peese_results <- meta_analysis(
  data = filtered_data,
  outvar = outv,
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
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = TRUE,
  prec_weighted = TRUE,
  estimation = "UWLS",
  cluster_se = TRUE
)

# Perform meta-analysis with WAAP estimation
ak_results <- meta_analysis(
  data = filtered_data,
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  prec_weighted = FALSE,
  estimation = "AK",
  cluster_se = TRUE,
  cutoff_val=1,
  AK_modelmu = "t",
  AK_symmetric = FALSE,
  AK_conf_level = 0.68
)



# Extract intercept estimates (corrected IRF)
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

corrected_irf_peese<-as.data.frame(do.call(rbind,lapply(peese_results, extract_intercepts, method = "PESEE")))
corrected_irf_waap<-as.data.frame(do.call(rbind,lapply(waap_results, extract_intercepts, method = "WAAP")))
corrected_irf_ak <- as.data.frame(do.call(rbind,lapply(ak_results, extract_intercepts, method = "AK")))

corrected_irf_peese$period<-as.numeric(rownames(corrected_irf_peese))
corrected_irf_waap$period<-as.numeric(rownames(corrected_irf_waap))
corrected_irf_ak$period<-as.numeric(rownames(corrected_irf_ak))

# Generate the average IRF plot with PEESE correction
out_avg_irf_plot <- plot_average_irfs(
  filtered_data %>% filter(outcome==outv & period.month %in% seq(0,60, by = 3)),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = T
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
  add_ribbons(
    data = corrected_irf_ak,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(118, 37, 166, 0.8)'
  ) %>%
  add_lines(
    data = corrected_irf_peese,
    x = ~period,
    y = ~estimate,
    name = "PEESE correction Mean",
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  add_lines(
    data = corrected_irf_waap,
    x = ~period,
    y = ~estimate,
    name = "WAAP correction Mean",
    line = list(color = 'rgba(125, 103, 138, 0.89)')
  ) %>%
  add_lines(
    data = corrected_irf_ak,
    x = ~period,
    y = ~estimate,
    name = "AK correction Mean",
    line = list(color = 'rgba(55, 13, 79, 0.89)')
  ) %>%
  layout(
    title = "Output response to 100 bp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect in %"),
    hovermode = "compare"
  )

out_avg_irf_plot


################################################################################
# Results for inflation
# Filter the data
filtered_data <- data %>%
  filter(!quality_concern, period.month %in% seq(0,60, by = 3))

# Set up parameters for PEESE estimation
prd <- seq(3,60, by = 3) # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
wins_para <- 0.02  # 5 % winsorization
outv<-"inflation"
conf_lev <- 0.95  # Confidence level

# Perform meta-analysis with PEESE estimation
peese_results <- meta_analysis(
  data = filtered_data,
  outvar = outv,
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
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = TRUE,
  prec_weighted = TRUE,
  estimation = "UWLS",
  cluster_se = TRUE
)

# Perform meta-analysis with WAAP estimation
ak_results <- meta_analysis(
  data = filtered_data,
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  prec_weighted = FALSE,
  estimation = "AK",
  cluster_se = TRUE,
  cutoff_val=1,
  AK_modelmu = "t",
  AK_symmetric = FALSE,
  AK_conf_level = 0.68
)



# Extract intercept estimates (corrected IRF)
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

corrected_irf_peese<-as.data.frame(do.call(rbind,lapply(peese_results, extract_intercepts, method = "PESEE")))
corrected_irf_waap<-as.data.frame(do.call(rbind,lapply(waap_results, extract_intercepts, method = "WAAP")))
corrected_irf_ak <- as.data.frame(do.call(rbind,lapply(ak_results, extract_intercepts, method = "AK")))

corrected_irf_peese$period<-as.numeric(rownames(corrected_irf_peese))
corrected_irf_waap$period<-as.numeric(rownames(corrected_irf_waap))
corrected_irf_ak$period<-as.numeric(rownames(corrected_irf_ak))

# Generate the average IRF plot with PEESE correction
infl_avg_irf_plot <- plot_average_irfs(
  filtered_data %>% filter(outcome==outv & period.month %in% seq(0,60, by = 3)),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = T
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
  add_ribbons(
    data = corrected_irf_ak,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "AK correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(118, 37, 166, 0.8)'
  ) %>%
  add_lines(
    data = corrected_irf_peese,
    x = ~period,
    y = ~estimate,
    name = "PEESE correction Mean",
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  add_lines(
    data = corrected_irf_waap,
    x = ~period,
    y = ~estimate,
    name = "WAAP correction Mean",
    line = list(color = 'rgba(125, 103, 138, 0.89)')
  ) %>%
  add_lines(
    data = corrected_irf_ak,
    x = ~period,
    y = ~estimate,
    name = "AK correction Mean",
    line = list(color = 'rgba(55, 13, 79, 0.89)')
  ) %>%
  layout(
    title = "Price level response to 100 bp rate shock, average and p-bias corrected IRFs",
    xaxis = list(title = "Period (Months)"),
    yaxis = list(title = "Effect in %"),
    hovermode = "compare"
  )

infl_avg_irf_plot
############################################################ country charts ###########################################################################################

filtered_data <- data %>%
  filter(!quality_concern, period.month %in% seq(0,60, by = 3))


# Extract intercept estimates (corrected IRF)
extract_intercepts <- function(results) {
  intercepts <- lapply(results, function(model) {
    ci <- confint(model, level = 0.89)  # Using 89% confidence interval as in the app
    c(estimate = unname(coef(model)[1]),
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


# Set up parameters for PEESE estimation
prd <- seq(3,60, by = 3) # Periods to look at
funnel_se_option <- "avg"  # Using average standard error
wins_para <- 0.02  # 5 % winsorization
outv<-"inflation"

# Perform meta-analysis with PEESE estimation for subsamples
peese_results_us <- meta_analysis(
  data = filtered_data %>% filter(us==1),
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = FALSE,
  prec_weighted = TRUE,
  estimation = "PEESE",
  cluster_se = TRUE
)

peese_results_ea12 <- meta_analysis(
  data = filtered_data %>% filter(ea12==1),
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = FALSE,
  prec_weighted = TRUE,
  estimation = "PEESE",
  cluster_se = TRUE
)


peese_results_em <- meta_analysis(
  data = filtered_data %>% filter(upper_middle==1),
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = FALSE,
  prec_weighted = TRUE,
  estimation = "PEESE",
  cluster_se = TRUE
)


peese_results_otheradv <- meta_analysis(
  data = filtered_data %>% filter(advanced==1 & ea12!=1 &us!=1),
  outvar = outv,
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = FALSE,
  prec_weighted = TRUE,
  estimation = "PEESE",
  cluster_se = TRUE
)


# extract effects for subsamples
corrected_irf_peese_us <- extract_intercepts(peese_results_us)
corrected_irf_peese_ea12 <- extract_intercepts(peese_results_ea12)
corrected_irf_peese_em <- extract_intercepts(peese_results_em)
corrected_irf_peese_otheradv <- extract_intercepts(peese_results_otheradv)
#corrected_irf_waap <- extract_intercepts(waap_results)

# Generate the average IRF sub-plots with PEESE correction for subsamples
out_avg_irf_plot_us <- plot_average_irfs(
  filtered_data %>% filter(us==1 & outcome==outv & period.month  %in% seq(0,60, by = 3)),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL
) %>%
  add_ribbons(
    data = corrected_irf_peese_us,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(255,0,0,0.2)'
  ) %>%
  add_lines(
    data = corrected_irf_peese_us,
    x = ~period,
    y = ~estimate,
    name = "PEESE correction Mean",
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  layout(
    #title = "Output response to 100 bp rate shock, average and p-bias corrected IRFs",
    #xaxis = list(title = "Period (Months)"),
    yaxis = list(#title = "Effect in p.p.",
                  range=c(-2.5,1)),
    hovermode = "compare"
  )

out_avg_irf_plot_ea12 <- plot_average_irfs(
  filtered_data %>% filter(ea12==1 & outcome==outv & period.month  %in% seq(0,60, by = 3)),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = F
) %>%
  add_ribbons(
    data = corrected_irf_peese_ea12,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(255,0,0,0.2)'
  ) %>%
  add_lines(
    data = corrected_irf_peese_ea12,
    x = ~period,
    y = ~estimate,
    name = "PEESE correction Mean",
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  layout(
    #title = "Output response to 100 bp rate shock, average and p-bias corrected IRFs",
    #xaxis = list(title = "Period (Months)"),
    yaxis = list(#title = "Effect in p.p.",
                 range=c(-2.5,1)),
    hovermode = "compare"
  )

out_avg_irf_plot_em <- plot_average_irfs(
  filtered_data %>% filter(upper_middle==1 & outcome==outv & period.month  %in% seq(0,60, by = 3)),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = F
) %>%
  add_ribbons(
    data = corrected_irf_peese_em,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(255,0,0,0.2)'
  ) %>%
  add_lines(
    data = corrected_irf_peese_em,
    x = ~period,
    y = ~estimate,
    name = "PEESE correction Mean",
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  layout(
    #title = "Output response to 100 bp rate shock, average and p-bias corrected IRFs",
    #xaxis = list(title = "Period (Months)"),
    yaxis = list(#title = "Effect in p.p.",
                  range=c(-2.5,1)),
    hovermode = "compare"
  )

out_avg_irf_plot_otheradv <- plot_average_irfs(
  filtered_data %>% filter(advanced==1 & ea12!=1 &us!=1 & outcome==outv & period.month  %in% seq(0,60, by = 3)),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = F
) %>%
  add_ribbons(
    data = corrected_irf_peese_otheradv,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    name = "PEESE correction",
    line = list(color = 'rgba(0,0,0,0)'),
    fillcolor = 'rgba(255,0,0,0.2)'
  ) %>%
  add_lines(
    data = corrected_irf_peese_otheradv,
    x = ~period,
    y = ~estimate,
    name = "PEESE correction Mean",
    line = list(color = 'rgba(100,20,200,0.2)')
  ) %>%
  layout(
    #title = "Output response to 100 bp rate shock, average and p-bias corrected IRFs",
    #xaxis = list(title = "Period (Months)"),
    yaxis = list(#title = "Effect in p.p.",
                 range=c(-2.5,1)),
    hovermode = "compare"
  )


#### Create overall plot
subplot(out_avg_irf_plot_us, out_avg_irf_plot_ea12, out_avg_irf_plot_em, out_avg_irf_plot_otheradv, nrows = 2, margin = 0.04) %>% layout(
  title = 'Price level response to 100 bp rate shock, average and p-bias corrected IRFs', ####### change title for output or rate response
  yaxis = list(title = "Effect in %"),  # y-axis for plot 1 ####### adjsut to p.p. for rate response
  xaxis3 = list(title = "Period (Months)"), # x-axis for plot 3
  yaxis3 = list(title = "Effect in %"), # y-axis for plot 3 ####### adjsut to p.p. for rate response
  xaxis4 = list(title = "Period (Months)")  # x-axis for plot 4
) %>% layout(annotations = list(
  list(x = 0.2, y = 1, text = "US", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom", font = list(size = 16)),
  list(x = 0.8, y = 1, text = "Euro Area", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom", font = list(size = 16)),
  list(x = 0.2, y = 0.45, text = "Emerging Economies", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom", font = list(size = 16)),
  list(x = 0.8, y = 0.45, text = "Other Advanced", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom", font = list(size = 16))
),
margin = list(t = 80))



