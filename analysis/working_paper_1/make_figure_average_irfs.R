# Creates uncorrected average IRF plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing

# Source required functions ----
source(here::here("analysis/R/plot_average_irfs.R"))

# Define general folder path to save figures and their data
save_path <- "analysis/working_paper_1/figures/average_irfs/"

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
  show_legend = TRUE,
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
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_output_median$data, here::here(save_path, "avg_irf_output_median.csv"))
# Change plot title
avg_irf_output_median <- avg_irf_output_median$plot %>% plotly::layout(
  title = "Average and median IRF for Output"
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
     width = 1500,
     height = NULL
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
  show_legend = TRUE,
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
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_pricelevel_median$data, here::here(save_path, "avg_irf_pricelevel_median.csv"))
# Change plot title
avg_irf_pricelevel_median <- avg_irf_pricelevel_median$plot %>% plotly::layout(
  title = "Average and median IRF for Inflation"
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
  title = "Average and median IRF for Inflation"
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
  title = "Average and median IRF for Interest Rate"
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
    yaxis = list(title = "Effect (%)",
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
figure_average_irfs_output_pricelevel_rate_with_median <- subplot(avg_irf_output_median, 
                                                                  avg_irf_pricelevel_median, 
                                                                  avg_irf_rate_median, 
                                                                  nrows = 2, 
                                                                  margin = 0.05) %>% layout(
  showlegend = FALSE,
  title = 'Average and median IRFs') %>% layout(annotations = list(
  list(x = 0.25, y = 1, text = "Output", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.75, y = 1, text = "Price level", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom"),
  list(x = 0.25, y = 0.45, text = "Interest rate", showarrow = FALSE, xref = "paper", yref = "paper",
       xanchor = "center", yanchor = "bottom")
), margin = list(t = 60)) %>%
  layout(
    xaxis = list(title = "Months"),
    xaxis2 = list(title = "Months"),
    xaxis3 = list(title = "Months"),
    yaxis = list(title = "Effect (%)",
                 range = list(-2.8,0.8)
    ),
    yaxis2 = list(range = list(-2.8,0.8)
    ),
    yaxis3 = list(title = "Effect (%)", 
                  range = list(-1,1.5)
    ),
    hovermode = "compare"
  ) 

# Display figure
figure_average_irfs_output_pricelevel_rate_with_median

# Save as pdf
orca(figure_average_irfs_output_pricelevel_rate_with_median,
     file = "analysis/working_paper_1/figures/average_irfs/figure_average_irfs_output_pricelevel_rate_with_median.pdf",
     scale = NULL,
     width = 1500,
     height = 1000
)

# For sub-samples ----
## For output ----
out_var <- "output"
### For top journals vs other publications ----
y_lims <- c(-4.7, 1.1)
#### Top journals
avg_irf_output_top_journals <- plot_average_irfs(
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
write_csv(avg_irf_output_top_journals$data, here::here(save_path, "avg_irf_output_top_journals.csv"))
# Change plot title
avg_irf_output_top_journals <- avg_irf_output_top_journals$plot %>% plotly::layout(
  title = "Average and median IRF for output in top journals"
)

#### Other publications
avg_irf_output_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_cbanker <- plot_average_irfs(
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
write_csv(avg_irf_output_cbanker$data, here::here(save_path, "avg_irf_output_cbanker.csv"))
# Change plot title
avg_irf_output_cbanker <- avg_irf_output_cbanker$plot %>% plotly::layout(
  title = "Average and median IRF for output in central bank affiliated publications"
)

avg_irf_output_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
y_lims <- c(-2.8,0.8)
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

### For identifaction methods ("chol", "signr", "hf", "idother", "nr") ----
### For all publications
#### For Cholesky
avg_irf_output_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_idother <- plot_average_irfs(
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
write_csv(avg_irf_output_idother$data, here::here(save_path, "avg_irf_output_idother.csv"))
# Change plot title
avg_irf_output_idother <- avg_irf_output_idother$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
avg_irf_output_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
y_lims <- c(-7, 3)
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
avg_irf_output_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_idother_top_journals <- plot_average_irfs(
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
write_csv(avg_irf_output_idother_top_journals$data, here::here(save_path, "avg_irf_output_idother_top_journals.csv"))
# Change plot title
avg_irf_output_idother_top_journals <- avg_irf_output_idother_top_journals$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
avg_irf_output_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_idother_other_publications <- plot_average_irfs(
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
write_csv(avg_irf_output_idother_other_publications$data, here::here(save_path, "avg_irf_output_idother_other_publications.csv"))
# Change plot title
avg_irf_output_idother_other_publications <- avg_irf_output_idother_other_publications$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
avg_irf_output_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_us <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, us == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_ea12 <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, ea12 == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_upper_middle <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, upper_middle_income == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_output_other_high_income <- plot_average_irfs(
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
write_csv(avg_irf_output_other_high_income$data, here::here(save_path, "avg_irf_output_other_high_income.csv"))
# Change plot title
avg_irf_output_other_high_income <- avg_irf_output_other_high_income$plot %>% plotly::layout(
  title = "Other high_income"
)

#### Joint plot 
y_lims <- c(-3.3, 1.6)
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
avg_irf_pricelevel_top_journals <- plot_average_irfs(
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
write_csv(avg_irf_pricelevel_top_journals$data, here::here(save_path, "avg_irf_pricelevel_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_top_journals <- avg_irf_pricelevel_top_journals$plot %>% plotly::layout(
  title = "Top journals"
)

#### Other publications
avg_irf_pricelevel_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_cbanker <- plot_average_irfs(
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
write_csv(avg_irf_pricelevel_cbanker$data, here::here(save_path, "avg_irf_pricelevel_cbanker.csv"))
# Change plot title
avg_irf_pricelevel_cbanker <- avg_irf_pricelevel_cbanker$plot %>% plotly::layout(
  title = "Central bank affiliated"
)

avg_irf_pricelevel_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
y_lims <- c(-4.2, 0.9)
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
avg_irf_pricelevel_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_idother <- plot_average_irfs(
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
write_csv(avg_irf_pricelevel_idother$data, here::here(save_path, "avg_irf_pricelevel_idother.csv"))
# Change plot title
avg_irf_pricelevel_idother <- avg_irf_pricelevel_idother$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
avg_irf_pricelevel_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
y_lims <- c(-7, 2.8)
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
avg_irf_pricelevel_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_idother_top_journals <- plot_average_irfs(
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
write_csv(avg_irf_pricelevel_idother_top_journals$data, here::here(save_path, "avg_irf_pricelevel_idother_top_journals.csv"))
# Change plot title
avg_irf_pricelevel_idother_top_journals <- avg_irf_pricelevel_idother_top_journals$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
avg_irf_pricelevel_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
y_lims <- c(-7, 1.4)
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
avg_irf_pricelevel_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_idother_other_publications <- plot_average_irfs(
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
write_csv(avg_irf_pricelevel_idother_other_publications$data, here::here(save_path, "avg_irf_pricelevel_idother_other_publications.csv"))
# Change plot title
avg_irf_pricelevel_idother_other_publications <- avg_irf_pricelevel_idother_other_publications$plot %>% plotly::layout(
  title = "Other identification methods"
)

#### For narrative
avg_irf_pricelevel_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
y_lims <- c(-7, 1.4)
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
y_lims <- c(-8, 1.3)
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
avg_irf_pricelevel_us <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, us == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_ea12 <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, ea12 == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_upper_middle <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, upper_middle_income == 1),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_pricelevel_other_high_income <- plot_average_irfs(
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
write_csv(avg_irf_pricelevel_other_high_income$data, here::here(save_path, "avg_irf_pricelevel_other_high_income.csv"))
# Change plot title
avg_irf_pricelevel_other_high_income <- avg_irf_pricelevel_other_high_income$plot %>% plotly::layout(
  title = "Other high income economies"
)

#### Joint plot 
y_lims <- c(-2.5, 1)
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
avg_irf_rate_top_journals <- plot_average_irfs(
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
write_csv(avg_irf_rate_top_journals$data, here::here(save_path, "avg_irf_rate_top_journals.csv"))
# Change plot title
avg_irf_rate_top_journals <- avg_irf_rate_top_journals$plot %>% plotly::layout(
  title = "Top journals"
)

#### Other publications
avg_irf_rate_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
    yaxis = list(title = "Effect (%)",
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
avg_irf_rate_cbanker <- plot_average_irfs(
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
write_csv(avg_irf_rate_cbanker$data, here::here(save_path, "avg_irf_rate_cbanker.csv"))
# Change plot title
avg_irf_rate_cbanker <- avg_irf_rate_cbanker$plot %>% plotly::layout(
  title = "Central bank affiliated"
)

avg_irf_rate_non_cbanker <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, cbanker == "non-central bank affiliated"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
    yaxis = list(title = "Effect (%)",
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
avg_irf_rate_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_chol$data, here::here(save_path, "avg_irf_rate_chol.csv"))

#### For Sign restrictions
avg_irf_rate_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_signr$data, here::here(save_path, "avg_irf_rate_signr.csv"))

#### For High frequency
avg_irf_rate_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_hf$data, here::here(save_path, "avg_irf_rate_hf.csv"))

#### For Other identification methods
avg_irf_rate_idother <- plot_average_irfs(
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
write_csv(avg_irf_rate_idother$data, here::here(save_path, "avg_irf_rate_idother.csv"))

#### For narrative
avg_irf_rate_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_rate_chol_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_chol_top_journals$data, here::here(save_path, "avg_irf_rate_chol_top_journals.csv"))

#### For Sign restrictions
avg_irf_rate_signr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_signr_top_journals$data, here::here(save_path, "avg_irf_rate_signr_top_journals.csv"))

#### For High frequency
avg_irf_rate_hf_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_hf_top_journals$data, here::here(save_path, "avg_irf_rate_hf_top_journals.csv"))

#### For Other identification methods
avg_irf_rate_idother_top_journals <- plot_average_irfs(
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
write_csv(avg_irf_rate_idother_top_journals$data, here::here(save_path, "avg_irf_rate_idother_top_journals.csv"))

#### For narrative
avg_irf_rate_nr_top_journals <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "top journal"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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
avg_irf_rate_chol_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_chol_other_publications$data, here::here(save_path, "avg_irf_rate_chol_other_publications.csv"))

#### For Sign restrictions
avg_irf_rate_signr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_signr_other_publications$data, here::here(save_path, "avg_irf_rate_signr_other_publications.csv"))

#### For High frequency
avg_irf_rate_hf_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = TRUE,
  return_data = TRUE
)
# Save data as csv
write_csv(avg_irf_rate_hf_other_publications$data, here::here(save_path, "avg_irf_rate_hf_other_publications.csv"))

#### For Other identification methods
avg_irf_rate_idother_other_publications <- plot_average_irfs(
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
write_csv(avg_irf_rate_idother_other_publications$data, here::here(save_path, "avg_irf_rate_idother_other_publications.csv"))

#### For narrative
avg_irf_rate_nr_other_publications <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr", top_5_or_tier == "other publication"),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
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


