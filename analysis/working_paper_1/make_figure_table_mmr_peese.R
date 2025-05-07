# Creates figures and tables from MMR estimations for WLS-PEESE

# Source the MMR preparation file ----
source(here::here("analysis/working_paper_1/mmr_preparation.R"))

# Potential outlier ---- 
# One study has strong effects on top journal coefficients and other identification, but there is no obvious reason for excluding the study. 
# For comparison, activate the line below to remove study with key KB9TBSNM from d_no_qc
# d_no_qc <- d_no_qc %>% filter(key != "KB9TBSNM")

# Estimation ----

## For output ----
out_var <- "output"

### WLS-PEESE model estimation ----
mmr_output_wls_peese <- meta_analysis(d_no_qc, 
                                     outvar = out_var, 
                                     se_option = "upper", 
                                     periods = chosen_periods,
                                     wins = wins_para, 
                                     prec_weighted = TRUE,
                                     estimation = "PEESE", 
                                     cluster_se = TRUE, 
                                     mods = baseline_mods)
#### Table ----
##### Create html output
modelsummary::modelsummary(mmr_output_wls_peese[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("WLS-PEESE", out_var),
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
##### Save table as png
modelsummary::modelsummary(mmr_output_wls_peese[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_wls_peese", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
##### Save table as tex
modelsummary::modelsummary(mmr_output_wls_peese[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_wls_peese", ".tex"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
#### Plots ----
##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "(Intercept)",
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.4, 0)) +
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))
(p2 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-1.3, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))
# Combined plot p1, p2:
(figure_mmr_output_wls_peese_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_intercept_p_bias.pdf"),
       plot = figure_mmr_output_wls_peese_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)
y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p4 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p5 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p6 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_output_wls_peese, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)
##### Corrected effects ----
prediction_conf_level = 0.67
###### For different identification methods (top_5_or_tier and cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0.14, # See table 1 in working paper
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = prediction_conf_level
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_output_wls_peese_corrected_effects_ident <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    labs(
      title = "P-bias corrected effects, 14 % top journal, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-1, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "#1f77b4", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "#1f77b4", 
               size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_corrected_effects_ident.pdf"),
       plot = mmr_output_wls_peese_corrected_effects_ident,
       device = "pdf",
       width = 7,
       height = 5)

####### Compare corrected with sub-sample averages ----
# Delete uncorrected average
prediction <- prediction %>% filter(source != "Uncorrected")
mmr_output_wls_peese_subsample_averages_vs_corrected_effects_ident <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # # Add confidence intervals without outer lines
    # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
    #             alpha = 0.1,
    #             color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3"#,
      # "Uncorrected" = "#1f77b4"
    )) +
    # scale_fill_manual(values = c(
    #   "Chol/SVAR" = "#112EB8",
    #   "High Frequency" = "#E41A1C",
    #   "Narrative" = "orange",
    #   "SignR" = "#4DAF4A",
    #   "Other" = "#984EA3",
    #   "Uncorrected" = "#1f77b4"
    # )) +
    labs(
      title = "P-bias corrected effects assuming 14 % top journal, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-3, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)
# Get data for uncorrected Chol/SVAR
avg_irf_output_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
chol <- data.frame(
  period = avg_irf_output_chol$data$period.month,
  average = avg_irf_output_chol$data$avg.effect
)
# Get data for uncorrected High Frequency
avg_irf_output_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
hf <- data.frame(
  period = avg_irf_output_hf$data$period.month,
  average = avg_irf_output_hf$data$avg.effect
)
# Get data for uncorrected Narrative
avg_irf_output_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
nr <- data.frame(
  period = avg_irf_output_nr$data$period.month,
  average = avg_irf_output_nr$data$avg.effect
)
# Get data for uncorrected SignR
avg_irf_output_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
SignR <- data.frame(
  period = avg_irf_output_signr$data$period.month,
  average = avg_irf_output_signr$data$avg.effect
)
# Get data for uncorrected Other
avg_irf_output_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
other <- data.frame(
  period = avg_irf_output_idother$data$period.month,
  average = avg_irf_output_idother$data$avg.effect
)
# Add uncorrected sub-sample lines to plot
(mmr_output_wls_peese_subsample_averages_vs_corrected_effects_ident <- mmr_output_wls_peese_subsample_averages_vs_corrected_effects_ident +
  geom_line(data = chol, aes(x = period, y = average), color = "#112EB8", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = hf, aes(x = period, y = average), color = "#E41A1C", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = nr, aes(x = period, y = average), color = "orange", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) + 
  geom_line(data = SignR, aes(x = period, y = average), color = "#4DAF4A", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = other, aes(x = period, y = average), color = "#984EA3", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_subsample_averages_vs_corrected_effects_ident.pdf"),
       plot = mmr_output_wls_peese_subsample_averages_vs_corrected_effects_ident,
       device = "pdf",
       width = 7,
       height = 5)

###### For other publications for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0,
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = prediction_conf_level
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_output_wls_peese_corrected_effects_ident_other_pub <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    labs(
      title = "P-bias corrected effects, 0 % top journal, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-1, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "#1f77b4", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "#1f77b4", 
               size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_corrected_effects_ident_other_pub.pdf"),
       plot = mmr_output_wls_peese_corrected_effects_ident_other_pub,
       device = "pdf",
       width = 7,
       height = 5)
###### For top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = prediction_conf_level
  )
  
  predictions <- do.call(rbind, mmr_output$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
avg_irf_output <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = TRUE,
  show_median = FALSE,
  return_data = TRUE
)
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_output$data$period.month,
  predicted_value = avg_irf_output$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_output_wls_peese_corrected_effects_ident_other_pub <- ggplot(prediction, 
                                                                 aes(x = period, 
                                                                     color = source, 
                                                                     fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    labs(
      title = "P-bias corrected effects, 100 % top journal, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-1, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "#1f77b4", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "#1f77b4", 
               size = 2)
)
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_corrected_effects_ident_top_journals.pdf"),
       plot = mmr_output_wls_peese_corrected_effects_ident_other_pub,
       device = "pdf",
       width = 7,
       height = 5)


### Robustness model estimation ----
#### Table ----
mmr_output_wls_peese_robust <- meta_analysis(d_no_qc,
                                   outvar = out_var, 
                                   se_option = "upper", 
                                   periods = chosen_periods,
                                   wins = wins_para,
                                   prec_weighted = TRUE, 
                                   estimation = "PEESE", 
                                   cluster_se = TRUE, 
                                   mods = c(
                                     baseline_mods,
                                     robustness_mods_output)
                                   )
##### Create html output
modelsummary::modelsummary(mmr_output_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Robustness", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = c(
                             peese_coef_names, 
                             robusntess_coef_names_output)
                           )
##### Save table as png - all controls
modelsummary::modelsummary(mmr_output_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_wls_peese_robust", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = c(
                             peese_coef_names,
                             robusntess_coef_names_output)
                           )
##### Save table as png - only baseline coefficients 
modelsummary::modelsummary(mmr_output_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_wls_peese_robust_baseline_coef", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = c(
                             peese_coef_names)
                           )
##### Save table as tex - only baseline coefficients 
modelsummary::modelsummary(mmr_output_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_output_wls_peese_robust_baseline_coef", ".tex"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = c(
                             peese_coef_names)
                           )

#### Plots ----
##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "(Intercept)",
                                   custom_title = "Corrected reference response") +
    coord_cartesian(ylim = c(-0.4, 0)) +
    # No subtitle
    # theme(plot.subtitle = element_blank()) +
    # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
    labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-1.3, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_output_wls_peese_robust_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_robust_intercept_p_bias.pdf"),
       plot = figure_mmr_output_wls_peese_robust_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)

y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p7 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p8 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p9 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_est_broadlp_ardl", 
                                  custom_title = "LP and ARDL") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p10 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_est_broadfavar", 
                                   custom_title = "FAVAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p11 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_est_broadother_var", 
                                   custom_title = "Other VAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p12 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_est_broaddsge", 
                                   custom_title = "DSGE") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p13 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "outcome_measure_output_consgap", 
                                   custom_title = "Output gap") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p14 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "outcome_measure_output_consip", 
                                   custom_title = "IP") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p15 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_inttypeweek_month", 
                                   custom_title = "weekly/monthly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p16 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "group_inttypeyear", 
                                   custom_title = "yearly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p17 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "pub_year_dm", 
                                   custom_title = "Publication year") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p18 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "num_cit_dm", 
                                   custom_title = "Number of citations") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p19 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "prefer", 
                                   custom_title = "Preferred") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p20 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "byproduct", 
                                   custom_title = "By-product") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p21 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "freqmonth", 
                                   custom_title = "Monthly") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p22 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "freqannual", 
                                   custom_title = "Annual") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p23 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "panel", 
                                   custom_title = "Panel") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots as before 
combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) 
(combined_plot +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_robust_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

# Combine other moderators
(combined_plot_est <- (p9 + p10) / (p11 + p12) / (p13 + p14) / (p15 + p16) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_wls_peese_robust_moderators_est.pdf"),
       plot = combined_plot_est,
       device = "pdf",
       width = 7,
       height = 12)

# Create one empty plot
empty_plot <- ggplot() +
  theme_minimal()

# Combine remaining moderators
(combined_plot_other <- (p17 + p18) / (p19 + p20) / (p21 + p22) / (p23 + empty_plot) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))

## For price level ----
out_var <- "inflation"

### WLS-PEESE model estimation ----
mmr_pricelevel_wls_peese <- meta_analysis(d_no_qc, 
                                         outvar = out_var, 
                                         se_option = "upper", 
                                         periods = chosen_periods,
                                         wins = wins_para, 
                                         prec_weighted = TRUE, 
                                         estimation = "PEESE", 
                                         cluster_se = TRUE, 
                                         mods = baseline_mods)
#### Table ----
##### Create html output
modelsummary::modelsummary(mmr_pricelevel_wls_peese[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("WLS-PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = peese_coef_names)
##### Save table as png
modelsummary::modelsummary(mmr_pricelevel_wls_peese[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_wls_peese", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
##### Save table as tex
modelsummary::modelsummary(mmr_pricelevel_wls_peese[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_wls_peese", ".tex"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
#### Plots ----
##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "(Intercept)", 
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.2, 0.1)) +
   # No subtitle
   # theme(plot.subtitle = element_blank()) +
   # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-2.5, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_pricelevel_wls_peese_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_intercept_p_bias.pdf"),
       plot = figure_mmr_pricelevel_wls_peese_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)
y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots on identification methods
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)
##### Corrected effects ----
###### For different identification methods (top_5_or_tier and cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0.14, # See table 1 in working paper
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_pricelevel <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_pricelevel$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
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
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_pricelevel$data$period.month,
  predicted_value = avg_irf_pricelevel$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_pricelevel_wls_peese_corrected_effects_ident <- ggplot(prediction, 
                                                           aes(x = period, 
                                                               color = source, 
                                                               fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    labs(
      title = "P-bias corrected effects, 14 % top journals, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-0.75, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "#1f77b4", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "#1f77b4", 
               size = 2))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_corrected_effects_ident.pdf"),
       plot = mmr_pricelevel_wls_peese_corrected_effects_ident,
       device = "pdf",
       width = 7,
       height = 5)

####### Compare corrected with sub-sample averages ----
# Delete uncorrected average
prediction <- prediction %>% filter(source != "Uncorrected")
(mmr_pricelevel_wls_peese_subsample_averages_vs_corrected_effects_ident <-  ggplot(prediction, 
                                                                                    aes(x = period, 
                                                                                    color = source, 
                                                                                    fill = source)) +
  # # Add confidence intervals without outer lines
  # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
  #             alpha = 0.1,
  #             color = NA) +
  geom_line(aes(y = predicted_value), 
            linewidth = 1) +
  geom_point(aes(y = predicted_value), 
             size = 2) +
  scale_color_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "orange",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"#,
    # "Uncorrected" = "#1f77b4"
  )) +
  # scale_fill_manual(values = c(
  #   "Chol/SVAR" = "#112EB8",
  #   "High Frequency" = "#E41A1C",
  #   "Narrative" = "orange",
  #   "SignR" = "#4DAF4A",
  #   "Other" = "#984EA3",
  #   "Uncorrected" = "#1f77b4"
  # )) +
  labs(
    title = "P-bias corrected effects assuming 14 % top journal, 54 % CB affiliation",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-2, 0.25)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)
)
# Get data for uncorrected Chol/SVAR
avg_irf_pricelevel_chol <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "chol"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
chol <- data.frame(
  period = avg_irf_pricelevel_chol$data$period.month,
  average = avg_irf_pricelevel_chol$data$avg.effect
)
# Get data for uncorrected High Frequency
avg_irf_pricelevel_hf <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "hf"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
hf <- data.frame(
  period = avg_irf_pricelevel_hf$data$period.month,
  average = avg_irf_pricelevel_hf$data$avg.effect
)
# Get data for uncorrected Narrative
avg_irf_pricelevel_nr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "nr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
nr <- data.frame(
  period = avg_irf_pricelevel_nr$data$period.month,
  average = avg_irf_pricelevel_nr$data$avg.effect
)
# Get data for uncorrected SignR
avg_irf_pricelevel_signr <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "signr"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
SignR <- data.frame(
  period = avg_irf_pricelevel_signr$data$period.month,
  average = avg_irf_pricelevel_signr$data$avg.effect
)
# Get data for uncorrected Other
avg_irf_pricelevel_idother <- plot_average_irfs(
  d_no_qc %>% filter(period.month %in% seq(0,60,by=3), outcome == out_var, group_ident_broad == "idother"),
  winsor = TRUE,
  wins_par = wins_para,
  return_data = TRUE,
  corrected_irf = NULL
)
other <- data.frame(
  period = avg_irf_pricelevel_idother$data$period.month,
  average = avg_irf_pricelevel_idother$data$avg.effect
)
# Add uncorrected sub-sample lines to plot
(mmr_pricelevel_wls_peese_subsample_averages_vs_corrected_effects_ident <- mmr_pricelevel_wls_peese_subsample_averages_vs_corrected_effects_ident +
    geom_line(data = chol, aes(x = period, y = average), color = "#112EB8", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
    geom_line(data = hf, aes(x = period, y = average), color = "#E41A1C", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
    geom_line(data = nr, aes(x = period, y = average), color = "orange", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) + 
    geom_line(data = SignR, aes(x = period, y = average), color = "#4DAF4A", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
    geom_line(data = other, aes(x = period, y = average), color = "#984EA3", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_subsample_averages_vs_corrected_effects_ident.pdf"),
       plot = mmr_pricelevel_wls_peese_subsample_averages_vs_corrected_effects_ident,
       device = "pdf",
       width = 7,
       height = 5)


###### For other publications for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0,
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_pricelevel <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_pricelevel$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
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
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_pricelevel$data$period.month,
  predicted_value = avg_irf_pricelevel$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_pricelevel_wls_peese_corrected_effects_ident_other_pub <- ggplot(prediction, 
                                                           aes(x = period, 
                                                               color = source, 
                                                               fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    labs(
      title = "P-bias corrected effects, 0 % top journals, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-0.75, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "#1f77b4", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "#1f77b4", 
               size = 2))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_corrected_effects_ident_other_pub.pdf"),
       plot = mmr_pricelevel_wls_peese_corrected_effects_ident_other_pub,
       device = "pdf",
       width = 7,
       height = 5)
###### For top journals for different identification methods (cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 1,
    cbanker = 0.54, # See table 1 in working paper
    variance_winsor = 0
  )
  
  mmr_pricelevel <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = TRUE,
    estimation = "PEESE",
    cluster_se = TRUE,
    mods = baseline_mods,
    pred_data = pred_data,
    pred_conf_level = 0.68
  )
  
  predictions <- do.call(rbind, mmr_pricelevel$predictions)
  return(predictions)
}

# Generate predictions for all methods
methods <- list(
  "chol" = "Chol/SVAR",
  "hf" = "High Frequency",
  "nr" = "Narrative", 
  "signr" = "SignR",
  "idother" = "Other"
)

prediction <- map_dfr(names(methods), function(method) {
  predictions <- get_predictions(method)
  predictions$source <- methods[[method]]
  return(predictions)
}, .id = "method_id")

# Get data for uncorrected sample average 
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
uncorrected_irf <- data.frame(
  method_id = 0,
  period = avg_irf_pricelevel$data$period.month,
  predicted_value = avg_irf_pricelevel$data$avg.effect,
  std_error = NA,
  ci_lower = NA,
  ci_upper = NA,
  source = "Uncorrected"
)
# Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

# Plot corrected effects
(mmr_pricelevel_wls_peese_corrected_effects_ident_other_pub <- ggplot(prediction, 
                                                                     aes(x = period, 
                                                                         color = source, 
                                                                         fill = source)) +
    # Add confidence intervals without outer lines
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                alpha = 0.1,
                color = NA) +
    geom_line(aes(y = predicted_value), 
              linewidth = 1) +
    geom_point(aes(y = predicted_value), 
               size = 2) +
    scale_color_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    scale_fill_manual(values = c(
      "Chol/SVAR" = "#112EB8",
      "High Frequency" = "#E41A1C",
      "Narrative" = "orange",
      "SignR" = "#4DAF4A",
      "Other" = "#984EA3",
      "Uncorrected" = "#1f77b4"
    )) +
    labs(
      title = "P-bias corrected effects, 100 % top journals, 54 % CB affiliation",
      x = "Month",
      y = "Effect",
      color = "Identification",
      fill = "Identification"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(-0.75, 0.25)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"), 
              aes(y = predicted_value), 
              color = "#1f77b4", 
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"), 
               aes(y = predicted_value), 
               color = "#1f77b4", 
               size = 2))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_corrected_effects_ident_top_journals.pdf"),
       plot = mmr_pricelevel_wls_peese_corrected_effects_ident_other_pub,
       device = "pdf",
       width = 7,
       height = 5)

### Robustness model estimation ----
#### Table ----
mmr_pricelevel_wls_peese_robust <- meta_analysis(d_no_qc,
                                       outvar = out_var, 
                                       se_option = "upper", 
                                       periods = chosen_periods,
                                       wins = wins_para, 
                                       prec_weighted = TRUE, 
                                       estimation = "PEESE", 
                                       cluster_se = TRUE, 
                                       mods = c(
                                         baseline_mods,
                                         robustness_mods_pricelevel)
                                       )
##### Create html output
modelsummary::modelsummary(mmr_pricelevel_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("Robustness", "PEESE", out_var), 
                           gof_map = NULL, 
                           coef_map = c(
                             peese_coef_names, 
                             robusntess_coef_names_pricelevel)
                           )
##### Save table as png
modelsummary::modelsummary(mmr_pricelevel_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_wls_peese_robust", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = c(
                             peese_coef_names,
                             robusntess_coef_names_pricelevel)
                           )
##### Save table as png - only baseline coefficients 
modelsummary::modelsummary(mmr_pricelevel_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_wls_peese_robust_baseline_coef", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
##### Save table as tex - only baseline coefficients 
modelsummary::modelsummary(mmr_pricelevel_wls_peese_robust[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_peese/", "mmr_pricelevel_wls_peese_robust_baseline_coef", ".tex"),
                           stars = FALSE,
                           fmt = 3,
                           # statistic = NULL,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = peese_coef_names)
#### Plots ----
##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "(Intercept)",
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.2, 0.1)) +
   # No subtitle
   # theme(plot.subtitle = element_blank()) +
   # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "variance_winsor", 
                                   custom_title = "P-bias coefficient (variance)") +
    coord_cartesian(ylim = c(-2.5, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_pricelevel_wls_peese_robust_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))

# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_robust_intercept_p_bias.pdf"),
       plot = figure_mmr_pricelevel_wls_peese_robust_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 3)
y_lims <- c(-0.75, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p7 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p8 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p9 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_est_broadlp_ardl", 
                                  custom_title = "LP and ARDL") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p10 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_est_broadfavar", 
                                   custom_title = "FAVAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p11 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_est_broadother_var", 
                                   custom_title = "Other VAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p12 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_est_broaddsge", 
                                   custom_title = "DSGE") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p13 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "outcome_measure_pricelevel_conscore", 
                                   custom_title = "Core") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p14 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "outcome_measure_pricelevel_consdeflator", 
                                   custom_title = "Deflator") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p15 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "outcome_measure_pricelevel_conswpi", 
                                   custom_title = "WPI") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p16 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_inttypeweek_month", 
                                   custom_title = "weekly/monthly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p17 <- create_mmr_coefficient_plot(mmr_pricelevel_wls_peese_robust, "group_inttypeyear", 
                                   custom_title = "yearly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p18 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "pub_year_dm", 
                                   custom_title = "Publication year") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p19 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "num_cit_dm", 
                                   custom_title = "Number of citations") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p20 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "prefer", 
                                   custom_title = "Preferred") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p21 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "byproduct", 
                                   custom_title = "By-product") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p22 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "freqmonth", 
                                   custom_title = "Monthly") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p23 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "freqannual", 
                                   custom_title = "Annual") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p24 <- create_mmr_coefficient_plot(mmr_output_wls_peese_robust, "panel", 
                                   custom_title = "Panel") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine plots as before
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_robust_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 9)

# Create one empty plot
empty_plot <- ggplot() +
  theme_minimal()

# Combine other moderators
(combined_plot_est <- (p9 + p10) / (p11 + p12) / (p17 + p16) / (p13 + p14) / (p15 + empty_plot) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_wls_peese_robust_moderators_est.pdf"),
       plot = combined_plot_est,
       device = "pdf",
       width = 7,
       height = 12)

# Combine remaining moderators
(combined_plot_other <- (p18 + p19) / (p20 + p21) / (p22 + p23) / (p24 + empty_plot) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))


