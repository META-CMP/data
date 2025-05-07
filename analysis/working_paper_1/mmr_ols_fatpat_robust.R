# Creates figures for baseline and robustness check of MMR with OLS FAT-PET with all moderators

# Source the MMR preparation file ----
source(here::here("analysis/working_paper_1/mmr_preparation.R"))

# Estimation ----

## For output ----
out_var <- "output"

### OLS-FAT-PET model estimation ----
mmr_output_ols_fatpet <- meta_analysis(d_no_qc, 
                                      outvar = out_var, 
                                      se_option = "upper", 
                                      periods = chosen_periods,
                                      wins = wins_para, 
                                      prec_weighted = FALSE,
                                      estimation = "FAT-PET", 
                                      cluster_se = TRUE, 
                                      mods = baseline_mods)
#### Table ----
##### Create html output
modelsummary::modelsummary(mmr_output_ols_fatpet[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("OLS-FAT-PET", out_var),
                           gof_map = "nobs",
                           coef_map = fatpet_coef_names)
##### Save table as png
modelsummary::modelsummary(mmr_output_ols_fatpet[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_fatpet/", "mmr_output_ols_fatpet", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = fatpet_coef_names)
##### Save table as tex
modelsummary::modelsummary(mmr_output_ols_fatpet[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_fatpet/", "mmr_output_ols_fatpet", ".tex"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = fatpet_coef_names)
#### Plots ----
##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "(Intercept)",
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.75, 0.25)) +
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))
(p2 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "standarderror_winsor", 
                                   custom_title = "P-bias coefficient (SE)") +
    coord_cartesian(ylim = c(-1.25, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))
# Combined plot p1, p2:
(figure_mmr_output_ols_fatpet_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_ols_fatpet_intercept_p_bias.pdf"),
       plot = figure_mmr_output_ols_fatpet_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 2.5)
y_lims <- c(-1.5, 0.75)
p3 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p4 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p5 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p6 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_output_ols_fatpet, "cbanker", 
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
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_ols_fatpet_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 6)

### Corrected effects for OLS version of FAT-PET baseline ----
prediction_conf_level = 0.67
#### For different identification methods (top_5_or_tier and cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0.14, # See table 1 in working paper
    cbanker = 0.54, # See table 1 in working paper
    standarderror_winsor = 0
  )
  
  mmr_output <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = FALSE,
    estimation = "FAT-PET",
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

#### Plot corrected effects ----
mmr_output_baseline_corrected_effects_ident_OLS <- ggplot(prediction, 
                                                       aes(x = period, 
                                                           color = source, 
                                                           fill = source)) +
    # # Add confidence intervals without outer lines
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
    coord_cartesian(ylim = c(-1.2, 0.5)) +
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
mmr_output_baseline_corrected_effects_ident_OLS

#### Save plot as pdf ----
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_corrected_effects_ident_OLS.pdf"),
       plot = mmr_output_baseline_corrected_effects_ident_OLS,
       device = "pdf",
       width = 7,
       height = 5)

# Plot with uncorrected sub-sample lines without bounds
mmr_output_baseline_corrected_effects_ident_OLS_with_subsamples <- ggplot(prediction,
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
  scale_fill_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "orange",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"#,
    # "Uncorrected" = "#1f77b4"
  )) +
  labs(
    title = "P-bias corrected effects, 14 % top journal, 54 % CB affiliation",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  # # Add uncorrected
  # geom_line(data = prediction %>% filter(source == "Uncorrected"),
  #           aes(y = predicted_value),
  #           color = "#1f77b4",
  #           linetype = "dashed") +
  # geom_point(data = prediction %>% filter(source == "Uncorrected"),
  #            aes(y = predicted_value),
  #            color = "#1f77b4",
  #            size = 2) +
  geom_line(data = chol, aes(x = period, y = average), color = "#112EB8", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = hf, aes(x = period, y = average), color = "#E41A1C", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = nr, aes(x = period, y = average), color = "orange", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) + 
  geom_line(data = SignR, aes(x = period, y = average), color = "#4DAF4A", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = other, aes(x = period, y = average), color = "#984EA3", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  coord_cartesian(ylim = c(-3, 0.3))
mmr_output_baseline_corrected_effects_ident_OLS_with_subsamples

#### Save plot with sub-sample lines as pdf ----
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_output_baseline_corrected_effects_ident_OLS_with_subsamples.pdf"),
       plot = mmr_output_baseline_corrected_effects_ident_OLS_with_subsamples,
       device = "pdf",
       width = 7,
       height = 5)

### Robustness model estimation (also used for BMA comparison) ----
mmr_output_robust_ols_fatpet <- meta_analysis(d_no_qc,
                                             outvar = out_var, 
                                             se_option = "upper", 
                                             periods = chosen_periods,
                                             wins = wins_para,
                                             prec_weighted = FALSE, 
                                             estimation = "FAT-PET", 
                                             cluster_se = TRUE, 
                                             mods = c(
                                               baseline_mods,
                                               robustness_mods_output)
)

## For price level ----
out_var <- "inflation"


### OLS-FAT-PET model estimation ----
mmr_pricelevel_ols_fatpet <- meta_analysis(d_no_qc, 
                                          outvar = out_var, 
                                          se_option = "upper", 
                                          periods = chosen_periods,
                                          wins = wins_para, 
                                          prec_weighted = FALSE, 
                                          estimation = "FAT-PET", 
                                          cluster_se = TRUE, 
                                          mods = baseline_mods)
#### Table ----
##### Create html output
modelsummary::modelsummary(mmr_pricelevel_ols_fatpet[as.character(chosen_periods_tables)], 
                           output = "gt", 
                           stars = TRUE, 
                           conf_level = conflevel, 
                           title = paste("OLS-FAT-PET", out_var), 
                           gof_map = NULL, 
                           coef_map = fatpet_coef_names)
##### Save table as png
modelsummary::modelsummary(mmr_pricelevel_ols_fatpet[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_fatpet/", "mmr_pricelevel_ols_fatpet", ".png"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = fatpet_coef_names)
##### Save table as tex
modelsummary::modelsummary(mmr_pricelevel_ols_fatpet[as.character(chosen_periods_tables)],
                           output = paste0("analysis/working_paper_1/tables/mmr_fatpet/", "mmr_pricelevel_ols_fatpet", ".tex"),
                           stars = FALSE,
                           fmt = 3,
                           conf_level = conflevel,
                           gof_map = "nobs",
                           coef_map = fatpet_coef_names)
#### Plots ----
##### Coefficient plots ----
(p1 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "(Intercept)", 
                                   custom_title = "Corrected reference response") +
   coord_cartesian(ylim = c(-0.25, 0.4)) +
   # No subtitle
   # theme(plot.subtitle = element_blank()) +
   # Add subtitle "Corrected response for Cholesky/SVAR, no top journal, no CB affiliation"
   labs(subtitle = "Cholesky/SVAR, no top journal, no CB affiliation") +
   # No axis labels
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank()))

(p2 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "standarderror_winsor", 
                                   custom_title = "P-bias coefficient (SE)") +
    coord_cartesian(ylim = c(-1.25, 0)) +
    # No subtitle
    theme(plot.subtitle = element_blank()) +
    # No axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()))

# Combined plot p1, p2:
(figure_mmr_pricelevel_ols_fatpet_intercept_p_bias <- (p1 + p2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_ols_fatpet_intercept_p_bias.pdf"),
       plot = figure_mmr_pricelevel_ols_fatpet_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 2.5)
y_lims <- c(-1, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) + 
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on top journal 
p7 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Plot on CB affiliation
p8 <- create_mmr_coefficient_plot(mmr_pricelevel_ols_fatpet, "cbanker", 
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
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_ols_fatpet_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 6)

### Corrected effects for OLS version of FAT-PET baseline ----

#### For different identification methods (top_5_or_tier and cbanker roughly at sample average) ----
get_predictions <- function(method, levels = c("chol", "hf", "nr", "signr", "idother")) {
  pred_data <- data.frame(
    group_ident_broad = factor(method, levels = levels),
    top_5_or_tier = 0.14, # See table 1 in working paper
    cbanker = 0.54, # See table 1 in working paper
    standarderror_winsor = 0
  )
  
  mmr_pricelevel <- meta_analysis(
    d_no_qc,
    outvar = out_var,
    se_option = "upper",
    periods = chosen_periods,
    wins = wins_para,
    prec_weighted = FALSE,
    estimation = "FAT-PET",
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
# # Join uncorrected_irf to prediction df with source = uncorrected
prediction <- rbind(prediction, uncorrected_irf)

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

#### Plot corrected effects ----
mmr_pricelevel_baseline_corrected_effects_ident_OLS <- ggplot(prediction, 
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
    coord_cartesian(ylim = c(-1.2, 0.5)) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # # Add uncorrected
    geom_line(data = prediction %>% filter(source == "Uncorrected"),
              aes(y = predicted_value),
              color = "#1f77b4",
              linetype = "dashed") +
    geom_point(data = prediction %>% filter(source == "Uncorrected"),
               aes(y = predicted_value),
               color = "#1f77b4",
               size = 2)
mmr_pricelevel_baseline_corrected_effects_ident_OLS

#### Save plot as pdf ----
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_corrected_effects_ident_OLS.pdf"),
       plot = mmr_pricelevel_baseline_corrected_effects_ident_OLS,
       device = "pdf",
       width = 7,
       height = 5)

# Plot with uncorrected sub-sample lines without bounds
mmr_pricelevel_baseline_corrected_effects_ident_OLS_with_subsamples <- ggplot(prediction,
                                                                              aes(x = period, 
                                                                              color = source, 
                                                                              fill = source)) +
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
  scale_fill_manual(values = c(
    "Chol/SVAR" = "#112EB8",
    "High Frequency" = "#E41A1C",
    "Narrative" = "orange",
    "SignR" = "#4DAF4A",
    "Other" = "#984EA3"#,
    # "Uncorrected" = "#1f77b4"
  )) +
  labs(
    title = "P-bias corrected effects, 14 % top journals, 54 % CB affiliation",
    x = "Month",
    y = "Effect",
    color = "Identification",
    fill = "Identification"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-3, 0.3)) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  # # # Add uncorrected
  # geom_line(data = prediction %>% filter(source == "Uncorrected"),
  #           aes(y = predicted_value),
  #           color = "#1f77b4",
  #           linetype = "dashed") +
  # geom_point(data = prediction %>% filter(source == "Uncorrected"),
  #            aes(y = predicted_value),
  #            color = "#1f77b4",
  #            size = 2) +
  # Add uncorrected sub-sample lines to plot
  geom_line(data = chol, aes(x = period, y = average), color = "#112EB8", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = hf, aes(x = period, y = average), color = "#E41A1C", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = nr, aes(x = period, y = average), color = "orange", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) + 
  geom_line(data = SignR, aes(x = period, y = average), color = "#4DAF4A", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75) +
  geom_line(data = other, aes(x = period, y = average), color = "#984EA3", linetype = "dashed", inherit.aes = FALSE, linewidth = 0.75)
mmr_pricelevel_baseline_corrected_effects_ident_OLS_with_subsamples

#### Save plot with sub-sample lines as pdf ----
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_mmr_pricelevel_baseline_corrected_effects_ident_OLS_with_subsamples.pdf"),
       plot = mmr_pricelevel_baseline_corrected_effects_ident_OLS_with_subsamples,
       device = "pdf",
       width = 7,
       height = 5)

### Robustness model estimation (also used for BMA comparison) ----
mmr_pricelevel_robust_ols_fatpet <- meta_analysis(d_no_qc,
                                                   outvar = out_var, 
                                                   se_option = "upper", 
                                                   periods = chosen_periods,
                                                   wins = wins_para, 
                                                   prec_weighted = FALSE, 
                                                   estimation = "FAT-PET", 
                                                   cluster_se = TRUE, 
                                                   mods = c(
                                                     baseline_mods,
                                                     robustness_mods_pricelevel)
)


