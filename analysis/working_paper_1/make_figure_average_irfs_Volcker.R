# Creates average IRF plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Source required functions ----
source(here::here("analysis/R/plot_average_irfs.R"))
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/kasy_MetaStudiesFunctions.R"))
source(here::here("analysis/R/kasy_RobustVariance.R"))
source(here::here("analysis/R/kasy_MetaStudiesPlots.R"))

# Define general folder path to save figures and their data
save_path <- "analysis/working_paper_1/figures/average_irfs/"


# Capping procedure for period 0 precision and se ----
source(here::here("analysis/working_paper_1/period_0_capping_se_prec.R"))

# Redefining top_5_or_tier and cbanker as factors
d_no_qc$top_5_or_tier <- factor(d_no_qc$top_5_or_tier, levels = c(0, 1), labels = c("other publication", "top journal"))
d_no_qc$cbanker <- factor(d_no_qc$cbanker, levels = c(0, 1), labels = c("non-central bank affiliated", "central bank affiliated"))


# Robustness check period during Volcker: Use only Pre-Volcker period for US ----
d_no_qc <- d_no_qc %>%
  filter(us == 1) %>%
  filter(start_date >= "1979-07-01",end_date   <= "2002-04-01")

# For output ----
out_var <- "output"

avg_irf_output_median <- plot_average_irfs(
  d_no_qc %>% dplyr::filter(period.month %in% seq(0, 60, by = 3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  show_CIs = TRUE,
  show_percentiles = FALSE
)

# Save data
#write_csv(avg_irf_output_median$data, file.path(save_path, "avg_irf_output_median_Volcker.csv"))

df_plot <- avg_irf_output_median$data

# Long format for lines with FINAL legend labels
lines_df <- df_plot %>%
  select(period.month, avg.effect, median.effect) %>%
  pivot_longer(cols = c(avg.effect, median.effect),
               names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         "avg.effect"    = "Mean effect from literature",
                         "median.effect" = "Median effect from literature"))

# Colors to match your screenshot
col_mean   <- "navy"  # dark blue line
col_median <- "red3"  # red line
fill_mean  <- "skyblue1"  # light blue ribbon
fill_med   <- "red1"  # light red ribbon

p_out <- ggplot(df_plot, aes(x = period.month)) +
  
  # Mean (blue) confidence bands
  geom_ribbon(aes(ymin = avg_CI.lower_95, ymax = avg_CI.upper_95),
              fill = fill_mean, alpha = 0.25) +
  geom_ribbon(aes(ymin = avg_CI.lower_68, ymax = avg_CI.upper_68),
              fill = fill_mean, alpha = 0.5) +
  
  # Median (red) confidence bands
  geom_ribbon(aes(ymin = median_CI.lower_95, ymax = median_CI.upper_95),
              fill = fill_med, alpha = 0.10) +
  geom_ribbon(aes(ymin = median_CI.lower_68, ymax = median_CI.upper_68),
              fill = fill_med, alpha = 0.22) +
  
  # Lines
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
  geom_line(data = lines_df,
            aes(y = value, color = series),
            linewidth = 1.2) +
  
  # Axes (same style as your example)
  coord_cartesian(ylim = c(-2.75, 0.75)) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-2.5, 0.5, by = 0.5), expand = c(0, 0)) +
  
  # Legend colors + labels already set in `series`
  scale_color_manual(values = c(
    "Mean effect from literature"   = col_mean,
    "Median effect from literature" = col_median
  )) +
  
  labs(
    title = "Output response (%)",
    x = "Month",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, color = "grey30"),
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(color = "grey30"),
    axis.ticks.y = element_line(color = "grey30"),
    axis.line.y  = element_line(color = "grey30"),
    axis.title.x = element_text(color = "grey30"),
    legend.text  = element_text(color = "grey30"),
    legend.title = element_text(color = "grey30")
  )

p_out

# Export PDF
out_pdf <- file.path(save_path, "avg_irf_output_median_Volcker.pdf")
ggsave(
  filename = out_pdf,
  plot     = p_out,
  width    = 360/72,
  height   = (486 * 0.65)/72,
  units    = "in",
  device   = cairo_pdf
)



# For price level ----
out_var <- "inflation"

avg_irf_pricelevel_median <- plot_average_irfs(
  d_no_qc %>% dplyr::filter(period.month %in% seq(0, 60, by = 3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  show_CIs = TRUE,
  show_percentiles = FALSE
)

# Save data
#write_csv(avg_irf_pricelevel_median$data, file.path(save_path, "avg_irf_pricelevel_median_Volcker.csv"))

df_plot <- avg_irf_pricelevel_median$data

# Long format for lines with FINAL legend labels
lines_df <- df_plot %>%
  select(period.month, avg.effect, median.effect) %>%
  pivot_longer(cols = c(avg.effect, median.effect),
               names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         "avg.effect"    = "Mean effect from literature",
                         "median.effect" = "Median effect from literature"))

# Colors to match your screenshot
col_mean   <- "navy"  # dark blue line
col_median <- "red3"  # red line
fill_mean  <- "skyblue1"  # light blue ribbon
fill_med   <- "red1"  # light red ribbon

p_out <- ggplot(df_plot, aes(x = period.month)) +
  
  # Mean (blue) confidence bands
  geom_ribbon(aes(ymin = avg_CI.lower_95, ymax = avg_CI.upper_95),
              fill = fill_mean, alpha = 0.25) +
  geom_ribbon(aes(ymin = avg_CI.lower_68, ymax = avg_CI.upper_68),
              fill = fill_mean, alpha = 0.5) +
  
  # Median (red) confidence bands
  geom_ribbon(aes(ymin = median_CI.lower_95, ymax = median_CI.upper_95),
              fill = fill_med, alpha = 0.10) +
  geom_ribbon(aes(ymin = median_CI.lower_68, ymax = median_CI.upper_68),
              fill = fill_med, alpha = 0.22) +
  
  # Lines
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
  geom_line(data = lines_df,
            aes(y = value, color = series),
            linewidth = 1.2) +
  
  # Axes (same style as your example)
  coord_cartesian(ylim = c(-2.75, 0.75)) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-2.5, 0.5, by = 0.5), expand = c(0, 0)) +
  
  # Legend colors + labels already set in `series`
  scale_color_manual(values = c(
    "Mean effect from literature"   = col_mean,
    "Median effect from literature" = col_median
  )) +
  
  labs(
    title = "Price level response (%)",
    x = "Month",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, color = "grey30"),
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(color = "grey30"),
    axis.ticks.y = element_line(color = "grey30"),
    axis.line.y  = element_line(color = "grey30"),
    axis.title.x = element_text(color = "grey30"),
    legend.text  = element_text(color = "grey30"),
    legend.title = element_text(color = "grey30")
  )

p_out

# Export PDF
out_pdf <- file.path(save_path, "avg_irf_pricelevel_median_Volcker.pdf")
ggsave(
  filename = out_pdf,
  plot     = p_out,
  width    = 360/72,
  height   = (486 * 0.65)/72,
  units    = "in",
  device   = cairo_pdf
)


# For interest rate ----
out_var <- "rate"

avg_irf_rate_median <- plot_average_irfs(
  d_no_qc %>% dplyr::filter(period.month %in% seq(0, 60, by = 3), outcome == out_var),
  period_limit = 60,
  winsor = TRUE,
  wins_par = wins_para,
  corrected_irf = NULL,
  show_legend = FALSE,
  show_median = TRUE,
  return_data = TRUE,
  show_CIs = TRUE,
  show_percentiles = FALSE
)

# Save data
#write_csv(avg_irf_rate_median$data, file.path(save_path, "avg_irf_rate_median_Volcker.csv"))

df_plot <- avg_irf_rate_median$data

# Long format for lines with FINAL legend labels
lines_df <- df_plot %>%
  select(period.month, avg.effect, median.effect) %>%
  pivot_longer(cols = c(avg.effect, median.effect),
               names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         "avg.effect"    = "Mean effect from literature",
                         "median.effect" = "Median effect from literature"))

# Colors to match your screenshot
col_mean   <- "navy"  # dark blue line
col_median <- "red3"  # red line
fill_mean  <- "skyblue1"  # light blue ribbon
fill_med   <- "red1"  # light red ribbon

p_out <- ggplot(df_plot, aes(x = period.month)) +
  
  # Mean (blue) confidence bands
  geom_ribbon(aes(ymin = avg_CI.lower_95, ymax = avg_CI.upper_95),
              fill = fill_mean, alpha = 0.25) +
  geom_ribbon(aes(ymin = avg_CI.lower_68, ymax = avg_CI.upper_68),
              fill = fill_mean, alpha = 0.5) +
  
  # Median (red) confidence bands
  geom_ribbon(aes(ymin = median_CI.lower_95, ymax = median_CI.upper_95),
              fill = fill_med, alpha = 0.10) +
  geom_ribbon(aes(ymin = median_CI.lower_68, ymax = median_CI.upper_68),
              fill = fill_med, alpha = 0.22) +
  
  # Lines
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
  geom_line(data = lines_df,
            aes(y = value, color = series),
            linewidth = 1.2) +
  
  # Axes (same style as your example)
  coord_cartesian(ylim = c(-1, 1.5)) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-1, 1.5, by = 0.5), expand = c(0, 0)) +
  
  # Legend colors + labels already set in `series`
  scale_color_manual(values = c(
    "Mean effect from literature"   = col_mean,
    "Median effect from literature" = col_median
  )) +
  
  labs(
    title = "Interest rate response (%-points)",
    x = "Month",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, color = "grey30"),
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(color = "grey30"),
    axis.ticks.y = element_line(color = "grey30"),
    axis.line.y  = element_line(color = "grey30"),
    axis.title.x = element_text(color = "grey30"),
    legend.text  = element_text(color = "grey30"),
    legend.title = element_text(color = "grey30")
  )

p_out

# Export PDF
out_pdf <- file.path(save_path, "avg_irf_rate_median_Volcker.pdf")
ggsave(
  filename = out_pdf,
  plot     = p_out,
  width    = 360/72,
  height   = (486 * 0.65)/72,
  units    = "in",
  device   = cairo_pdf
)