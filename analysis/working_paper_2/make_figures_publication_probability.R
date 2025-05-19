# Creates conditional publication probability plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/setup_wp_2.R"))

# Load required libraries ----
library(JWileymisc) # For winsorizing
library(patchwork) # For arranging plots
library(latex2exp) # For latex in plots

# Source required functions ----
source(here("analysis/R/meta_analysis.R"))
source(here("analysis/R/apply_winsorization.R"))
source(here("analysis/R/kasy_MetaStudiesFunctions.R"))
source(here("analysis/R/kasy_RobustVariance.R"))
source(here("analysis/R/kasy_MetaStudiesPlots.R"))

# Selected months for AK estimation ----
months <- seq(3, 60, by = 3)

# For employment ----
out_var <- "emp"

## Estimation ----
emp_ak <- meta_analysis(d_no_qc,
                           outvar = out_var,
                           se_option = "upper",
                           periods = months,
                           wins = wins_para,
                           prec_weighted = FALSE,
                           estimation = "AK",
                           cluster_se = TRUE,
                           cutoff_val = 1, # 68 % level as cutoff
                           AK_modelmu = "t",
                           AK_symmetric = FALSE,
                           AK_conf_level = conflevel,
                           ak_plot = "pub_prob_only",
                           AK_plot_prob_y_range = c(-100, 100)
)

## Create combined plots ----
plots <- list()
# # Version without confidence bands:
# for (month in months) {
#   plot_name <- paste0("p", month)
#   plots[[plot_name]] <- emp_ak[[as.character(month)]]$plot + 
#     theme_minimal() + 
#     labs(subtitle = paste("Month", month)) + 
#     labs(y = "Publication probability")
# }
# With confidence bands
for (month in months) {
  plot_name <- paste0("p", month)
  
  # Get the existing plot
  current_plot <- emp_ak[[as.character(month)]]$plot
  
  # Get confidence intervals from the tidy data frame
  tidy_data <- emp_ak[[as.character(month)]]$tidy
  
  # Find the publication probability parameter rows (skip μ, τ, and df for "t" model)
  start_row <- 4  # Since you're using AK_modelmu = "t"
  
  # The intervals based on cutoff_val = 1 and AK_symmetric = FALSE
  intervals <- list(
    c(-Inf, -1),
    c(-1, 0),
    c(0, 1),
    c(1, Inf)
  )
  
  # Add confidence bands as rectangles
  for (i in 1:length(intervals)) {
    row_idx <- start_row + (i - 1)
    current_plot <- current_plot +
      annotate("rect", 
               xmin = intervals[[i]][1], 
               xmax = intervals[[i]][2], 
               ymin = tidy_data$conf.low[row_idx], 
               ymax = tidy_data$conf.high[row_idx], 
               alpha = 0.2, fill = "blue")
  }
  
  # Finish styling the plot
  plots[[plot_name]] <- current_plot +
    theme_minimal() + 
    labs(subtitle = paste("Month", month)) + 
    labs(y = "Publication probability")
}
# All quarters
combined_plot_all <- plots$p3 + plots$p6  + plots$p9 + plots$p12 + plots$p15  + plots$p18 + plots$p21 + plots$p24 + plots$p27 + plots$p30 + plots$p33 + plots$p36 + plots$p39 + plots$p42 + plots$p45 + plots$p48 + plots$p51  + plots$p54  + plots$p57 +plots$p60 + 
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_all
# Every two quarters up to 4 years
combined_plot_two_quarters <- plots$p6 + plots$p12+ plots$p18+ plots$p24+ plots$p30+ plots$p36 + plots$p42 + plots$p48 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_two_quarters
# Plot for few periods
combined_plot_years <- plots$p3 + plots$p12 + plots$p24 + plots$p36 + plots$p48 + #plots$p60 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_years
## Save the plots ----
# Figure publication probability employment
ggsave("analysis/working_paper_2/figures/publication_probability/figure_publication_probability_emp.pdf", 
       combined_plot_years, 
       width = 12, 
       height = 4)
# Alternative more periods (alternative)
ggsave("analysis/working_paper_2/figures/publication_probability/figure_publication_probabiltiy_emp_more_periods.pdf",
       combined_plot_two_quarters, 
       width = 16, 
       height = 6)

# All periods (appendix)
ggsave("analysis/working_paper_2/figures/publication_probability/figure_publication_probabiltiy_emp_all_periods.pdf", 
       combined_plot_all, 
       width = 36, 
       height = 16)

# For unemployment rate ----
out_var <- "unemp"

## Estimation ----
unemp_ak <- meta_analysis(d_no_qc,
                               outvar = out_var,
                               se_option = "lower",
                               periods = months,
                               wins = wins_para,
                               prec_weighted = FALSE,
                               estimation = "AK",
                               cluster_se = TRUE,
                               cutoff_val = 1, # 68 % level as cutoff
                               AK_modelmu = "t",
                               AK_symmetric = FALSE,
                               AK_conf_level = conflevel,
                               ak_plot = "pub_prob_only",
                               AK_plot_prob_y_range = c(-10, 20) # Use c(0, 12.5) for narrower plots without confidence bands 
)
## Create combined plots ----
plots <- list()
# # Version without confidence bands:
# for (month in months) {
#   plot_name <- paste0("p", month)
#   plots[[plot_name]] <- unemp_ak[[as.character(month)]]$plot + 
#     theme_minimal() + 
#     labs(subtitle = paste("Month", month)) + 
#     labs(y = "Publication probability")
# }
# With confidence bands
for (month in months) {
  plot_name <- paste0("p", month)
  
  # Get the existing plot
  current_plot <- unemp_ak[[as.character(month)]]$plot
  
  # Get confidence intervals from the tidy data frame
  tidy_data <- unemp_ak[[as.character(month)]]$tidy
  
  # Find the publication probability parameter rows (skip μ, τ, and df for "t" model)
  start_row <- 4  # Since you're using AK_modelmu = "t"
  
  # The intervals based on cutoff_val = 1 and AK_symmetric = FALSE
  intervals <- list(
    c(-Inf, -1),
    c(-1, 0),
    c(0, 1),
    c(1, Inf)
  )
  
  # Add confidence bands as rectangles
  for (i in 1:length(intervals)) {
    row_idx <- start_row + (i - 1)
    current_plot <- current_plot +
      annotate("rect", 
               xmin = intervals[[i]][1], 
               xmax = intervals[[i]][2], 
               ymin = tidy_data$conf.low[row_idx], 
               ymax = tidy_data$conf.high[row_idx], 
               alpha = 0.2, fill = "blue")
  }
  
  # Finish styling the plot
  plots[[plot_name]] <- current_plot +
    theme_minimal() + 
    labs(subtitle = paste("Month", month)) + 
    labs(y = "Publication probability")
}
# All quarters
combined_plot_all <- plots$p3 + plots$p6  + plots$p9 + plots$p12 + plots$p15  + plots$p18 + plots$p21 + plots$p24 + plots$p27 + plots$p30 + plots$p33 + plots$p36 + plots$p39 + plots$p42 + plots$p45 + plots$p48 + plots$p51  + plots$p54  + plots$p57 +plots$p60 + 
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_all
# Every two quarters up to 4 years
combined_plot_two_quarters <- plots$p6 + plots$p12+ plots$p18+ plots$p24+ plots$p30+ plots$p36 + plots$p42 + plots$p48 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_two_quarters
# Plot for few periods
combined_plot_years <- plots$p3 + plots$p12 + plots$p24 + plots$p36 + plots$p48 + #plots$p60 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_years

## Save the plots ----
# Figure publication probability unemployment rate (appendix)
ggsave("analysis/working_paper_2/figures/publication_probability/figure_publication_probability_unemp.pdf",
       combined_plot_years, 
       width = 12, 
       height = 4)
# Alternative more periods
ggsave("analysis/working_paper_2/figures/publication_probability/figure_publication_probability_unemp_more_periods.pdf",
       combined_plot_two_quarters, 
       width = 16, 
       height = 6)
# All periods (appendix)
ggsave("analysis/working_paper_2/figures/publication_probability/figure_publication_probability_unemp_all_periods.pdf",
       combined_plot_all, 
       width = 36, 
       height = 16) 

