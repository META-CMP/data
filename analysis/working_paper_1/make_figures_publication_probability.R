# Creates conditional publication probability plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

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

# For output ----
out_var <- "output"

## Estimation ----
output_ak <- meta_analysis(d_no_qc,
                           outvar = out_var,
                           se_option = "upper",
                           periods = months,
                           wins = wins_para,
                           prec_weighted = FALSE,
                           estimation = "AK",
                           cluster_se = TRUE,
                           cutoff_val=1, # 68 % level as cutoff
                           AK_modelmu = "t",
                           AK_symmetric = FALSE,
                           AK_conf_level = conflevel,
                           ak_plot = "pub_prob_only",
                           AK_plot_prob_y_range = c(0, 40)
)

## Create combined plots ----
plots <- list()
for (month in months) {
  plot_name <- paste0("p", month)
  plots[[plot_name]] <- output_ak[[as.character(month)]]$plot + 
    theme_minimal() + 
    labs(subtitle = paste("Month", month)) + 
    labs(y = "Pub.Pr.")
}
# All quarters
combined_plot_all <- plots$p3 + plots$p6  + plots$p9 + plots$p12 + plots$p15  + plots$p18 + plots$p21 + plots$p24 + plots$p27 + plots$p30 + plots$p33 + plots$p36 + plots$p39 + plots$p42 + plots$p45 + plots$p48 + plots$p51  + plots$p54  + plots$p57 +plots$p60 + 
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_all
# Every two quarters up to 4 years
combined_plot_two_quarters <- plots$p6 + plots$p12+ plots$p18+ plots$p24+ plots$p30+ plots$p36 + plots$p42 + plots$p48 +
plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_two_quarters
# Very few periods
combined_plot_years <- plots$p3 + plots$p12 + plots$p24 + plots$p36 + plots$p48 + #plots$p60 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_years

# Save the plots ----
# Figure publication probability output
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_output.pdf", 
       combined_plot_years, 
       width = 12, 
       height = 4)
# Alternative more periods (alternative)
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probabiltiy_output_more_periods.pdf",
       combined_plot_two_quarters, 
       width = 16, 
       height = 6)

# All periods (appendix)
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probabiltiy_output_all_periods.pdf", 
       combined_plot_all, 
       width = 36, 
       height = 16)

# For price level ----
out_var <- "inflation"

## Estimation ----
pricelevel_ak <- meta_analysis(d_no_qc,
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
                               AK_plot_prob_y_range = c(0, 12.5)
)
## Create combined plots ----
plots <- list()
for (month in months) {
  plot_name <- paste0("p", month)
  plots[[plot_name]] <- pricelevel_ak[[as.character(month)]]$plot + 
    theme_minimal() + 
    labs(subtitle = paste("Month", month)) + 
    labs(y = "Pub.Pr.")
}
# All quarters
combined_plot_all <- plots$p3 + plots$p6  + plots$p9 + plots$p12 + plots$p15  + plots$p18 + plots$p21 + plots$p24 + plots$p27 + plots$p30 + plots$p33 + plots$p36 + plots$p39 + plots$p42 + plots$p45 + plots$p48 + plots$p51  + plots$p54  + plots$p57 +plots$p60 + 
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_all
# Every two quarters up to 4 years
combined_plot_two_quarters <- plots$p6 + plots$p12+ plots$p18+ plots$p24+ plots$p30+ plots$p36 + plots$p42 + plots$p48 +
plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_two_quarters
# Very few periods
combined_plot_years <- plots$p3 + plots$p12 + plots$p24 + plots$p36 + plots$p48 + #plots$p60 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_years

# Save the plots ----
# Figure publication probability price level (appendix)
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_pricelevel.pdf",
       combined_plot_years, 
       width = 12, 
       height = 4)
# Alternative more periods
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_pricelevel_more_periods.pdf",
       combined_plot_two_quarters, 
       width = 16, 
       height = 6)
# All periods (appendix)
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_pricelevel_all_periods.pdf",
       combined_plot_all, 
       width = 36, 
       height = 16) 

# For the interest rate ----
out_var <- "rate"

## Estimation ----
rate_ak <- meta_analysis(d_no_qc,
                         outvar = out_var,
                         se_option = "avg",
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
                         AK_plot_prob_y_range = c(0, 3)
)
## Create combined plots ----
plots <- list()
for (month in months) {
  plot_name <- paste0("p", month)
  plots[[plot_name]] <- rate_ak[[as.character(month)]]$plot + 
    theme_minimal() + 
    labs(subtitle = paste("Month", month)) + 
    labs(y = "Pub.Pr.")
}
# All quarters
combined_plot_all <- plots$p3 + plots$p6  + plots$p9 + plots$p12 + plots$p15  + plots$p18 + plots$p21 + plots$p24 + plots$p27 + plots$p30 + plots$p33 + plots$p36 + plots$p39 + plots$p42 + plots$p45 + plots$p48 + plots$p51  + plots$p54  + plots$p57 +plots$p60 + 
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_all
# Every two quarters up to 4 years
combined_plot_two_quarters <- plots$p6 + plots$p12+ plots$p18+ plots$p24+ plots$p30+ plots$p36 + plots$p42 + plots$p48 +
plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_two_quarters
# Very few periods
combined_plot_years <- plots$p3 + plots$p12 + plots$p24 + plots$p36 + plots$p48 + #plots$p60 +
  plot_layout(nrow = 1, byrow = TRUE) # As one row
combined_plot_years

# Save the plots ----
# Figure publication probability interest rate (appendix)
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_rate.pdf",
       combined_plot_years, 
       width = 12, 
       height = 4)
# Alternative more periods
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_rate_more_periods.pdf",
       combined_plot_two_quarters, 
       width = 16, 
       height = 6)
# All periods (appendix)
ggsave("analysis/working_paper_1/figures/publication_probability/figure_publication_probability_rate_all_periods.pdf",
       combined_plot_all, 
       width = 36, 
       height = 16) 




