# MMR robustness: Bayesian Model Averaging (BMA) and OLS-FAT-PAT MMR comparison

# MMR OLS with SE (FAT-PET without inverse-variance weights) ----
source(here::here("analysis/working_paper_1/mmr_ols_fatpat_robust.R")) 

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Outcome- and horizon-specific de-meaning of pub_year and num_cit 
d_no_qc <- d_no_qc %>%
  group_by(outcome, period.month) %>%
  mutate(
    pub_year_dm =  pub_year - mean(pub_year, na.rm = TRUE),
    num_cit_dm = num_cit - mean(num_cit, na.rm = TRUE)
  ) %>%
  ungroup()
pub_year_mod <- "pub_year_dm"
pub_year_coef_name <- c("pub_year_dm" = "Publication year")
num_cit_mod <- "num_cit_dm"
num_cit_coef_name <- c("num_cit_dm" = "# citations")

# Load other required libraries ----
library(fastDummies) # Automatic dummy creation
library(JWileymisc) # For winsorizing
library(BMS) # Bayesian model averaging

# Source other required functions ----
source(here::here("analysis/R/bma_loop.R"))
source(here::here("analysis/R/pip_plot.R"))
source(here::here("analysis/R/post_mean_plot.R"))
source(here::here("analysis/R/cond_pos_sign_plot.R"))

# Define general folder path to save figures and their data
save_path <- "analysis/working_paper_1/figures/mmr"

# For output ----

out_var <- "output"

## Select variables for BMA and winsorize ----

bma_data <- d_no_qc %>% 
  filter(outcome == out_var) %>%
  mutate(         
    mean.effect = JWileymisc::winsorizor(mean.effect, wins_para, na.rm = TRUE),
    SE = (JWileymisc::winsorizor(SE.upper, wins_para, na.rm = TRUE))
  ) %>% 
  select(
    period.month, # Necessary for the BMA loop to work
    mean.effect, # (Intercept)
    
    ### P-bias test ###
    SE,
    
    ### Baseline specification ###
    # Consolidated identification approach
    group_ident_broad,
    # Publication characteristics
    cbanker,
    top_5_or_tier,
    
    ### Robustness moderators ###
    # Consolidated estimation methods
    group_est_broad,
    # Consolidated outcome measures
    outcome_measure_output_cons,
    # Consolidated interest rate types
    group_inttype,
    # Publication year (de-meaned)
    pub_year_dm,
    # Number of citations (de-meaned)
    num_cit_dm,
    # Preferred estimate
    prefer,
    # Byproduct 
    byproduct,
    # Data frequency 
    freq,
    # Panel (vs time series)
    panel
  )

## BMA analysis with fixed baseline regressors ----

results_bma_output <- bma_loop(data = bma_data,
                                 # Fixing the baseline variabels to be included in all models
                                 fixed.vars = c(
                                   "group_ident_broad_hf",
                                   "group_ident_broad_nr",
                                   "group_ident_broad_signr",
                                   "group_ident_broad_idother",
                                   "top_5_or_tier",
                                   "cbanker"
                                 ),
                                 burn_ = 1e6,
                                 iter_ = 3e6,
                                 nmodel = 5000,
                                 mprior = "dilut",
                                 g = "UIP"
)
# Save results 
saveRDS(results_bma_output, here::here("analysis/working_paper_1/figures/mmr/bma_output_with_fixed.rds"))
# Load results
# results_bma_output <- readRDS(here::here("analysis/working_paper_1/figures/mmr/bma_output_with_fixed.rds"))

## BMA analysis with no fixed regressors ----

results_bma_output_no_fixed <- bma_loop(data = bma_data,
                               burn_ = 1e6,
                               iter_ = 3e6,
                               nmodel = 5000,
                               mprior = "dilut",
                               g = "UIP"
)
# Save results
saveRDS(results_bma_output_no_fixed, here::here("analysis/working_paper_1/figures/mmr/bma_output_no_fixed.rds"))
# Load results
# results_bma_output_no_fixed <- readRDS(here::here("analysis/working_paper_1/figures/mmr/bma_output_no_fixed.rds"))

## Comparison between BMA and MMR results ----
#### Plots ----
##### Coefficient plots ----
p1 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "(Intercept)", conf_levels = c(0.67, 0.89, 0.97),
                                  custom_title = "Corrected reference response") +
  coord_cartesian(ylim = c(-0.75, 0.25)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Add BMA lines:
p1 <- p1 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "(Intercept)"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "(Intercept)"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 1.5) + 
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "(Intercept)"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "(Intercept)"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p2 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "standarderror_winsor", conf_levels = c(0.67, 0.89, 0.97),
                                  custom_title = "P-bias coefficient (SE)") +
  coord_cartesian(ylim = c(-1.25, 0)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p2 <- p2 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "SE"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "SE"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", size = 1.5) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "SE"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "SE"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

y_lims <- c(-1.5, 0.75)
p3 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_ident_broadhf", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p3 <- p3 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_hf"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_hf"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_hf"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_hf"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p4 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_ident_broadnr", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p4 <- p4 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_nr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_nr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_nr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_nr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p5 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_ident_broadsignr", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p5 <- p5 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_signr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_signr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_signr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_signr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p6 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_ident_broadidother", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p6 <- p6 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_idother"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_ident_broad_idother"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_idother"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_ident_broad_idother"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p7 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "top_5_or_tier", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p7 <- p7 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "top_5_or_tier"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "top_5_or_tier"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "top_5_or_tier"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "top_5_or_tier"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p8 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "cbanker", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p8 <- p8 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "cbanker"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "cbanker"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "cbanker"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "cbanker"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p9 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_est_broadlp_ardl", conf_levels = c(0.67, 0.89, 0.97), 
                                  custom_title = "LP and ARDL") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p9 <- p9 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_lp_ardl"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_lp_ardl"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_lp_ardl"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_lp_ardl"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p10 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_est_broadfavar", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "FAVAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p10 <- p10 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_favar"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_favar"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_favar"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_favar"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p11 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_est_broadother_var", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Other VAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p11 <- p11 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_other_var"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_other_var"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_other_var"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_other_var"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p12 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_est_broaddsge", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "DSGE") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p12 <- p12 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_dsge"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_est_broad_dsge"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_dsge"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_est_broad_dsge"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p13 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "outcome_measure_output_consgap", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Output gap") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p13 <- p13 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "outcome_measure_output_cons_gap"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "outcome_measure_output_cons_gap"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_gap"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_gap"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p14 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "outcome_measure_output_consip", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "IP") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p14 <- p14 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "outcome_measure_output_cons_ip"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "outcome_measure_output_cons_ip"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_ip"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_ip"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p15 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_inttypeweek_month", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "weekly/monthly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p15 <- p15 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_inttype_week_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_inttype_week_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_inttype_week_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_inttype_week_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p16 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "group_inttypeyear", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "yearly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p16 <- p16 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "group_inttype_year"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "group_inttype_year"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_inttype_year"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "group_inttype_year"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p17 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "pub_year_dm", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Publication year") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p17 <- p17 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "pub_year_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "pub_year_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "pub_year_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "pub_year_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p18 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "num_cit_dm", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Number of citations") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p18 <- p18 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "num_cit_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "num_cit_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "num_cit_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "num_cit_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p19 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "prefer", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Preferred") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p19 <- p19 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "prefer"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "prefer"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "prefer"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "prefer"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p20 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "byproduct", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "By-product") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p20 <- p20 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "byproduct"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "byproduct"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "byproduct"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "byproduct"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p21 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "freqmonth", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Monthly") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p21 <- p21 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "freq_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "freq_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "freq_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "freq_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p22 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "freqannual", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Annual") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p22 <- p22 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "freq_annual"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "freq_annual"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "freq_annual"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "freq_annual"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p23 <- create_mmr_coefficient_plot(mmr_output_robust_ols_fatpet, "panel", conf_levels = c(0.67, 0.89, 0.97), 
                                   custom_title = "Panel") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p23 <- p23 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output, "panel"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output, "panel"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_output_no_fixed, "panel"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_output_no_fixed, "panel"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

# Combined plot p1, p2:
(figure_mmr_output_robust_intercept_p_bias <- (p1 + p2) #+
    # plot_annotation(
    #   caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    # )
  )
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_ols_mmr_bma_output_robust_intercept_p_bias.pdf"),
       plot = figure_mmr_output_robust_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 2.5)

# Combine plots as before 
combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) 
(combined_plot <- combined_plot +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_ols_mmr_bma_output_robust_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 6)

# Combine other moderators
(combined_plot_est <- (p9 + p10) / (p11 + p12) / (p13 + p14) / (p15 + p16) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_ols_mmr_bma_output_robust_moderators_est.pdf"),
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


##### PIP plots ----

###### Function for simple PIP plots across horizons ----
create_simple_plot <- function(
    data,                        # Data frame with columns for x and y values
    x_name = "x",                # Name of the x-axis variable
    y_name = "y",                # Name of the y-axis variable
    custom_title = NULL,         # Custom title for the plot
    custom_subtitle = NULL,      # Custom subtitle
    x_lab = "X",                 # X-axis label
    y_lab = "Y",                 # Y-axis label
    line_color = "black",        # Color for the line
    point_color = "black",       # Color for the points
    show_zero_line = TRUE,       # Whether to show the zero reference line
    x_breaks = NULL,             # Custom breaks for x-axis
    y_limits = c(0, 1),     # Custom y-axis limits c(lower, upper)
    point_size = 1,              # Size of points
    line_width = 0.5             # Width of line
) {
  
  # Rename columns for consistent processing
  plot_data <- data
  names(plot_data)[names(plot_data) == x_name] <- "x"
  names(plot_data)[names(plot_data) == y_name] <- "y"
  
  # Ensure x is numeric
  plot_data$x <- as.numeric(as.character(plot_data$x))
  
  # Set title
  if (is.null(custom_title)) {
    title <- "Data Plot"
  } else {
    title <- custom_title
  }
  
  # Set subtitle
  subtitle <- custom_subtitle
  
  # Set x-axis breaks if not provided
  if (is.null(x_breaks)) {
    x_breaks <- seq(min(plot_data$x, na.rm = TRUE), 
                    max(plot_data$x, na.rm = TRUE), 
                    by = max(1, round((max(plot_data$x, na.rm = TRUE) - 
                                         min(plot_data$x, na.rm = TRUE)) / 6)))
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = x)) +
    # Add the values
    geom_line(aes(y = y), color = line_color, linewidth = line_width) +
    geom_point(aes(y = y), color = point_color, size = point_size) +
    # Customize appearance
    theme_minimal() +
    labs(x = x_lab,
         y = y_lab,
         title = title) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = x_breaks)
  
  # Set y-axis limits if provided
  if (!is.null(y_limits) && length(y_limits) == 2) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  # Add subtitle if provided
  if (!is.null(subtitle)) {
    p <- p + labs(subtitle = subtitle) +
      theme(plot.subtitle = element_text(hjust = 0.5))
  }
  
  # Add zero line if requested
  if (show_zero_line) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", 
                        color = "red", alpha = 0.5)
  }
  
  return(p)
}

###### (Intercept)
pip_1 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("(Intercept)"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Intercept",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "(Intercept)"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "(Intercept)"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### SE
pip_2 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("SE"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "SE",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "SE"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "SE"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### HF
pip_3 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_ident_broad_hf"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "HF",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_hf"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_hf"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### NR
pip_4 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_ident_broad_nr"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "NR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_nr"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_nr"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### SignR
pip_5 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_ident_broad_signr"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "SignR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_signr"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_signr"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Other
pip_6 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_ident_broad_idother"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Other",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_idother"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_ident_broad_idother"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Top 5 or Tier
pip_7 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("top_5_or_tier"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Top journal",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "top_5_or_tier"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "top_5_or_tier"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### CB affiliated
pip_8 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("cbanker"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "CB affiliated",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "cbanker"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "cbanker"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### LP & ARDL
pip_9 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_est_broad_lp_ardl"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "LP & ARDL",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_lp_ardl"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_lp_ardl"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### FAVAR
pip_10 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_est_broad_favar"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "FAVAR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_favar"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_favar"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Other VAR
pip_11 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_est_broad_other_var"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Other VAR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_other_var"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_other_var"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### DSGE
pip_12 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_est_broad_dsge"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "DSGE",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_dsge"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_est_broad_dsge"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Output gap
pip_13 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("outcome_measure_output_cons_gap"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Output gap",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_gap"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_gap"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### IP
pip_14 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("outcome_measure_output_cons_ip"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "IP",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_ip"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "outcome_measure_output_cons_ip"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Weekly/monthly rate
pip_15 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_inttype_week_month"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Weekly/monthly rate",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_inttype_week_month"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_inttype_week_month"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Yearly rate
pip_16 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("group_inttype_year"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Yearly rate",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "group_inttype_year"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "group_inttype_year"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Pub year
pip_17 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("pub_year_dm"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Pub year",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "pub_year_dm"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "pub_year_dm"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Num. cit
pip_18 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("num_cit_dm"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Num. cit",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "num_cit_dm"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "num_cit_dm"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Preferred
pip_19 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("prefer"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Preferred",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "prefer"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "prefer"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### By-product
pip_20 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("byproduct"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "By-product",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "byproduct"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "byproduct"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Freq monthly
pip_21 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("freq_month"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Monthly",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "freq_month"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "freq_month"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Freq annual
pip_22 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("freq_annual"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Annual",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "freq_annual"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "freq_annual"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Panel
pip_23 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_output, c("panel"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Panel",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_output_no_fixed, "panel"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_output_no_fixed, "panel"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

# Combined plot pip_1, pip_2:
(figure_bma_output_robust_intercept_p_bias_pip <- (pip_1 + pip_2) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_bma_output_robust_intercept_p_bias_pip.pdf"),
       plot = figure_bma_output_robust_intercept_p_bias_pip,
       device = "pdf",
       width = 7,
       height = 2.5)

# Combine plots as before 
(combined_plot <- (pip_3 + pip_4) / (pip_5 + pip_6) / (pip_7 + pip_8))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_bma_output_robust_moderators_pip.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 6)

# Combine other moderators
(combined_plot_est <- (pip_9 + pip_10) / (pip_11 + pip_12) / (pip_13 + pip_14) / (pip_15 + pip_16) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_bma_output_robust_moderators_est_pip.pdf"),
       plot = combined_plot_est,
       device = "pdf",
       width = 7,
       height = 12)

# Combine remaining moderators
(combined_plot_other <- (pip_17 + pip_18) / (pip_19 + pip_20) / (pip_21 + pip_22) / (pip_23 + empty_plot) +
    plot_annotation(
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))

# For price level ---- 

out_var <- "inflation"

## Select variables for BMA and winsorize ----
bma_data <- d_no_qc %>% 
  filter(outcome == out_var) %>%
  mutate(         
    mean.effect = JWileymisc::winsorizor(mean.effect, wins_para, na.rm = TRUE),
    SE = (JWileymisc::winsorizor(SE.upper, wins_para, na.rm = TRUE))
  ) %>% 
  select(
    period.month, # Necessary for the BMA loop to work
    mean.effect, # (Intercept)
    
    ### P-bias test ###
    SE,
    
    ### Baseline specification ###
    # Consolidated identification approach
    group_ident_broad,
    # Publication characteristics
    cbanker,
    top_5_or_tier,
    ### Robustness moderators ###
    # Consolidated estimation methods
    group_est_broad,
    # Consolidated outcome measures
    outcome_measure_pricelevel_cons,
    # Consolidated interest rate types
    group_inttype,
    # Publication year (de-meaned)
    pub_year_dm,
    # Number of citations (de-meaned)
    num_cit_dm,
    # Preferred estimate
    prefer,
    # Byproduct
    byproduct,
    # Data frequency
    freq,
    # Panel (vs time series)
    panel
  )

## BMA analysis with fixed baseline regressors ----
results_bma_pricelevel <- bma_loop(data = bma_data,
                                 # Fixing the baseline variabels to be included in all models
                                 fixed.vars = c(
                                   "group_ident_broad_hf",
                                   "group_ident_broad_nr",
                                   "group_ident_broad_signr",
                                   "group_ident_broad_idother",
                                   "top_5_or_tier",
                                   "cbanker"
                                 ),
                                 burn_ = 1e6,
                                 iter_ = 3e6,
                                 nmodel = 5000,
                                 mprior = "dilut",
                                 g = "UIP")
# Save results
saveRDS(results_bma_pricelevel, here::here("analysis/working_paper_1/figures/mmr/bma_pricelevel_with_fixed.rds"))
# Load results
# results_bma_pricelevel <- readRDS(here::here("analysis/working_paper_1/figures/mmr/bma_pricelevel_with_fixed.rds"))

## BMA analysis without fixed baseline regressors ----
results_bma_pricelevel_no_fixed <- bma_loop(data = bma_data,
                                            burn_ = 1e6,
                                            iter_ = 3e6,
                                            nmodel = 5000,
                                            mprior = "dilut",
                                            g = "UIP")
# Save results
saveRDS(results_bma_pricelevel_no_fixed, here::here("analysis/working_paper_1/figures/mmr/bma_pricelevel_no_fixed.rds"))
# Load results
# results_bma_pricelevel_no_fixed <- readRDS(here::here("analysis/working_paper_1/figures/mmr/bma_pricelevel_no_fixed.rds"))

## Comparison between BMA and MMR results ---- 

#### Plots ----
##### Coefficient plots ----
p1 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "(Intercept)",
                                  custom_title = "Corrected reference response") +
  coord_cartesian(ylim = c(-0.25, 0.4)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p1 <- p1 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "(Intercept)"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "(Intercept)"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "(Intercept)"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "(Intercept)"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p2 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "standarderror_winsor", 
                                  custom_title = "P-bias coefficient (SE)") +
  coord_cartesian(ylim = c(-1.25, 0)) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p2 <- p2 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "SE"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "SE"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", size = 1.5) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "SE"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "SE"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

y_lims <- c(-1, 0.5)
p3 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_ident_broadhf", 
                                  custom_title = "HF") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p3 <- p3 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_hf"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_hf"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_hf"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_hf"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p4 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_ident_broadnr", 
                                  custom_title = "NR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p4 <- p4 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_nr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_nr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_nr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_nr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p5 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_ident_broadsignr", 
                                  custom_title = "SignR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p5 <- p5 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_signr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_signr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_signr"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_signr"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p6 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_ident_broadidother", 
                                  custom_title = "Other") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p6 <- p6 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_idother"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_ident_broad_idother"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_idother"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_idother"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p7 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "top_5_or_tier", 
                                  custom_title = "Top journal") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p7 <- p7 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "top_5_or_tier"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "top_5_or_tier"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "top_5_or_tier"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "top_5_or_tier"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p8 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "cbanker", 
                                  custom_title = "CB affiliated") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p8 <- p8 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "cbanker"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "cbanker"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "cbanker"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "cbanker"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p9 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_est_broadlp_ardl", 
                                  custom_title = "LP and ARDL") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p9 <- p9 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_lp_ardl"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_lp_ardl"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_lp_ardl"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_lp_ardl"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p10 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_est_broadfavar", 
                                   custom_title = "FAVAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p10 <- p10 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_favar"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_favar"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_favar"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_favar"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p11 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_est_broadother_var", 
                                   custom_title = "Other VAR") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p11 <- p11 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_other_var"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_other_var"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_other_var"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_other_var"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p12 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_est_broaddsge", 
                                   custom_title = "DSGE") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p12 <- p12 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_dsge"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_est_broad_dsge"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_dsge"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_est_broad_dsge"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p13 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "outcome_measure_pricelevel_conscore", 
                                   custom_title = "Core") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p13 <- p13 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "outcome_measure_pricelevel_cons_core"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "outcome_measure_pricelevel_cons_core"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_core"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_core"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p14 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "outcome_measure_pricelevel_consdeflator", 
                                   custom_title = "Deflator") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p14 <- p14 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "outcome_measure_pricelevel_cons_deflator"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "outcome_measure_pricelevel_cons_deflator"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_deflator"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_deflator"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p15 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "outcome_measure_pricelevel_conswpi", 
                                   custom_title = "WPI") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p15 <- p15 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "outcome_measure_pricelevel_cons_wpi"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "outcome_measure_pricelevel_cons_wpi"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_wpi"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_wpi"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p16 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_inttypeweek_month", 
                                   custom_title = "weekly/monthly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p16 <- p16 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_inttype_week_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_inttype_week_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_inttype_week_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_inttype_week_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p17 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "group_inttypeyear", 
                                   custom_title = "yearly rate") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p17 <- p17 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "group_inttype_year"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "group_inttype_year"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_inttype_year"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "group_inttype_year"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p18 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "pub_year_dm", 
                                   custom_title = "Publication year") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p18 <- p18 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "pub_year_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "pub_year_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "pub_year_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "pub_year_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p19 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "num_cit_dm", 
                                   custom_title = "Number of citations") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p19 <- p19 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "num_cit_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "num_cit_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "num_cit_dm"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "num_cit_dm"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p20 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "prefer", 
                                   custom_title = "Preferred") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p20 <- p20 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "prefer"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "prefer"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "prefer"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "prefer"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p21 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "byproduct", 
                                   custom_title = "By-product") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p21 <- p21 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "byproduct"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "byproduct"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "byproduct"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "byproduct"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p22 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "freqmonth", 
                                   custom_title = "Monthly") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p22 <- p22 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "freq_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "freq_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "freq_month"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "freq_month"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p23 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "freqannual", 
                                   custom_title = "Annual") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p23 <- p23 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "freq_annual"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "freq_annual"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "freq_annual"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "freq_annual"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

p24 <- create_mmr_coefficient_plot(mmr_pricelevel_robust_ols_fatpet, "panel", 
                                   custom_title = "Panel") +
  coord_cartesian(ylim = y_lims) +
  # No subtitle
  theme(plot.subtitle = element_blank()) +
  # No axis labels
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# Add BMA lines:
p24 <- p24 +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel, "panel"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel, "panel"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "red", linetype = "dotdash", linewidth = 2) +
    geom_line(data = data.frame(period = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "panel"))$Period, 
                                post_mean = plotly_data(post_mean_plot(results_bma_pricelevel_no_fixed, "panel"))$`Post Mean`),
              aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

# Combined plot p1, p2:
(figure_mmr_pricelevel_robust_intercept_p_bias <- (p1 + p2) #+
    # plot_annotation(
    #   caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    # )
  )
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_ols_mmr_bma_pricelevel_robust_intercept_p_bias.pdf"),
       plot = figure_mmr_pricelevel_robust_intercept_p_bias,
       device = "pdf",
       width = 7,
       height = 2.5)

# Combine plots as before
(combined_plot <- (p3 + p4) / (p5 + p6) / (p7 + p8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5)),
      caption = "Shaded areas show 67%, 89% and 97% confidence intervals"
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_ols_mmr_bma_pricelevel_robust_moderators.pdf"),
       plot = combined_plot,
       device = "pdf",
       width = 7,
       height = 6)

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
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_ols_mmr_bma_pricelevel_robust_moderators_est.pdf"),
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

##### PIP plot ----

###### (Intercept)
pip_1 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("(Intercept)"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Intercept",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "(Intercept)"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "(Intercept)"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### SE
pip_2 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("SE"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "SE",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "SE"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "SE"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### HF
pip_3 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_ident_broad_hf"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "HF",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_hf"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_hf"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### NR
pip_4 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_ident_broad_nr"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "NR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_nr"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_nr"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### SignR
pip_5 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_ident_broad_signr"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "SignR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_signr"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_signr"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Other
pip_6 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_ident_broad_idother"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Other",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_idother"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_ident_broad_idother"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Top 5 or tier
pip_7 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("top_5_or_tier"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Top journal",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "top_5_or_tier"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "top_5_or_tier"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### CB affiliated
pip_8 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("cbanker"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "CB affiliated",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "cbanker"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "cbanker"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### LP and ARDL
pip_9 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_est_broad_lp_ardl"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "LP and ARDL",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_lp_ardl"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_lp_ardl"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### FAVAR
pip_10 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_est_broad_favar"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "FAVAR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_favar"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_favar"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Other VAR
pip_11 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_est_broad_other_var"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Other VAR",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_other_var"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_other_var"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### DSGE
pip_12 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_est_broad_dsge"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "DSGE",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_dsge"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_est_broad_dsge"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Core
pip_13 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("outcome_measure_pricelevel_cons_core"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Core",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_core"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_core"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Deflator
pip_14 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("outcome_measure_pricelevel_cons_deflator"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Deflator",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_deflator"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_deflator"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### WPI
pip_15 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("outcome_measure_pricelevel_cons_wpi"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "WPI",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_wpi"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "outcome_measure_pricelevel_cons_wpi"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Weekly/monthly rate
pip_16 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_inttype_week_month"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Weekly/monthly rate",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_inttype_week_month"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_inttype_week_month"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Yearly rate
pip_17 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("group_inttype_year"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Yearly rate",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_inttype_year"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "group_inttype_year"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Publication year
pip_18 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("pub_year_dm"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Publication year",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "pub_year_dm"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "pub_year_dm"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Number of citations
pip_19 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("num_cit_dm"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Number of citations",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "num_cit_dm"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "num_cit_dm"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Preferred
pip_20 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("prefer"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Preferred",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "prefer"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "prefer"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### By-product
pip_21 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("byproduct"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "By-product",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "byproduct"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "byproduct"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Monthly
pip_22 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("freq_month"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Monthly",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "freq_month"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "freq_month"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Annual
pip_23 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("freq_annual"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Annual",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "freq_annual"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "freq_annual"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

###### Panel
pip_24 <- create_simple_plot(
  data = plotly_data(pip_plot(results_bma_pricelevel, c("panel"))),
  x_name = "Period",
  y_name = "PIP",
  custom_title = "Panel",
  # custom_subtitle = "Sample Data",
  line_color = "red",        
  point_color = "red",       
  x_lab = "Month",
  y_lab = "PIP"
) + 
  geom_line(data = data.frame(period = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "panel"))$Period, 
                              post_mean = plotly_data(pip_plot(results_bma_pricelevel_no_fixed, "panel"))$PIP),
            aes(x = period, y = post_mean), color = "blue", linetype = "longdash", linewidth = 1)

# Combined plot p1, p2:
(figure_pricelevel_robust_intercept_p_bias_pip <- (pip_1 + pip_2))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_pricelevel_robust_intercept_p_bias_pip.pdf"),
       plot = figure_pricelevel_robust_intercept_p_bias_pip,
       device = "pdf",
       width = 7,
       height = 2)

# Combine plots as before
(combined_plot_pip <- (pip_3 + pip_4) / (pip_5 + pip_6) / (pip_7 + pip_8) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5))
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_pricelevel_robust_moderators_pip.pdf"),
       plot = combined_plot_pip,
       device = "pdf",
       width = 7,
       height = 6)

# Combine other moderators
(combined_plot_est_pip <- (pip_9 + pip_10) / (pip_11 + pip_12) / (pip_16 + pip_17) / (pip_13 + pip_14) / (pip_15 + empty_plot) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5))
    ))
# Save as pdf
ggsave(here::here("analysis/working_paper_1/figures/mmr/figure_pricelevel_robust_moderators_est_pip.pdf"),
       plot = combined_plot_est_pip,
       device = "pdf",
       width = 7,
       height = 12)

# Combine remaining moderators
(combined_plot_other_pip <- (pip_18 + pip_19) / (pip_20 + pip_21) / (pip_22 + pip_23) / (pip_24 + empty_plot) +
    plot_annotation(
      theme = theme(plot.title = element_text(hjust = 0.5))
    ))

