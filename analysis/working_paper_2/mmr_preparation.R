# Preparation for MMR (baseline and robustness) and BMA exercises

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/setup_wp_2.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing
library(patchwork) # For combining plots
library(viridis) # For color palette in funnel plots

# Source required functions ----
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/create_mmr_coefficient_plot.R"))
source((here::here("analysis/R/funnel_plot.R")))
source(here::here("analysis/R/plot_average_irfs.R"))

# Define periods for estimation ----
chosen_periods <- seq(0, 60, by = 3)
chosen_periods_tables <- c(
  3,
  6,
  12,
  18,
  24,
  30,
  36,
  42,
  48,
  54,
  60
)

# Define coefficient names and moderator variables ----
## Baseline model ----
### FAT-PET coefficient names 
fatpet_coef_name <- c(
  "(Intercept)" = "Intercept",
  "standarderror_winsor"= "SE"
)
### PEESE coefficient names 
peese_coef_name <- c(
  "(Intercept)" = "Intercept",
  "variance_winsor"= "Variance"
)
### Consolidated identification methods
ident_mod <- "group_ident_broad"
ident_coef_name <- c(
  'group_ident_broadhf' = 'High frequency', 
  'group_ident_broadnr' = 'Narrative',
  'group_ident_broadsignr' = 'Sign restrictions',
  'group_ident_broadidother' = 'Other identificiation '
)
### Top journal dummy 
top_journal_mod <- c("top_5_or_tier") 
top_journal_coef_name <- c('top_5_or_tier' = 'Top tier publication')
### CB affiliation dummy
cb_mod <- c("cbanker")
cb_coef_name <- c('cbanker' = 'Central bank related')
### Baseline OLS-FAT-PET and WLS-PEESE model combination 
baseline_mods <- c(ident_mod, top_journal_mod, cb_mod)
fatpet_coef_names <- c(fatpet_coef_name, ident_coef_name, top_journal_coef_name, cb_coef_name)
peese_coef_names <- c(peese_coef_name, ident_coef_name, top_journal_coef_name, cb_coef_name)
## Robustness models ----
### Consolidated estimation methods
est_mod <- "group_est_broad"
est_coef_name <- c(
  'group_est_broadlp_ardl' = 'LP and ARDL',
  'group_est_broadfavar' = 'FAVAR', 
  'group_est_broadother_var' = 'Other VAR',
  'group_est_broaddsge' = 'DSGE'
)
### Consolidated outcome measures
output_measure <- "outcome_measure_output_cons"
output_measure_coef_name <- c(
  'outcome_measure_output_consip' = 'Ind. prod.',
  'outcome_measure_output_consgap' = 'Output gap')
pricelevel_measure <- "outcome_measure_pricelevel_cons"
pricelevel_measure_coef_name <- c(
  'outcome_measure_pricelevel_consdeflator' = 'Deflator',
  'outcome_measure_pricelevel_conswpi' = 'WPI',
  'outcome_measure_pricelevel_conscore' = 'Core')
### Consolidated interest rate types
int_rate_mod <- "group_inttype"
int_rate_coef_name <- c(
  'group_inttypeweek_month' = 'weekly/monthly rate',
  'group_inttypeyear' = 'yearly rate'
)
### Publication year and number of citations
#### Outcome- and horizon-specific de-meaning of pub_year and num_cit 
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
### Preferred estimate
prefer_mod <- "prefer"
prefer_coef_name <- c("prefer" = "Preferred estimate")
### Byproduct 
byproduct_mod <- "byproduct"
byproduct_coef_name <- c("byproduct" = "By-product")
### Data frequency 
freq_mod <- "freq"
freq_coef_name <- c("freqmonth" = "Monthly data",
                    "freqannual" = "Yearly data")
### Panel (vs time series)
panel_mod <- "panel"
panel_coef_name <- c("panel" = "Panel")

### Robustness model combinations
robustness_mods_output <- c(est_mod, 
                            output_measure, 
                            int_rate_mod, 
                            pub_year_mod,
                            num_cit_mod,
                            prefer_mod,
                            byproduct_mod,
                            freq_mod,
                            panel_mod)
robusntess_coef_names_output <- c(est_coef_name, 
                                  output_measure_coef_name, 
                                  int_rate_coef_name, 
                                  pub_year_coef_name,
                                  num_cit_coef_name,
                                  prefer_coef_name,
                                  byproduct_coef_name,
                                  freq_coef_name,
                                  panel_coef_name)
robustness_mods_pricelevel <- c(est_mod, 
                                pricelevel_measure, 
                                int_rate_mod,
                                pub_year_mod,
                                num_cit_mod,
                                prefer_mod,
                                byproduct_mod,
                                freq_mod,
                                panel_mod)
robusntess_coef_names_pricelevel <- c(est_coef_name, 
                                      int_rate_coef_name, 
                                      pricelevel_measure_coef_name,
                                      pub_year_coef_name,
                                      num_cit_coef_name,
                                      prefer_coef_name,
                                      byproduct_coef_name,
                                      freq_coef_name,
                                      panel_coef_name)

# Hardcode the maximum of period 0 precision to allow estimation ---- 
## Create vectors of the columns we want to process ----

se_cols <- c("SE.avg", "SE.upper", "SE.lower")
precision_cols <- c("precision.avg", "precision.upper", "precision.lower")

for (outcome in unique(d_no_qc$outcome)) {
  # Subset data for this outcome where period.month == 1
  subset_data <- d_no_qc[d_no_qc$period.month == 1 & d_no_qc$outcome == outcome, ]
  
  # Process SE columns (check against minimums)
  for (col in se_cols) {
    winsorized_values <- winsorizor(subset_data[[col]], percentile = wins_para)
    min_value <- min(winsorized_values)
    
    # Only overwrite values that are below the minimum
    mask <- d_no_qc$period.month == 0 &
      d_no_qc$outcome == outcome &
      d_no_qc[[col]] < 2*min_value
    
    d_no_qc[mask, col] <- 2*min_value
  }
  
  # Process precision columns (check against maximums)
  for (col in precision_cols) {
    winsorized_values <- winsorizor(subset_data[[col]], percentile = wins_para)
    max_value <- max(winsorized_values)
    
    # Only overwrite values that are above the maximum
    mask <- d_no_qc$period.month == 0 &
      d_no_qc$outcome == outcome &
      d_no_qc[[col]] > 2*max_value
    
    d_no_qc[mask, col] <- 2*max_value
  }
}
