# Creates tables and plots with randomization test results at different horizons

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/setup_wp_2.R"))

# Source required functions ----
source(here::here("analysis/R/data_prep_counterfactual.R"))
source(here::here("analysis/R/run_randomization_test.R"))
source(here::here("analysis/R/randomization_test_battery.R"))
source(here::here("analysis/R/format_randomization_table.R"))
source(here::here("analysis/R/plot_randomization_tests.R"))

# Define windows ----
deltas <- seq(0.5, 0.05, by = -0.005)

# Define ylims ----
ylims <- c(0, 1)

# For employment ----
out_var <- "emp"

## Data prep ----
d_z_stat_neg_emp <- data_prep_counterfactual(d_no_qc,
                                                outvar = out_var,
                                                se_option = "upper", 
                                                periods = seq(0, max(d_no_qc$period.month), 1),
                                                wins = wins_para,
                                                only_negative = TRUE,
                                                only_positive = FALSE)

### Add stylized horizons
d_z_stat_neg_emp <- d_z_stat_neg_emp %>% 
  mutate(horizon = case_when(
    period.month < vsr ~ horizons[1],
    (period.month >= vsr & period.month <= smr) ~ horizons[2],
    (period.month > smr & period.month <= mlr) ~ horizons[3],
    period.month > mlr ~ horizons[4]
  ),
  horizon = factor(horizon, levels = horizons)
  )

## Full sample ----

### Test at 68 % level ----
random_test_emp_68 <- randomization_test_battery(
  data = d_z_stat_neg_emp,
  statistic = "abs_z_stat",
  thresholds = c(1), # 68 % level 
  group = "horizon",
  deltas = deltas
) 
#### Format and save table 
table_random_test_emp_68 <- format_randomization_table(random_test_emp_68,
                                         group_order = horizons)
#### Visualize 
(figure_random_test_emp_68 <- plot_randomization_tests(
  table_random_test_emp_68,
  group_order = horizons,
  threshold = "-1 (68 % level) - Employment",
  y_limits = ylims
))
#### Save figure as pdf 
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_emp_68.pdf", 
       figure_random_test_emp_68, 
       width = 12, 
       height = 8)

### Test at 90 % level ----
random_test_emp_90 <- randomization_test_battery(
  data = d_z_stat_neg_emp,
  statistic = "abs_z_stat",
  thresholds = c(1.645), # 90 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_emp_90 <- format_randomization_table(random_test_emp_90,
                                         group_order = horizons)
#### Visualize
(figure_random_test_emp_90 <- plot_randomization_tests(
  table_random_test_emp_90,
  group_order = horizons,
  threshold = "-1.645 (90 % level) - Employment",
  y_limits = ylims
))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_emp_90.pdf", 
       figure_random_test_emp_90, 
       width = 12, 
       height = 8)

### Test at 95 % level ----
random_test_emp_95 <- randomization_test_battery(
  data = d_z_stat_neg_emp,
  statistic = "abs_z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_emp_95 <- format_randomization_table(random_test_emp_95,
                                         group_order = horizons)
#### Visualize
(figure_random_test_emp_95 <- plot_randomization_tests(
  table_random_test_emp_95,
  group_order = horizons,
  threshold = "-1.96 (95 % level) - Employment",
  y_limits = ylims
))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_emp_95.pdf", 
       figure_random_test_emp_95, 
       width = 12, 
       height = 8)

### Test at 99 % level ----
random_test_emp_99 <- randomization_test_battery(
  data = d_z_stat_neg_emp,
  statistic = "abs_z_stat",
  thresholds = c(2.576), # 99 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_emp_99 <- format_randomization_table(random_test_emp_99,
                                         group_order = horizons)
#### Visualize
(figure_random_test_emp_99 <- plot_randomization_tests(
  table_random_test_emp_99,
  group_order = horizons,
  threshold = "2.576 (99 % level) - Employment",
  y_limits = ylims
))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_emp_99.pdf", 
       figure_random_test_emp_99, 
       width = 12, 
       height = 8)

# For unemployment rate ----
out_var <- "unemp"

## Data prep ----
d_z_stat_neg_unemp <- data_prep_counterfactual(d_no_qc,
                                                outvar = out_var,
                                                se_option = "lower",
                                                periods = seq(0, max(d_no_qc$period.month), 1),
                                                wins = wins_para,
                                                only_negative = TRUE,
                                                only_positive = FALSE)

### Add stylized horizons
d_z_stat_neg_unemp <- d_z_stat_neg_unemp %>% 
  mutate(horizon = case_when(
    period.month < vsr ~ horizons[1],
    (period.month >= vsr & period.month <= smr) ~ horizons[2],
    (period.month > smr & period.month <= mlr) ~ horizons[3],
    period.month > mlr ~ horizons[4]
  ),
  horizon = factor(horizon, levels = horizons)
  )

## Full sample ----

### Test at 68 % level ----
random_test_unemp_68 <- randomization_test_battery(
  data = d_z_stat_neg_unemp,
  statistic = "abs_z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_unemp_68 <- format_randomization_table(random_test_unemp_68,
                                         group_order = horizons)
#### Visualize
(figure_random_test_unemp_68 <- plot_randomization_tests(
  table_random_test_unemp_68,
  group_order = horizons,
  threshold = "-1 (68 % level) - Unemployment",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_unemp_68.pdf", 
       figure_random_test_unemp_68, 
       width = 12, 
       height = 8)

### Test at 90 % level ----
random_test_unemp_90 <- randomization_test_battery(
  data = d_z_stat_neg_unemp,
  statistic = "abs_z_stat",
  thresholds = c(1.645), # 90 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_unemp_90 <- format_randomization_table(random_test_unemp_90,
                                         group_order = horizons)
#### Visualize
(figure_random_test_unemp_90 <- plot_randomization_tests(
  table_random_test_unemp_90,
  group_order = horizons,
  threshold = "-1.645 (90 % level) - Unemployment",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_unemp_90.pdf", 
       figure_random_test_unemp_90, 
       width = 12, 
       height = 8)

### Test at 95 % level ----
random_test_unemp_95 <- randomization_test_battery(
  data = d_z_stat_neg_unemp,
  statistic = "abs_z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_unemp_95 <- format_randomization_table(random_test_unemp_95,
                                         group_order = horizons)
#### Visualize
(figure_random_test_unemp_95 <- plot_randomization_tests(
  table_random_test_unemp_95,
  group_order = horizons,
  threshold = "-1.96 (95 % level) - Unemployment",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_unemp_95.pdf", 
       figure_random_test_unemp_95, 
       width = 12, 
       height = 8)

### Test at 99 % level ----
random_test_unemp_99 <- randomization_test_battery(
  data = d_z_stat_neg_unemp,
  statistic = "abs_z_stat",
  thresholds = c(2.576), # 99 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_unemp_99 <- format_randomization_table(random_test_unemp_99,
                                         group_order = horizons)
#### Visualize
(figure_random_test_unemp_99 <- plot_randomization_tests(
  table_random_test_unemp_99,
  group_order = horizons,
  threshold = "2.576 (99 % level) - Unemployment",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_2/figures/randomization_tests/figure_random_test_unemp_99.pdf", 
       figure_random_test_unemp_99, 
       width = 12, 
       height = 8)
