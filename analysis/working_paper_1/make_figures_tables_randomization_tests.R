# Creates tables and plots with randomization test results at different horizons

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Source required functions ----
source(here::here("analysis/R/data_prep_counterfactual.R"))
source(here::here("analysis/R/run_randomization_test.R"))
source(here::here("analysis/R/randomization_test_battery.R"))
source(here::here("analysis/R/format_randomization_table.R"))
source(here::here("analysis/R/plot_randomization_tests.R"))

# Define windows ----
deltas <- seq(0.5, 0.005, by = -0.005)

# Define ylims ----
ylims <- c(0, 1)

# For output ----
out_var <- "output"

## Data prep ----
d_z_stat_neg_output <- data_prep_counterfactual(d_no_qc,
                                                outvar = out_var,
                                                se_option = "upper", # "upper" is important for results (to comnpare use "avg")
                                                periods = seq(0, max(d_no_qc$period.month), 1),
                                                wins = wins_para,
                                                only_negative = TRUE,
                                                only_positive = FALSE)

### Add stylized horizons
d_z_stat_neg_output <- d_z_stat_neg_output %>% 
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
random_test_output_68 <- randomization_test_battery(
  data = d_z_stat_neg_output,
  statistic = "z_stat",
  thresholds = c(1), # 68 % level 
  group = "horizon",
  deltas = deltas
) 
#### Format and save table 
table_random_test_output_68 <- format_randomization_table(random_test_output_68,
                                         group_order = horizons)
#### Visualize 
(figure_random_test_output_68 <- plot_randomization_tests(
  table_random_test_output_68,
  group_order = horizons,
  threshold = "1 (68 % level) - Output",
  y_limits = ylims
))
#### Save figure as pdf 
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68.pdf", 
       figure_random_test_output_68, 
       width = 12, 
       height = 8)

### Test at 90 % level ----
random_test_output_90 <- randomization_test_battery(
  data = d_z_stat_neg_output,
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_90 <- format_randomization_table(random_test_output_90,
                                         group_order = horizons)
#### Visualize
(figure_random_test_output_90 <- plot_randomization_tests(
  table_random_test_output_90,
  group_order = horizons,
  threshold = "1.645 (90 % level) - Output",
  y_limits = ylims
))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_90.pdf", 
       figure_random_test_output_90, 
       width = 12, 
       height = 8)

### Test at 95 % level ----
random_test_output_95 <- randomization_test_battery(
  data = d_z_stat_neg_output,
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_95 <- format_randomization_table(random_test_output_95,
                                         group_order = horizons)
#### Visualize
(figure_random_test_output_95 <- plot_randomization_tests(
  table_random_test_output_95,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Output",
  y_limits = ylims
))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95.pdf", 
       figure_random_test_output_95, 
       width = 12, 
       height = 8)

### Test at 99 % level ----
random_test_output_99 <- randomization_test_battery(
  data = d_z_stat_neg_output,
  statistic = "z_stat",
  thresholds = c(2.576), # 99 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_99 <- format_randomization_table(random_test_output_99,
                                         group_order = horizons)
#### Visualize
(figure_random_test_output_99 <- plot_randomization_tests(
  table_random_test_output_99,
  group_order = horizons,
  threshold = "2.576 (99 % level) - Output",
  y_limits = ylims
))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_99.pdf", 
       figure_random_test_output_99, 
       width = 12, 
       height = 8)

## Top journals vs other publications ----

### For top journals only ----

#### Test at 68 % level ----
random_test_output_68_top_journals <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(top_5_or_tier == 1),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_68_top_journals <- format_randomization_table(random_test_output_68_top_journals,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_68_top_journals <- plot_randomization_tests(
  table_random_test_output_68_top_journals,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - Top journals",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_top_journals.pdf",
       figure_random_test_output_68_top_journals,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_output_95_top_journals <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(top_5_or_tier == 1),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_95_top_journals <- format_randomization_table(random_test_output_95_top_journals,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_95_top_journals <- plot_randomization_tests(
  table_random_test_output_95_top_journals,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Output - Top journals",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95_top_journals.pdf",
       figure_random_test_output_68_top_journals,
       width = 12, 
       height = 8)

### For other publications only ----

#### Test at 68 % level ----
random_test_output_68_other_publications <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(top_5_or_tier == 0),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_68_other_publications <- format_randomization_table(random_test_output_68_other_publications,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_68_other_publications <- plot_randomization_tests(
  table_random_test_output_68_other_publications,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - Other publications",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_other_publications.pdf",
       figure_random_test_output_68_other_publications,
       width = 12, 
       height = 8)
#### Save as vector graphic
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_other_publications.pdf",
       figure_random_test_output_68_other_publications,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_output_95_other_publications <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(top_5_or_tier == 0),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_95_other_publications <- format_randomization_table(random_test_output_95_other_publications,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_95_other_publications <- plot_randomization_tests(
  table_random_test_output_95_other_publications,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Output - Other publications",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95_other_publications.pdf",
       figure_random_test_output_95_other_publications,
       width = 12, 
       height = 8)

#### Joining plots 68
figure_random_test_output_68_top_vs_other <- gridExtra::grid.arrange(figure_random_test_output_68_other_publications, 
                                                                figure_random_test_output_68_top_journals, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_top_vs_other.pdf",
       figure_random_test_output_68_top_vs_other,
       width = 24, 
       height = 12)

#### Joining plots 95
figure_random_test_output_95_top_vs_other <- gridExtra::grid.arrange(figure_random_test_output_95_other_publications, 
                                                                figure_random_test_output_95_top_journals, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95_top_vs_other.pdf",
       figure_random_test_output_95_top_vs_other,
       width = 24, 
       height = 12)

## cbanker vs non-cbanker ----

### For cbanker only ----

#### Test at 68 % level ----
random_test_output_68_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(cbanker == 1),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_68_cbanker <- format_randomization_table(random_test_output_68_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_68_cbanker <- plot_randomization_tests(
  table_random_test_output_68_cbanker,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - Central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_cbanker.pdf",
       figure_random_test_output_68_cbanker,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_output_95_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(cbanker == 1),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_95_cbanker <- format_randomization_table(random_test_output_95_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_95_cbanker <- plot_randomization_tests(
  table_random_test_output_95_cbanker,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Output - Central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95_cbanker.pdf",
       figure_random_test_output_95_cbanker,
       width = 12, 
       height = 8)

### For non-cbanker only ----

#### Test at 68 % level ----
random_test_output_68_non_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(cbanker == 0),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_68_non_cbanker <- format_randomization_table(random_test_output_68_non_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_68_non_cbanker <- plot_randomization_tests(
  table_random_test_output_68_non_cbanker,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - Non-central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_non_cbanker.pdf",
       figure_random_test_output_68_non_cbanker,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_output_95_non_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(cbanker == 0),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_95_non_cbanker <- format_randomization_table(random_test_output_95_non_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_95_non_cbanker <- plot_randomization_tests(
  table_random_test_output_95_non_cbanker,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Output - Non-central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95_non_cbanker.pdf",
       figure_random_test_output_95_non_cbanker,
       width = 12, 
       height = 8)

#### Joining plots 68
figure_random_test_output_68_cbanker_vs_non_cbanker <- gridExtra::grid.arrange(figure_random_test_output_68_cbanker, 
                                                                figure_random_test_output_68_non_cbanker, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_cbanker_vs_non_cbanker.pdf",
       figure_random_test_output_68_cbanker_vs_non_cbanker,
       width = 24, 
       height = 12)

#### Joining plots 95
figure_random_test_output_95_cbanker_vs_non_cbanker <- gridExtra::grid.arrange(figure_random_test_output_95_non_cbanker,
                                                                    figure_random_test_output_95_cbanker, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_95_cbanker_vs_non_cbanker.pdf",
       figure_random_test_output_95_cbanker_vs_non_cbanker,
       width = 24, 
       height = 12)

### For different identification strategies ----

#### For hf only ---- 

##### Test at 68 % level ----
random_test_output_68_hf <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(group_ident_broad == "hf"),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_68_hf <- format_randomization_table(random_test_output_68_hf,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_68_hf <- plot_randomization_tests(
  table_random_test_output_68_hf,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - HF",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_hf.pdf",
       figure_random_test_output_68_hf,
       width = 12, 
       height = 8)

#### For nr only ----

##### Test at 68 % level ----
random_test_output_68_nr <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(group_ident_broad == "nr"),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_output_68_nr <- format_randomization_table(random_test_output_68_nr,
                                          group_order = horizons)
#### Visualize
(figure_random_test_output_68_nr <- plot_randomization_tests(
  table_random_test_output_68_nr,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - NR",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_nr.pdf",
       figure_random_test_output_68_nr,
       width = 12, 
       height = 8)

##### Test at 90 % level ----
random_test_output_90_nr <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(group_ident_broad == "nr"),
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = "horizon",
  deltas = deltas
)
##### Format and save table
table_random_test_output_90_nr <- format_randomization_table(random_test_output_90_nr,
                                          group_order = horizons)
##### Visualize
(figure_random_test_output_90_nr <- plot_randomization_tests(
  table_random_test_output_90_nr,
  group_order = horizons,
  threshold = "1.645 (90 % level) - Output - NR",
  y_limits = ylims))
##### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_90_nr.pdf",
       figure_random_test_output_90_nr,
       width = 12, 
       height = 8)

#### For chol only ----

##### Test at 68 % level ----
random_test_output_68_chol <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(group_ident_broad == "chol"),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
##### Format and save table
table_random_test_output_68_chol <- format_randomization_table(random_test_output_68_chol,
                                          group_order = horizons)
##### Visualize
(figure_random_test_output_68_chol <- plot_randomization_tests(
  table_random_test_output_68_chol,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - chol",
  y_limits = ylims))
##### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_chol.pdf",
       figure_random_test_output_68_chol,
       width = 12, 
       height = 8)

#### For signr only ----

##### Test at 68 % level ----
random_test_output_68_signr <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(group_ident_broad == "signr"),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
##### Format and save table
table_random_test_output_68_signr <- format_randomization_table(random_test_output_68_signr,
                                          group_order = horizons)
##### Visualize
(figure_random_test_output_68_signr <- plot_randomization_tests(
  table_random_test_output_68_signr,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - signr",
  y_limits = ylims))
##### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_signr.pdf",
       figure_random_test_output_68_signr,
       width = 12, 
       height = 8)

#### For idother only ----

##### Test at 68 % level ----
random_test_output_68_idother <- randomization_test_battery(
  data = d_z_stat_neg_output %>% filter(group_ident_broad == "idother"),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
##### Format and save table
table_random_test_output_68_idother <- format_randomization_table(random_test_output_68_idother,
                                          group_order = horizons)
##### Visualize
(figure_random_test_output_68_idother <- plot_randomization_tests(
  table_random_test_output_68_idother,
  group_order = horizons,
  threshold = "1 (68 % level) - Output - idother",
  y_limits = ylims))
##### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_output_68_idother.pdf",
       figure_random_test_output_68_idother,
       width = 12, 
       height = 8)

# For price level ----
out_var <- "inflation"

## Data prep ----
d_z_stat_neg_pricelevel <- data_prep_counterfactual(d_no_qc,
                                                outvar = out_var,
                                                se_option = "upper", # check if "upper" is important for results (to comnpare use "avg")
                                                periods = seq(0, max(d_no_qc$period.month), 1),
                                                wins = wins_para,
                                                only_negative = TRUE,
                                                only_positive = FALSE)

### Add stylized horizons
d_z_stat_neg_pricelevel <- d_z_stat_neg_pricelevel %>% 
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
random_test_pricelevel_68 <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel,
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_68 <- format_randomization_table(random_test_pricelevel_68,
                                         group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_68 <- plot_randomization_tests(
  table_random_test_pricelevel_68,
  group_order = horizons,
  threshold = "1 (68 % level) - Price level",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68.pdf", 
       figure_random_test_pricelevel_68, 
       width = 12, 
       height = 8)

### Test at 90 % level ----
random_test_pricelevel_90 <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel,
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_90 <- format_randomization_table(random_test_pricelevel_90,
                                         group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_90 <- plot_randomization_tests(
  table_random_test_pricelevel_90,
  group_order = horizons,
  threshold = "1.645 (90 % level) - Price level",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_90.pdf", 
       figure_random_test_pricelevel_90, 
       width = 12, 
       height = 8)

### Test at 95 % level ----
random_test_pricelevel_95 <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel,
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_95 <- format_randomization_table(random_test_pricelevel_95,
                                         group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_95 <- plot_randomization_tests(
  table_random_test_pricelevel_95,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Price level",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95.pdf", 
       figure_random_test_pricelevel_95, 
       width = 12, 
       height = 8)

### Test at 99 % level ----
random_test_pricelevel_99 <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel,
  statistic = "z_stat",
  thresholds = c(2.576), # 99 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_99 <- format_randomization_table(random_test_pricelevel_99,
                                         group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_99 <- plot_randomization_tests(
  table_random_test_pricelevel_99,
  group_order = horizons,
  threshold = "2.576 (99 % level) - Price level",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_99.pdf", 
       figure_random_test_pricelevel_99, 
       width = 12, 
       height = 8)

## Top journals vs other publications ----

### For top journals only ----

#### Test at 68 % level ----
random_test_pricelevel_68_top_journals <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 1),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_68_top_journals <- format_randomization_table(random_test_pricelevel_68_top_journals,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_68_top_journals <- plot_randomization_tests(
  table_random_test_pricelevel_68_top_journals,
  group_order = horizons,
  threshold = "1 (68 % level) - Price level - Top journals",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68_top_journals.pdf",
       figure_random_test_pricelevel_68_top_journals,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_pricelevel_95_top_journals <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 1),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_95_top_journals <- format_randomization_table(random_test_pricelevel_95_top_journals,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_95_top_journals <- plot_randomization_tests(
  table_random_test_pricelevel_95_top_journals,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Price level - Top journals",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95_top_journals.pdf",
       figure_random_test_pricelevel_95_top_journals,
       width = 12, 
       height = 8)

### For other publications only ----

#### Test at 68 % level ----
random_test_pricelevel_68_other_publications <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 0),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_68_other_publications <- format_randomization_table(random_test_pricelevel_68_other_publications,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_68_other_publications <- plot_randomization_tests(
  table_random_test_pricelevel_68_other_publications,
  group_order = horizons,
  threshold = "1 (68 % level) - Price level - Other publications",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68_other_publications.pdf",
       figure_random_test_pricelevel_68_other_publications,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_pricelevel_95_other_publications <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(top_5_or_tier == 0),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_95_other_publications <- format_randomization_table(random_test_pricelevel_95_other_publications,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_95_other_publications <- plot_randomization_tests(
  table_random_test_pricelevel_95_other_publications,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Price level - Other publications",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95_other_publications.pdf",
       figure_random_test_pricelevel_95_other_publications,
       width = 12, 
       height = 8)

#### Joining plots 68
figure_random_test_pricelevel_68_top_vs_other <- gridExtra::grid.arrange(figure_random_test_pricelevel_68_other_publications, 
                                                                figure_random_test_pricelevel_68_top_journals, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68_top_vs_other.pdf",
       figure_random_test_pricelevel_68_top_vs_other,
       width = 24, 
       height = 12)

#### Joining plots 95
figure_random_test_pricelevel_95_top_vs_other <- gridExtra::grid.arrange(figure_random_test_pricelevel_95_other_publications, 
                                                                figure_random_test_pricelevel_95_top_journals, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95_top_vs_other.pdf",
       figure_random_test_pricelevel_95_top_vs_other,
       width = 24, 
       height = 12)

## cbanker vs non-cbanker ----

### For cbanker only ----

#### Test at 68 % level ----
random_test_pricelevel_68_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(cbanker == 1),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_68_cbanker <- format_randomization_table(random_test_pricelevel_68_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_68_cbanker <- plot_randomization_tests(
  table_random_test_pricelevel_68_cbanker,
  group_order = horizons,
  threshold = "1 (68 % level) - Price level - Central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68_cbanker.pdf",
       figure_random_test_pricelevel_68_cbanker,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_pricelevel_95_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(cbanker == 1),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_95_cbanker <- format_randomization_table(random_test_pricelevel_95_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_95_cbanker <- plot_randomization_tests(
  table_random_test_pricelevel_95_cbanker,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Price level - Central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95_cbanker.pdf",
       figure_random_test_pricelevel_95_cbanker,
       width = 12, 
       height = 8)

### For non-cbanker only ----

#### Test at 68 % level ----
random_test_pricelevel_68_non_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(cbanker == 0),
  statistic = "z_stat",
  thresholds = c(1), # 68 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_68_non_cbanker <- format_randomization_table(random_test_pricelevel_68_non_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_68_non_cbanker <- plot_randomization_tests(
  table_random_test_pricelevel_68_non_cbanker,
  group_order = horizons,
  threshold = "1 (68 % level) - Price level - Non-central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68_non_cbanker.pdf",
       figure_random_test_pricelevel_68_non_cbanker,
       width = 12, 
       height = 8)

#### Test at 95 % level ----
random_test_pricelevel_95_non_cbanker <- randomization_test_battery(
  data = d_z_stat_neg_pricelevel %>% filter(cbanker == 0),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_pricelevel_95_non_cbanker <- format_randomization_table(random_test_pricelevel_95_non_cbanker,
                                          group_order = horizons)
#### Visualize
(figure_random_test_pricelevel_95_non_cbanker <- plot_randomization_tests(
  table_random_test_pricelevel_95_non_cbanker,
  group_order = horizons,
  threshold = "1.96 (95 % level) - Price level - Non-central bank related",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95_non_cbanker.pdf",
       figure_random_test_pricelevel_95_non_cbanker,
       width = 12, 
       height = 8)

#### Joining plots 68
figure_random_test_pricelevel_68_cbanker_vs_non_cbanker <- gridExtra::grid.arrange(figure_random_test_pricelevel_68_non_cbanker, 
                                                                figure_random_test_pricelevel_68_cbanker, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_68_cbanker_vs_non_cbanker.pdf",
       figure_random_test_pricelevel_68_cbanker_vs_non_cbanker,
       width = 24, 
       height = 12)

#### Joining plots 95
figure_random_test_pricelevel_95_cbanker_vs_non_cbanker <- gridExtra::grid.arrange(figure_random_test_pricelevel_95_non_cbanker,
                                                                    figure_random_test_pricelevel_95_cbanker, 
                                                                ncol = 2)
#### Save as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_pricelevel_95_cbanker_vs_non_cbanker.pdf",
       figure_random_test_pricelevel_95_cbanker_vs_non_cbanker,
       width = 24, 
       height = 12)

# For interest rate ----
out_var <- "rate"

## Data prep ----
d_z_stat_neg_rate <- data_prep_counterfactual(d_no_qc,
                                                outvar = out_var,
                                                se_option = "avg",
                                                periods = seq(0, max(d_no_qc$period.month), 1),
                                                wins = wins_para,
                                                only_negative = FALSE,
                                                only_positive = TRUE)

### Add stylized horizons
d_z_stat_neg_rate <- d_z_stat_neg_rate %>% 
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
random_test_68_rate <- randomization_test_battery(
  data = d_z_stat_neg_rate,
  statistic = "z_stat",
  thresholds = c(1), # 68 % level 
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_68_rate <- format_randomization_table(random_test_68_rate,
                                         group_order = horizons)
#### Visualize
(figure_random_test_68_rate <- plot_randomization_tests(
  table_random_test_68_rate,
  group_order = horizons,
  threshold = "1 (68 % level) - Interest rate",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_68_rate.pdf", 
       figure_random_test_68_rate, 
       width = 12, 
       height = 8)

### Test at 90 % level ----
random_test_90_rate <- randomization_test_battery(
  data = d_z_stat_neg_rate,
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = "horizon",
  deltas = deltas
)
#### Format and save table
table_random_test_90_rate <- format_randomization_table(random_test_90_rate,
                                         group_order = horizons)
#### Visualize
(figure_random_test_90_rate <- plot_randomization_tests(
  table_random_test_90_rate,
  group_order = horizons,
  threshold = "1.645 (90 % level) - Interest rate",
  y_limits = ylims))
#### Save figure as pdf
ggsave("analysis/working_paper_1/figures/randomization_tests/figure_random_test_90_rate.pdf", 
       figure_random_test_90_rate, 
       width = 12, 
       height = 8)



