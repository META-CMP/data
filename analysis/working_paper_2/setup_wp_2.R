# Load generally required libraries ----
library(here) # For file paths
library(tidyverse) # For data manipulation

# Load data ----
data_path <- here("data/final_data_working_paper_1.RData") # Using the same data as for wp1
load(data_path)

# Correct top journal classification for "Quantitative Economics" (Rank 47 in SJR 2022)
data <- data %>% 
  mutate(is_top_tier = ifelse(`publication title` == "Quantitative Economics", 1, top_5_or_tier))
# PAPER 2 SPECIFIC FILTERING ----
# Filter to only include emp/unemp outcomes and rate outcomes from models that study emp/unemp
data <- data %>%
  group_by(key, model_id) %>%
  mutate(has_emp_or_unemp = any(outcome %in% c("emp", "unemp"))) %>%
  ungroup() %>%
  filter(
    outcome %in% c("emp", "unemp") |
      (outcome == "rate" & has_emp_or_unemp)
  ) %>%
  select(-has_emp_or_unemp)

# Filter to include only US models (us == 1) ---- 
data <- data %>% filter(us == 1)

# Store data with quality concerns for robustness checks ----
d_qc <- data
cat("With quality concern:\n")
table(d_qc$outcome)
cat("\nNumber of observations:", nrow(d_qc), "\n")
cat("Number of unique models:", d_qc %>% distinct(key, model_id) %>% nrow(), "\n")

# Filter out models with quality concerns ----
d_no_qc <- data %>% filter(quality_concern != 1)
cat("Without quality concern:\n")
table(d_no_qc$outcome)
cat("\nNumber of observations:", nrow(d_no_qc), "\n")
cat("Number of unique models:", d_no_qc %>% distinct(key, model_id) %>% nrow(), "\n")


# Define global winsorization parameter ----
wins_para <- 0.02

# Set a global confidence interval level for regression tables ----
conflevel <- 0.89

# Define stylized time horizons ----
vsr <- 1
smr <- 12
mlr <- 36
horizons <- c(
  paste0("impact (0m)"),
  paste0("short run (", vsr, "m - ", smr, "m)"),
  paste0("medium run (", smr + 1, "m - ", mlr, "m)"),
  paste0("long run (> ", mlr, "m)")
)

# Set global parameters for plots ----
y_lims <- c(-2.7, 0.7)
titles_size <- 16

# Set seed for reproducibility ----
set.seed(42)
