# Load generally required libraries ----
library(here) # For file paths
library(tidyverse) # For data manipulation

# Load data ----
data_path <- here("data/final_data_working_paper_1.RData")
load(data_path)

# Store data with quality concerns for robustness checks ----
d_qc <- data

# Filter out models with quality concerns ----
d_no_qc <- data %>% filter(quality_concern != 1)

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


