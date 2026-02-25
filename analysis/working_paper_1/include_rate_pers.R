# Load generally required libraries ----
library(here) # For file paths
library(tidyverse) # For data manipulation

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Include rate shock persistence up to 6 months----
rate_persistence <- d_no_qc %>% 
  filter(
    outcome == "rate",
    period.month %in% c(0, 3, 6)
  ) %>% 
  group_by(key, model_id) %>% 
  summarise(
    rate_pers_6 = mean(rate_mean.effect, na.rm = TRUE),
    .groups = "drop"
  )
d_no_qc <- d_no_qc %>% 
  left_join(rate_persistence, by = c("key", "model_id"))
rm(rate_persistence)


# Include rate shock persistence up to 1 year----
rate_persistence <- d_no_qc %>% 
  filter(
    outcome == "rate",
    period.month %in% c(0, 3, 6, 9, 12)
  ) %>% 
  group_by(key, model_id) %>% 
  summarise(
    rate_pers_12 = mean(rate_mean.effect, na.rm = TRUE),
    .groups = "drop"
  )
d_no_qc <- d_no_qc %>% 
  left_join(rate_persistence, by = c("key", "model_id"))
rm(rate_persistence)

# Include rate shock persistence up to 2 year----
rate_persistence <- d_no_qc %>% 
  filter(
    outcome == "rate",
    period.month %in% c(0, 3, 6, 9, 12, 15, 18, 21, 24)
  ) %>% 
  group_by(key, model_id) %>% 
  summarise(
    rate_pers_24 = mean(rate_mean.effect, na.rm = TRUE),
    .groups = "drop"
  )
d_no_qc <- d_no_qc %>% 
  left_join(rate_persistence, by = c("key", "model_id"))
rm(rate_persistence)
