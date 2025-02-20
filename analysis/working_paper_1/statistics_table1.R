# Descriptive statistis for table1

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

library(dplyr)

df<- d_no_qc

df <- df %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate")))

df %>%
  count(group_est_broad) %>%
  mutate(percent = n / sum(n) * 100)

df %>%
  count(vecm) %>%
  mutate(percent = n / sum(n) * 100)

df %>%
  count(group_ident_broad) %>%
  mutate(percent = n / sum(n) * 100)

df %>%
  count(top_5_or_tier) %>%
  mutate(percent = n / sum(n) * 100)

df %>%
  count(cbanker) %>%
  mutate(percent = n / sum(n) * 100)


library(readxl)

df1 <- read_excel(here("data/study_characteristics/citations_for_included_studies.xlsx"),
                  sheet = "Sheet 1")

merged_df <- inner_join(df1, df, by = "key")

custom_summary <- function(x) {
  c(
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
}
options(digits = 10)  # Set digits to 10
# Example usage with a vector or a column from a data frame:
custom_summary(merged_df$publication_year)
custom_summary(merged_df$num_cit.x)
custom_summary(merged_df$num_cit.y)

# back to the previous data set

df %>%
  count(main) %>%
  mutate(percent = n / sum(n) * 100) %>%
  print(n = Inf)  # print all rows

main_gdp <- ((576 + 8888 + 1649 + 1602 + 10571 + 1014 +2996 + 416 + 
              431 + 7412 + 1289 + 1323 + 38997+ 9994 + 778 + 720 +309 + 
                240)/ 146463) * 100
print(main_gdp)

main_inflation <- ((36 + 736 + 576 + 1649 + 1602 + 10571 + 1014 + 2996 +
                      7412 + 1289 + 38997 + 9994 + 7439 + 848 + 445 + 720 +
                      400 + 240 + 142)/ 146463) * 100
print(main_inflation)

main_other <- ((278 + 36 + 63 + 736 + 576 + 1602 + 2996 + 416 + 431 +
                  7412 + 1289  + 1323 + 9994 + 778 + 848 + 445 + 720 +
                  400 + 46871 + 309 + 240 + 142)/ 146463) * 100
print(main_other)


df %>%
  count(outcome_measure) %>%
  mutate(percent = n / sum(n) * 100)

outcome_gdp <- ((26891 + 42)/ 146463) * 100
print(outcome_gdp)

outcome_ip <- ((21398)/ 146463) * 100
print(outcome_ip)

outcome_gap <- ((3074)/ 146463) * 100
print(outcome_gap)

outcome_cpi <- ((33308)/ 146463) * 100
print(outcome_cpi)

outcome_deflator <- ((12984)/ 146463) * 100
print(outcome_deflator)

outcome_otherprice <- ((2898 + 1329)/ 146463) * 100
print(outcome_otherprice)

df %>%
  count(group_inttype) %>%
  mutate(percent = n / sum(n) * 100)


df %>%
  count(us) %>%
  mutate(percent = n / sum(n) * 100)

df %>%
  count(ea12) %>%
  mutate(percent = n / sum(n) * 100)

other_advanced <- df %>%
  filter(us != 1, ea12 != 1)

other_advanced %>%
  count(country_dev) %>%
  mutate(percent = n / sum(n) * 100)

high_income <- (38558 / 146463) * 100
print(high_income)

emerging <- ((324 + 2950 + 4916 + 7624) / 146463) * 100
print(emerging)

df %>%
  count(panel) %>%
  mutate(percent = n / sum(n) * 100)

df %>%
  count(transformation) %>%
  mutate(percent = n / sum(n) * 100)

transformation_yes <- ((10214 + 15733) / 146463) * 100
print(transformation_yes)

transformation_no <- ((715 + 75262 + 44539) / 146463) * 100
print(transformation_no)

