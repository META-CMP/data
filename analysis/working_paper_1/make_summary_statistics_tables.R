# ------------------------------------------------------------------------------
# Setup and Data Import ----
# ------------------------------------------------------------------------------
library(dplyr)
library(knitr)
library(kableExtra)
library(readxl)

# Source your setup file (adjust the path as needed)
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Use d_no_qc as df and filter out unwanted outcome measures
df <- d_no_qc %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate")))
df <- df %>% filter(period.month %in% seq(0, 60, by = 3))

# Read in citation data and merge with df based on "key"
df1 <- read_excel(here("data/study_characteristics/citations_for_included_studies.xlsx"),
                  sheet = "Sheet 1")
merged_df <- inner_join(df1, df, by = "key")

# We set df equal to merged_df
merged_df <- merged_df %>% filter(period.month %in% seq(0, 60, by = 3))
merged_df <- merged_df %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate")))

df <- merged_df

# Summaries for continuous publication variables:
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
options(digits = 10)


# ------------------------------------------------------------------------------
# 1. Compute Shares for Categorical Moderators ----
# ------------------------------------------------------------------------------
period_month <- df %>%
  count(period.month) %>%
  mutate(share = n / sum(n) * 100)

# For Estimation method values (from df$group_est_broad):
estimation_shares <- df %>%
  count(group_est_broad) %>%
  mutate(share = n / sum(n) * 100)

#Create estimation values to paste in the table

var <- estimation_shares[["share"]][[1]]
lp_ardl <- estimation_shares[["share"]][[2]]
favar <- estimation_shares[["share"]][[3]]
other_var <- estimation_shares[["share"]][[4]]
dsge <- estimation_shares[["share"]][[5]]

# Count how much VECM are in the data
vecm <- df %>%
  count(vecm) %>%
  mutate(share = n / sum(n) * 100)

# Create VECM value
vecm <- vecm[["share"]][[2]]
vecm

# For Identification method values (from df$group_ident_broad):
identifcation_shares <- df %>%
  count(group_ident_broad) %>%
  mutate(share = n / sum(n) * 100)

#Create identification values to be paste in the table

chol <- identifcation_shares[["share"]][[1]]
hf <- identifcation_shares[["share"]][[2]]
nr <- identifcation_shares[["share"]][[3]]
signr <- identifcation_shares[["share"]][[4]]
idother <- identifcation_shares[["share"]][[5]]

# For Variable frequency:
# Annual frequency
annual <- df %>%
  count(annual) %>%
  mutate(share = n / sum(n) * 100)

#Create annual frequency value 
annual <- annual[["share"]][[2]]

#Create quarterly frequency value 
quarter <- df %>%
  count(quarter) %>%
  mutate(share = n / sum(n) * 100)

quarter <- quarter[["share"]][[2]]


#Create monthly frequency value 

monthly <- df %>%
  count(month) %>%
  mutate(share = n / sum(n) * 100)

monthly <- monthly[["share"]][[2]]


#Create top tier value 

top_5_or_tier <- df %>%
  count(top_5_or_tier) %>%
  mutate(share = n / sum(n) * 100)

top_5_or_tier <- top_5_or_tier[["share"]][[2]]

#Create central bank value 

cbanker<- df %>%
  count(cbanker) %>%
  mutate(share = n / sum(n) * 100)

cbanker <- cbanker[["share"]][[2]]

#Create publication year mean value 

stats_pubyear <- custom_summary(df$publication_year)

mean_pubyear <- stats_pubyear[["Mean"]]
sd_pubyear <- stats_pubyear[["SD"]]
sd_pubyear

#Create number of citations mean value 

stats_numcit <- custom_summary(df$num_cit.x)

mean_numcit <- stats_numcit[["Mean"]]
sd_numcit <- stats_numcit[["SD"]]
sd_numcit

#Create byproduct value 

byproduct <- df %>%
  count(byproduct) %>%
  mutate(share = n / sum(n) * 100)

byproduct <- byproduct[["share"]][[2]]

prefer <- df %>%
  count(prefer) %>%
  mutate(share = n / sum(n) * 100)

#Create preferred estimate value 

prefer <- prefer[["share"]][[2]]

country_us <- df %>%
  count(us) %>%
  mutate(share = n / sum(n) * 100)

#Create US country share value 

us <- country_us[["share"]][[2]]

country_ea12 <- df %>%
  count(ea12) %>%
  mutate(share = n / sum(n) * 100)

#Create EA-12 share value 

ea12 <- country_ea12[["share"]][[2]]

#Create other advanced countries share value 

other_advanced <- df %>%
  count(country_dev) %>%
  mutate(share = n / sum(n) * 100)

advanced <- 
  other_advanced[["share"]][[1]] - 
  country_us[["share"]][[2]] - country_ea12[["share"]][[2]]

#Create emerging countries share value 

emerging <- 
  other_advanced[["share"]][[2]] + other_advanced[["share"]][[3]] +
  other_advanced[["share"]][[4]] + other_advanced[["share"]][[5]]

#For outcome measures

outcome <- df %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate", "rate")))

outcome_measure <- outcome %>%
  count(outcome_measure) %>%
  mutate(share = n / sum(n) * 100)

#Create outcome measures share values

outcome_gdp <- outcome_measure[["share"]][[5]] + outcome_measure[["share"]][[6]]
outcome_ip <- outcome_measure[["share"]][[7]]
outcome_gap <- outcome_measure[["share"]][[4]]
outcome_cpi <- outcome_measure[["share"]][[2]]
outcome_deflator <- outcome_measure[["share"]][[3]]
outcome_other <- outcome_measure[["share"]][[1]] + outcome_measure[["share"]][[8]]

# For interest rate type:
interest_type <- df %>%
  count(group_inttype) %>%
  mutate(share = n / sum(n) * 100)

#Create interest rates share values 

overnight <- interest_type[["share"]][[1]]
lending <- interest_type[["share"]][[2]]
year_rate <- interest_type[["share"]][[3]]

#For the type of the data

type_data <- df %>%
  count(panel) %>%
  mutate(share = n / sum(n) * 100)

#Create panel data share value 

panel <- type_data[["share"]][[2]]

#Create time series share value 
time_series <- type_data[["share"]][[1]]

#For transformed IRF if needed

trans_irf <- df %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate", "rate"))) %>%
  count(transformation) %>%
  mutate(share = n / sum(n) * 100)

growth <- trans_irf[["share"]][[1]]
levels <- trans_irf[["share"]][[2]]
log_level <- trans_irf[["share"]][[3]]
log_diff <- trans_irf[["share"]][[4]]

transformed <- trans_irf[["share"]][[1]] + trans_irf[["share"]][[4]]
no_transformed <- trans_irf[["share"]][[2]] + trans_irf[["share"]][[3]]


# ------------------------------------------------------------------------------
# 2. Create table 1
# ------------------------------------------------------------------------------

# Create the data frame with Variable and Value
table_data1 <- data.frame(
  Variable = c(
    # Estimation method
    "VAR", "LP-ARDL", "FAVAR", "Other VAR", "DSGE",
    # Identification approach
    "Cholesky", "Sign restrictions", "High-Frequency", "Narrative", "Other identification",
    # Publication characteristics
    "Top publication", 
    "Central bank",
    "Publication year",
    "Citations",
    "By-product",
    "Preferred estimate",
    "US", "Euro Area", "Other advanced", "Emerging"
  ),
  Explanation = c(
    # Estimation Methods
    "Vector autoregression (VAR) + vector error correction models (VECM)",
    "Local projection (LP) and autoregressive distributed lag (ARDL) models",
    "Factor Augmented VAR (FAVAR) estimation", 
    "Other VAR estimation method (e.g. TVP-VAR, GVAR)", 
    "Estimated DSGE model",
    # Identification Methods
    "Cholesky decomposition or SVAR restrictions",
    "Sign restrictions of responses", 
    "High frequency information", 
    "Narrative approach", 
    "Other strategy (e.g. long-run restrictions, heteroskedasticity, DSGE)",
    # Publication characteristics
    "Paper published in top-50 economics journal", 
    "Central bank outlet or authors affiliated with central bank",
    "Publication year of the study",
    "Number of citations in Google Scholar",
    "Indicator whether the estimate was not the main research question",
    "The authors signal an estimate to be their preferred one",
    # Country groups
    "Estimates based on US data", 
    "Estimates based on Euro Area country data", 
    "Other advanced countries (e.g. UK, Japan, Canada, Australia)", 
    "Emerging market countries"
  ),
  Share = c(
    # Estimation method
    var, lp_ardl, favar, other_var, dsge,
    # Identification approach
    chol, signr, hf, nr, idother,
    # Publication characteristics
    top_5_or_tier, cbanker, mean_pubyear, mean_numcit, byproduct, prefer,
    #Country groups
    us, ea12, advanced, emerging
  )
)

# Format the table
table_data1 %>%
  kable(
    format = "html",
    digits = 2,
    align = c("l", "l", "r"),
    col.names = c("Variable", "Explanation", "Share in % / Mean"),
    caption = "Table 1: Description and summary statistics for moderator variables"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "left"
  ) %>%
  pack_rows("Estimation method", 1, 5, bold = TRUE, hline_after = TRUE) %>%
  pack_rows("Identification method", 6, 10, bold = TRUE, hline_after = TRUE) %>%
  pack_rows("Publication characteristics", 11, 16, bold = TRUE, hline_after = TRUE) %>%
  pack_rows("Country groups", 17, 20, bold = TRUE, hline_after = TRUE) %>%
  add_header_above(c(" " = 2, "Summary Statistics" = 1))

# ------------------------------------------------------------------------------
# 3. Create table 2
# ------------------------------------------------------------------------------

# Create the data frame with Variable and Value
table_data2 <- data.frame(
  Variable = c(
    # Measurement and sample characteristics
    "GDP", "IP", "Output gap", "CPI", "GDP deflator", "Other price",
    "Overnight rate", "Lending rate", "Year rate", 
    "Annual frequency", "Quarterly frequency", "Monthly frequency", 
    "Panel data", "Time series", "Transformed IRF - No", "Transformed IRF - Yes"
  ),
  Explanation = c(
    # Measurement and sample characteristics
    "GDP is the output variable", 
    "Industrial production is the output variable",
    "Output gap is the output variable",
    "Consumer Price Index is the price level variable",
    "GDP deflator is the price level variable",
    "Other price measure (e.g. Core or Wholesale Price Index)",
    "Short-term rates (including money market and policy rates)",
    "Weekly to monthly lending and bond rates",
    "Yearly to longer-term rates",
    "Estimates based on annual frequency data",
    "Estimates based on quarterly frequency data",
    "Estimates based on monthly frequency data",
    "Panel data used",
    "Time series data used",
    # Transformation
    "Indicator whether we transformed the IRF estimates - No",
    "Indicator whether we transformed the IRF estimates - Yes"
  ),
  Share = c(
    # Measurement and sample characteristics
    outcome_gdp, outcome_ip, outcome_gap,
    outcome_cpi, outcome_deflator, outcome_other,
    overnight, lending, year_rate,
    annual, quarter, monthly,
    panel, time_series,
    # Transformation
    no_transformed, transformed
  )
)

# Format the table
table_data2 %>%
  kable(
    format = "html",
    digits = 2,
    align = c("l", "l", "r"),
    col.names = c("Variable", "Explanation", "Share in % / Mean"),
    caption = "Table 2: Description and summary statistics for other control variables"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "left"
  ) %>%
  pack_rows("Measurement and sample characteristics", 1, 14, bold = TRUE, hline_after = TRUE) %>%
  pack_rows("Transformation", 15, 16, bold = TRUE, hline_after = TRUE) %>%
  add_header_above(c(" " = 2, "Summary Statistics" = 1))

# ------------------------------------------------------------------------------
# 4. Save to file
# ------------------------------------------------------------------------------

write.csv(table_data1, file = here::here("analysis/working_paper_1/tables/summary_statistics/table1.csv"), 
          row.names = FALSE)

write.csv(table_data2, file = here::here("analysis/working_paper_1/tables/summary_statistics/table2.csv"), 
          row.names = FALSE)

