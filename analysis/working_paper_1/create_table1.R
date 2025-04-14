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

# For demonstration, we set df equal to merged_df
merged_df <- merged_df %>% filter(period.month %in% seq(0, 60, by = 3))
merged_df <- merged_df %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate")))

df <- merged_df

# (Optional) Example summaries for continuous publication variables:
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
# For Estimation method values (from df$group_est_broad):
estimation_shares <- df %>%
  count(group_est_broad) %>%
  mutate(share = n / sum(n) * 100)

var <- estimation_shares[["share"]][[1]]
lp_ardl <- estimation_shares[["share"]][[2]]
favar <- estimation_shares[["share"]][[3]]
other_var <- estimation_shares[["share"]][[4]]
dsge <- estimation_shares[["share"]][[5]]

vecm <- df %>%
  count(vecm) %>%
  mutate(share = n / sum(n) * 100)

vecm <- vecm[["share"]][[2]]

# For Identification method values (from df$group_ident_broad):
identifcation_shares <- df %>%
  count(group_ident_broad) %>%
  mutate(share = n / sum(n) * 100)

chol <- identifcation_shares[["share"]][[1]]
hf <- identifcation_shares[["share"]][[2]]
nr <- identifcation_shares[["share"]][[3]]
signr <- identifcation_shares[["share"]][[4]]
idother <- identifcation_shares[["share"]][[5]]

# For Variable frequency:
annual <- df %>%
  count(annual) %>%
  mutate(share = n / sum(n) * 100)

annual <- annual[["share"]][[2]]

quarter <- df %>%
  count(quarter) %>%
  mutate(share = n / sum(n) * 100)

quarter <- quarter[["share"]][[2]]

monthly <- df %>%
  count(month) %>%
  mutate(share = n / sum(n) * 100)

monthly <- monthly[["share"]][[2]]

top_5_or_tier <- df %>%
  count(top_5_or_tier) %>%
  mutate(share = n / sum(n) * 100)

top_5_or_tier <- top_5_or_tier[["share"]][[2]]

cbanker<- df %>%
  count(cbanker) %>%
  mutate(share = n / sum(n) * 100)

cbanker <- cbanker[["share"]][[2]]

stats_pubyear <- custom_summary(df$publication_year)

mean_pubyear <- stats_pubyear[["Mean"]]

stats_numcit <- custom_summary(df$num_cit.x)

mean_numcit <- stats_numcit[["Mean"]]



byproduct <- df %>%
  count(byproduct) %>%
  mutate(share = n / sum(n) * 100)

byproduct <- byproduct[["share"]][[2]]

prefer <- df %>%
  count(prefer) %>%
  mutate(share = n / sum(n) * 100)

prefer <- prefer[["share"]][[2]]

country_us <- df %>%
  count(us) %>%
  mutate(share = n / sum(n) * 100)

us <- country_us[["share"]][[2]]

country_ea12 <- df %>%
  count(ea12) %>%
  mutate(share = n / sum(n) * 100)

ea12 <- country_ea12[["share"]][[2]]


other_advanced <- df %>%
  count(country_dev) %>%
  mutate(share = n / sum(n) * 100)

advanced <- 
  other_advanced[["share"]][[1]] - 
  country_us[["share"]][[2]] - country_ea12[["share"]][[2]]

emerging <- 
  other_advanced[["share"]][[2]] + other_advanced[["share"]][[3]] +
  other_advanced[["share"]][[4]] + other_advanced[["share"]][[5]]

outcome <- df %>%
  filter(!(outcome_measure %in% c("emp", "emp_rate", "une_rate", "rate")))

outcome_measure <- outcome %>%
  count(outcome_measure) %>%
  mutate(share = n / sum(n) * 100)

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

overnight <- interest_type[["share"]][[1]]
lending <- interest_type[["share"]][[2]]
year_rate <- interest_type[["share"]][[3]]

type_data <- df %>%
  count(panel) %>%
  mutate(share = n / sum(n) * 100)

panel <- type_data[["share"]][[2]]
time_series <- type_data[["share"]][[1]]

trans_irf <- df %>%
  count(transformation) %>%
  mutate(share = n / sum(n) * 100)


# ------------------------------------------------------------------------------
# 2. Create table 1
# ------------------------------------------------------------------------------


# Create the data frame with Variable and Value
table_data <- data.frame(
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
    # Measurement and sample characteristics
    "US", "Euro Area", "Other advanced", "Emerging",
    "GDP", "IP", "Output gap", "CPI", "GDP deflator", "Other price",
    "Overnight rate", "Lending rate", "Year rate", 
    "Annual frequency", "Quarterly frequency", "Monthly frequency", 
    "Panel data", "Time series"
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
    # Measurement and sample characteristics
    "Estimates based on US data", 
    "Estimates based on Euro Area country data", 
    "Other advanced countries (e.g. UK, Japan, Canada, Australia)", 
    "Emerging market countries",
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
    "Time series data used"
  ),
  Share = c(
    # Estimation method
    var, lp_ardl, favar, other_var, dsge,
    # Identification approach
    chol, signr, hf, nr, idother,
    # Publication characteristics
    top_5_or_tier, cbanker, mean_pubyear, mean_numcit, byproduct, prefer,
    # Measurement and sample characteristics
    us, ea12, advanced, emerging,
    outcome_gdp, outcome_ip, outcome_gap,
    outcome_cpi, outcome_deflator, outcome_other,
    overnight, lending, year_rate,
    annual, quarter, monthly,
    panel, time_series
  )
)

# Format the table
table_data %>%
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
  pack_rows("Measurement and sample characteristics", 17, 34, bold = TRUE, hline_after = TRUE) %>%
  add_header_above(c(" " = 2, "Summary Statistics" = 1))

# ------------------------------------------------------------------------------
# 3. Save to file (optional)
# ------------------------------------------------------------------------------

write.csv(table_data, file = here::here("analysis/working_paper_1/tables/summary_statistics/summary_statistics.csv"), 
          row.names = FALSE)

