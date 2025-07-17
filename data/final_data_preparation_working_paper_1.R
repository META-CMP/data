# Data preparation for working paper 1 ----
load("data/final_join_json_irf_data.RData") # Load the data from final_join()

## Load packackages ----
library(dplyr) # for data manipulation
library(tidyverse) # for data manipulation
library(stringi) # for advanced str_split 
library(zoo) # for easier manipulation of date format 
library(readxl) # to read excel file
library(lubridate) # for date transformation

## Load publications data and join with full text screening data ----
load("data/sjr.RData")
data <- data %>% left_join(
  df_full_bib %>% 
    select(key,
           `publication title`,
           type = `item type`,
           pub_year = `publication year`,
           BibtexKey,
           is_top_tier,
           is_top_5),
  "key")

# Consolidate top 5 and top tier dummies ---- 
data$top_5_or_tier <- ifelse(data$is_top_5 == 1 | data$is_top_tier == 1, TRUE, FALSE)
data$top_5_or_tier <- ifelse(is.na(data$top_5_or_tier), FALSE, data$top_5_or_tier)
data$is_top_5 <- ifelse(is.na(data$is_top_5), 0, data$is_top_5)
data$is_top_tier <- ifelse(is.na(data$is_top_tier), 0, data$is_top_tier)

# Calculate average standard error and precision options ----
data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
data$precision.avg <- 1 / data$SE.avg
data$precision.lower <- 1 / data$SE.lower
data$precision.upper <- 1 / data$SE.upper

# Merge the corresponding interest rate IRF value as a moderator variable to each observation ----

# Get interest rate IRF data
rate <- data %>% 
  filter(data$outcome_var == "rate")

# Get the necessary variables of the rate IRFs and remove outcome_var
rate <- rate %>%
  select(key,
         model_id,
         period:SE.lower) %>%
  select(-outcome_var)

# Rename colnames
colnames(rate)[4:ncol(rate)] <- paste("rate_",colnames(rate)[4:ncol(rate)],sep="")

# Merge to complete dataset and remove temporary rate dataframe
data <- data %>% dplyr::left_join(rate, by=c("key","model_id","period"))
remove(rate)

# Create transformation, periodicity, real_output dummy, outcome_measure and outcome variable ----

# Get the different elements of the outcome_var string
split_list <- stri_split_fixed(
  str = data$outcome_var, 
  pattern = "_", 
  n = 3)

# Extract the first element of the outcome_var string and set the transformation factor accordingly. 
data$transformation <- sapply(
  split_list, function(x) ifelse(1 <= length(x), x[1], NA)
  )
unique(data$transformation)

# Extract the second element of the outcome_var string and set the periodicity factor accordingly. 
data$periodicity <- sapply(split_list, function(x) ifelse(2 <= length(x), x[2], NA))
unique(data$periodicity)

# Extract third element of the outcome var string which is used to obtain the real_output dummy, outcome_measure and outcome variable.
split_list <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

# Create real_output dummy (for NA, the information was not coded in full text screening)
data$real_output <- ifelse(split_list %in% c("rgdp", "ip", "rip", "rgnp", "rgap"), TRUE, NA)

# Remove r from the outcome measure
split_list<-sub("^r", "", split_list)

# Create outcome_measure variable one of c("gdp","cpi","ip", "deflator","une_rate","emp_rate","gap","emp","wpi","core","price_level","gnp")
data$outcome_measure <- split_list
data$outcome_measure <- ifelse(data$outcome_var == "rate", "rate", data$outcome_measure)
unique(data$outcome_measure)

# Create outcome variable one of c("output", "inflation", "emp")
data$outcome <- ifelse(data$outcome_measure == "rate", "rate", ifelse(data$outcome_measure %in% c("gdp", "ip", "gap", "gnp"), "output", ifelse(data$outcome_measure %in% c("cpi", "deflator", "wpi", "core", "price_level"), "inflation", "emp")))
unique(data$outcome)

# Splitting emp and unemp --- 
data$outcome <- ifelse(data$outcome_measure == "une_rate", "unemp", data$outcome)
unique(data$outcome)

# Generate dummy for models or studies with quality_concern ---- 
data$quality_concern <- grepl("quality_concern", data$study_notes)
data$quality_concern <- ifelse(grepl("quality_concern", data$model_notes), TRUE, data$quality_concern)

# Generate dummy if ARDL model has been used ----
data <- data %>% mutate(dyn_ols=ifelse(dyn_ols=="ARDL" | dyn_ols=="ardl",TRUE,FALSE))

# Consolidate identification strategies (group_ident_broad) ---- 
data <- data %>%
  mutate(group_ident_broad = case_when(
    # Group 1: chol, chol+svar
    (chol == 1 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "chol",
    (chol == 1 & svar == 1 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "chol",
    
    # Group 2: signr, signr+SVAR, chol+signr, signr+IV
    (signr == 1 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "signr",
    (signr == 1 & svar == 1 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "signr",
    (chol == 1 & signr == 1 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "signr",
    (signr == 1 & iv == 1 & chol == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "signr",
    
    # Group 3: svar
    (svar == 1 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "chol",
    
    # Group 4: hf+iv, hf, hf+signr, hf+signr+iv, hf+chol, hf+signr+chol, hf+iv+svar, hf+iv+chol
    (hf == 1 & iv == 1 & signr == 0 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & iv == 0 & signr == 0 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & signr == 1 & iv == 0 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & signr == 1 & iv == 1 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & chol == 1 & signr == 0 & iv == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & signr == 1 & chol == 1 & iv == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & iv == 1 & svar == 1 & chol == 0 & signr == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (hf == 1 & iv == 1 & chol == 1 & svar == 0 & signr == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    
    # Group 5: forecast+nr, forecast+nr+iv, forecast+nr+chol,
    (forecast_based == 1 & nr == 1 & iv == 0 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & nr == 1 & iv == 1 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & nr == 1 & chol == 1 & iv == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    
    # Group6:  forecast+chol, forecast+iv, forecast+svar+signr, forecast+chol+signr, forecast+signr, forecast+svar
    (forecast_based == 1 & chol == 0 & nr == 0 & iv == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & chol == 1 & nr == 0 & iv == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & iv == 1 & nr == 0 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & svar == 1 & signr == 1 & nr == 0 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & chol == 1 & signr == 1 & nr == 0 & iv == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & signr == 1 & nr == 0 & iv == 0 & chol == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & svar == 1 & nr == 0 & iv == 0 & chol == 0 & signr == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    
    # Group 7: iv+svar, iv, iv+idother, iv+chol
    (iv == 1 & svar == 1 & nr == 0 & chol == 0 & signr == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "idother",
    (iv == 1 & nr == 0 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "idother",
    (iv == 1 & idother == 1 & nr == 0 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "idother",
    (iv == 1 & chol == 1 & nr == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "idother",
    
    # Group 8: Idother, idother+svar, nr+svar, idother+svar+signr, idother+chol, nr
    (idother == 1 & svar == 0 & nr == 0 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "idother",
    (idother == 1 & svar == 1 & nr == 0 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "idother",
    (nr == 1 & svar == 1 & idother == 0 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "nr",
    (idother == 1 & svar == 1 & signr == 1 & nr == 0 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "idother",
    (idother == 1 & chol == 1 & svar == 0 & signr == 0 & nr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "idother",
    (nr == 1 & svar == 0 & idother == 0 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0) ~ "nr",
    
    # Group 9: forecast+hf+iv, forecast+nr+hf+iv, hf+nr+svar, hf+event+forecast+iv, forecast+chol+hf, forecast+nr+hf, iv+forecast+nr+signr+svar, forecast+iv+chol+hf, forecast+iv+event+chol+hf, forecast+nr+chol+signr, forecast+hf
    (forecast_based == 1 & hf == 1 & iv == 1 & nr == 0 & chol == 0 & signr == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (forecast_based == 1 & nr == 1 & hf == 1 & iv == 1 & chol == 0 & signr == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (hf == 1 & nr == 1 & svar == 1 & forecast_based == 0 & chol == 0 & signr == 0 & iv == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (hf == 1 & event == 1 & forecast_based == 1 & iv == 1 & chol == 0 & signr == 0 & svar == 0 & heteroskedas == 0 & longrun == 0 & idother == 0 & nr == 0) ~ "hf",
    (forecast_based == 1 & chol == 1 & hf == 1 & nr == 0 & signr == 0 & iv == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (forecast_based == 1 & nr == 1 & hf == 1 & iv == 0 & chol == 0 & signr == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (iv == 1 & forecast_based == 1 & nr == 1 & signr == 1 & svar == 1 & chol == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & iv == 1 & chol == 1 & hf == 1 & nr == 0 & signr == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (forecast_based == 1 & iv == 1 & event == 1 & chol == 1 & hf == 1 & nr == 0 & signr == 0 & svar == 0 & heteroskedas == 0 & longrun == 0 & idother == 0) ~ "hf",
    (forecast_based == 1 & nr == 1 & chol == 1 & signr == 1 & hf == 0 & iv == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & hf == 1 & nr == 0 & chol == 0 & signr == 0 & iv == 0 & svar == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    
    # Group 10: longrun, longrun+svar, longrun+chol, longrun+svar+iv
    (longrun == 1 & svar == 0 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    (longrun == 1 & svar == 1 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    (longrun == 1 & chol == 1 & svar == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    (longrun == 1 & svar == 1 & iv == 1 & chol == 0 & signr == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    
    # Group 11: event+signr, event+hf+iv, event+chol
    (event == 1 & signr == 1 & hf == 0 & iv == 0 & chol == 0 & svar == 0 & heteroskedas == 0 & forecast_based == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "nr",
    (event == 1 & hf == 1 & iv == 1 & signr == 0 & chol == 0 & svar == 0 & heteroskedas == 0 & forecast_based == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "nr",
    (event == 1 & chol == 1 & signr == 0 & hf == 0 & iv == 0 & svar == 0 & heteroskedas == 0 & forecast_based == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "nr",
    
    # Group 12: heteroskedascticity, chol+heteroskedascticity, svar+heteroskedascticity, signr+hf+heteroskedascticity, iv+forecast+nr+heteroskedascticity
    (heteroskedas == 1 & chol == 0 & svar == 0 & signr == 0 & hf == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "idother",
    (heteroskedas == 1 & chol == 1 & svar == 0 & signr == 0 & hf == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "idother",
    (heteroskedas == 1 & svar == 1 & chol == 0 & signr == 0 & hf == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "idother",
    (heteroskedas == 1 & signr == 1 & hf == 1 & chol == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & svar == 0 & longrun == 0 & idother == 0) ~ "hf",
    (heteroskedas == 1 & iv == 1 & forecast_based == 1 & nr == 1 & chol == 0 & signr == 0 & hf == 0 & event == 0 & svar == 0 & longrun == 0 & idother == 0) ~ "nr",
    (heteroskedas == 1 & signr == 0 & hf == 1 & chol == 0 & iv == 1 & forecast_based == 0 & event == 0 & nr == 0 & svar == 0 & longrun == 0 & idother == 0) ~ "hf",
    
    # Default case
    TRUE ~ "Other"
  ))

# Transform group_ident_broad into factor and make "chol" the reference 
data$group_ident_broad <- factor(data$group_ident_broad, 
                                    levels = c("chol", 
                                               "hf", 
                                               "nr", 
                                               "signr", 
                                               "idother"))
levels(data$group_ident_broad)

# Consolidate estimation methods ---- 
data <- data %>%
  mutate(group_est_broad = case_when(
    # Group 1: var, standard VAR
    (var == 1 & lp == 0 & vecm == 0 & dyn_ols == 0 & fvar == 0 & tvar == 0 & gvar == 0 & dsge == 0 & varother == 0) ~ "standard_var",
    
    # Group 2: lp_ardl
    ((lp == 1 | dyn_ols == 1) & fvar == 0) ~ "lp_ardl",
    
    # Group 3: favar
    (fvar == 1) ~ "favar",
    
    # Group 4: other_var
    ((varother == 1 | tvar == 1 | gvar == 1 | vecm == 1) & lp == 0 & dyn_ols == 0 & fvar == 0 & dsge == 0) ~ "other_var",
    
    # Group 5: dsge
    (dsge == 1) ~ "dsge",
    
    # Default case
    TRUE ~ "Other"
    
  ))

# Transform into factor and make "standard_var" the reference 
data$group_est_broad <- factor(data$group_est_broad, 
                                  levels = c("standard_var", 
                                             "lp_ardl", 
                                             "favar", 
                                             "other_var", 
                                             "dsge"))
levels(data$group_est_broad)

# Consolidate interest rates ----  
## ---- Step 1: Define Classification Rules ----
# Function to classify inttypes based on keywords like "month", "day", "year", etc.
classify_inttype <- function(inttype) {
  if (grepl("overnight|a_money_market|callrate|call_rate|cashrate|cash_rate|q_fed_funds|a_fed_funds|a_fedfunds|a_eonia", inttype, ignore.case = TRUE)) {
    return("overnight")
  } else if (grepl("7_day|14_day|month|short_term|short_rate|a_repo|a_discount|a_interbank|a_MRO|a_policy_rate|a_target_rate|a_pribor|a_wibor|a_bubor|a_bank_rate", inttype, ignore.case = TRUE)) {
    return("week_month")
  } else if (grepl("year|long_term|2_year|1_year|ssr|a_shadow_rate|government_bond|mibor|shadow_fed_funds", inttype, ignore.case = TRUE)) {
    return("year")
  } else {
    return("unclear")  # Mark cases that do not match any rule
  }
}
## --- Step 2: Extract Unique `inttypes` ----
all_inttypes <- unique(data$inttype)  
## --- Step 3: Apply Classification ----
classified_inttypes <- sapply(all_inttypes, classify_inttype)
# Convert the result into a dataframe for reference
classification_lookup <- data.frame(
  inttype = all_inttypes,
  group_inttype = classified_inttypes,
  stringsAsFactors = FALSE
)
# Print
print(classification_lookup, row.names = FALSE)
## --- Step 4: Map Classification Back to Original Dataframe ----
data <- merge(
  data,
  classification_lookup,
  by.x = "inttype",
  by.y = "inttype",
  all.x = TRUE
)
# Make group_inttype a factor with "overnight" as reference
data$group_inttype <- factor(data$group_inttype, levels = c("overnight", "week_month", "year", "unclear"))

# Create consolidated `outcome_measure_output_cons` for output ----
## transform into factor and make "gdp" (now including "gnp") the reference (only a very small number of observations uses "gnp")
data <- data %>%
  mutate(outcome_measure_output_cons = case_when(
    outcome == "output" & outcome_measure == "gnp" ~ "gdp",
    outcome == "output" ~ outcome_measure,
    TRUE ~ NA_character_  # For non-output rows
  )) %>%
  mutate(outcome_measure_output_cons = factor(outcome_measure_output_cons, 
                                              levels = c("gdp", "ip", "gap")))
levels(data$outcome_measure_output_cons)

# Create consolidated `outcome_measure_pricelevel_cons` for price level ----
## transform `outcome_measure_pricelevel_cons` into factor and make "cpi" the reference
data <- data %>%
  mutate(outcome_measure_pricelevel_cons = case_when(
    outcome == "inflation" ~ outcome_measure,
    TRUE ~ NA_character_  # For non-pricelevel rows
  )) %>% 
  mutate(outcome_measure_pricelevel_cons = factor(outcome_measure_pricelevel_cons, 
                                             levels = c("cpi", "deflator", "wpi", "core")))
levels(data$outcome_measure_pricelevel_cons)

# Consolidate data frequency ----
data$freq <- case_when(
  data$month == 1 ~ "month",
  data$quarter == 1 ~ "quarter",
  data$annual == 1 ~ "annual"
)
# Convert to factor
data$freq <- factor(data$freq, 
                              levels = c("quarter", "month", "annual"))

# Generate byproduct dummy ----
data$byproduct <- mapply(function(main, outcome) {
  # Split main into components
  main_components <- unlist(strsplit(main, "\\s+"))
  
  # Check if it's a byproduct based on the rules
  is_byproduct <- switch(outcome,
                         "inflation" = !("inflation" %in% main_components),
                         "output" = !("gdp" %in% main_components),
                         "unemp" = !("emp" %in% main_components),
                         "emp" = !("emp" %in% main_components),
                         "rate" = TRUE  # rate is always a byproduct as it has no direct main category
  )
  
  return(as.numeric(is_byproduct))
}, data$main, data$outcome)

# Generate number of observations used for a specific model ----

# Unnest `n_of_countries` and make numeric
data <- data %>% unnest(n_of_countries) %>%
  mutate(n_of_countries = as.numeric(n_of_countries))
unique(data$n_of_countries)


convert_quarter <- function(quarter_str) {
  # Extract quarter and year
  qtr <- as.numeric(substring(quarter_str, 2, 2))
  year <- as.numeric(substring(quarter_str, 4))
  
  # Convert to date (first day of the quarter)
  return(ymd(paste0(year, "-", (qtr - 1) * 3 + 1, "-01")))
}

# Apply transformations
data <- data %>%
  mutate(
    # Determine date type
    start_type = case_when(
      substr(start, 1, 1) == "Q" ~ "quarter",
      grepl("-", start) ~ "month",
      nchar(start) == 4 ~ "year",
      TRUE ~ "unknown"
    ),
    end_type = case_when(
      substr(end, 1, 1) == "Q" ~ "quarter",
      grepl("-", end) ~ "month",
      nchar(end) == 4 ~ "year",
      TRUE ~ "unknown"
    ),
    # Convert start dates
    start_date = case_when(
      start_type == "quarter" ~ convert_quarter(start),
      start_type == "month" ~ as.Date(my(start)),
      start_type == "year" ~ ymd(paste0(start, "-01-01"))
    ),
    # Convert end dates
    end_date = case_when(
      end_type == "quarter" ~ convert_quarter(end),
      end_type == "month" ~ as.Date(my(end)),
      end_type == "year" ~ ymd(paste0(end, "-12-31"))
    ),
    # Calculate observations
    observations_calc = case_when(
      start_type == "quarter" ~ ((year(end_date) - year(start_date)) * 4 + (quarter(end_date) - quarter(start_date)) + 1) * n_of_countries,
      start_type == "month" ~ ((year(end_date) - year(start_date)) * 12 + (month(end_date) - month(start_date)) + 1) * n_of_countries,
      start_type == "year" ~ (year(end_date) - year(start_date) + 1) * n_of_countries
    )
  ) %>%
  select(-start_type, -end_type)

# Transform samplesize to numeric
data$samplesize <- as.numeric(data$samplesize)

# Use information on samplesize provided directly by primary studies or our calculated sample size to obtain the number of observations per model:
data$observations <- ifelse(is.na(data$samplesize), data$observations_calc, data$samplesize)
anyNA(data$observations) # Should be FALSE, i.e. no NAs

# Country/region variables ----

# Recode the list_of_countries such that models estimated with multiple countries have proper country code formatting
data <- data %>% unnest(list_of_countries)
data$list_of_countries <- strsplit(data$list_of_countries, " ")

# Correct UK to GB (iso2c)
data$list_of_countries <- ifelse(data$list_of_countries=="UK", "GB", data$list_of_countries)
any(data$list_of_countries == "UK") # Should be FALSE

# Create US dummy
data$us <- ifelse(data$list_of_countries == "US", 1, 0)

# Define the Euro area 12 (EA12) countries
ea12_countries <- c("BE", "DE", "ES", "FR", "IE", "IT", "LU", "NL", "AT", "PT", "FI", "GR")

# Create ea12 dummy
in_ea12 <- function(vec) {
  all(vec %in% ea12_countries)
}
data$ea12 <- sapply(data$list_of_countries, in_ea12)
data$ea12 <- ifelse(data$list_of_countries == "EA", TRUE, data$ea12)

# Create vectors for World Bank income classifications for 2025 fiscal year (based on https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups, accessed 23122024)
wb_groups <- readxl::read_excel("data/world_bank_country_goups_2025_fiscal_year.xlsx")
income_groups <- unique(wb_groups$`Income group`)
# Create function to process each wb income group
process_income_group <- function(data, income_groups) {
  result <- data %>% 
    filter(`Income group` == income_groups) %>%
    select("Code", "Economy")
  
  result$iso2c <- countrycode::countrycode(result$Code, "wb", "iso2c")
  return(result)
}
# Process each income level
wb_low_income <- process_income_group(wb_groups, income_groups[1])
wb_upper_middle_income <- process_income_group(wb_groups, income_groups[2])
wb_high_income <- process_income_group(wb_groups, income_groups[3])
wb_lower_middle_income <- process_income_group(wb_groups, income_groups[4])
# Add special cases, e.g. EA
any(wb_high_income$iso2c == "EA", na.rm = TRUE) # Should be FALSE
wb_high_income <- rbind(wb_high_income, data.frame(Code = NA, Economy = "Euro Area", iso2c = "EA"))

# Function to classify each vector
classify_vector <- function(vec, 
                            high_income = wb_high_income$iso2c, 
                            upper_middle_income = wb_upper_middle_income$iso2c,
                            lower_middle_income = wb_lower_middle_income$iso2c,
                            low_income = wb_low_income$iso2c
                            ) {
  all_high <- all(vec %in% high_income)
  all_upper_middle <- all(vec %in% upper_middle_income)
  all_lower_middle <- all(vec %in% lower_middle_income)
  all_low <- all(vec %in% low_income)
  
  if (all_high) {
    return("high_income")
  } else if (all_upper_middle) {
    return("upper_middle_income")
  } else if (all_lower_middle) {
    return("lower_middle_income")
  } else if (all_low) {
    return("low_income")
  } else {
    return("mixed_or_unclear")
  }
}
# Test function to run a case and print results
test_classification <- function(countries, description) {
  result <- classify_vector(countries)
  cat(sprintf("Test: %s\nCountries: %s\nResult: %s\n\n", 
              description, 
              paste(countries, collapse=", "), 
              result))
}
# Single country tests
test_classification("DE", "Single high income country")
test_classification("BR", "Single upper middle income country")
test_classification("IN", "Single lower middle income country")
test_classification("BI", "Single low income country")
# Multi-country same group tests
test_classification(c("DE", "FR", "US"), "Multiple high income countries")
test_classification(c("BR", "CN", "ZA"), "Multiple upper middle income countries")
test_classification(c("IN", "PH"), "Multiple lower middle income countries")
test_classification(c("BI", "RW"), "Multiple low income countries")
# Mixed group tests
test_classification(c("DE", "BR"), "Mix: high and upper middle")
test_classification(c("US", "IN"), "Mix: high and lower middle")
test_classification(c("DE", "BR", "IN"), "Mix: high, upper middle, and lower middle")
test_classification(c("DE", "BR", "IN", "BI"), "Mix: all income groups")

# Apply the function to each vector in data$list_of_countries and create a factor variable
data$country_dev <- sapply(data$list_of_countries, classify_vector)
# View the unique combinations 
View(data %>% select(country_dev,list_of_countries) %>% distinct() %>% arrange(country_dev,list_of_countries))
# View counts
data %>%
  group_by(country_dev) %>%
  summarise(n = n())

# Add income groups as dummies
data$high_income <- ifelse(data$country_dev == "high_income", TRUE, FALSE)
data$upper_middle_income <- ifelse(data$country_dev == "upper_middle_income", TRUE, FALSE)
data$lower_middle_income <- ifelse(data$country_dev == "lower_middle_income", TRUE, FALSE)
data$low_income <- ifelse(data$country_dev == "low_income", TRUE, FALSE)

# Unnest variables which are unnecessarily lists and use 0/1 coding instead of TRUE/FALSE coding. ---- 
# Which of data are lists?
data %>% map_lgl(is.list)
# Unnest
data <- data %>% 
  unnest(model_notes) %>% 
  unnest(prefer) %>% 
  unnest(forecast_based) %>% 
  unnest(nr) %>% 
  unnest(chol) %>% 
  unnest(var) %>% 
  unnest(idother) %>% 
  unnest(bayes) %>% 
  unnest(dsge) %>% 
  mutate_if(is.logical, as.numeric)

# Merge publication characteristics data ----

# Correct incorrect publication title
data$`publication title` <- ifelse(data$`publication title` == "International Journal of …", "International Journal of Finance & Economics", data$`publication title`)

# Read in data
ranking_impact <- read_excel(here::here("data/study_characteristics/journals_ranking_impact.xlsx"))
citations <- read_excel(here::here("data/study_characteristics/citations_for_included_studies.xlsx"))

# Merge num_cit (number of citations) using the study key. 
data <- data %>% 
  left_join(citations %>% select(key, num_cit), by = "key")

# Merge journal ranking and impact data
data <- data %>% 
  left_join(ranking_impact %>% select(publication.title, journal_ranking, journal_impact), by = c("publication title" = "publication.title"))
data$journal_impact <- ifelse(is.na(data$journal_impact), 0, data$journal_impact)
data$journal_ranking <- ifelse(is.na(data$journal_ranking), 0, data$journal_ranking)

# Tidy up ----
remove(in_ea12)
remove(classify_vector)
remove(citations)
remove(ranking_impact)
remove(process_income_group)
remove(wb_high_income)
remove(wb_lower_middle_income)
remove(wb_low_income)
remove(wb_upper_middle_income)
remove(income_groups)
remove(ea12_countries)
remove(convert_quarter)
remove(split_list)
remove(test_classification)
remove(wb_groups)
remove(classification_lookup)
remove(classify_inttype)
remove(all_inttypes)
remove(classified_inttypes)
remove(df_full_bib)

# Store data ----
save(data, file = "data/final_data_working_paper_1.RData")
