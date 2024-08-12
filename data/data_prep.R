rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
## version.string R version 4.2.1 (2022-06-23 ucrt)

#setwd and load dataset
# setwd("~/data")
load("data/preliminary_data_01082024.RData")
load("data/papers_test.RData")# has been locally saved by ME




# load packackages 
library(dplyr) # for data manipulation
library(tidyverse) # for data manipulation
library(stringi) # for advanced str_split 
library(zoo) # for easier manipulation of date format 
library(readxl) # to read excel file

################################################################### join excle info dataset ##########################################################
data<-data %>% left_join(df_full_bib %>% select(key,`publication title`,type = `item type`, pub_year = `publication year`,BibtexKey,is_top_tier,is_top_5),"key")
# Join top 5 and top tier
data$top_5_or_tier <- ifelse(data$is_top_5 == 1 | data$is_top_tier == 1, TRUE, FALSE)
data$top_5_or_tier <- ifelse(is.na(data$top_5_or_tier), FALSE, data$top_5_or_tier)
#sum(data$top_5_or_tier,na.rm = FALSE)
data<-data %>% mutate(pub_year=2024-pub_year)
################################################################### prepare variables for publication bias tests #############################################################


###### Calculate the average standard error and precision options
data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
data$precision.avg <- 1 / data$SE.avg
data$precision.lower <- 1 / data$SE.lower
data$precision.upper <- 1 / data$SE.upper


###### filter out rate response and merge it as an explanatory variable

# filter date responses
rate<-data %>% dplyr::filter(data$outcome_var=="rate")


# select only the necessary variables of the rate dataset
rate<-rate %>% dplyr::select(key,model_id,period:SE.lower) %>% dplyr::select(-outcome_var)

# rename colnames that they correspond to the rate responses
colnames(rate)[4:ncol(rate)] <- paste("rate_",colnames(rate)[4:ncol(rate)],sep="")

# merge to main dataset
data<-data %>% dplyr::left_join(rate, by=c("key","model_id","period"))
remove(rate)


###### create transformation, periodicity, real_output dummy, outcome_measure and outcome variable

# get the different elements of the outcome_var string
split_list<-stri_split_fixed(str = data$outcome_var, pattern = "_", n = 3)

# Extract the first element of the outcome_var string and set the transformation factor accordingly. 
data$transformation <- sapply(split_list, function(x) ifelse(1 <= length(x), x[1], NA))

# Extract the second element of the outcome_var string and set the periodicity factor accordingly. 
data$periodicity <- sapply(split_list, function(x) ifelse(2 <= length(x), x[2], NA))

# Extract third element of the outcome var string which is used to obtain the real_output dummy, outcome_measure and outcome variable.
split_list <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

# create real_output dummy
data$real_output<-ifelse(split_list %in% c("rgdp","ip","rip","rgnp","rgap"),TRUE,NA)
data$real_output<-ifelse(split_list %in% c("gdp","gnp","gap"),FALSE,data$real_output)

# remove r from the outcome measure
split_list<-sub("^r", "", split_list)

# create outcome measure variable one of c("gdp","cpi","ip", "deflator","une_rate","emp_rate","gap","emp","wpi","core","price_level","gnp")
data$outcome_measure<-split_list
data$outcome_measure<-ifelse(data$outcome_var=="rate","rate",data$outcome_measure)

# create outcome variable one of c("gdp","inflation","emp")
data$outcome<-ifelse(data$outcome_measure=="rate","rate",ifelse(data$outcome_measure %in% c("gdp","ip","gap","gnp"),"output",ifelse(data$outcome_measure %in% c("cpi","deflator","wpi","core","price_level"),"inflation","emp")))


# Splitting emp and unemp
data$outcome <- ifelse(data$outcome_measure == "une_rate", "unemp", data$outcome)



################################################################### prepare variables for regression analysis #############################################################


##### generate main research question dummy (main_research_q)

fun <- function(x, y) {
  grepl(x, y, fixed = TRUE)
}
# first replace gdp by output to have it in the same form as in data$outcome. 
data$main<-gsub("gdp", "output", data$main)
# apply function
data$main_research_q<-mapply(fun, data$outcome, data$main)
remove(fun)



##### generate dummy if model or study has quality_concern
data$quality_concern<-grepl("quality_concern",data$study_notes)
data$quality_concern<-ifelse(grepl("quality_concern",data$model_notes),TRUE, data$quality_concern)


##### generate dummy if study or model estimates are obtained by using a regime approach other than lower or upper or cut vs hike
data$regime<-grepl("regime",data$study_notes)
data$regime<-ifelse(grepl("regime",data$model_notes),TRUE, data$regime)


##### create variable measuring the initial shock size (shock_size)
data <- data %>%
  group_by(key, model_id) %>%
  mutate(shock_size = first(rate_mean.effect[period.month == 0], default = NA)) %>% ungroup()

# If basis point shock is directly available.
data$shock_size <- ifelse(grepl("bp", data$size),as.double(sub("bp", "", data$size))/100,data$shock_size)


# If % shock is available, transform to basis points
data$shock_size <-  ifelse(grepl("%", data$size),as.double(sub("%", "", data$size)),data$shock_size)

summary(data$shock_size)



##### generate dummy if ARDL model has been used 
data<-data %>% mutate(dyn_ols=ifelse(dyn_ols=="ARDL" | dyn_ols=="ardl",TRUE,FALSE))

#data<-data %>% filter(quality_concern==FALSE)

data <- data %>%
  mutate(group_ident_broad = case_when(
    # Group 1: Chol, chol+SVAR
    (chol == 1 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "chol",
    (chol == 1 & svar == 1 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "chol",
    
    # Group 2: signr, signr+SVAR, chol+signr, signr+IV
    (signr == 1 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "signr",
    (signr == 1 & svar == 1 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "signr",
    (chol == 1 & signr == 1 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "signr",
    (signr == 1 & iv == 1 & chol == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "signr",
    
    # Group 3: chol
    (svar == 1 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "chol",
    
    # Group 4: HF+IV, HF, HF+signr, HF+signr+iv, hf+chol, hf+signr+chol, hf+iv+svar, hf+iv+chol
    (hf == 1 & iv == 1 & signr == 0 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & iv == 0 & signr == 0 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & signr == 1 & iv == 0 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & signr == 1 & iv == 1 & chol == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & chol == 1 & signr == 0 & iv == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & signr == 1 & chol == 1 & iv == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0 & svar == 0) ~ "hf",
    (hf == 1 & iv == 1 & svar == 1 & chol == 0 & signr == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    (hf == 1 & iv == 1 & chol == 1 & svar == 0 & signr == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & longrun == 0 & idother == 0) ~ "hf",
    
    # Group 5: Forecast+NR, Forecast+nr+iv, forecast+nr+chol,
    (forecast_based == 1 & nr == 1 & iv == 0 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & nr == 1 & iv == 1 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & nr == 1 & chol == 1 & iv == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    
    # Group6:  Forecast+chol, Forecast+Iv, forecast+svar+signr, forecast+chol+signr, forecast+signr, forecast+svar
    (forecast_based == 1 & chol == 0 & nr == 0 & iv == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & chol == 1 & nr == 0 & iv == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & iv == 1 & nr == 0 & chol == 0 & signr == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & svar == 1 & signr == 1 & nr == 0 & chol == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & chol == 1 & signr == 1 & nr == 0 & iv == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & signr == 1 & nr == 0 & iv == 0 & chol == 0 & svar == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    (forecast_based == 1 & svar == 1 & nr == 0 & iv == 0 & chol == 0 & signr == 0 & hf == 0 & heteroskedas == 0 & event == 0 & longrun == 0 & idother == 0) ~ "nr",
    
    # Group 7: IV+SVAR, IV, IV+idother, IV+chol
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
    
    # Group 9: forecast+hf+iv, Forecast+nr+hf+iv, hf+nr+svar, hf+event+forecast+iv, forecast+chol+hf, Forecast+nr+hf, iv+forecast+nr+signr+svar, forecast+iv+chol+hf, forecast+iv+event+chol+hf, forecast+nr+chol+signr, forecast+hf
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
    
    # Group 10: Longrun, longrun+SVAR, longrun+chol, longrun+SVAR+iv
    (longrun == 1 & svar == 0 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    (longrun == 1 & svar == 1 & chol == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    (longrun == 1 & chol == 1 & svar == 0 & signr == 0 & iv == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    (longrun == 1 & svar == 1 & iv == 1 & chol == 0 & signr == 0 & hf == 0 & heteroskedas == 0 & forecast_based == 0 & event == 0 & nr == 0 & idother == 0) ~ "idother",
    
    # Group 11: Event+signr, event+hf+iv, event+chol
    (event == 1 & signr == 1 & hf == 0 & iv == 0 & chol == 0 & svar == 0 & heteroskedas == 0 & forecast_based == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "nr",
    (event == 1 & hf == 1 & iv == 1 & signr == 0 & chol == 0 & svar == 0 & heteroskedas == 0 & forecast_based == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "nr",
    (event == 1 & chol == 1 & signr == 0 & hf == 0 & iv == 0 & svar == 0 & heteroskedas == 0 & forecast_based == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "nr",
    
    # Group 12: Heteroskedascticity, chol+Heteroskedascticity, svar+Heteroskedascticity, signr+hf+Heteroskedascticity, iv+forecast+nr+Heteroskedascticity
    (heteroskedas == 1 & chol == 0 & svar == 0 & signr == 0 & hf == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "idother",
    (heteroskedas == 1 & chol == 1 & svar == 0 & signr == 0 & hf == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "idother",
    (heteroskedas == 1 & svar == 1 & chol == 0 & signr == 0 & hf == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & longrun == 0 & idother == 0) ~ "idother",
    (heteroskedas == 1 & signr == 1 & hf == 1 & chol == 0 & iv == 0 & forecast_based == 0 & event == 0 & nr == 0 & svar == 0 & longrun == 0 & idother == 0) ~ "hf",
    (heteroskedas == 1 & iv == 1 & forecast_based == 1 & nr == 1 & chol == 0 & signr == 0 & hf == 0 & event == 0 & svar == 0 & longrun == 0 & idother == 0) ~ "nr",
    (heteroskedas == 1 & signr == 0 & hf == 1 & chol == 0 & iv == 1 & forecast_based == 0 & event == 0 & nr == 0 & svar == 0 & longrun == 0 & idother == 0) ~ "hf",
    
    # Default case
    TRUE ~ "Other"
  ))


data$is_top_5<-ifelse(is.na(data$is_top_5),0,data$is_top_5)
data$is_top_tier<-ifelse(is.na(data$is_top_tier),0,data$is_top_tier)

# Store data for use in app
# save(data,file = "data/preliminary_data_test.RData")

##### generate number of observations used for a specific model

# Ensure `n_of_countries` is not nested and is numeric
data <- data %>% unnest(n_of_countries) %>%
  mutate(n_of_countries = as.numeric(n_of_countries))

# Replace long dash with short dash
data <- data %>%
  mutate(start = ifelse(start == "Q4–2000", "Q4-2000", start),
         end = ifelse(end == "Q4–2015", "Q4-2015", end))

# # Calculate `observations_calc`


# data <- data %>%
#   mutate(observations_calc = case_when(
#     substr(start, 1, 1) == "Q" ~ ((as.yearqtr(end, format = "Q%q-%Y") - as.yearqtr(start, format = "Q%q-%Y")) * 4 + 1) * n_of_countries,
#     substr(start, 1, 1) != "Q" & grepl("-", start) ~ ((as.yearmon(end, format = "%m-%Y") - as.yearmon(start, format = "%m-%Y")) * 12 + 1) * n_of_countries,
#     nchar(start) == 4 ~ (as.numeric(end) - as.numeric(start) + 1) * n_of_countries
#   ))



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

print(data)



# set samplesize to numeric
data$samplesize<-as.numeric(data$samplesize)

# use information on the samplesize provided by the initial studies or our calculated sample size to obtain the number of observations per model. 
data$observations<-ifelse(is.na(data$samplesize), data$observations_calc, data$samplesize)
#anyNA(data$observations) # no nas. 

#summary(data$observations)

##### Generate factor indicating whether the estimates are obtained by using an US, EA, or other region. 

data$list_of_countries<-ifelse(data$list_of_countries=="UK","GB",data$list_of_countries)

# Recode the list_of_countries into a proper list of countries
# first unlist the current list
data<-data %>% unnest(list_of_countries)
# create a new list, where each country of a Panel corresponds to an item of the new list. 
data$list_of_countries<-strsplit(data$list_of_countries, " ")


# Define the Euro area 12 (EA12) countries
ea12_countries <- c("BE", "DE", "ES", "FR", "IE", "IT", "LU", "NL", "AT", "PT", "FI", "GR")

# Example list of vectors (replace with your actual list)

# Function to create dummy variable
in_ea12 <- function(vec) {
  all(vec %in% ea12_countries)
}

# Apply the function to each vector in the list
data$ea12 <- sapply(data$list_of_countries, in_ea12)
data$ea12<-ifelse(data$list_of_countries=="EA",TRUE,data$ea12)

#sum(data$ea12)


# test<-data %>% 
#   group_by(list_of_countries) %>% 
#   summarise(n = n())



high_income <- c("AU", "AT", "BE", "CA", "DK", "FI", "FR", "DE", "GR", "HK", "IE", 
                 "IL", "IT", "JP", "KR", "NL", "NZ", "NO", "PT", "SG", "ES", "SE", 
                 "CH", "TW", "GB", "US","EA","CY","LU","MT","IS","UK")

upper_middle_income <- c("AR", "BG", "BR", "CN", "CO", "CZ", "EE", "HU", "ID", 
                         "IN", "LV", "LT", "MY", "MX", "PE", "PL", "RO", "RU", 
                         "SA", "SI", "SK", "ZA", "TH", "TR","HR","MK","CL",
                         
                         "BD", "BI", "EG", "GT", "KE", "MN", "OM", "PH", "PK", 
                         "RW", "TZ", "UA", "UG", "VN","AM", "BH", "KH", "KW", "QA", "SG", "LK")

#lower_middle_income_low <- c()



# Function to classify each vector
classify_vector <- function(vec) {
  all_advanced <- all(vec %in% high_income)
  all_upper_middle <- all(vec %in% upper_middle_income)
  #all_lower_middle <- all(vec %in% lower_middle_income_low)
  
  if (all_advanced) {
    return("Advanced")
  } else if (all_upper_middle) {
    return("upper_middle")
  # } else if (all_lower_middle) {
  #   return("lower_middle")
  } else {
    return("Mixed or Unclassified")
  }
}

# Apply the function to each vector in the list and create a factor variable
data$country_dev <- sapply(data$list_of_countries, classify_vector)
#data$country_dev <- factor(classification, levels = c("Advanced", "Emerging", "Frontier", "Mixed or Unclassified"))
# 
# test<-data %>%
#   group_by(country_dev) %>%
#   summarise(n = n())

# Get advanced and emerigin as dummy
data$advanced <- ifelse(data$country_dev == "Advanced", TRUE, FALSE)
data$upper_middle <- ifelse(data$country_dev == "upper_middle", TRUE, FALSE)
data$mixed_unclass <- ifelse(data$country_dev == "Mixed or Unclassified", TRUE, FALSE)




##### generate dummy if short term interest rate is used as the shock variable. At the moment short term interest rate is defined as < 1 month.
split_list<-stri_split_fixed(str = data$inttype, pattern = "_", n = 3)

# Extract nth element from each split list
data$intrest_rate <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

data$interest_rate_short<-ifelse(grepl("3_month",data$intrest_rate) | grepl("1_month",data$intrest_rate) | grepl("6_month",data$intrest_rate) | grepl("1_year",data$intrest_rate)| grepl("2_year",data$intrest_rate) | grepl("ssr",data$intrest_rate) | grepl("shadow_fed_funds",data$intrest_rate)| grepl("long_term_rate",data$intrest_rate)| grepl("shadow_rate",data$intrest_rate),FALSE, TRUE)

remove(split_list)


##### unlist all the variables which are still lists and use 0/1 coding instead of TRUE/FALSE coding.
data<-data %>% unnest(n_of_countries) %>% unnest(chol) %>% unnest(bayes) %>% unnest(dsge) %>% unnest(idother) %>% unnest(var) %>% unnest(model_notes) %>% unnest(prefer) %>% unnest(nr) %>% unnest(forecast_based) %>% mutate_if(is.logical, as.numeric) 
#  %>% unnest(page) %>% unnest(figure) 




######################################################################### merge publication characteristics ###########################################################

# correct wrong publication title
data$`publication title` <- ifelse(data$`publication title` == "International Journal of …", "International Journal of Finance & Economics", data$`publication title`)

# read in data
ranking_impact<-read_excel("~/data/data/study_characteristics/journals_ranking_impact.xlsx")
citations<-read_excel("~/data/data/study_characteristics/citations_for_included_studies.xlsx")

# merge num_cit (number of citations) using the study key. 
data<-data %>% left_join(citations %>% select(key,num_cit),by="key")

data<-data %>% left_join(ranking_impact %>% select(publication.title, journal_ranking,journal_impact), by=c("publication title"="publication.title"))

data$journal_impact<-ifelse(is.na(data$journal_impact),0,data$journal_impact)
data$journal_ranking<-ifelse(is.na(data$journal_ranking),0,data$journal_ranking)

remove(citations)
remove(ranking_impact)



########################################################################## merge external data #######################################################################


# create start and end year of the sample based on start and end information in the dataset and calculate the mean year of the sample
# data<-data %>%
#   mutate(start_year= as.numeric(case_when(substr(start,1,1)=="Q" ~ format(as.Date(as.yearqtr(start, format = "Q%q-%Y")),"%Y"),
#                                           substr(start,1,1)!="Q" & grepl("-", start) == TRUE ~ format(as.Date(as.yearmon(start, format = "%m-%Y")),"%Y"),
#                                           nchar(start)==4 ~ format(as.Date(start, format = "%Y"),"%Y"))),
#          end_year= as.numeric(case_when(substr(end,1,1)=="Q" ~ format(as.Date(as.yearqtr(end, format = "Q%q-%Y")),"%Y"),
#                                         substr(end,1,1)!="Q" & grepl("-", end) == TRUE ~ format(as.Date(as.yearmon(end, format = "%m-%Y")),"%Y"),
#                                         nchar(end)==4 ~ format(as.Date(end, format = "%Y"),"%Y"))),
#          mean_year=(start_year+end_year)/2
#   )


data <- data %>%
  mutate(
    # Extract year from start and end dates
    start_year = year(start_date),
    end_year = year(end_date),
    mean_year = (start_year + end_year) / 2
  ) %>%
  select(-start_date, -end_date)

summary(data$mean_year)

# read in the external-data.csv file and drop the unnecessary column
data_merged<-read.csv("~/data/data/merge_external_data/external-data.csv") %>% select(-X)


# recode year as numeric
data_merged$year<-as.numeric(data_merged$year)

# define EA membership 
ea_membership <- data.frame(
  country = c("BE", "DE", "FI", "FR", "GR", "IE", "IT", "LU", "NL", "AT", "PT", "ES", "SI", "MT", "CY", "SK", "EE", "LV", "LT"),
  start_year = c(1970, 1970, 1970, 1970, 2001, 1970, 1970, 1970, 1970, 1970,
                 1970, 1970, 2007, 2008, 2008, 2009, 2011, 2014, 2015)
)

# join the ea_membership information to the external data
data_merged<-data_merged  %>% left_join(ea_membership, by = c("ccode" = "country"))

# dummy for each year whether a country is part of the EA in that year
data_merged <- data_merged %>%
  mutate(ea = ifelse(year >= start_year, TRUE, FALSE))

# build EA average where each country has the same weight and the set ccode for these observations as EA
ea_average <- data_merged %>%
  filter(ea == TRUE) %>%
  group_by(year) %>%
  summarize(across(tradegl:exrate, ~ mean(.x, na.rm = TRUE))) %>% mutate(ccode="EA")


# merge that information to the external data.
data_merged<-data_merged %>% select(-ea,-start_year) %>% bind_rows(ea_average)


# unnest year and list of countries and merge the respective external data in a first step. Secondly calculate the average of the external data for each model and study. 
data_merged<-data %>% select(list_of_countries,key,model_id,start_year,end_year) %>% 
  mutate(year = map2(start_year, end_year, seq, by = 1)) %>%
  unnest(year) %>% unnest(list_of_countries) %>%
  left_join(data_merged, by = c('year',"list_of_countries"="ccode")) %>%
  filter(year >= start_year & year <= end_year) %>%
  select(-list_of_countries) %>% 
  group_by(key, model_id,start_year,end_year) %>%
  summarise(across(everything(),~ mean(., na.rm = TRUE)))
data_merged

# merge these averages of the external data to the dataset based on study key and model_id. 
data<-data %>% ungroup() %>% left_join(data_merged %>% ungroup %>% select(key, model_id,tradegl:exrate), by=c("key","model_id"))


remove(data_merged)
remove(ea_average)
remove(ea_membership)
remove(df_full_bib)
remove(ea12_countries)
remove(high_income)
remove(upper_middle_income)
remove(convert_quarter)
remove(classify_vector)
remove(in_ea12)

# Store data for use in app
save(data,file = "data/preliminary_data_test.RData")

