rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
## version.string R version 4.2.1 (2022-06-23 ucrt)

#setwd and load dataset
# setwd("~/data")
load("data/preliminary_data_20062024.RData")


# load packackages 
library(dplyr) # for data manipulation
library(tidyverse) # for data manipulation
library(stringi) # for advanced str_split 
library(zoo) # for easier manipulation of date format 


################################################################### prepare variables for publication bias tests #############################################################


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
data$outcome<-ifelse(data$outcome_measure=="rate","rate",ifelse(data$outcome_measure %in% c("gdp","ip","gap","gnp"),"gdp",ifelse(data$outcome_measure %in% c("cpi","deflator","wpi","core","price_level"),"inflation","emp")))


# Splitting emp and unemp
data$outcome <- ifelse(data$outcome_measure == "une_rate", "unemp", data$outcome)



################################################################### prepare variables for regression analysis #############################################################


##### generate main research question dummy (main_research_q)

fun <- function(x, y) {
  grepl(x, y, fixed = TRUE)
}
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

# Store data for use in app
save(data,file = "data/preliminary_data_test.RData")

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






##### generate dummy if short term interest rate is used as the shock variable. At the moment short term interest rate is defined as < 1 month.
split_list<-stri_split_fixed(str = data$inttype, pattern = "_", n = 3)

# Extract nth element from each split list
data$intrest_rate <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

data$interest_rate_short<-ifelse(grepl("3_month",data$intrest_rate) | grepl("1_month",data$intrest_rate) | grepl("6_month",data$intrest_rate) | grepl("1_year",data$intrest_rate)| grepl("2_year",data$intrest_rate) | grepl("ssr",data$intrest_rate) | grepl("shadow_fed_funds",data$intrest_rate)| grepl("long_term_rate",data$intrest_rate)| grepl("shadow_rate",data$intrest_rate),FALSE, TRUE)

remove(split_list)


##### unlist all the variables which are still lists and use 0/1 coding instead of TRUE/FALSE coding.
data<-data %>% unnest(n_of_countries) %>% unnest(chol) %>% unnest(bayes) %>% unnest(dsge) %>% unnest(idother) %>% unnest(var) %>% unnest(model_notes) %>% unnest(prefer) %>% mutate_if(is.logical, as.numeric) 
#  %>% unnest(page) %>% unnest(figure) 




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




