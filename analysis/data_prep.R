rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
## version.string R version 4.2.1 (2022-06-23 ucrt)

#setwd and load dataset
setwd("~/data")
load("preliminary_data.RData")


# load packackages 
library(dplyr) # for data manipulation
library(tidyverse) # for data manipulation
library(stringi) # for advanced str_split 
library(zoo) # for easier manipulation of date format 


################################################################### prepare variables for publication bias tests #############################################################


###### filter out rate response and merge it as an explanatory variable

# filter date responses
rate<-data %>% dplyr::filter(data$outcome_var=="rate")
# filter outcome variables responses
other<-data %>% dplyr::filter(data$outcome_var!="rate")

# select only the necessary variables of the rate dataset
rate<-rate %>% dplyr::select(key,model_id,period:SE.lower) %>% dplyr::select(-outcome_var)

# rename colnames that they correspond to the rate responses
colnames(rate)[4:ncol(rate)] <- paste("rate_",colnames(rate)[4:ncol(rate)],sep="")

# merge to main dataset
data<-other %>% dplyr::left_join(rate, by=c("key","model_id","period"))
remove(other)


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

# create outcome variable one of c("gdp","inflation","emp")
data$outcome<-ifelse(data$outcome_measure %in% c("gdp","ip","gap","gnp"),"gdp",ifelse(data$outcome_measure %in% c("cpi","deflator","wpi","core","price_level"),"inflation","emp"))


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



##### generate dummy if ARDL model has been used 
data<-data %>% mutate(dyn_ols=ifelse(dyn_ols=="ARDL" | dyn_ols=="ardl",TRUE,FALSE))



##### generate number of observations used for a specific model

# Ensure `n_of_countries` is not nested and is numeric
data <- data %>% unnest(n_of_countries) %>%
  mutate(n_of_countries = as.numeric(n_of_countries))

# Replace long dash with short dash
data <- data %>%
  mutate(start = ifelse(start == "Q4–2000", "Q4-2000", start),
         end = ifelse(end == "Q4–2015", "Q4-2015", end))

# Calculate `observations_calc`
data <- data %>%
  mutate(observations_calc = case_when(
    substr(start, 1, 1) == "Q" ~ ((as.yearqtr(end, format = "Q%q-%Y") - as.yearqtr(start, format = "Q%q-%Y")) * 4 + 1) * n_of_countries,
    substr(start, 1, 1) != "Q" & grepl("-", start) ~ ((as.yearmon(end, format = "%m-%Y") - as.yearmon(start, format = "%m-%Y")) * 12 + 1) * n_of_countries,
    nchar(start) == 4 ~ (as.numeric(end) - as.numeric(start) + 1) * n_of_countries
  ))

# set samplesize to numeric
data$samplesize<-as.numeric(data$samplesize)

# use information on the samplesize provided by the initial studies or our calculated sample size to obtain the number of observations per model. 
data$observations<-ifelse(is.na(data$samplesize), data$observations_calc, data$samplesize)
#anyNA(data$observations) # no nas. 



##### Generate factor indicating whether the estimates are obtained by using an US, EA, or other region. 
data$us<-as.factor(ifelse(data$list_of_countries=="US","US",ifelse(data$list_of_countries=="EA","EA","other")))



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
data<-data %>%
  mutate(start_year= as.numeric(case_when(substr(start,1,1)=="Q" ~ format(as.Date(as.yearqtr(start, format = "Q%q-%Y")),"%Y"),
                                          substr(start,1,1)!="Q" & grepl("-", start) == TRUE ~ format(as.Date(as.yearmon(start, format = "%m-%Y")),"%Y"),
                                          nchar(start)==4 ~ format(as.Date(start, format = "%Y"),"%Y"))),
         end_year= as.numeric(case_when(substr(end,1,1)=="Q" ~ format(as.Date(as.yearqtr(end, format = "Q%q-%Y")),"%Y"),
                                        substr(end,1,1)!="Q" & grepl("-", end) == TRUE ~ format(as.Date(as.yearmon(end, format = "%m-%Y")),"%Y"),
                                        nchar(end)==4 ~ format(as.Date(end, format = "%Y"),"%Y"))),
         mean_year=(start_year+end_year)/2
  )

# read in the external-data.csv file and drop the unnecessary column
data_merged<-read.csv("~/data/data/merge_external_data/external-data.csv") %>% select(-X)

# recode year as numeric
data_merged$year<-as.numeric(data_merged$year)


# Recode the list_of_countries into a proper list of countries
# first unlist the current list
data<-data %>% unnest(list_of_countries)
# create a new list, where each country of a Panel corresponds to an item of the new list. 
data$list_of_countries<-strsplit(data$list_of_countries, " ")


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




