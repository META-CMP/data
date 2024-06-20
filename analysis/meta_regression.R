rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
source("data/data_prep.R")


data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
library(clubSandwich)# for coef test results
library(modelsummary)# to nicely print results


data<-data_back

out<-'gdp'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)


results_list<-list()
coef_test_data<-list()
confint_data<-list()

summary(data)
equation<-mean.effect_winsor ~standarderror_winsor+exrate+gdppc+cbi+findev+fingl+infl+tradegl+mean_year+regime+quality_concern+observations+main_research_q+outcome_measure+as.factor(periodicity)+as.factor(transformation)+rate_mean.effect+cbanker+decomposition+convent+pure_rate_shock+lrir+fx+foreignir+inflexp+eglob+find+outpgap+comprice+panel+n_of_countries+us+month+quarter+upr+lor+varother+dsge+bayes+gvar+tvar+fvar+dyn_ols+vecm+lp+idother+longrun+heteroskedas+hf+signr+svar+chol+event+nr+forecast_based+iv+prefer+shock_size+interest_rate_short+as.factor(rid1)+model_id
# +fexch#+real_output # only for output regression

periods <- c(3, 6, 12, 18, 24, 30, 36, 48)


for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$precision <- 1 / data_period$StandardError
  
  # Winsorize data
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  # Store the winsorized data
  
  
  # Calculate variance winsorised
  data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
  
  # Calculate PrecVariance winsorised
  data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor
  
  # Calculate (precision-weighted) average
  regwa <- lm(equation, data = data_period_winsor)#, weights = precvariance_winsor
  results_list[[paste0(x, ".ols")]] <- regwa
  
  coef_test_data[[paste0(x, ".ols")]]<-coef_test(regwa, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  
  confint_data[[paste0(x, ".ols")]]<-confint(regwa, level=0.95)
}





#modelsummary(results_list, output = "gt",vcov = 'cr0', cluster = data_period_winsor$key,stars = TRUE) # only works if cluster variable has the same number of observations across lists. 
modelsummary(results_list, output = "gt",stars = TRUE)

coef_test_data<-data.table::rbindlist(coef_test_data, fill = T,idcol = ".id")
coef_test_data


######################## search for explaining factors of high precision: 
library(car)
#equation<-precvariance_winsor ~mean_year+regime+quality_concern+observations+main_research_q+outcome_measure+as.factor(periodicity)+as.factor(transformation)+cbanker+decomposition+convent+pure_rate_shock+lrir+fx+foreignir+inflexp+eglob+find+outpgap+comprice+panel+n_of_countries+us+month+quarter+upr+lor+varother+dsge+bayes+gvar+tvar+fvar+dyn_ols+vecm+lp+idother+longrun+heteroskedas+hf+signr+svar+chol+event+nr+forecast_based+iv+prefer+interest_rate_short+as.factor(rid1)+model_id

equation<-precvariance_winsor ~mean_year+quality_concern+observations+main_research_q+relevel(as.factor(outcome_measure), ref = "gdp")+as.factor(periodicity)+relevel(as.factor(transformation), ref = "log")+cbanker+panel+n_of_countries+us+month+dsge+lp+prefer+as.factor(rid1)+model_id
# +fexch#+real_output # only for output regression

periods <- c(3, 6, 12, 18, 24, 30, 36, 48)
results_list<-list()
vif<-list()

for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)


data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
data_period$precision <- 1 / data_period$StandardError

# Winsorize data
data_period_winsor <- data_period
data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.0002), na.rm = TRUE)
data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.0002), na.rm = TRUE)
data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor

# Store the winsorized data


# Calculate variance winsorised
data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2

# Calculate PrecVariance winsorised
data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor

dataspeed<-data_period_winsor %>% ungroup() %>% select(precvariance_winsor, mean_year, regime, quality_concern, observations, main_research_q, outcome_measure, periodicity,transformation, cbanker, decomposition, convent, pure_rate_shock, lrir, fx, foreignir, inflexp, eglob, find, outpgap, comprice, panel, n_of_countries, us, month, quarter, upr, lor, varother, dsge, bayes, gvar, tvar, fvar, dyn_ols, vecm, lp, idother, longrun, heteroskedas, hf, signr, svar, chol, event, nr, forecast_based, iv, prefer, interest_rate_short,rid1, model_id)

anyNA(dataspeed)

test<-dataspeed %>% 
  summarise(across(everything(), ~ sum(is.na(.x))))

dataspeed<-na.omit(dataspeed)

vif_values <- car::vif(lm(dataspeed))
vif[[paste0(x, ".vif")]]<-vif_values

# Calculate (precision-weighted) average
regwa <- lm(equation, data = data_period_winsor)#, weights = precvariance_winsor
both_model <- step(regwa, direction = "both")
results_list[[paste0(x, ".ols")]] <- both_model
}


modelsummary(results_list, output = "gt",stars = TRUE)
