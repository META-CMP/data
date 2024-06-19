rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
source("data/data_prep.R")


data_back<-data


# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(clubSandwich)# for coef test results
library(modelsummary) # to nicely show the model summary results
library(JWileymisc) # for Winsorization


data<-data_back

out<-'inflation'#c("gdp", "inflation", "unemp", "emp")
outcome<-"the price level" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)




# Create empty lists to store the results
results_list <- list()
coef_test_data <- list()
confint_data <- list()
cluster_var <- list()


periods <- c(3, 6, 12, 18, 24, 30, 36, 48)



# Loop through periods
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
  
  # Calculate (precision-weighted) average - Unweighted Mean
  regwa <- lm(mean.effect_winsor ~ 1, data = data_period_winsor)
  results_list[[paste0(x, ".ols")]] <- regwa
  
  coef_test_data[[paste0(x, ".ols")]]<-coef_test(regwa, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  cluster_var[[paste0(x, ".ols")]]<-data_period_winsor$key
  
  confint_data[[paste0(x, ".ols")]]<-confint(regwa, level=0.95)
  
  # Baseline WLS - FAT-PET
  wls_pbias <- lm(mean.effect_winsor ~ standarderror_winsor, weights = precvariance_winsor, data = data_period_winsor)
  results_list[[paste0(x, ".wls")]] <- wls_pbias
  
  coef_test_data[[paste0(x, ".wls")]]<-coef_test(wls_pbias, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  cluster_var[[paste0(x, ".wls")]]<-data_period_winsor$key
  
  confint_data[[paste0(x, ".wls")]]<-confint(wls_pbias, level=0.95)
  
}

# naive T and CRO already fixed? 

# show results
modelsummary(results_list, output = "gt",stars = TRUE)

# show results for clustered standard errors based on the study level. 
coef_test_data<-data.table::rbindlist(coef_test_data, fill = T,idcol = ".id")
coef_test_data
