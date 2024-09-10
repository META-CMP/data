rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


library(here)

data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)
source(here("analysis/R/EK.R"))


data_back<-data


# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization



data<-data_back

out<-'output'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% out)


# set winsorization level
wins<-0.02


periods <- c(3, 6, 12, 18, 24, 30, 36, 48)
# create empty list to store EK results
EKresults_list<-list()

# Loop through periods
for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  # Winsorize data
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, wins, na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, wins, na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  # Store the winsorized data and select only the relevant variables
  
  data_BomRachinger <- data_period_winsor %>%  dplyr::select(mean.effect_winsor, standarderror_winsor,key)
  
  
  est_EK<-EK(data=data_BomRachinger,verbose=T)
  
  # reports the EK results
  object<-c("EK's mean effect estimate (alpha1)","standard error","EK's publication bias estimate (delta)","standard error")
  
  EKresults<-data.frame(t(c(est_EK$effect, est_EK$SE_effect,est_EK$pubbias, est_EK$SE_pubbias)))
  colnames(EKresults)<-object
  
  EKresults_list[[paste0(x)]] <- EKresults
}


data.table::rbindlist(EKresults_list, fill = T,idcol = ".id")

