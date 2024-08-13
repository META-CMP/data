rm(list = ls())



setwd("~/data")


load("data/preliminary_data_test.RData")
source("analysis/R/stem_method.R")

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization


data<-data_back

out<-'output'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)


data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, c(0.02), na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, c(0.02), na.rm = TRUE)) %>% 
  mutate(precision_winsor=1 /standarderror_winsor) %>% ungroup() %>% 
  filter(quality_concern!=1)


# #data <- data %>%
#   group_by(key,period.month) %>%
#   sample_n(size = 1,replace = F) %>% ungroup()


stem_results<-list()


periods <- c(3, 6, 12,15, 18,21, 24, 30, 36, 48)

# Loop through periods
for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period_Furukawa <- data_period %>%  dplyr::select(mean.effect, StandardError)
  
  stem_results[[paste0(x, ".stem")]] <- stem(data_period_Furukawa$mean.effect, data_period_Furukawa$StandardError, param)
  
}

lapply(stem_results, `[[`, 1)

#data.table::rbindlist(lapply(stem_results, `[[`, 1), fill = T,idcol = ".id")

plot_for_period<-12

entry<-which(periods == plot_for_period)


#data$period.month==test

stem_funnel(data %>% dplyr::filter(period.month==plot_for_period) %>% pull(mean.effect), data %>% filter(period.month %in% plot_for_period) %>% pull(StandardError), stem_results[[entry]]$estimates)

stem_results[[entry]]$estimates



# median_data <- data_median(data_period_winsor, "key", "mean.effect_winsor", "standarderror_winsor") # some error in this code if we look at the data median. 
# test <- stem(median_data$coefficient, median_data$standard_error, param)
