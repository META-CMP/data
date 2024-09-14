rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


library(here)


data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)


data_back<-data


# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(modelsummary) # to nicely show the model summary results
library(JWileymisc) # for Winsorization


data<-data_back

out<-'inflation'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% out)





# Create empty lists to store the results
results_list_regwa_topten <- list()
confint_data_regwa_topten <- list()


periods <- c(3, 6, 12, 18, 24, 30, 36, 48)

wins<-0.02
# Loop through periods
for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  #data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  # Winsorize data
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, wins, na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, wins, na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  # Store the winsorized data
  
  
  # Calculate variance winsorised
  data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
  
  # Calculate PrecVariance winsorised
  data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor
  
  
  topten_data_period <- data_period_winsor %>% dplyr::mutate(rank = rank(desc(standarderror_winsor))) %>%
    dplyr::filter(rank > quantile(rank, .9)) %>%
    dplyr::arrange(rank)
  
  print(paste("Observations top 10% ", nrow(topten_data_period)))
  # Calculate (precision-weighted) average
  #result in Gechert and Heimberger (2022): Table 3, column (5)
  #weighted-average
  regwa_topten <- lm(mean.effect_winsor~1, data=topten_data_period, weights=precvariance_winsor)
  results_list_regwa_topten[[paste0(x, ".top_10")]]<-regwa_topten
  #confidence interval
  confint_data_regwa_topten[[paste0(x, ".top_10")]]<-confint(regwa_topten, level=0.95)
  
  
  #WAAP (weighted average of the adequately powered)
  #powered
  data_period_winsor$powered <- abs(data_period_winsor$mean.effect/2.8) # 2.8 critical value according to the original paper
  #Non-linear tests
  data_period_winsor$WAAP <- ifelse(data_period_winsor$standarderror_winsor < data_period_winsor$powered, 1, 0)
  data_period_winsor_WAAP <- data_period_winsor
  data_period_winsor_WAAP <- subset(data_period_winsor, WAAP %in% c('1'))
  
  #weighted average of the adequately powered
  regwa_data_period_WAAP <- lm(mean.effect_winsor~1, data=data_period_winsor_WAAP, weights=precvariance_winsor)
  results_list_regwa_topten[[paste0(x, ".waap")]]<-regwa_data_period_WAAP
  #confidence interval
  confint_data_regwa_topten[[paste0(x, ".waap")]]<-confint(regwa_data_period_WAAP, level=0.95)
}

modelsummary(results_list_regwa_topten, output = "gt",stars = TRUE)

confint_data_regwa_topten
