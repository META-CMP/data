rm(list = ls())



library(here)

data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)
source(here("analysis/R/stem_method.R"))

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization


data<-data_back



out<-'output'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% out & transformation=="log")

# set winsorization parameter
wins<-0.02

data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, wins, na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, wins, na.rm = TRUE)) %>% 
  mutate(precision_winsor=1 /standarderror_winsor) %>% ungroup() %>% 
  filter(quality_concern!=1)## filter out quality concerned studies



### if you want to draw a random estimate per study uncomment this code
# data <- data %>%
#   group_by(key,period.month) %>%
#   sample_n(size = 1,replace = F) %>% ungroup()

#set up empty list to stroe results
stem_results<-list()


periods <- c(3, 6, 12,15, 18,21, 24, 30, 36, 48)

# Loop through periods
for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period_Furukawa <- data_period %>%  dplyr::select(mean.effect_winsor, standarderror_winsor)
  
  stem_results[[paste0(x, ".stem")]] <- stem(data_period_Furukawa$mean.effect_winsor, data_period_Furukawa$standarderror_winsor, param)
  
}

lapply(stem_results, `[[`, 1)

#data.table::rbindlist(lapply(stem_results, `[[`, 1), fill = T,idcol = ".id")


### selesct specific period for the stem based funnel
plot_for_period<-30
entry<-which(periods == plot_for_period)


# Plot the stem based funnel for this period. 
stem_funnel(data %>% dplyr::filter(period.month==plot_for_period) %>% pull(mean.effect_winsor), data %>% filter(period.month %in% plot_for_period) %>% pull(standarderror_winsor), stem_results[[entry]]$estimates)

stem_results[[entry]]$estimates



# median_data <- data_median(data_period_winsor, "key", "mean.effect_winsor", "standarderror_winsor") # some error in this code if we look at the data median. 
# test <- stem(median_data$coefficient, median_data$standard_error, param)
