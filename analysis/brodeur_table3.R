
####################### This code aims to replicate the analysis of brodeur 2020 table 3. 

rm(list = ls())



library(here)

data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)


data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
library(broom)# to apply function for different thresholds 
library(purrr)# to apply function for different thresholds 
library(stargazer)# for table
library(rlang)# to get the sym function



data<-data_back

out<-'output'#c("output", "inflation", "unemp", "emp")

data <- subset(data, outcome %in% out)

periods <- c(3, 6, 12,15, 18,21, 24, 30, 36,48)
data<-data %>% filter(period.month %in% periods)

data<-data %>% filter(quality_concern!=1)
wins<-0.02


data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, wins, na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, wins, na.rm = TRUE)) %>% 
  mutate(z_stat=abs(mean.effect/StandardError)) %>% 
  mutate(z_stat_winsor=abs(mean.effect_winsor/standarderror_winsor)) %>% 
  mutate(x=1:length(StandardError) / 100)


# save under a different name 
data_gg<-data


###### to filter out negative effects
data_gg<-data_gg %>% filter(mean.effect<=0)#& transformation=="log"


###### to filter out positive effefts
#data_gg<-data_gg %>% filter(mean.effect>=0)



###### function to calculate share and binomial test p.value per group for a respective threshold and delta
summarize_results <- function(df, group_var, threshold, delta) {
  group_var <- sym(group_var)
  
  df %>%
    group_by(!!group_var) %>%
    mutate(binomial = ifelse(z_stat >= threshold, 1, 0),
           within_delta = abs(z_stat - threshold) < delta) %>%
    filter(within_delta) %>%
    summarize(
      proportion = sum(binomial) / n(),
      pvalue = binom.test(sum(binomial), n(), p = 0.5, alternative = "greater")$p.value,
      obs = n(),
      .groups = 'drop'
    ) %>%
    mutate(threshold = threshold, delta = delta)
}

# Define thresholds and deltas
# Threshold set to 68%,90%, and 95% confidence region
thresholds <- c(1,1.645, 1.96)
# define deltas the same way as in the Brodeur paper
deltas <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.075, 0.05)



# Apply the function across all thresholds and deltas per identification strategy # however we ignore the period.month  horizon. Therefore the data should be filtered to only contain periods which behave similar. For example with # data_gg %>% filter(period.month <=30 & period.month >=12)
results_ident <- expand.grid(threshold = thresholds, delta = deltas) %>%
  pmap_dfr(function(threshold, delta) {
    summarize_results(data_gg %>% filter(period.month <=30 & period.month >=12), "group_ident_broad", threshold, delta)
  })

# Apply the function across all thresholds and deltas per period.month
results_period <- expand.grid(threshold = thresholds, delta = deltas) %>%
  pmap_dfr(function(threshold, delta) {
    summarize_results(data_gg, "period.month", threshold, delta)
  })



# manipulate the resutls to show it in a table as in brodeur for identification strategy groups. 
results_ident_long <- results_ident %>%
  filter(threshold==1) %>% ######## select here the significance threshold. Currently it is set to 1, which represents the 68% confidence level. 
  mutate(threshold_delta = paste0(threshold, "±", delta)) %>%
  pivot_longer(cols = c(proportion, pvalue, obs),
               names_to = "measure",
               values_to = "value") %>%
  unite(measure_threshold_delta, measure, threshold_delta, sep = "_") %>%
  pivot_wider(names_from = group_ident_broad, values_from = value)

# manipulate the resutls to show it in a table as in brodeur for period.month groups. 
results_period_long <- results_period %>%
  filter(threshold==1) %>%  ######## select here the significance threshold. Currently it is set to 1, which represents the 68% confidence level. 
  mutate(threshold_delta = paste0(threshold, "±", delta)) %>%
  pivot_longer(cols = c(proportion, pvalue, obs),
               names_to = "measure",
               values_to = "value") %>%
  unite(measure_threshold_delta, measure, threshold_delta, sep = "_") %>%
  pivot_wider(names_from = period.month, values_from = value)



###################### plot tables

# Create the latex table using stargazer
stargazer(results_ident_long, type = "latex",
          summary = FALSE,
          title = "Summary of Results",
          align = TRUE,
          column.sep.width = "1pt",
          no.space = TRUE)


# plot dt table for identification strategies

library(DT)
datatable(results_ident_long %>% mutate(across(is.numeric, signif, digits = 3)), rownames = FALSE, options = list(
  dom = 't',
  ordering=F,
  pageLength = 20,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#eda698', 'color': '#fff'});",
    "}")
))%>%
  formatStyle(
    4:ncol(results_period_long),
    color = styleInterval(c(0.05,0.1), c('blue',"red", 'black'))
  )


# plot dt table different period.month observations.

datatable(results_period_long %>% mutate(across(is.numeric, signif, digits = 3)), rownames = FALSE, options = list(
  dom = 't',
  ordering=F,
  pageLength = 24,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#eda698', 'color': '#fff'});",
    "}")
))%>%
  formatStyle(
    4:ncol(results_period_long),
    color = styleInterval(c(0.05,0.1), c('blue',"red", 'black'))
  )

##################################################### replicate brodeur results: they are the same with respect to their stata code, but different with respect to their paper ##################################

# # Read in the data
# setwd("C:/Users/Enzinger/Downloads/R_brodeur/R/data")
# test <- read.csv("MM Data.csv")
# 
# summarize_results <- function(df, threshold, delta,method) {
#   df %>%
#     group_by(method) %>%
#     mutate(binomial = ifelse(t >= threshold, 1, 0),
#            within_delta = abs(t - threshold) < delta) %>%
#     filter(within_delta) %>%
#     summarize(
#       proportion = sum(binomial) / n(),
#       pvalue = binom.test(sum(binomial), n(), p = 0.5,alternative = "greater")$p.value,
#       obs = n(),
#       .groups = 'drop'
#     ) %>%
#     mutate(threshold = threshold, delta = delta)
# }
# 
# # Apply the function across all thresholds and deltas
# results <- expand.grid(threshold = thresholds, delta = deltas) %>%
#   pmap_dfr(function(threshold, delta) {
#     summarize_results(test, threshold, delta, method)
#   })
# 
# results_long <- results %>%
#   filter(threshold==1.96) %>% 
#   mutate(threshold_delta = paste0(threshold, "±", delta)) %>%
#   pivot_longer(cols = c(proportion, pvalue, obs),
#                names_to = "measure",
#                values_to = "value") %>%
#   unite(measure_threshold_delta, measure, threshold_delta, sep = "_") %>%
#   pivot_wider(names_from = method, values_from = value)
