
####################### This code aims to replicate the analysis of brodeur 2020 table 3. 

rm(list = ls())



setwd("~/data")


load("data/preliminary_data_test.RData")

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

out<-'gdp'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)

periods <- c(3, 6, 12, 18, 24, 30, 36,48)
data<-data %>% filter(period.month %in% periods)# omit two studies which lead to issues if we use winsorized data


data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, c(0.02), na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, c(0.02), na.rm = TRUE)) %>% 
  mutate(z_stat=abs(mean.effect/StandardError)) %>% 
  mutate(z_stat_winsor=abs(mean.effect_winsor/standarderror_winsor)) %>% 
  mutate(x=1:length(StandardError) / 100)


# look at data where most p-hacking happens propbably. 
data_gg<-data %>% filter(period.month <=30,period.month >=12)


###### function to calculate share and binomial test p.value per group 
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
thresholds <- c(1,1.645, 1.96)
deltas <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.075, 0.05)



# Apply the function across all thresholds and deltas per identification strategy
results_ident <- expand.grid(threshold = thresholds, delta = deltas) %>%
  pmap_dfr(function(threshold, delta) {
    summarize_results(data_gg, "group_ident_broad", threshold, delta)
  })

# Apply the function across all thresholds and deltas per period.month
results_period <- expand.grid(threshold = thresholds, delta = deltas) %>%
  pmap_dfr(function(threshold, delta) {
    summarize_results(data_gg, "period.month", threshold, delta)
  })



# Transform the data for presentation
results_ident_long <- results_ident %>%
  filter(threshold==1) %>% 
  mutate(threshold_delta = paste0(threshold, "±", delta)) %>%
  pivot_longer(cols = c(proportion, pvalue, obs),
               names_to = "measure",
               values_to = "value") %>%
  unite(measure_threshold_delta, measure, threshold_delta, sep = "_") %>%
  pivot_wider(names_from = group_ident_broad, values_from = value)


results_period_long <- results_period %>%
  filter(threshold==1) %>% 
  mutate(threshold_delta = paste0(threshold, "±", delta)) %>%
  pivot_longer(cols = c(proportion, pvalue, obs),
               names_to = "measure",
               values_to = "value") %>%
  unite(measure_threshold_delta, measure, threshold_delta, sep = "_") %>%
  pivot_wider(names_from = period.month, values_from = value)


# Create the text table using stargazer
stargazer(results_ident_long, type = "text",
          summary = FALSE,
          title = "Summary of Results",
          align = TRUE,
          column.sep.width = "1pt",
          no.space = TRUE)


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
    4:9,
    color = styleInterval(c(0.05,0.1), c('blue',"red", 'black'))
  )




library(DT)
datatable(results_period_long %>% mutate(across(is.numeric, signif, digits = 3)), rownames = FALSE, options = list(
  dom = 't',
  ordering=F,
  pageLength = 20,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#eda698', 'color': '#fff'});",
    "}")
))%>%
  formatStyle(
    4:9,
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
