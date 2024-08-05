

####################### This code aims to replicate the analysis of brodeur 2020 table 4. 

rm(list = ls())



setwd("~/data")


load("data/preliminary_data_test.RData")

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
library(margins)
library(broom)
library(sandwich)
library(lmtest)



data<-data_back

out<-'output'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% out)

periods <- c(12)
data<-data %>% filter(period.month %in% periods)# omit two studies which lead to issues if we use winsorized data



# calculate z_statistic and winsoirzised z-statistic. Probably the winzorized one is irrelevant. 
data<-data %>% group_by(period.month) %>% 
  mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, c(0.02), na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, c(0.02), na.rm = TRUE)) %>% 
  mutate(z_stat=abs(mean.effect/StandardError)) %>% 
  mutate(z_stat_winsor=abs(mean.effect_winsor/standarderror_winsor)) 


# Create dummy variables before for estimates before 2020.
data<-data %>% 
  mutate(before_2020=ifelse(pub_year<2020,1,0))

# As in the original paper we use the inverse of the number of tests presented in the same article to weight observations. Therefore we obtain obs_weight by:
########### Maybe this should be done for the smaller datasets used for the estimation??
data<-data %>% 
  group_by(key) %>% mutate(obs_weight=1/n()) %>% ungroup()




run_probit_analysis <- function(data, delta, threshold, weights_col, cluster_var) {
  
  # Create binomial variable
  data <- data %>% 
    mutate(binomial = ifelse(z_stat >= threshold, 1, 0),
           within_delta = abs(z_stat - threshold) < delta
    ) 
  
  # Filter the data based on the threshold
  data <- data %>% filter(within_delta == TRUE)
  
  # Define the formula inside the function #### does not work outside the funciton due to whatever reason. 
  formula <- as.formula("binomial ~ group_ident_broad + cbanker + is_top_tier + before_2020")
  
  # Perform the probit regression with weights
  model <- glm(formula, data = data, family = binomial(link = "probit"), weights = data[[weights_col]])
  
  # Calculate clustered standard errors
  cluster <- data[[cluster_var]]
  vcovCL <- vcovCL(model, cluster = cluster)
  
  # Calculate marginal effects
  margins_model <- margins(model)
  
  # Get the marginal effects summary
  margins_summary <- summary(margins_model)
  
  # Store the results using broom
  model_summary <- tidy(model, conf.int = TRUE)
  
  # Add clustered standard errors to model summary
  model_summary$std.error <- sqrt(diag(vcovCL))
  model_summary$statistic <- model_summary$estimate / model_summary$std.error
  model_summary$p.value <- 2 * pnorm(abs(model_summary$statistic), lower.tail = FALSE)
  
  # Add additional information
  list(model = model, margins_summary = margins_summary, model_summary = model_summary[,1:5])
}

data$`publication title`

# Example usage of the function
# Assuming 'data' is your data frame
results <- run_probit_analysis(data, delta = 0.1, threshold = 1.96, weights_col = "obs_weight", cluster_var = "key")


library(DT)
datatable(results$model_summary %>% mutate(across(is.numeric, signif, digits = 3)), rownames = FALSE, options = list(
  dom = 't',
  ordering=F,
  pageLength = 9,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#eda698', 'color': '#fff'});",
    "}")
))%>%
  formatStyle(
   5,
    color = styleInterval(c(0.05,0.1), c('blue',"red", 'black'))
  )


# ##################################################### brodeur regressopm results currently not replicaated #####################################################
# 
# # Read in the data
# setwd("C:/Users/Enzinger/Downloads/R_brodeur/R/data")
# test <- read.csv("MM Data.csv")
# 
# test <- test %>%
#   mutate(journal_article_cluster = paste(journal*10000, article, sep = "_"))
# 
# test$journal
# test<-test %>% 
#   group_by(key) %>% mutate(obs_weight=1/n()) %>% ungroup()
# 
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
