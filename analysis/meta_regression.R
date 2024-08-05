rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
load("data/preliminary_data_test.RData")


data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
library(clubSandwich)# for coef test results
library(modelsummary)# to nicely print results


data<-data_back

out<-'output'#c("gdp", "inflation", "unemp", "emp")
#outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)


#summary(data)

#data$size<-ifelse(is.na(data$size)|data$size=="1SD",data$shock_size,data$size)
data$us<-ifelse(data$list_of_countries=="US",1,0)

names(data)
unique(data$conf)

############ Full possible model without external data.



equation<-mean.effect_winsor ~standarderror_winsor+group_ident_broad+lp+vecm+dyn_ols+fvar+tvar+gvar+dsge+varother+panel+bayes+regime+upr+lor+hike+cut+decomposition+convent+pure_rate_shock+lrir+fx+foreignir+inflexp+eglob+find+outpgap+comprice+month+main_research_q+prefer+mean_year+log(observations)+ea12+us+upper_middle+n_of_countries+outcome_measure+transformation+cum+interest_rate_short+cbanker+pub_year+is_top_tier+is_top_5+journal_impact+num_cit+model_id+rid1+tradegl+log(infl)+fingl+findev+cbi+log(gdppc)+exrate

# currently not included: rate_mean.effect (due to missing data),country_dev, conf, intrest_rate, external control variables. (periodicity does not really make sense the way it currently looks, we would need to replace the log a values) (real output does not make sense currently, we would need to check the non real ones again.) (initial shock size needs to be adjusted - size)

small<-mean.effect_winsor ~standarderror_winsor+group_ident_broad+lp+vecm+dyn_ols+fvar+tvar+gvar+dsge+varother+panel+bayes+regime+upr+lor+hike+cut+decomposition+convent+cbanker+is_top_tier+is_top_5+journal_impact+num_cit+main_research_q

small<-mean.effect_winsor ~standarderror_winsor+group_ident_broad+decomposition+cbanker+is_top_tier+is_top_5

#lp+vecm+dyn_ols+fvar+tvar+gvar+dsge+varother+panel+bayes+convent+journal_impact+num_cit+main_research_q einstweilen raus

#colSums(is.na(data_period_winsor %>% select(standarderror_winsor,group_ident_broad,lp,vecm,dyn_ols,fvar,tvar,gvar,dsge,varother,cbanker,is_top_tier,is_top_5)))



external<- mean.effect_winsor ~tradegl+log(infl)+fingl+findev+cbi+log(gdppc)+exrate


sum(is.na(data$main_research_q))

data %>% select(key,model_id,tradegl:exrate) %>% group_by(key,model_id) %>% summarise(across(tradegl:exrate, ~ mean(.x, na.rm = TRUE))) %>% mutate(infl=log(infl),gdppc=log(gdppc)) %>% 
  pivot_longer(cols = tradegl:exrate, names_to = "variable", values_to = "value") %>% ggplot( aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Plots for All Variables", x = "Value", y = "Density") +
  theme_minimal()


data %>% select(key,model_id,num_cit,journal_impact) %>% group_by(key,model_id) %>% summarise(across(num_cit:journal_impact, ~ mean(.x, na.rm = TRUE)))  %>% 
  pivot_longer(cols = num_cit:journal_impact, names_to = "variable", values_to = "value") %>% ggplot( aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Plots for All Variables", x = "Value", y = "Density") +
  theme_minimal()

# +fexch#+real_output # only for output regression

periods <- c(3, 6, 12, 18, 24, 30, 36)

results_list<-list()
coef_test_data<-list()
confint_data<-list()
vif_list <- list()


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
  
  
  # Calculate VIF for each variable
  vif_values <- car::vif(lm(small,data=data_period_winsor))
  # Print the VIF values
  vif_list[[paste0(x)]] <- vif_values
  
  
  # Calculate (precision-weighted) average
  regwa <- lm(small, data = data_period_winsor, weights = precvariance_winsor)#
  results_list[[paste0(x, ".ols")]] <- regwa
  
  coef_test_data[[paste0(x, ".ols")]]<-coef_test(regwa, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  
  confint_data[[paste0(x, ".ols")]]<-confint(regwa, level=0.95)
}

#modelsummary(results_list, output = "gt",vcov = 'cr0', cluster = data_period_winsor$key,stars = TRUE) # only works if cluster variable has the same number of observations across lists. 
modelsummary(results_list, output = "gt",stars = TRUE)

create_modelsummary_list_vif <- function(vif_element) {
  ti <- data.frame(
    term = rownames(vif_element),
    estimate = unname(vif_element)[,1],
    std.error = rep(NA, length(unname(vif_element)[,1]))
  )
  
  gl <- data.frame(
    stat1 = "VIF values",
    stat2 = "NA"
  )
  
  mod <- list(
    tidy = ti,
    glance = gl
  )
  
  class(mod) <- "modelsummary_list"
  return(mod)
}

# Apply the function to each element in vif_list
mod_list <- lapply(vif_list, create_modelsummary_list_vif)

modelsummary(mod_list)


coef_test_data<-data.table::rbindlist(coef_test_data, fill = T,idcol = ".id")
coef_test_data


plot(density(2024-data$`publication year`))

