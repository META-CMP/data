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

out<-'inflation'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)


unique(data$outcome)







#periods <- 6

periods <- c(3, 6, 12, 18, 24, 30, 36, 48)

for (x in periods) {

# Subset data for the current period
data_period <- subset(data, period.month == x)
  
data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2

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






data_bms<-data_period_winsor %>% select(mean.effect_winsor,standarderror_winsor, mean_year, regime, quality_concern, observations, main_research_q, outcome_measure, rate_mean.effect, cbanker, decomposition, convent, lrir, fx, foreignir, inflexp, eglob, find, comprice, country_dev, month, upr, lor, dsge, bayes, fvar, lp, signr, svar, chol, nr, prefer,iv,forecast_based)

library(fastDummies)

data_bms<-fastDummies::dummy_cols(data_bms) %>% select(-outcome_measure_cpi,-outcome_measure,-rate_mean.effect,-country_dev,-`country_dev_Mixed or Unclassified`)

#data_bms<-data_bms %>% select(mean.effect_winsor,standarderror_winsor, mean_year, regime, quality_concern, observations, main_research_q)

#head(fastDummies::dummy_cols(data_bms))
test<-data_bms %>% 
  summarise(across(everything(), ~ sum(is.infinite(.x))))

dataspeed<-na.omit(data_bms)




# ## build the model matrix
# X <- model.matrix(mean.effect_winsor ~ ., data = data_bms)
# ## check for *multi*collinear combinations
# library(caret)
# (lc <- caret::findLinearCombos(X))

library(car)


# # Calculate VIF for each variable
# vif_values <- car::vif(lm(data_bms))
# #test<-cor(dataspeed)
# # Print the VIF values
# print(vif_values)

data_bms<-as.data.frame(data_bms)

#//Starts estimation
library(BMS)
speed <- bms(data_bms, g="UIP", mprior="uniform", user.int=FALSE,nmodel = 200000,burn = 50000, iter=10000, mcmc="bd")#,start.value = c(1:35)



#undebug(topmodels.bma)


# speed$arguments$X.data[, 1]
# test$arguments$X.data[, 1]

# plot(speed)
# summary(speed)
plotConv(speed)

#//Results
print(coef(speed, order.by.pip = T, exact=T, include.constant=T))

#image(speed,yprop2pip=TRUE,col=c("black","lightgrey"))

png(file=paste0("./results/",out,"/plots/bma/bma_plot_", x, "_months_winsor.png"),
    width     = 8.25,
    height    = 5.25,
    units     = "in",
    res       = 150,
    pointsize = 10)
image(speed, cex.axis=.5, order.by.pip = T, yprop2pip=F)#,col=c("orange","lightgrey"), main=paste0("BMA image ",outcome," ",x," months")
dev.off()
}


parameters::parameters(speed)
modelsummary(as.data.frame(coef(speed)), output = "gt",stars = TRUE)
#density(speed, reg="dsge")
