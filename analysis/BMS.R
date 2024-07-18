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
library(fastDummies)# to automatically create dummies for factor and character variables using dummy_cols
library(car)# for vif factors
library(BMS)# BMS package for bayesian model averaging.

data<-data_back

# subset data for outcome variable of interest. 
outvar<-'gdp'#c("gdp", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% outvar)

#x<-3

periods <- c(3, 6, 12, 18, 24, 30, 36, 48)
results_list <- list()
vif_list <- list()
bms_full_list <- list()

for (x in periods) {

# Subset data for the current period
data_period <- subset(data, period.month == x)


# calculate standard errors  
data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2

  # Winsorize data
data_period_winsor <- data_period
data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
# Calculate variance winsorised
data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
  
# Calculate PrecVariance winsorised
data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor


# Create dataset for BMS estimation
data_bms<-data_period_winsor %>% select(mean.effect_winsor,standarderror_winsor, mean_year, regime, quality_concern, observations, main_research_q, outcome_measure, cbanker, decomposition, convent, lrir, fx, foreignir, inflexp, eglob, find, comprice, country_dev, month, upr, lor, dsge, bayes, fvar, lp, signr, svar, chol, nr, prefer,iv,forecast_based)# exclude rate mean effect

# automatically create dummies for factor and character variables
data_bms<-fastDummies::dummy_cols(data_bms,remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)# maybe exclude -`country_dev_Mixed or Unclassified`

# check if there are NA values in the dataset
if (anyNA(data_bms)) {stop("some selected variables contain NA values")}



# Calculate VIF for each variable
vif_values <- car::vif(lm(data_bms))
# Print the VIF values
vif_list[[paste0(x)]] <- vif_values


###################### BMS
# data.frame necessary for BMS estimation
data_bms<-as.data.frame(data_bms)

#BMS estimation
speed <- bms(data_bms, g="UIP", mprior="uniform", user.int=FALSE,nmodel = 20000,burn = 5000, iter=10000, mcmc="bd")#,start.value = c(1:35)

bms_full_list[[paste0(x)]] <- speed

#save results in results list BMS
results_list[[paste0(x)]] <- coef(speed, order.by.pip = T, exact=T, include.constant=T)


# png(file=paste0("./results/",out,"/plots/bma/bma_plot_", x, "_months_winsor.png"),
#     width     = 8.25,
#     height    = 5.25,
#     units     = "in",
#     res       = 150,
#     pointsize = 10)
# image(speed, cex.axis=.5, order.by.pip = T, yprop2pip=F)#,col=c("orange","lightgrey"), main=paste0("BMA image ",outcome," ",x," months")
# dev.off()

### image(speed,yprop2pip=TRUE,col=c("black","lightgrey"))
}

########## Print vif values


# function to map vif_list to modelsummary_list element
create_modelsummary_list_vif <- function(vif_element) {
  ti <- data.frame(
    term = names(vif_element),
    estimate = unname(vif_element),
    std.error = rep(NA, length(vif_element))
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

# show vif factors for all time periods
modelsummary(mod_list)


########## Print BMS results
create_modelsummary_list_bms_pip <- function(result_element) {
  ti <- data.frame(
    term = names(result_element[,1]),
    estimate = paste0("PIP: ",round(unname(result_element[,1]),2)),
    std.error = paste0("Sign: ",round(unname(result_element[,4]),2))
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

create_modelsummary_list_bms_coef <- function(result_element,bms_element) {
  ti <- data.frame(
    term = paste0(names(result_element[,1])),
    estimate = paste0("[",round(unname(result_element[,1]),2),"] \n",format(round(unname(result_element[,2]),4),nsmall=2)),
    std.error = unname(result_element[,3])
  )
  
  gl <- data.frame(
    K = bms_element[[1]]$K,
    N = bms_element[[1]]$N,
    Models.visited = bms_element[[1]]$models.visited,
    burn=bms_element[[1]]$burn,
    iter=bms_element[[1]]$iter,
    msize=bms_element[[1]]$msize
  )
  
  mod <- list(
    tidy = ti,
    glance = gl
  )
  
  class(mod) <- "modelsummary_list"
  return(mod)
}



#mod_list_coef <- lapply(results_list, create_modelsummary_list_bms_coef)

mod_list_coef <- Map(create_modelsummary_list_bms_coef,results_list,bms_full_list)
mod_list_pip<- lapply(results_list, create_modelsummary_list_bms_pip)


# show vif factors for all time periods
modelsummary(mod_list_coef)
modelsummary(mod_list_pip)
#modelsummary(mod)



# ti <- data.frame(
#   term = names(results_list[[1]][,1]),
#   estimate = paste0("PIP: ",round(unname(results_list[[1]][,1]),2)," - Mean: ",round(unname(results_list[[1]][,2]),3)),
#   std.error = paste0("Sign: ",round(unname(results_list[[1]][,4]),2)," - SE: ",round(unname(results_list[[1]][,3]),3))
# 

test<-bms_full_list[[1]][[1]]$models.visited

element<-4

plot(bms_full_list[[element]])
plotConv(bms_full_list[[element]])
image(bms_full_list[[element]], cex.axis=.5, order.by.pip = T, yprop2pip=F)#,col=c("orange","lightgrey"), main=paste0("BMA image ",outcome," ",x," months")
density(bms_full_list[[element]], reg="dsge")


