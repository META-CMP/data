
####################### This code is instrumental variables regression to get MAIVE

rm(list = ls())



library(here)

data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)
source(here("analysis/R/apply_winsorization.R"))

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
library(fixest) #for IV fixed effects clustered

## Load and install the packages that we'll be using
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mfx, tidyverse, hrbrthemes, estimatr, ivreg, fixest, sandwich, 
               lmtest, margins, vtable, broom, modelsummary)

## Make sure we have at least version 0.6.0 of ivreg
if (numeric_version(packageVersion("ivreg")) < numeric_version("0.6.0")) install.packages("ivreg")


data<-data_back

out<-'output'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% out)



#  Default option for MAIVE: MAIVE-PET-PEESE, unweighted, with instrumented SEs

data<-data %>% filter(observations>25 & quality_concern!=1)# omit two studies which lead to issues if we use winsorized data
periods <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60)

object<-c("MAIVE constant","MAIVE coefficient","MAIVE standard error","F-test of first step in IV","Hausman-type test (to be used with caution)","Critical Value of Chi2(1)","AR Confidence interval")
maive_df<-data.frame(object)
maive_list<-list()

wins<-0.02


for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$precision <- 1 / data_period$StandardError
  data_period$inv_obs <- 1/data_period$observations
  
  # winsorize
  data_period <- apply_winsorization(data_period, wins)
  
  dat<-data_period %>% dplyr::select(mean.effect_winsor,standarderror_winsor,inv_obs,observations,key)
  #dat$observations<-log(dat$observations)
  #dat<-dat %>% filter(observations!=0)
  
  # plot(density(log(data_period_winsor$standarderror_winsor)))
  # plot(density($StandardError))
  # plot(density(dat$observations))
  # summary(data_period_winsor$StandardError)
  # summary(data_period_winsor$standarderror_winsor)
  
  MAIVE = 
    feols(
      mean.effect_winsor ~ 1 | key | standarderror_winsor ~ inv_obs,
      data = dat, cluster=dat[,c("key")]
    )
  
  maive_list[[paste0(x, ".fixest")]]<-MAIVE
  
  
  value<-c(MAIVE$constant,MAIVE$beta,MAIVE$SE,MAIVE$`F-test`,MAIVE$Hausman,MAIVE$Chi2,paste(MAIVE$AR_CI, collapse = " "))
  
  # Set column name of the df to current horzion
  new_col_name <- paste0("Horizon_", x)
  # Calculate the new column (for example, cumulative sum with a different offset)
  maive_df[[new_col_name]] <- value
  
  
}


# show table
library(DT)
datatable(maive_df, rownames = FALSE, options = list(
  dom = 't',ordering=F,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#eda698', 'color': '#fff'});",
    "}")
))