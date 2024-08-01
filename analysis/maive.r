
####################### This code is originally from http://meta-analysis.cz/maive/maive_R_package.zip

rm(list = ls())



setwd("~/data")


source("data/data_prep.R")
source("analysis/R/maivefunction.R")

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization


data<-data_back

out<-'gdp'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)



#  Default option for MAIVE: MAIVE-PET-PEESE, unweighted, with instrumented SEs

data<-data %>% filter(observations>25)# omit two studies which lead to issues if we use winsorized data


periods <- c(3, 6, 12, 18, 24, 30, 36)

object<-c("MAIVE coefficient","MAIVE standard error","F-test of first step in IV","Hausman-type test (to be used with caution)","Critical Value of Chi2(1)","AR Confidence interval")
maive_df<-data.frame(object)
maive_list<-list()


for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  #   data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  #     dat<-data_period %>% select(mean.effect,StandardError,observations,key)
  #
  #   # Estimate maive:
  #     MAIVE=maive(dat=dat,method=method,weight=weight,instrument=instrument,studylevel=studylevel,AR=AR)
  #
  # }
  
  data_period <- subset(data, period.month==x)
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  
  
  dat<-data_period_winsor %>% select(mean.effect_winsor,standarderror_winsor,observations,key)
  #dat$observations<-log(dat$observations)
  #dat<-dat %>% filter(observations!=0)
  
  # plot(density(log(data_period_winsor$standarderror_winsor)))
  # plot(density($StandardError))
  # plot(density(dat$observations))
  # summary(data_period_winsor$StandardError)
  # summary(data_period_winsor$standarderror_winsor)
 
  
  # default options are method=3; weight=0; instrument=1; studylevel=0; AR=0 
  # method: PET:1, PEESE:2, PET-PEESE:3, EK:4 (default 3)
  method<-3
  # weighting: default no weight: 0 ; weights: 1, adjusted weights: 2 (default 0)
  weight<-
  # instrumenting (default 1)  1 yes, 0 no 
  instrument<-1
  # correlation at study level: none: 0 (default), fixed effects: 1, cluster: 2
  studylevel<-0
  # Anderson-Rubin confidence interval for weak instruments (only for unweighted MAIVE -- PET, PEESE or PET-PEESE): 0 no, 1 yes
  AR<-0
  
  #Run maive function:
  MAIVE=maive(dat=dat,method=method,weight=weight,instrument=instrument,studylevel=studylevel,AR=AR) # if we set AR=1 the function runs for ever. # these inputs could also be determined in the package. 
  # OPTIONS: 
 
  
  maive_list[[paste0(x, ".maive")]]<-MAIVE
  
  
  value<-c(MAIVE$beta,MAIVE$SE,MAIVE$`F-test`,MAIVE$Hausman,MAIVE$Chi2,paste(MAIVE$AR_CI, collapse = " "))
  
  # Set column name of the df to current horzion
  new_col_name <- paste0("Horizon_", x)
    # Calculate the new column (for example, cumulative sum with a different offset)
  maive_df[[new_col_name]] <- value
  
  
}

library(DT)
datatable(maive_df, rownames = FALSE, options = list(
  dom = 't',ordering=F,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#eda698', 'color': '#fff'});",
    "}")
))
