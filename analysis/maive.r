
####################### This code is originally from http://meta-analysis.cz/maive/maive_R_package.zip

rm(list = ls())



setwd("~/data")


load("data/preliminary_data_test.RData")
source("analysis/R/maivefunction.R")
source("analysis/R/apply_winsorization.R")

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization


data<-data_back

out<-'output'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% out)



#  Default option for MAIVE: MAIVE-PET-PEESE, unweighted, with instrumented SEs

data<-data %>% filter(observations>25 & quality_concern!=1)# omit two studies which lead to issues if we use winsorized data


# data <- data %>%
#   group_by(key,period.month) %>%
#   sample_n(size = 1,replace = F) %>% ungroup()

periods <- c(3, 6, 12, 15, 18,21, 24, 30, 36)


object<-c("MAIVE coefficient","MAIVE standard error","F-test of first step in IV","Hausman-type test (to be used with caution)","Critical Value of Chi2(1)","AR Confidence interval")
maive_df<-data.frame(object)
maive_list<-list()

wins<-0.02

for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$precision <- 1 / data_period$StandardError
  
  # winsorize
  data_period <- apply_winsorization(data_period, wins)
  
  dat<-data_period %>% dplyr::select(mean.effect_winsor,standarderror_winsor,observations,key)
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
  weight<-0
  # instrumenting (default 1)  1 yes, 0 no 
  instrument<-1
  # correlation at study level: none: 0 (default), fixed effects: 1, cluster: 2
  studylevel<-2
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

