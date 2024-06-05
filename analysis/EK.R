rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
source("analysis/data_prep.R")


data_back<-data


# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization



EK <- function(data, verbose = T){
  
  # install missing packages
  if (!require('lmtest')) install.packages('lmtest'); library('lmtest')
  if (!require('zoo')) install.packages('zoo'); library('zoo')
  
  # Input validation
  stopifnot(
    is.data.frame(data), # Only data frame
    is.logical(verbose), # Only boolean
    ncol(data) == 2, # Effect data, Standard error data
    sapply(data, is.numeric) # Only numeric input
  )
  # Rename source data
  colnames(data) <- c("bs", "sebs")
  
  # Create new variables
  data$ones <- 1
  M <- nrow(data)
  sebs_min <- min(data$sebs)
  sebs_max <- max(data$sebs)
  data$sebs2 <- data$sebs^2
  data$wis <- data$ones / data$sebs2
  data$bs_sebs <- data$bs / data$sebs
  data$ones_sebs <- data$ones / data$sebs
  data$bswis <- data$bs * data$wis
  wis_sum <- sum(data$wis) # Redundant
  
  # FAT-PET
  fat_pet <- lm(bs_sebs ~ 0 + ones_sebs + ones, data = data) # No constant
  # Auxiliary
  fat_pet_est <- coeftest(fat_pet)["ones_sebs", "Estimate"] # Fat pet ones_sebs estimate
  fat_pet_se <- coeftest(fat_pet)["ones_sebs", "Std. Error"] # Fat pet ones_sebs standard error
  # End auxiliary
  pet <- fat_pet_est
  t1_linreg <- fat_pet_est / fat_pet_se
  b_lin <-  fat_pet_est
  Q1_lin <- sum(resid(fat_pet)^2)
  abs_t1_linreg <- abs(t1_linreg) 
  
  # PEESE
  peese_model <- lm(bs_sebs ~ 0 + ones_sebs + sebs, data = data) # No constant
  # Auxiliary
  peese_est <- coeftest(peese_model)["ones_sebs", "Estimate"]
  # End auxiliary
  peese <- peese_est
  b_sq <- peese_est
  Q1_sq <- sum(resid(peese_model)^2) # Sum of squared residuals
  
  # FAT-PET-PEESE
  if (abs_t1_linreg > qt(0.975, M-2)) {
    combreg <- b_sq
    Q1 <- Q1_sq
  } else {
    combreg <- b_lin
    Q1 <- Q1_lin
  }
  
  # Estimation of random effects variance component
  df_m <- df.residual(peese_model) # DoF from the last regression (peese-model)
  sigh2hat <- max(0, M * ((Q1 / (M - df_m - 1)) - 1) / wis_sum)
  sighhat <- sqrt(sigh2hat)
  
  # Cutoff value for EK
  if (combreg > 1.96 * sighhat) {
    a1 <- (combreg - 1.96 * sighhat) * (combreg + 1.96 * sighhat) / (2 * 1.96 * combreg)
  } else {
    a1 <- 0
  }
  
  # Rename variables - messy source code, kept to the original
  names(data)[names(data) == "bs"] <- "bs_original"
  names(data)[names(data) == "bs_sebs"] <- "bs"
  names(data)[names(data) == "ones_sebs"] <- "constant"
  names(data)[names(data) == "ones"] <- "pub_bias"
  
  # Regressions and coefficient extraction in various scenarios
  if (a1 > sebs_min & a1 < sebs_max) {
    data$sebs_a1 <- ifelse(data$sebs > a1, data$sebs - a1, 0)
    data$pubbias <- data$sebs_a1 / data$sebs
    ek_regression <- lm(bs ~  0 + constant + pubbias, data = data)
    b0_ek <- coef(ek_regression)[1]
    b1_ek <- coef(ek_regression)[2]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    sd1_ek <- summary(ek_regression)$coefficients[2, 2]
  } else if (a1 < sebs_min) {
    ek_regression <- lm(bs ~ 0 + constant + pub_bias, data = data)
    b0_ek <- coef(ek_regression)[1]
    b1_ek <- coef(ek_regression)[2]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    sd1_ek <- summary(ek_regression)$coefficients[2, 2]
  } else if (a1 > sebs_max) {
    ek_regression <- lm(bs ~ 0 + constant, data = data)
    b0_ek <- coef(ek_regression)[1]
    sd0_ek <- summary(ek_regression)$coefficients[1, 2]
    b1_ek <- NA
    sd1_ek <- NA
  }
  # # Print results to console if desired
  # if (verbose){
  #   cat("EK's mean effect estimate (alpha1) and standard error:   ")
  #   cat(b0_ek) # Mean effect estimate
  #   cat(",  ")
  #   cat(sd0_ek) # Mean effect standard error
  #   cat("\n EK's publication bias estimate (delta) and standard error:   ")
  #   cat(b1_ek) # Pub bias estimate
  #   cat(",  ")
  #   cat(sd1_ek) # Pub bias standard error
  # }
  # Return the four coefficients
  
  # return (c(b0_ek, sd0_ek, b1_ek, sd1_ek))
  my_list <- list("effect"=b0_ek, "SE_effect"=sd0_ek,"pubbias"=b1_ek,"SE_pubbias"=sd1_ek)
  return(my_list)
}

data<-data_back

out<-'inflation'#c("gdp", "inflation", "unemp", "emp")
outcome<-"the price level" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)

EKresults_list<-list()



periods <- c(3, 6, 12, 18, 24, 30, 36, 48)



# Loop through periods
for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  # Winsorize data
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  # Store the winsorized data
  
  data_period_Furukawa <- data_period_winsor %>%  select(mean.effect_winsor, standarderror_winsor)
  
  
  # executes the R function EK
  data_BomRachinger <- data_period_Furukawa
  
  est_EK<-EK(data=data_BomRachinger,verbose=T)
  
  # reports the EK results
  object<-c("EK's mean effect estimate (alpha1)","standard error","EK's publication bias estimate (delta)","standard error")
  
  EKresults<-data.frame(t(c(est_EK$effect, est_EK$SE_effect,est_EK$pubbias, est_EK$SE_pubbias)))
  colnames(EKresults)<-object
  
  EKresults_list[[paste0(x)]] <- EKresults
}


data.table::rbindlist(EKresults_list, fill = T,idcol = ".id")

