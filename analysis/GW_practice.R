# "A Generalized-Weights Solution to Sample Overlap in Meta-Analysis in Practice" (Pedro Bom and Heiko Rachinger)
# R code for constructing the Omega correlation matrix and obtaining the GW estimator together with its standard error 
# This version: February 2024

# DOWNLOADED FROM https://osf.io/g5t2j/

# For instructions, see the accompanying document.

rm(list = ls())



setwd("~/data")


source("data/data_prep.R")

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
# Load required libraries
if (!require('readxl')) install.packages('readxl'); library('readxl')   
# Load the metafor package for meta-analysis
if (!require('metafor')) install.packages('metafor'); library('metafor')
# Install and load the writexl package
if (!require('writexl')) install.packages('writexl'); library('writexl')

data<-data_back

out<-'gdp'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)

periods <- c(3, 6, 12, 18, 24, 30, 36,48)
data<-data %>% filter(period.month %in% periods)#


data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, c(0.02), na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, c(0.02), na.rm = TRUE))


#prepare columns - and select columns 
data$na<-NA
data$na1<-NA
data$na2<-NA
data$na3<-NA
data$est<-0
data$freq<-ifelse(data$month==1,"M",ifelse(data$quarter==1,"Q","A"))
data$layer<-"N"
data <- data %>%
  mutate(list_countries = sapply(list_of_countries, function(x) paste(x, collapse = " ")))

data<-data %>% select(mean.effect_winsor,standarderror_winsor,na,na1,start_year,end_year,est,freq,layer,na2,na3,list_countries)

# Set conservativeIV
conservativeIV <- 1

# Rename variables
colnames(data) <- c("period.month","beta", "se", "pcc", "se_pcc", "start", "end", "est", "freq", "layer", "list_regions", "numb_regions", "list_countries")

data<-data %>% filter(period.month==12) %>% ungroup() %>% select(-period.month)

# Determine the number of moderators
N_vars <- ncol(data)
N_mods <- N_vars - 12
M <- nrow(data)

# Display information about the number of moderators
cat("You work with", N_mods, "moderators\n")

for (i in 13:N_vars) {
  col_name <- paste("mod", i-12, sep = "")
  colnames(data)[i] <- col_name
}


# Check whether to analyze regression coefficients or PCCs
data$analyze_reg <- ifelse(!is.na(data$beta) & !is.na(data$se) & is.na(data$pcc) & is.na(data$se_pcc), 1, 0)
data$analyze_pcc <- ifelse(is.na(data$beta) & is.na(data$se) & !is.na(data$pcc) & !is.na(data$se_pcc), 1, 0)
if (data$analyze_reg[1] == 1) {
  cat("You analyze regression coefficients\n")
} else if (data$analyze_pcc[1] == 1) {
  cat("You analyze PCCs\n")
} else {
  cat("Revise your input: either all have to be regression coefficients, or all have to be PCCs, both together with their respective SE.\n")
}

# Display information about the analysis type
if (conservativeIV == 1) {
  cat("You work with the conservative OLS-IV formula\n")
}


# Generate matrices and perform calculations (similar logic as in Stata)

ones <- rep(1, M)
Sigma <- matrix(0, nrow = M, ncol = M)
Sigma_beta <- matrix(0, nrow = M, ncol = M)
Sigma12 <- matrix(0, nrow = M, ncol = M)
Cqp <- matrix(0, nrow = M, ncol = M)
covpq <- matrix(0, nrow = M, ncol = M)
Omega <- matrix(0, nrow = M, ncol = M)
Omega12 <- matrix(0, nrow = M, ncol = M)
case_matrix <- matrix(0, nrow = M, ncol = M)
F_T <- matrix(1, nrow = M, ncol = M)
rel_aggG <- matrix(0, nrow = M, ncol = M)
C_geo <- matrix(0, nrow = M, ncol = M)
relC_geo <- matrix(0, nrow = M, ncol = M)
prop <- matrix(0, nrow = M, ncol = M)

# Coding the layer
data$n_layer <- ifelse(data$layer == "R", 1, ifelse(data$layer == "N", 2, ifelse(data$layer == "S", 3, NA)))

# Coding the frequency
data$n_freq <- ifelse(data$freq == "A", 3, ifelse(data$freq == "Q", 2, ifelse(data$freq == "M", 1, NA)))

# Get the time series length
data$length <- ifelse(data$n_freq == 3, (data$end - data$start + 1) * 1,
                      ifelse(data$n_freq == 2, (data$end - data$start + 1) * 4,
                             ifelse(data$n_freq == 1, (data$end - data$start + 1) * 12, NA)))

# Count the spatial units in list
data$n_list <- 0
data$list <- NA
data$name_upper <- NA

data$n_list[data$n_layer == 3] <- lengths(strsplit(data$list_countries[data$n_layer == 3], " "))
data$list[data$n_layer == 3] <- data$list_countries[data$n_layer == 3]

data$n_list[data$n_layer == 2] <- lengths(strsplit(data$list_countries[data$n_layer == 2], " "))
data$list[data$n_layer == 2] <- data$list_countries[data$n_layer == 2]

data$n_list[data$n_layer == 1] <- lengths(strsplit(data$list_regions[data$n_layer == 1], " "))
data$list[data$n_layer == 1] <- data$list_regions[data$n_layer == 1]
data$name_upper[data$n_layer == 1] <- data$list_countries[data$n_layer == 1]

# Calculate N
data$N <- data$length * data$n_list


# Loop through i and j
for (i in 1:M) {
  for (j in 1:M) {
    # Getting the pairwise temporal aggregation pattern i vs j
    if (data$n_freq[i] == 3 & data$n_freq[j] == 2) {
      F_T[i, j] <- 4
      F_T[j, i] <- 1/4
    }
    
    if (data$n_freq[i] == 3 & data$n_freq[j] == 1) {
      F_T[i, j] <- 12
      F_T[j, i] <- 1/12
    }
    
    if (data$n_freq[i] == 2 & data$n_freq[j] == 1) {
      F_T[i, j] <- 3
      F_T[j, i] <- 1/3
    }


    
    # Getting the pairwise geographical aggregation pattern i vs j
    rel_aggG[i, j] <- data$n_layer[i] - data$n_layer[j]
    
    if (rel_aggG[i, j] == 0) {
      # Compare the lists of i and j and calculate how many of i are in j and how many of j are in i
      C_geo[i, j] <- 0
      C_geo[j, i] <- 0
      
      ci <- data$n_list[i]
      for (k in 1:ci) {
        cj <- data$n_list[j]
        for (l in 1:cj) {
          li<-strsplit(data$list[i],split= " ")
          lii<-unlist(li)
          lj<-strsplit(data$list[j],split= " ")  
          ljj<-unlist(lj)
          
          if (identical(lii[k],ljj[l])==1) {
            C_geo[i, j] <- C_geo[i, j] + 1
          }
        }
      }
      
      relC_geo[i, j] <- C_geo[i, j] / data$n_list[i]
      C_geo[j, i] <- C_geo[i, j]                        #    IN STATA CODE HAVE THESE.
      relC_geo[j, i] <- C_geo[j, i] / data$n_list[j]
    }
    
    
    # If i is 1 layer lower than j  e.g., i is a list of regions and j is a list of countries
    if (rel_aggG[i, j] == -1) {
      # If i is regional and j is country
      if (data$n_layer[i] == 1) {
        ci <- data$n_list[i]
        cj <- data$n_list[j]
        for (l in 1:ci) {
          for (k in 1:cj) {
            li<-strsplit(data$name_upper[i],split= " ")
            lii<-unlist(li)
            lj<-strsplit(data$list[j],split= " ")  
            ljj<-unlist(lj)
            if (identical(lii[l],ljj[k])==1) {
              prop[i, j] <- prop[i, j] + data$n_list[i] / data$numb_regions[i] * 1 / data$n_list[j]
              C_geo[j, i] <- 1
            }
          }
        }
      }
      # If i is country and j is supranational
      if (data$n_layer[i] == 2) {
        ci <- data$n_list[i]
        for (k in 1:ci) {
          cj <- data$n_list[j]
          for (l in 1:cj) {
          li<-strsplit(data$list[i],split= " ")
          lii<-unlist(li)
          lj<-strsplit(data$list[j],split= " ")  
          ljj<-unlist(lj)
            if (identical(lii[k],ljj[l])==1) {
              prop[i, j] <- data$n_list[i] / data$n_list[j]
              C_geo[j, i] <- 1
            }
          }
        }
      }
    }
        # If i is 2 layers lower than j
        if (rel_aggG[i, j] == -2) {
          cj <- data$n_list[j]
          for (k in 1:cj) {
            # Compare the name of the upper layer of i with the list of j
            lj<-strsplit(data$list[j],split= " ")  
            ljj<-unlist(lj)
            if (ljj[k] == data$name_upper[i]) {
              prop[i, j] <- data$n_list[i] / data$numb_regions[i] * 1 / data$n_list[j]
              C_geo[j, i] <- 1
            }
          }
        }
      }
    }


  # Creating an empty matrix for case
  case <- matrix(0, nrow = M, ncol = M)

for (i in 1:M) {
  for (j in 1:M) {
    
    # Determine in which scenario we are: pairwise
    
    # Same temp aggregation
    if (F_T[i, j] == 1) {
      
      # Same geo aggregation
      if (rel_aggG[i, j] == 0) {
        case[i, j] <- 1  # Standard case
      }
      
      # i less geo aggregated
      if (rel_aggG[i, j] == -1) {
        case[i, j] <- 2  # Geo aggregation
      }
      
      # i 2 less geo aggregated
      if (rel_aggG[i, j] == -2) {
        case[i, j] <- 6  # Geo aggregation
      }
    }
    
    # i less temp aggregated
    if (F_T[i, j] < 1) {
      
      # Same geo aggregated
      if (rel_aggG[i, j] == 0) {
        case[i, j] <- 3  # Temp aggregation
      }
      
      # i less geo aggregated
      if (rel_aggG[i, j] == -1) {
        case[i, j] <- 4  # Double aggregation
      }
      
      # i 2 less geo aggregated
      if (rel_aggG[i, j] == -2) {
        case[i, j] <- 7  # Double aggregation
      }
    }
    
    # i more temp aggregated
    if (F_T[i, j] > 1) {
      
      # i less geo aggregated
      if (rel_aggG[i, j] == -1) {
        case[i, j] <- 5
      }
      
      # i 2 less geo aggregated
      if (rel_aggG[i, j] == -2) {
        case[i, j] <- 8  # i 2 less geo aggregated
      }
    }
  }
}

# If we analyze pcc, replace beta by pcc
  if (any(data$analyze_pcc == 1)) {
    data$beta[data$analyze_pcc == 1] <- data$pcc[data$analyze_pcc == 1]
    data$se[data$analyze_pcc == 1] <- data$se_pcc[data$analyze_pcc == 1]
  }

  
# Create Omega and Sigma matrices
  
  # Loop through i and j
  for (i in 1:M) {
    Sigma[i, i] <- data$se[i] * data$se[i]
    
    for (j in 1:M) {
      if (data$est[i] == data$est[j]) {
        covpq[i, j] <- data$se[i] * data$se[j] / sqrt(data$N[i] * data$N[j])
      } else if (data$est[i] < data$est[j]) {
        covpq[i, j] <- data$se[i] * data$se[i] / sqrt(data$N[i] * data$N[i])
      } else if (data$est[i] > data$est[j]) {
        covpq[i, j] <- data$se[j] * data$se[j] / sqrt(data$N[j] * data$N[j])
      }
      
      if (conservativeIV == 1) {
      covpq[i, j] <- data$se[i] * data$se[j] / sqrt(data$N[i] * data$N[j])
    }

    Cqp[i, j] <- max(0, (min(data$end[i], data$end[j]) - max(data$start[i], data$start[j])) + 1) * ((data$n_freq[j] == 3) * 1 + (data$n_freq[j] == 2) * 4 + (data$n_freq[j] == 1) * 12) * C_geo[j, i]

    if (case[i, j] == 1) {
      Omega[i, j] <- Cqp[i, j] * covpq[i, j]
    } else if (case[i, j] == 2) {
      Omega[i, j] <- Cqp[i, j] * prop[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    } else if (case[i, j] == 6) {
      Omega[i, j] <- Cqp[i, j] * prop[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    } else if (case[i, j] == 3) {
      Omega[i, j] <- Cqp[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    } else if (case[i, j] == 4) {
      Omega[i, j] <- Cqp[i, j] * prop[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    } else if (case[i, j] == 7) {
      Omega[i, j] <- Cqp[i, j] * prop[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    } else if (case[i, j] == 5) {
      Omega[i, j] <- Cqp[i, j] / F_T[i, j] * prop[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    } else if (case[i, j] == 8) {
      Omega[i, j] <- Cqp[i, j] / F_T[i, j] * prop[i, j] * covpq[i, j]
      Omega[j, i] <- Omega[i, j]
    }
  }
}

  
# Get the meta-estimators
  
  # Estimate with meta-analysis and get tau^2 with DerSimonian-Laird RE model
  result <- rma.uni(data$beta, sei=data$se, method="DL")
  tau2 <- result$tau2
  cat("Estimate of random effect tau^2:", tau2, "\n")
  
  # GW estimator taking into account overlap
  re_diag <- diag(rep(tau2, M))

# Random effect on diagonal
Sigma_re <- Sigma + re_diag
Omega_re <- Omega + re_diag

# Save Omega_re to an Excel file
write_xlsx(as.data.frame(Omega_re), path = "Omega_matrix.xlsx", col_names = FALSE)

# Save results      
Omega12 <- t(solve(chol(Omega_re)))        
Sigma01 <- solve(chol(Sigma))
Sigma12 <- solve(chol(Sigma_re))

beta_star <- Omega12 %*% data$beta
ones_star <- Omega12 %*% rep(1, M)

beta_tilde <- Sigma12 %*% data$beta
ones_tilde <- Sigma12 %*% rep(1, M)

beta_tilde0 <- Sigma01 %*% data$beta
ones_tilde0 <- Sigma01 %*% rep(1, M)
                  
}

cat("Results of the meta-analysis:\n")

# GW - mean   
gw_model <- lm(beta_star ~ ones_star -1, data = data)
cat("GW:\n")
summary(gw_model)

# RE - mean
re_model <- lm(beta_tilde ~ ones_tilde -1 , data = data)
cat("RE:\n")
summary(re_model)

# FE/WLS - mean
fe_wls_model <- lm(beta_tilde0 ~ ones_tilde0 -1, data = data)
cat("FE/WLS:\n")
summary(fe_wls_model)


# If N_mods > 0, display additional meta-regression results
if (N_mods > 0) {
   cat("Results of the meta-regression:\n")
   mods=data[,13:N_vars]              
   mods=mods-t(matrix(1, N_mods, M)*colMeans(mods))
   
   mod_star <- Omega12 %*%as.matrix(mods)
   mod_tilde <- Sigma12 %*%as.matrix(mods)
   mod_tilde0 <- Sigma01 %*%as.matrix(mods)
   
   # GW - meta regression   
   gw_model <- lm(beta_star ~ ones_star + mod_star -1, data = data)
   cat("GW:\n")
   summary(gw_model)
   
   # RE - meta regression
   re_model <- lm(beta_tilde ~ ones_tilde + mod_tilde -1 , data = data)
   cat("RE:\n")
   summary(re_model)
   
   # FE/WLS - meta regression
   fe_wls_model <- lm(beta_tilde0 ~ ones_tilde0 + mod_tilde0 -1, data = data)
   cat("FE/WLS:\n")
   summary(fe_wls_model)
   
}
