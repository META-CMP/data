
####################### This code aims to replicate the analysis of brodeur 2020 figure 4. 

rm(list = ls())



setwd("~/data")


load("data/preliminary_data_test.RData")

data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization


data<-data_back

out<-'output'#c("gdp", "inflation", "unemp", "emp")
outcome<-"output" # c("output", "the price level", "employment", "unemployment")
data <- subset(data, outcome %in% out)

periods <- c(3, 6, 12,15, 18,21, 24, 30, 36,48)
data<-data %>% filter(period.month %in% periods)# omit two studies which lead to issues if we use winsorized data


data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
                                    mutate(standarderror_winsor=winsorizor(StandardError, c(0.02), na.rm = TRUE)) %>%
                                    mutate(mean.effect_winsor=winsorizor(mean.effect, c(0.02), na.rm = TRUE)) %>% 
                                    mutate(z_stat=abs(mean.effect/StandardError)) %>% 
                                    mutate(z_stat_winsor=abs(mean.effect_winsor/standarderror_winsor)) %>% 
                                    mutate(x=1:length(StandardError) / 100)



###############################  some plots of the z_statistic across different sub-samples




p<-ggplot(data, aes(x = z_stat_winsor)) +
  geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, alpha = 0.2) + 
  facet_wrap(~ period.month, scales = "free") +
  geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "solid") +
  stat_function(
    fun = function(x) dt(x, df = 2, ncp = 1.4),
    color = "red", linetype = "dashed", size = 1
  ) +
  labs(title = "Density plot", x = "Value", y = "Density") +
  xlim(-0.1, 10) +
  theme_classic()

p
ggsave(filename = paste0("All_methods", ".png"), plot = p, bg = "white")

ggplot(data %>% filter(is_top_tier==1), aes(x = z_stat_winsor, ..density..)) +
  geom_density(fill = "blue", alpha = 0.2,adjust = .7) +
  geom_histogram(binwidth = 0.1, alpha = 0.2) + 
  facet_wrap(~ period.month, scales = "free") +
  geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "solid") +
  labs(title = "Density plot", x = "Value", y = "Density") +
  xlim(-0.1, 10)+
  theme_minimal()

ggplot(data %>% filter(z_stat_winsor>5&transformation=="log"), aes(x = z_stat_winsor)) +
  geom_density(fill = "blue", alpha = 0.5,adjust = 1) +
  facet_wrap(~ period.month, scales = "free") +
  geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
  labs(title = "Density plot", x = "Value", y = "Density") +
  xlim(-0.1, 10)+
  theme_minimal()



ggplot(data, aes(x = z_stat_winsor)) +
  geom_density(fill = "blue", alpha = 0.5,adjust = .7) +
  facet_wrap(~ period.month, scales = "free") +
  geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
  labs(title = "Density plot", x = "Value", y = "Density") +
  xlim(-0.1, 10)+
  theme_minimal()



ggplot(data %>% filter(`publication year`>=2020), aes(x = z_stat_winsor)) +
  geom_density(fill = "blue", alpha = 0.5,adjust = .7) +
  facet_wrap(~ period.month, scales = "free") +
  geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
  labs(title = "Density plot", x = "Value", y = "Density") +
  xlim(-0.1, 10)+
  theme_minimal()

ggplot(data %>% filter(`publication year`<2020), aes(x = z_stat_winsor)) +
  geom_density(fill = "blue", alpha = 0.5,adjust = .7) +
  facet_wrap(~ period.month, scales = "free") +
  geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
  labs(title = "Density plot", x = "Value", y = "Density") +
  xlim(-0.1, 10)+
  theme_minimal()



######################################################## create plots with counterfactual distributions for different methods ################### 

df_np <- data.frame(
  method = unique(data$group_ident_broad),
  df = c(2, 2, 2,2,2),
  np = c(1.4, 1.4,1.4, 1.4, 1.4)
)

# Sample methods for illustration
methods <- df_np$method

# Loop through each method and create density plots
for (i in 1:length(methods)) {
  method <- methods[i]
  
  # Filter data for the current method
  data_gg <- data %>% filter(group_ident_broad == method)
  
  # Get degrees of freedom and non-centrality parameter for the current method
  current_df <- df_np %>% filter(method == method) %>% pull(df)
  current_np <- df_np %>% filter(method == method) %>% pull(np)
  
  # Plot
  p <- ggplot(data_gg, aes(x = z_stat_winsor)) +
    geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1, alpha = 0.2) + 
    facet_wrap(~ period.month, scales = "free") +
    geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
    stat_function(
      fun = function(x) dt(x, df = current_df, ncp = current_np),
      color = "red", linetype = "dashed", size = 1
    ) +
    labs(title = paste0("Density plot for ", method), x = "Value", y = "Density") +
    xlim(-0.1, 10) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0(method, ".png"), plot = p, bg = "white")
}


######################################################## create plots with counterfactual distributions for different methods ################### 

df_np <- data.frame(
  method = unique(data$chol_ident),
  df = c(2, 2),
  np = c(1.4, 1.4)
)

# Sample methods for illustration
methods <- df_np$method

# Loop through each method and create density plots
for (i in 1:length(methods)) {
  method <- methods[i]
  
  # Filter data for the current method
  data_gg <- data %>% filter(chol_ident == method)
  
  # Get degrees of freedom and non-centrality parameter for the current method
  current_df <- df_np %>% filter(method == method) %>% pull(df)
  current_np <- df_np %>% filter(method == method) %>% pull(np)
  
  # Plot
  p <- ggplot(data_gg, aes(x = z_stat_winsor)) +
    geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1, alpha = 0.2) + 
    facet_wrap(~ period.month, scales = "free") +
    geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
    stat_function(
      fun = function(x) dt(x, df = current_df, ncp = current_np),
      color = "red", linetype = "dashed", size = 1
    ) +
    labs(title = paste0("Density plot for ", method), x = "Value", y = "Density") +
    xlim(-0.1, 10) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0(method, ".png"), plot = p, bg = "white")
}



############################################################ create plots with different counterfactual distributions for different time horizons #######################


df_np <- data.frame(
  period.month = unique(data$period.month),
  df = c(1.65, 1.8, 2,2,2,2,2,2,3,4),
  np = c(1.5, 1.1, 1.4, 1.4, 1.4, 1.4, 1.4,1.6, 1.5, 1.2)
)


# Extract unique periods for iteration
periods <- unique(df_np$period.month)

# Loop through each period and create density plots
for (i in 1:length(periods)) {
  current_period <- periods[i]
  
  # Filter data for the current period
  data_gg <- data %>% filter(period.month == current_period)
  
  # Get degrees of freedom and non-centrality parameter for the current period
  current_df <- df_np %>% filter(period.month == current_period) %>% pull(df)
  current_np <- df_np %>% filter(period.month == current_period) %>% pull(np)
  
  # Plot
  p <- ggplot(data_gg, aes(x = z_stat_winsor)) +
    geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
    geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1, alpha = 0.2) + 
    facet_wrap(~ group_ident_broad, scales = "free") +
    geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
    stat_function(
      fun = function(x) dt(x, df = current_df, ncp = current_np),
      color = "red", linetype = "dashed", size = 1
    ) +
    labs(title = paste0("Density plot for period ", current_period), x = "Value", y = "Density") +
    xlim(-0.1, 10) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0(current_period, ".png"), plot = p, bg = "white")
}


############################################################ create plots with different counterfactual distributions for different time horizons #######################
data$chol_ident<-ifelse(data$group_ident_broad=="chol",1,0)

df_np <- data.frame(
  period.month = unique(data$period.month),
  df = c(1.65, 1.8, 2,2,2,2,2,2,3,4),
  np = c(1.5, 1.1, 1.4, 1.4, 1.4, 1.4, 1.4,1.6, 1.5, 1.2)
)


# Extract unique periods for iteration
periods <- unique(df_np$period.month)

# Loop through each period and create density plots
for (i in 1:length(periods)) {
  current_period <- periods[i]
  
  # Filter data for the current period
  data_gg <- data %>% filter(period.month == current_period)
  
  # Get degrees of freedom and non-centrality parameter for the current period
  current_df <- df_np %>% filter(period.month == current_period) %>% pull(df)
  current_np <- df_np %>% filter(period.month == current_period) %>% pull(np)
  
  # Plot
  p <- ggplot(data_gg, aes(x = z_stat_winsor)) +
    geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
    geom_density(fill = "blue", alpha = 0.2, adjust = 0.7) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1, alpha = 0.2) + 
    facet_wrap(~ chol_ident, scales = "free") +
    geom_vline(xintercept = c(1, 1.65, 1.96), linetype = "dotted") +
    stat_function(
      fun = function(x) dt(x, df = current_df, ncp = current_np),
      color = "red", linetype = "dashed", size = 1
    ) +
    labs(title = paste0("Density plot for period ", current_period), x = "Value", y = "Density") +
    xlim(-0.1, 10) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0(current_period, ".png"), plot = p, bg = "white")
}

############################################################################################################################################################################
############################################################################ fit distribution to z>5 #######################################################################
############################################################################################################################################################################


########### Try to fit distribution to period.month ==4: 
library(fitdistrplus)
library(stats4)



test<-data %>% filter(period.month %in% 24 & z_stat_winsor>5) %>% dplyr::pull(z_stat_winsor)-5

hist(test, breaks = 30, probability = TRUE, main = "Histogram of Original Data with Fitted Density", xlab = "Value")

# Generate example data from a non-central t-distribution
#set.seed(123)
#data <- rt(n = 100000, df = 2, ncp = 1.65)
# Define the log-likelihood function for the non-central t-distribution
log_likelihood_nct <- function(df, ncp) {
  -sum(dt(test, df = df, ncp = ncp, log = TRUE))
}


# Use optimization to estimate the parameters
fit_nct <- mle(log_likelihood_nct, start = list(df = 2, ncp = 1.65))
summary(fit_nct)

# Extract the estimated parameters
df_est <- coef(fit_nct)["df"]
ncp_est <- coef(fit_nct)["ncp"]

# Generate new data from the fitted non-central t-distribution
new_data <- rt(n = 1000, df = df_est, ncp = ncp_est)

# Verify the fit visually
hist(test, breaks = 30, probability = TRUE, main = "Histogram of Original Data with Fitted Density", xlab = "Value")
curve(dt(x, df = 2, ncp = 1.45), col = "blue", lwd = 2, add = TRUE)



################################## fit distribution to all period.month observations.


log_likelihood_nct <- function(df, ncp, observed_tvalues) {
  -sum(dt(observed_tvalues, df = df, ncp = ncp, log = TRUE))
}


optimize_parameters <- function(observed_tvalues, initial_df = 2, initial_ncp = 1.65) {
  # Define the log-likelihood function for optimization
  log_likelihood <- function(df, ncp) {
    log_likelihood_nct(df, ncp, observed_tvalues)
  }
  
  # Use optimization to estimate the parameters
  fit <- mle(log_likelihood, start = list(df = initial_df, ncp = initial_ncp))
  
  # Extract the estimated parameters
  df_est <- coef(fit)["df"]
  ncp_est <- coef(fit)["ncp"]
  
  return(list(df = df_est, ncp = ncp_est))
}


fit_all_periods <- function(data, threshold = 5) {
  results <- data %>%
    filter(z_stat_winsor > threshold) %>%
    group_by(period.month) %>%
    summarise(
      params = list(optimize_parameters(z_stat_winsor - threshold)),
      .groups = 'drop'
    ) %>%
    mutate(
      df = map_dbl(params, "df"),
      ncp = map_dbl(params, "ncp")
    ) %>%
    select(-params)
  
  return(results)
}

# Example usage
# Assuming 'data' is your dataframe
results <- fit_all_periods(data)

mean(results$df)
mean(results$ncp)

################################################################################ replicate results of brodeuer ###########################################################

# # Set the working directory
# setwd("C:/Users/Enzinger/Downloads/R_brodeur/R/data")
# 
# # Load the necessary libraries
# library(dplyr)
# library(haven)
# library(readr)
# library(ggplot2)
# 
# 
# # Load the data
# data.frame <- read.csv("MM Data1.csv")
# 
# 
# 
# #Calculate the z-statistic 
# #mean effect/SE
# 
# # Define the methods and their corresponding df and np values
# methods <- c("DID", "IV", "RCT", "RDD")
# df <- c(2, 2, 2, 2)
# np <- c(1.81, 1.65, 1.16, 1.51)
# 
# # Generate x
# data.frame$x <- 1:nrow(data.frame) / 100
# data.frame$x[data.frame$x > 1000] <- NA
# 
# # Loop over each method
# for (i in 1:length(methods)) {
#   method <- methods[i]
#   
#   # Generate pdf_t
#   data.frame$pdf_t <- dt(data.frame$x, df[i], np[i])
#   
#   # Plot
#   plot(data.frame$pdf_t ~ data.frame$x, type = "l", lty = 2, xlab = "z-statistic", ylab = "Density",
#        main = paste("Density plot for method", method), xlim = c(0, 10), ylim = c(0, 0.4))
#   lines(density(data.frame$t[data.frame$method == method & data.frame$t < 10]), col = "black")
#   abline(v = c(1,1.65, 1.96), lty = "dotted")
#   
#   # Save the plot
#   dev.copy(png, filename = paste0(method, ".png"))
#   dev.off()
# }
# 
