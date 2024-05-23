#Load data
setwd("~/data")

load("preliminary_data.RData")

library(dplyr)
library(tidyverse)
rate<-data %>% dplyr::filter(data$outcome_var=="rate")
other<-data %>% dplyr::filter(data$outcome_var!="rate")

rate<-rate %>% dplyr::select(key,model_id,period:SE.lower) %>% dplyr::select(-outcome_var)
#save(rate,file = "preliminary_rate_data.RData")

colnames(rate)[4:ncol(rate)] <- paste("rate_",colnames(rate)[4:ncol(rate)],sep="")

data<-other %>% dplyr::left_join(rate, by=c("key","model_id","period"))



library(stringi)

split_list<-stri_split_fixed(str = data$outcome_var, pattern = "_", n = 3)

# Extract nth element from each split list
data$transformation <- sapply(split_list, function(x) ifelse(1 <= length(x), x[1], NA))

data$periodicity <- sapply(split_list, function(x) ifelse(2 <= length(x), x[2], NA))


split_list <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

data$real_output<-ifelse(split_list %in% c("rgdp","ip","rip","rgnp","rgap"),TRUE,NA)
data$real_output<-ifelse(split_list %in% c("gdp","gnp","gap"),FALSE,data$real_output)

# remove r from the outcome measure
split_list<-sub("^r", "", split_list)

data$outcome_measure<-split_list

data$outcome<-ifelse(data$outcome_measure %in% c("gdp","ip","gap","gnp"),"gdp",ifelse(data$outcome_measure %in% c("cpi","deflator","wpi","core","price_level"),"inflation","emp"))

data_back<-data

############################################################### create funnel plots ####################################################################

########################################## Functions


library(ggplot2)
library(JWileymisc)

# Function to create funnel plots
create_funnel_plot <- function(data, period, winsorize = FALSE) {
  if (winsorize) {
    plot_title <- " (with Winsorization)"
    x <- "mean.effect_winsor"
    y <- "precision_winsor"
  } else {
    plot_title <- ""
    x <- "mean.effect"
    y <- "precision"
  }
  
  plot_funnel <- ggplot(data = data,
                        aes_string(x = x, y = y)) +
    geom_point(size = 0.5) +
    xlab("Mean effect size\n %-change of output in response to 100bp monetary policy shock") +
    ylab("Inverse of standard error (precision)") +
    ggtitle(paste("Funnel plots of effect of monetary policy on output\n", "(", period, "months after the shock)", plot_title)) +
    theme(title = element_text(size = 10, face = 'bold')) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    geom_vline(xintercept = 0, colour = "black", linetype = 2) +
    theme(legend.text = element_text(colour = "black", size = 4)) +
    theme(axis.text.x = element_text(size = 11)) +
    theme(axis.title.x = element_text(size = 11)) +
    theme(axis.text.y = element_text(size = 11)) +
    theme(axis.title.y = element_text(size = 11))
  
  return(plot_funnel)
}

create_funnel_plot_grid <- function(data, period, winsorize = FALSE) {
  if (winsorize) {
    plot_title <- " (with Winsorization)"
    x <- "mean.effect_winsor"
    y <- "precision_winsor"
  } else {
    plot_title <- ""
    x <- "mean.effect"
    y <- "precision"
  }
  
  plot_funnel <- ggplot(data = data,
                        aes_string(x = x, y = y)) +
    geom_point(size = 0.5) +
    xlab("Mean effect size") +
    ylab("Inverse of standard error (precision)") +
    ggtitle(paste(period, "months after the shock")) +
    theme(title = element_text(size = 10, face = 'bold')) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    geom_vline(xintercept = 0, colour = "black", linetype = 2) +
    theme(legend.text = element_text(colour = "black", size = 4)) +
    theme(axis.text.x = element_text(size = 11)) +
    theme(axis.title.x = element_text(size = 11)) +
    theme(axis.text.y = element_text(size = 11)) +
    theme(axis.title.y = element_text(size = 11))
  
  return(plot_funnel)
}


data<-data_back

out<-'gdp'
data <- subset(data, outcome %in% out)


plot_list <- list()
plot_list_winsor <- list()

periods <- c(3, 6, 12, 18, 24, 30, 36, 48)



############################################## Loop through periods and create plots

for (x in periods) {
  print(paste("Processing period:", x))  # Debugging
  data_period <- data %>% dplyr::filter(period.month == x)
  print(paste("Number of observations for period", x, ":", nrow(data_period)))  # Debugging
  
  if (nrow(data_period) == 0) {
    next  # Skip to the next iteration if there are no observations
  }
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  plot_funnel <- create_funnel_plot(data_period, x)
  ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months.png"), plot_funnel)
  plot_funnel <- create_funnel_plot_grid(data_period, x)
  plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)
all_month<-grid.arrange(grobs = plot_list, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on output",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of output in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months.png"), all_month, width = 30, height = 20, units = "cm")
all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on output (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of output in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months_winsor.png"), all_month_winsor, width = 30, height = 20, units = "cm")




##############################################  Baseline Regression and baseline WLS by precicion ###############################################################

# Create empty lists to store the results
results_list <- list()
coef_test_data <- list()
confint_data <- list()
cluster_var <- list()

library(clubSandwich)# for coef test results

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
  
  
  # Calculate variance winsorised
  data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
  
  # Calculate PrecVariance winsorised
  data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor
  
  # Calculate (precision-weighted) average
  regwa <- lm(mean.effect_winsor ~ 1, weights = precvariance_winsor, data = data_period_winsor)
  results_list[[paste0(x, ".ols")]] <- regwa
  
  coef_test_data[[paste0(x, ".ols")]]<-coef_test(regwa, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  cluster_var[[paste0(x, ".ols")]]<-data_period_winsor$key
  
  confint_data[[paste0(x, ".ols")]]<-confint(regwa, level=0.95)
  
  # Baseline WLS
  wls_pbias <- lm(mean.effect_winsor ~ standarderror_winsor, weights = precvariance_winsor, data = data_period_winsor)
  results_list[[paste0(x, ".wls")]] <- wls_pbias
  
  coef_test_data[[paste0(x, ".wls")]]<-coef_test(wls_pbias, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  cluster_var[[paste0(x, ".wls")]]<-data_period_winsor$key
  
  confint_data[[paste0(x, ".wls")]]<-confint(wls_pbias, level=0.95)
  
}

# weighted average noch ergänzen. 




# naive T and CRO already fixed? 


library(modelsummary)
#modelsummary(results_list, output = "gt",vcov = 'cr0', cluster = data_period_winsor$key,stars = TRUE) # only works if cluster variable has the same number of observations across lists. 
modelsummary(results_list, output = "gt",stars = TRUE)

coef_test_data<-data.table::rbindlist(coef_test_data, fill = T,idcol = ".id")
coef_test_data

# library(estimatr)
# 
# 
# regwa <- lm(mean.effect_winsor ~ 1, data = data_period_winsor)
# summary(regwa)
# 
# coef_test(regwa, vcov = "CR0", cluster = data_period_winsor$key, test = "naive-t")
# 
# regwa_robust <- lm_robust(mean.effect_winsor ~ 1, data = data_period_winsor,clusters = data_period_winsor$key,se_type = "CR0")
# summary(regwa_robust)
# 
# 
# 
# wls_pbias <- lm(mean.effect_winsor ~ standarderror_winsor, weights = precvariance_winsor, data = data_period_winsor)
# summary(wls_pbias)
# 
# coef_test(wls_pbias, vcov = "CR0",cluster = data_period_winsor$key, test = "naive-t")
# 
# regwa_robust_wls <- lm_robust(mean.effect_winsor ~ standarderror_winsor, weights = precvariance_winsor, data = data_period_winsor,clusters = data_period_winsor$key,se_type = "CR0")
# summary(regwa_robust_wls)
# 
# 
# results_list <- list()
# results_list[[1]]<-regwa
# results_list[[2]]<-wls_pbias




#################################################################### Furukawa (2021) stem function ############################################################


##################################################################### Functions ############

# we could also directly run the source code source("stem_method.R") if we install it in our repository. would probably be the cleaner way. 

##0 set stem parameter
tolerance = 10^(-3) #set level of sufficiently small stem to determine convergence
max_N_count = 10^3 #set maximum number of iteration before termination
param <- c(tolerance, max_N_count)

##1 outer algorithm
stem <- function(beta, se, param){
  #Initial Values
  N_study <- length(beta)
  
  # sending sigma0->infinity implies equal weights to all studies
  beta_equal <- mean(beta)
  max_sigma_squared <- variance_0(N_study, beta, se, beta_equal)
  max_sigma = sqrt(max_sigma_squared)
  min_sigma = 0
  tolerance = param[1]
  
  #Sorting data by ascending order of standard error
  data1 <- cbind(beta,se)
  data_sorted <- data1[order(data1[,2]),]
  beta_sorted <- data_sorted[,1]
  se_sorted <- data_sorted[,2]
  
  #Compute stem based estimates until convergence from max and min of sigma
  output_max <- stem_converge(max_sigma, beta_sorted, se_sorted, param)
  output_min <- stem_converge(min_sigma, beta_sorted, se_sorted, param)
  Y_max <- output_max$estimates
  Y_min <- output_min$estimates
  
  #Check whether max and min agree
  diff_sigma <- abs(Y_max[3] -  Y_min[3])
  if (diff_sigma > (2*tolerance)){
    multiple = 1
  }
  else{
    multiple = 0
  }
  
  #information in sample
  n_stem <- Y_max[4]
  sigma0 <- Y_max[3]
  inv_var <- 1/(se_sorted^2+sigma0^2)
  info_in_sample = sum(inv_var[1:n_stem])/sum(inv_var)
  
  #Return
  Y1 = c(Y_max, multiple, info_in_sample)
  Y2 <- t(Y1)
  Z1 <- output_max$MSE
  Z2 <- t(Z1)
  
  colnames(Y2) <- c("estimate","se", "sd of total heterogeneity", "n_stem", "n_iteration", "multiple", "% info used")
  colnames(Z2) <- c("MSE", "variance", "bias_squared")
  output <- list("estimates" = Y2, "MSE" = Z2)
  return(output)
}

stem_converge <- function(initial_sigma, beta_sorted, se_sorted, param){
  converged = 0
  N_count = 0
  tolerance = param[1]
  max_N_count = param[2]
  sigma0 = initial_sigma
  
  while (converged == 0){
    output <- stem_compute(beta_sorted, se_sorted, sigma0)
    Y_stem <- output$estimates
    sigma = Y_stem[3]
    evolution = abs(sigma0 - sigma)
    N_count = N_count + 1
    
    if (evolution<tolerance){
      converged = 1
    }
    else if (N_count > max_N_count){
      converged = 1
    }
    else{
      sigma0 = sigma
    }
  }
  Y <- c(Y_stem, N_count)
  Z <- output$MSE
  output <- list("estimates" = Y, "MSE" = Z)
  return(output)
}

##2 inner algorithm
stem_compute <- function(beta, se, sigma0){
  
  N_study = length(beta)
  
  # relevant bias squared
  Eb_all = weighted_mean(beta, se, sigma0)
  Eb_leave_top_out = weighted_mean(beta[2:N_study], se[2:N_study], sigma0)
  Eb_squared = weighted_mean_squared(beta[2:N_study], se[2:N_study], sigma0)
  MSE_original = Eb_squared - 2*beta[1]*Eb_leave_top_out
  # since Eb_squared[1] cannot be computed without bias
  
  # variance
  Var_all = variance_b(se, sigma0)
  
  # minimize MSE
  n_stem_min = 3
  MSE = MSE_original[(n_stem_min-1):(N_study-1)]
  Bias = MSE - Var_all[n_stem_min:N_study]
  index <- which.min(MSE)
  n_stem = index+(n_stem_min-1)
  
  # assign values
  beta_stem = Eb_all[n_stem]
  se_stem = Var_all[n_stem]^(0.5)
  var_stem = variance_0(N_study, beta, se, beta_stem)
  sigma_stem = sqrt(var_stem)
  
  # stack outputs
  Y = cbind(beta_stem,se_stem,sigma_stem,n_stem)
  Z = rbind(MSE, Var_all[n_stem_min:N_study], Bias[(n_stem_min-1):(N_study-1)])
  output <- list("estimates" = Y, "MSE" = Z)
  return(output)
}

variance_b <- function(se, sigma){
  N_study <- length(se)
  Y <- vector(mode = 'numeric', length = N_study)
  proportional_weights = 1/(se^2 + sigma^2)
  
  for (i in 1:N_study){
    Y[i] <- 1/sum(proportional_weights[1:i])
  }
  return(Y)
}


variance_0 <- function(n_stem, beta, se, beta_mean){
  # formula adopted from DerSimonian and Laird (1996)
  weights <- 1/(se[1:n_stem]^2)
  total_weight = sum(weights)
  
  Y1 <- (t(weights) %*% (beta[1:n_stem] - beta_mean)^2) - (n_stem - 1)
  Y2 <- total_weight - (t(weights) %*% weights)/total_weight
  var = pmax(0, Y1/Y2)
  
  Y <- var
  return(Y)
}

weighted_mean <- function(beta, se, sigma){
  N_study <- length(beta)
  Y <- vector(mode = 'numeric', length = N_study)
  
  proportional_weights <- 1/(se^2 + sigma^2)
  
  for (i in 1:N_study){
    Y[i] <- beta[1:i] %*% proportional_weights[1:i]/sum(proportional_weights[1:i])
  }
  return(Y)
}

weighted_mean_squared <- function(beta, se, sigma){
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  for (i in 2:N){
    Y1 <- sum(WB[1:i,1:i]) - sum(weights_beta[1:i]^2)
    Y2 <- sum(W[1:i,1:i]) - sum(weights[1:i]^2)
    Y[i] <- Y1/Y2
  }
  return(Y)
}

se_rescale <- function(se){
  Y <- -log10(se)
  return(Y)
}

stem_funnel <- function(beta_input, se_input, stem_estimates){
  #take stem estimates
  b_stem <- stem_estimates[1]
  SE_b_stem <- stem_estimates[2]
  sigma0 <- stem_estimates[3]
  n_stem <- stem_estimates[4]
  
  #cumulative estimates
  data_input <- cbind(beta_input,se_input)
  data_sorted <- data_input[order(data_input[,2]),]
  beta_sorted <- data_sorted[,1]
  se_sorted <- data_sorted[,2]
  cumulative_estimates = weighted_mean(beta_sorted, se_sorted, sigma0)
  
  #set values for figures
  t_stat <- 1.96
  lineswidth<-2.5
  filled_diamond <- 18
  points_size <-2
  se_axis_min <- 0
  beta_axis_min <- 0.6
  beta_axis_max <- 1.2
  labNames <- c('Coefficient ','Precision ')
  
  # rescale SE for ease of visual interpretation
  se_axis <- se_rescale(se_sorted)
  
  #--------------
  # plot
  #--------------
  
  plot.new()
  # adjust margin
  par(mar=c(4.1,4.1,1,1))
  # plot studies
  plot(beta_sorted, se_axis, 
       col=rgb(102, 102, 255, maxColorValue = 255), pch = 1, lwd = 2.5,
       xlim=c(beta_axis_min, beta_axis_max),
       xlab=substitute(paste(name, beta), list(name=labNames[1])),
       ylab=substitute(paste(name, -log(SE)), list(name=labNames[2]))) #@@@@@@ modify this line if rescaling with a different function    
  #stem (cumulative estimates)
  lines(cumulative_estimates, se_axis, col=rgb(96, 96, 96, maxColorValue = 255), lwd=lineswidth)
  #augment stem
  points(b_stem, se_axis[n_stem], pch=filled_diamond ,col=rgb(0, 0, 153, maxColorValue = 255), cex = points_size)
  segments(b_stem, se_axis[1], b_stem, se_axis_min,col=rgb(0, 0, 153, maxColorValue = 255), lwd=lineswidth)
  #stem-based estimate
  points(b_stem, se_axis[1], pch=filled_diamond ,col=rgb(255, 128, 0, maxColorValue = 255), cex = points_size)
  segments(b_stem-t_stat*SE_b_stem, se_axis[1], b_stem+t_stat*SE_b_stem, se_axis[1],col=rgb(255, 128, 0, maxColorValue = 255), lwd=lineswidth)
  #zero line
  abline(v=0, col=rgb(192, 192, 192, maxColorValue = 255), lty=2, lwd=lineswidth)
  
  #legend
  legend("topleft", #@@@@@@ modify this line if want to put legend in a different location
         legend = c("stem-based estimate","95 confidence interval","cumulative estimate", "minimal precision", "study") , 
         col = c(rgb(255, 128, 0, maxColorValue = 255) , 
                 rgb(255, 128, 0, maxColorValue = 255) ,
                 rgb(96, 96, 96, maxColorValue = 255) ,
                 rgb(0, 0, 153, maxColorValue = 255) ,  
                 rgb(102, 102, 255, maxColorValue = 255)) , 
         bty = "n", 
         lty = c(NA, 1, 1, NA, NA), lwd = c(NA, 2, 2, NA, 2.5),
         pch= c(18, NA, NA, 18, 1), 
         pt.cex = 1.8, cex = 0.8, horiz = FALSE, inset = c(0, 0),
         y.intersp=1.5)
  
}

stem_MSE <- function(V){
  MSE = V[,1]
  bias_squared = V[,3]
  variance = V[,2]
  N_study = dim(V)[1]
  
  #figure input
  N_min = 2
  lineset <- 2.5
  num_study <- (N_min+1):(N_study+1)
  
  layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  plot(num_study,bias_squared[N_min:N_study],type='l',  col="blue", lwd = lineset, xlab = 'Num of included studies i', ylab='', main= expression(Bias^2 - b[0]^2))
  plot(num_study,variance[N_min:N_study],type='l',  col="blue", lwd = lineset, xlab = 'Num of included studies i', ylab='', main = expression(Variance))
  plot(num_study,MSE[N_min:N_study],type='l',  col="blue", lwd = lineset, xlab = 'Num of included studies i', ylab='', main = expression(MSE - b[0]^2))
  
}

#4. auxiliary function
#install.packages("data.table")
library("data.table")

data_median <- function(data, id_var, main_var, additional_var){
  
  #1. drop rows with any NA
  complete_data <- na.omit(data)
  
  #2. rename columns
  column_id <- eval((substitute(complete_data[a], list(a = id_var))))
  colnames(column_id)[1] <- "id"
  column_main <- eval((substitute(complete_data[a], list(a = main_var))))
  colnames(column_main)[1] <- "main"
  column_additional <- eval((substitute(complete_data[a], list(a = additional_var))))
  colnames(column_additional)[1] <- "additional"
  
  columns_main_merged <- merge(column_id, column_main, by=0, all=TRUE) 
  columns_additional_merged <- merge(column_id, column_additional, by=0, all=TRUE) 
  
  #3. choose median of main_var along with id
  median_only <- aggregate(main~id,columns_main_merged,median)
  
  #4. merge with complete data
  median_together <- merge(median_only, columns_main_merged, by.x="id", by.y="id")
  median_all <- merge(median_together, columns_additional_merged, by.x="Row.names", by.y="Row.names")
  
  #5. choose data closest to the computed median
  median_all$diff_squared <- (median_all$main.x - median_all$main.y)^2
  table_form <- data.table(median_all)
  median_combined <- table_form[ , .SD[which.min(diff_squared)], by = id.x]
  
  #6. clean before output
  median_combined2 <- median_combined[order(median_combined$id.x),]
  median_combined3 <- median_combined2[, c("id.x", "main.x", "additional")]
  colnames(median_combined3)[1] <- "ID"
  colnames(median_combined3)[2] <- "coefficient"
  colnames(median_combined3)[3] <- "standard_error"
  Y <- median_combined3
  
  return(Y)
}

##################################################################### run functions ############

#Furukawa (2021) results
#dataset without NAs, because the stem function only works without NAs


stem_results<-list()

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
  
  stem_results[[paste0(x, ".stem")]] <- stem(data_period_Furukawa$mean.effect_winsor, data_period_Furukawa$standarderror_winsor, param)
  
}

lapply(stem_results, `[[`, 1)

data.table::rbindlist(lapply(stem_results, `[[`, 1), fill = T,idcol = ".id")



stem_funnel(data_period_Furukawa$mean.effect_winsor, data_period_Furukawa$standarderror_winsor, stem_results[[8]]$estimates)



median_data <- data_median(data_period_winsor, "key", "mean.effect_winsor", "standarderror_winsor") # some error in this code if we look at the data median. 
test <- stem(median_data$coefficient, median_data$standard_error, param)




#####################################################################   #Ioannidis et al. (2017) ######################################################################
#Top 10% of estimates with the smallest standard error



# Create empty lists to store the results
results_list_regwa_topten <- list()
confint_data_regwa_topten <- list()


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
  
  
  # Calculate variance winsorised
  data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
  
  # Calculate PrecVariance winsorised
  data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor
  
  
  topten_data_period <- data_period_winsor %>% dplyr::mutate(rank = rank(desc(standarderror_winsor))) %>%
    dplyr::filter(rank > quantile(rank, .9)) %>%
    dplyr::arrange(rank)
  
  print(paste("Observations top 10% ", nrow(topten_data_period)))
  # Calculate (precision-weighted) average
  #result in Gechert and Heimberger (2022): Table 3, column (5)
  #weighted-average
  regwa_topten <- lm(mean.effect_winsor~1, data=topten_data_period, weights=precvariance_winsor)
  results_list_regwa_topten[[paste0(x, ".top_10")]]<-regwa_topten
  #confidence interval
  confint_data_regwa_topten[[paste0(x, ".top_10")]]<-confint(regwa_topten, level=0.95)
  
  
  #WAAP (weighted average of the adequately powered)
  #powered
  data_period_winsor$powered <- abs(data_period_winsor$mean.effect/2.8) # why 2.8?
  #Non-linear tests
  data_period_winsor$WAAP <- ifelse(data_period_winsor$standarderror_winsor < data_period_winsor$powered, 1, 0)
  data_period_winsor_WAAP <- data_period_winsor
  data_period_winsor_WAAP <- subset(data_period_winsor, WAAP %in% c('1'))
  
  #weighted average of the adequately powered
  regwa_data_period_WAAP <- lm(mean.effect_winsor~1, data=data_period_winsor_WAAP, weights=precvariance_winsor)
  results_list_regwa_topten[[paste0(x, ".waap")]]<-regwa_data_period_WAAP
  #confidence interval
  confint_data_regwa_topten[[paste0(x, ".waap")]]<-confint(regwa_data_period_WAAP, level=0.95)
}

modelsummary(results_list_regwa_topten, output = "gt",stars = TRUE)

confint_data_regwa_topten

######################################################################## Bom-Rachinger (2019) ################################################################



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



EKresults_list<-list()

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








############################################################### create grouped funnel plots ####################################################################

########################################## Functions


library(ggplot2)
library(JWileymisc)
library(tidyr)
library(dplyr)
library(RColorBrewer)
myColors <- brewer.pal(4,"Set1")
names(myColors) <- levels(data_period_winsor$us)

create_funnel_plot_grid <- function(data, period, group_color, winsorize = FALSE) {
  if (winsorize) {
    plot_title <- " (with Winsorization)"
    x <- "mean.effect_winsor"
    y <- "precision_winsor"
  } else {
    plot_title <- ""
    x <- "mean.effect"
    y <- "precision"
  }
  
  plot_funnel <- ggplot(data = data,
                        aes_string(x = x, y = y, colour = group_color)) +
    geom_point(size = 1,alpha = 0.5) +
    xlab("Mean effect size") +
    ylab("Inverse of standard error (precision)") +
    ggtitle(paste(period, "months after the shock",group_color)) +
    theme(title = element_text(size = 10, face = 'bold')) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    geom_vline(xintercept = 0, colour = "black", linetype = 2) +
    theme(legend.text = element_text(colour = "black", size = 5)) +
    theme(axis.text.x = element_text(size = 11)) +
    theme(axis.title.x = element_text(size = 11)) +
    theme(axis.text.y = element_text(size = 11)) +
    theme(axis.title.y = element_text(size = 11))+
    scale_colour_manual(name = "transformation",values = myColors)+ 
    guides(color = guide_legend(override.aes = list(size = 2))) 
  
  return(plot_funnel)
}


data<-data_back

out<-'gdp'
data <- subset(data, outcome %in% out)



plot_list_winsor <- list()

periods <- c(3, 6, 12, 18, 24, 30, 36, 48)



############################################## Loop through periods and create plots

for (x in periods) {
  print(paste("Processing period:", x))  # Debugging
  data_period <- data %>% dplyr::filter(period.month == x)
  print(paste("Number of observations for period", x, ":", nrow(data_period)))  # Debugging
  
  if (nrow(data_period) == 0) {
    next  # Skip to the next iteration if there are no observations
  }
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  #plot_funnel <- create_funnel_plot(data_period, x)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months.png"), plot_funnel)
  #plot_funnel <- create_funnel_plot_grid(data_period, x)
  #plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  #plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, "transformation", winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)

all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on output (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of output in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months_winsor_trans.png"), all_month_winsor, width = 30, height = 20, units = "cm")








data$growth<-ifelse(data$transformation %in% c("gr","logdiff"),1,0)
data$us<-as.factor(ifelse(data$list_of_countries=="US","US",ifelse(data$list_of_countries=="EA","EA","other")))
# data_period_winsor$ea<-ifelse(data_period_winsor$list_of_countries=="EA",1,0)
# data_period_winsor$gb<-ifelse(data_period_winsor$list_of_countries=="GB",1,0)


unique(data_period_winsor$us)

#sum(data_period_winsor$upr)
#save<-data_period_winsor

plot_list_winsor <- list()

############################################## Loop through periods and create plots

for (x in periods) {
  print(paste("Processing period:", x))  # Debugging
  data_period <- data %>% dplyr::filter(period.month == x)
  print(paste("Number of observations for period", x, ":", nrow(data_period)))  # Debugging
  
  if (nrow(data_period) == 0) {
    next  # Skip to the next iteration if there are no observations
  }
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  #plot_funnel <- create_funnel_plot(data_period, x)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months.png"), plot_funnel)
  #plot_funnel <- create_funnel_plot_grid(data_period, x)
  #plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  #plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, "us", winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)

all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on output (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of output in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months_winsor_us.png"), all_month_winsor, width = 30, height = 20, units = "cm")


plot_list_winsor <- list()


data$cum<-as.factor(data$cum)
data$growth<-ifelse(data$transformation %in% c("gr","logdiff"),1,0)




############################################## Loop through periods and create plots

for (x in periods) {
  print(paste("Processing period:", x))  # Debugging
  data_period <- data %>% dplyr::filter(period.month == x) %>%  dplyr::filter(growth == 1)
  print(paste("Number of observations for period", x, ":", nrow(data_period)))  # Debugging
  
  if (nrow(data_period) == 0) {
    next  # Skip to the next iteration if there are no observations
  }
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  #plot_funnel <- create_funnel_plot(data_period, x)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months.png"), plot_funnel)
  #plot_funnel <- create_funnel_plot_grid(data_period, x)
  #plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  #plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, "cum", winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)

all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on output (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of output in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months_winsor_cum.png"), all_month_winsor, width = 30, height = 20, units = "cm")




library(ggplot2)
library(JWileymisc)
library(tidyr)
library(dplyr)
library(RColorBrewer)
myColors <- brewer.pal(3,"Set1")
names(myColors) <- levels(data$periodicity)

data$periodicity<- as.factor(data$periodicity)

plot_list_winsor <- list()

############################################## Loop through periods and create plots

for (x in periods) {
  print(paste("Processing period:", x))  # Debugging
  data_period <- data %>% dplyr::filter(period.month == x) %>%  dplyr::filter(growth == 1)
  print(paste("Number of observations for period", x, ":", nrow(data_period)))  # Debugging
  
  if (nrow(data_period) == 0) {
    next  # Skip to the next iteration if there are no observations
  }
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
  data_period$precision <- 1 / data_period$StandardError
  
  data_period_winsor <- data_period
  data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
  data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
  data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
  
  #plot_funnel <- create_funnel_plot(data_period, x)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months.png"), plot_funnel)
  #plot_funnel <- create_funnel_plot_grid(data_period, x)
  #plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  #plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  #ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, "periodicity", winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)

all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on output (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of output in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months_winsor_periodicity.png"), all_month_winsor, width = 30, height = 20, units = "cm")



#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(4,"Set1")
names(myColors) <- levels(data_period_winsor$transformation)


ggplot(data = data_period_winsor,
       aes(x = mean.effect_winsor, y = precision_winsor, colour = transformation)) +
  geom_point(size = 1,alpha = 0.5) +
  xlab("Mean effect size\n %-change of output in response to 100bp monetary policy shock") +
  ylab("Inverse of standard error (precision)") +
  ggtitle(paste("Funnel plots of effect of monetary policy on output\n", "(months after the shock)")) +
  theme(title = element_text(size = 10, face = 'bold')) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme(legend.text = element_text(colour = "black", size = 10)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11))+
  scale_colour_manual(name = "transformation",values = myColors)+ 
  guides(color = guide_legend(override.aes = list(size = 4))) 




ggplot(data = data_period_winsor,
       aes(x = mean.effect_winsor, y = precision_winsor, colour = us)) +
  geom_point(size = 1,alpha = 0.5) +
  xlab("Mean effect size\n %-change of output in response to 100bp monetary policy shock") +
  ylab("Inverse of standard error (precision)") +
  ggtitle(paste("Funnel plots of effect of monetary policy on output\n", "(months after the shock)")) +
  theme(title = element_text(size = 10, face = 'bold')) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme(legend.text = element_text(colour = "black", size = 10)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11))+
  scale_colour_manual(name = "transformation",values = myColors)+ 
  guides(color = guide_legend(override.aes = list(size = 4))) 



data_period_winsor$chol<-unlist(data_period_winsor$chol)
#as.vector(data_period_winsor$chol)
ggplot(data = data_period_winsor,
       aes(x = mean.effect_winsor, y = precision_winsor, colour = as.factor(chol))) +
  geom_point(size = 1,alpha = 0.5) +
  xlab("Mean effect size\n %-change of output in response to 100bp monetary policy shock") +
  ylab("Inverse of standard error (precision)") +
  ggtitle(paste("Funnel plots of effect of monetary policy on output\n", "(months after the shock)")) +
  theme(title = element_text(size = 10, face = 'bold')) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme(legend.text = element_text(colour = "black", size = 10)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11))+
  scale_colour_manual(name = "transformation",values = myColors)+ 
  guides(color = guide_legend(override.aes = list(size = 4))) 



data_period_winsor<- save

data_period_winsor<-data_period_winsor %>% filter(growth==1)

ggplot(data = data_period_winsor,
                      aes(x = mean.effect_winsor, y = precision_winsor, colour = transformation)) +
  geom_point(size = 1,alpha = 0.5) +
  xlab("Mean effect size\n %-change of output in response to 100bp monetary policy shock") +
  ylab("Inverse of standard error (precision)") +
  ggtitle(paste("Funnel plots of effect of monetary policy on output\n", "(months after the shock)")) +
  theme(title = element_text(size = 10, face = 'bold')) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position = "right") +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  theme(legend.text = element_text(colour = "black", size = 10)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11))

