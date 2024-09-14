rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


library(here)


data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)


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
outvar<-'output'#c("output", "inflation", "unemp", "emp")
data <- subset(data, outcome %in% outvar)


### if we want to randomly draw one model per study. 
# data <- data %>% 
#   group_by(key,period.month) %>%
#   sample_n(size = 1,replace = F) %>% ungroup()

# define periods and empty objects to strore results
periods <- c(3, 6, 12, 18, 24, 30, 36, 48)
results_list <- list()
vif_list <- list()
bms_full_list <- list()
#winsorization level
wins<-0.02


for (x in periods) {

    # Subset data for the current period
    data_period <- subset(data, period.month == x)
    
    
    # calculate standard errors  
    data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
    
      # Winsorize data
    data_period_winsor <- data_period
    data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, wins, na.rm = TRUE)
    data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect,wins, na.rm = TRUE)
    data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
      
    # Calculate variance winsorised
    data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
      
    # Calculate PrecVariance winsorised
    data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor
    
    
    # Create dataset for BMS estimation
    data_bms<-data_period_winsor %>% mutate(observations=log(observations),infl=log(infl),gdppc=log(gdppc)) %>%  select(mean.effect_winsor,standarderror_winsor,group_ident_broad,lp,vecm,dyn_ols,fvar,tvar,gvar,dsge,varother,panel,bayes,regime,upr,lor,hike,cut,decomposition,convent,pure_rate_shock,lrir,fx,foreignir,inflexp,eglob,find,outpgap,comprice,month,main_research_q,prefer,mean_year,ea12,us,upper_middle,n_of_countries,outcome_measure,transformation,cum,interest_rate_short,cbanker,pub_year,is_top_tier,is_top_5,journal_impact,num_cit,rid1,infl,gdppc,tradegl,fingl,findev,cbi,exrate)# exclude rate mean effect
    
    # automatically create dummies for factor and character variables
    data_bms<-fastDummies::dummy_cols(data_bms,remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)# set reference group to most frequent level of each factor varible and remove the original factor variable and only use the thereof created dummies. 
    
    # check if there are NA values in the dataset
    if (anyNA(data_bms)) {warning("some selected variables contain NA values")}
    na.omit(data_bms)
    
    
    # Calculate VIF for each variable
    vif_values <- car::vif(lm(data_bms))
    # store the VIF values
    vif_list[[paste0(x)]] <- vif_values
    
    
    ###################### BMS
    # data.frame necessary for BMS estimation
    data_bms<-as.data.frame(data_bms)
    
    #BMS estimation
    speed <- bms(data_bms, g="UIP", mprior="uniform", user.int=FALSE,nmodel = 20000,burn = 5000, iter=10000, mcmc="bd")#,start.value = c(1:35) # burn ins and iterations should be increased for final model
    
    #store bms results
    bms_full_list[[paste0(x)]] <- speed
    
    #save coefficients results in results list BMS
    results_list[[paste0(x)]] <- coef(speed, order.by.pip = T, exact=T, include.constant=T)


    
    # If you want to save the the BMS picture per period uncomment the code. 
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
# function to map results_list to modelsummary_list element # show PIP and sign of the coefficient for each variable. 
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

# function to map results_list to modelsummary_list element and include summary statistics of the BMS # show PIP, the coefficient and the corresponding standard error for each variable. 
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




# Apply the function to each element in vif_list results_list
mod_list_coef <- Map(create_modelsummary_list_bms_coef,results_list,bms_full_list)
mod_list_pip<- lapply(results_list, create_modelsummary_list_bms_pip)


# Show table containing PIP, the coefficient and the corresponding standard error for each variable. 
modelsummary(mod_list_coef)
# Show table containing PIP and sign of the coefficient for each variable. 
modelsummary(mod_list_pip)



#################### show model plots for a specific horizon:

desired_period.month <- 12
index<-which(periods%in% desired_period.month)
index

#Plot posterior and prior model size distribution and posterios model probabilities
plot(bms_full_list[[element]])

#plot posterios model probabilities
plotConv(bms_full_list[[element]])

# Plot typical BMS image 
image(bms_full_list[[element]], cex.axis=.5, order.by.pip = T, yprop2pip=F)#,col=c("orange","lightgrey"), main=paste0("BMA image ",outcome," ",x," months")

# Plot marginal density for a specifi moderator. In this case the dsge model 
density(bms_full_list[[element]], reg="dsge")


