data_back<-data

data<-data_back

out<-'gdp'
data <- subset(data, outcome %in% out)


periods <- c(3, 6, 12, 18, 24, 30, 36, 48)

summary(data)

equation<-mean.effect_winsor ~standarderror_winsor+exrate+gdppc+cbi+findev+fingl+infl+tradegl+mean_year+regime+quality_concern+observations+main_research_q+real_output+outcome_measure+as.factor(periodicity)+as.factor(transformation)+rate_mean.effect+cbanker+decomposition+convent+pure_rate_shock+lrir+fx+foreignir+inflexp+eglob+find+outpgap+comprice+panel+n_of_countries+us+month+quarter+upr+lor+varother+dsge+bayes+gvar+tvar+fvar+dyn_ols+vecm+lp+idother+longrun+heteroskedas+hf+signr+svar+chol+event+nr+forecast_based+iv+prefer+shock_size+interest_rate_short+as.factor(rid1)+model_id

# +fexch

results_list<-list()
coef_test_data<-list()
confint_data<-list()

library(JWileymisc)# to winsorisze data
library(clubSandwich)# for coef test results

for (x in periods) {
  print(paste("Processing period:", x))
  
  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)
  
  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
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
  regwa <- lm(equation, data = data_period_winsor)#, weights = precvariance_winsor
  results_list[[paste0(x, ".ols")]] <- regwa
  
  coef_test_data[[paste0(x, ".ols")]]<-coef_test(regwa, vcov = "CR0", 
                                                 cluster = data_period_winsor$key, test = "naive-t")
  
  
  confint_data[[paste0(x, ".ols")]]<-confint(regwa, level=0.95)
}



dataspeed<-data_period_winsor %>% ungroup() %>% select(mean.effect_winsor, standarderror_winsor, exrate, gdppc, cbi, findev, fingl, infl, tradegl, mean_year, regime, quality_concern, observations, main_research_q, real_output, outcome_measure, periodicity,transformation, rate_mean.effect, cbanker, decomposition, convent, pure_rate_shock, lrir, fx, foreignir, inflexp, eglob, find, outpgap, comprice, panel, n_of_countries, us, month, quarter, upr, lor, varother, dsge, bayes, gvar, tvar, fvar, dyn_ols, vecm, lp, idother, longrun, heteroskedas, hf, signr, svar, chol, event, nr, forecast_based, iv, prefer, shock_size, interest_rate_short,rid1, model_id)

library(fastDummies)

dataspeed<-fastDummies::dummy_cols(dataspeed) %>% select(-rid1,-us,-transformation,-periodicity,-outcome_measure,-rid1_me,-us_other, -transformation_log,-periodicity_a,-outcome_measure_ip)


dataspeed %>% 
  summarise(across(everything(), ~ sum(is.infinite(.x))))

dataspeed<-na.omit(dataspeed)

#dataspeed<-dataspeed[,1:10]


library(car)


# Calculate VIF for each variable
vif_values <- car::vif(lm(dataspeed))
test<-cor(dataspeed)
# Print the VIF values
print(vif_values)


#//Starts estimation
library(BMS)
speed <- bms(dataspeed, g="UIP", mprior="uniform", user.int=FALSE,nmodel = 1000, mcmc="bd")


library(modelsummary)
#modelsummary(results_list, output = "gt",vcov = 'cr0', cluster = data_period_winsor$key,stars = TRUE) # only works if cluster variable has the same number of observations across lists. 
modelsummary(results_list, output = "gt",stars = TRUE)

coef_test_data<-data.table::rbindlist(coef_test_data, fill = T,idcol = ".id")
coef_test_data


test<-data %>% filter(key=="NBR7WTIT")
