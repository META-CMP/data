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


#generate main research question dummy
fun <- function(x, y) {
  grepl(x, y, fixed = TRUE)
}

data$main_research_q<-mapply(fun, data$outcome, data$main)


library(zoo)


#data %>% filter(end=="Q4–2015")

data$start<-ifelse(data$start=="Q4–2000","Q4-2000",data$start)
data$end<-ifelse(data$end=="Q4–2015","Q4-2015",data$end)


data<-data %>%
  mutate(observations_calc = case_when(substr(start,1,1)=="Q" ~ ((as.yearqtr(end, format = "Q%q-%Y")-as.yearqtr(start, format = "Q%q-%Y"))*4+1)*as.numeric(n_of_countries),
                                       substr(start,1,1)!="Q" & grepl("-", start) == TRUE ~ ((as.yearmon(end, format = "%m-%Y")-as.yearmon(start, format = "%m-%Y"))*12+1)*as.numeric(n_of_countries),
                                       nchar(data$start)==4 ~ (as.numeric(end)-as.numeric(start)+1)*as.numeric(n_of_countries)))


data$samplesize<-as.numeric(data$samplesize)

unique(data$samplesize)

data$observations<-ifelse(is.na(data$samplesize), data$observations_calc, as.numeric(data$samplesize))




# set logical values to dummies.
data<-data %>% mutate_if(is.logical, as.numeric)  

#summary(data)
head(data)

#test<-data %>% filter(observations==1)
unique(data$study_notes)
unique(data$model_notes)

data$quality_concern<-grepl("quality_concern",data$study_notes)

data$quality_concern<-ifelse(grepl("quality_concern",data$model_notes),TRUE, data$quality_concern)


data$regime<-grepl("regime",data$study_notes)

data$regime<-ifelse(grepl("regime",data$model_notes),TRUE, data$regime)

sum(data$regime)

remove(other)
########################################################################## merge external data #######################################################################

data<-data %>%
  mutate(start_year= as.numeric(case_when(substr(start,1,1)=="Q" ~ format(as.Date(as.yearqtr(start, format = "Q%q-%Y")),"%Y"),
                                       substr(start,1,1)!="Q" & grepl("-", start) == TRUE ~ format(as.Date(as.yearmon(start, format = "%m-%Y")),"%Y"),
                                       nchar(start)==4 ~ format(as.Date(start, format = "%Y"),"%Y"))),
         end_year= as.numeric(case_when(substr(end,1,1)=="Q" ~ format(as.Date(as.yearqtr(end, format = "Q%q-%Y")),"%Y"),
                               substr(end,1,1)!="Q" & grepl("-", end) == TRUE ~ format(as.Date(as.yearmon(end, format = "%m-%Y")),"%Y"),
                               nchar(end)==4 ~ format(as.Date(end, format = "%Y"),"%Y"))),
         mean_year=(start_year+end_year)/2
         )


data_merged<-read.csv("~/data/data/merge_external_data/external-data.csv") %>% select(-X)

data_merged$year<-as.numeric(data_merged$year)

data<-data %>% unnest(list_of_countries)

data$list_of_countries<-strsplit(data$list_of_countries, " ")

data_merged<-data %>% select(list_of_countries,key,model_id,start_year,end_year) %>% 
  mutate(year = map2(start_year, end_year, seq, by = 1)) %>%
  unnest(year) %>% unnest(list_of_countries) %>%
  left_join(data_merged, by = c('year',"list_of_countries"="ccode")) %>%
  filter(year >= start_year & year <= end_year) %>%
  select(-list_of_countries) %>% 
  group_by(key, model_id,start_year,end_year) %>%
  summarise(across(everything(),~ mean(., na.rm = TRUE)))
data_merged

data<-data %>% ungroup() %>% left_join(data_merged %>% ungroup %>% select(key, model_id,tradegl:exrate), by=c("key","model_id"))




###############################################################################  reg #################################################################################
data_back<-data

data<-data_back

out<-'gdp'
data <- subset(data, outcome %in% out)


periods <- c(3, 6, 12, 18, 24, 30, 36, 48)

# can be deleted later on. Just to recode one NA in the prelimnary dataset which is later set to false. 
data<-data %>% mutate(prefer=ifelse(is.na(prefer),FALSE,prefer))

# True can be deleted for final dataset as these entries have been recoded. 
data<-data %>% mutate(dyn_ols=ifelse(dyn_ols=="TRUE" | dyn_ols=="ardl",TRUE,FALSE))


split_list<-stri_split_fixed(str = data$inttype, pattern = "_", n = 3)

# Extract nth element from each split list
data$intrest_rate <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

data$interest_rate_short<-ifelse(grepl("3_month",data$intrest_rate) | grepl("1_month",data$intrest_rate) | grepl("6_month",data$intrest_rate) | grepl("1_year",data$intrest_rate)| grepl("2_year",data$intrest_rate) | grepl("ssr",data$intrest_rate) | grepl("shadow_fed_funds",data$intrest_rate)| grepl("long_term_rate",data$intrest_rate)| grepl("shadow_rate",data$intrest_rate),FALSE, TRUE)


data<-data %>% unnest(n_of_countries) %>% unnest(chol) %>% unnest(bayes) %>% unnest(dsge) %>% unnest(idother) %>% unnest(var) %>% unnest(model_notes) %>% unnest(prefer) %>% mutate_if(is.logical, as.numeric) 
summary(data)
#data %>% unnest(figure)  %>% unnest(page) #data %>% filter(is.na(figure)) is.na figure set to figure Appendix D1.

data$us<-as.factor(ifelse(data$list_of_countries=="US","US",ifelse(data$list_of_countries=="EA","EA","other")))

# create inital shock size measure
data<-data %>%
  group_by(key,model_id) %>%
  mutate(shock_size = rate_mean.effect[period.month == 0])



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


