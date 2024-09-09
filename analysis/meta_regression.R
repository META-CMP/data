rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


library(here)

data_path <- here("data/preliminary_data_test.RData") # works
load(data_path)
source(here("analysis/R/meta_analysis.R"))
source(here("analysis/R/apply_winsorization.R"))


data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(JWileymisc) # for Winsorization
library(clubSandwich)# for coef test results
library(modelsummary)# to nicely print results
#install.packages("corrplot")
library(corrplot)# for correlation plot


data<-data_back


#summary(data)

data<-data %>% filter(quality_concern!=1)



chosen_periods<-c(3, 6, 12, 18, 24, 36,48)



######################################################################################################################################################################################################
############################################################################################ Regressions #############################################################################################
######################################################################################################################################################################################################


###################
# subsamples 
#data<-data %>% filter(start_year>=1980)
#data<-data %>% filter(end_year<=2007)

outv<-"output"
results1 <- meta_analysis(data , outvar = outv, se_option = "avg", periods = chosen_periods,
                          wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = c("group_ident_broad","top_5_or_tier","cbanker"))#,"rate_pers"



# Create html output
modelsummary::modelsummary(results1, output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE ",outv), gof_map = NULL, coef_map = c("(Intercept)"="Intercept","variance_winsor"= "Variance",'group_ident_broadhf' = 'High frequency', 'group_ident_broadnr' = 'Narrative','group_ident_broadsignr' = 'Sign restrictions','group_ident_broadidother' = 'Other identificiation ','top_5_or_tier' = 'Top tier publication','cbanker' = 'Central bank related'))


# create latex table
modelsummary::modelsummary(results1, output = "latex_tabular", stars = TRUE, conf_level = 0.80, title = paste0("PEESE ",outv), gof_map = NULL, coef_map = c("(Intercept)"="Intercept","variance_winsor"= "Variance",'group_ident_broadhf' = 'High frequency', 'group_ident_broadnr' = 'Narrative','group_ident_broadsignr' = 'Sign restrictions','group_ident_broadidother' = 'Other identificiation ','top_5_or_tier' = 'Top tier publication','cbanker' = 'Central bank related'))




#################################################################### different model comparison ################################################################
# as factor transformation and relevel to log is baseline
data$transformation<-as.factor(data$transformation)
data <- within(data, transformation <- relevel(transformation, ref = 3))

# as factor transformed
data$transformed<-as.factor(data$transformed)

###################################### Output

# Explanatory variables for each formula (excluding "standarderror_winsor")
equations <- list(c("group_ident_broad" ,"top_5_or_tier","cbanker"),
                 # c("group_ident_broad" ,"top_5_or_tier","I(top_5_or_tier*standarderror_winsor)"),
                  c("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q"),               
                  c("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q", "transformed", "cum"),
                  c("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q", "transformed", "cum", "lrir", "fx", "foreignir", "inflexp", "eglob", "find", "outpgap", "comprice"),
                  c("group_ident_broad", "cbanker","pub_year", "main_research_q", "transformed", "cum", "lrir", "fx", "foreignir", "inflexp", "eglob", "find", "outpgap", "comprice","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes" ,"top_5_or_tier","month","mean_year","ea12","us","upper_middle","cut","hike","lor","upr"))#,
                  #("group_ident_broad","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes" ,"top_5_or_tier","convent"))


results_list<-list()

# loop through the equations
for (i in 1:length(equations)) {
    model <- meta_analysis(data, outvar = "output", se_option = "avg", periods = chosen_periods,
                           wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equations[[i]])
    results_list[[paste0("equation.", i)]] <- model
}

######### chose period which should be shown in the output
desired_period <- 12
index<-which(chosen_periods%in% desired_period)
index
# Filter the results_list for the specific period


# Create html output
modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Output) period - ",desired_period), gof_map = NULL)


# create latex table
latex_table<-modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "latex_tabular", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Output) period - ",desired_period), gof_map = NULL, coef_map = c("(Intercept)"="Intercept","variance_winsor"= "Variance",'group_ident_broadhf' = 'High frequency', 'group_ident_broadnr' = 'Narrative','group_ident_broadsignr' = 'Sign restrictions','group_ident_broadidother' = 'Other identificiation ','top_5_or_tier' = 'Top tier publication','cbanker' = 'Central bank related'))

latex_table <- gsub(
  "& equation\\.1 & equation\\.2 & equation\\.3 & equation\\.4 & equation\\.5",
  "Variable &  Baseline & Robustness 1 & Robustness 2 & Robustness 3 & Robustness 4",
  latex_table
)

# Now print or save the modified table
cat(latex_table)

######################################################################## inflation ##########################################################

results_list<-list()

for (i in 1:length(equations)) {
  model <- meta_analysis(data, outvar = "inflation", se_option = "avg", periods = chosen_periods,
                         wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equations[[i]])
  results_list[[paste0("equation.", i)]] <- model
}


desired_period <- 24
index<-which(chosen_periods%in% desired_period)
index
# Filter the results_list for the specific period

modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Inflation) period - ",desired_period), gof_map = NULL)


####################################################################### unemployment ###########################################################

equations <- list(c("group_ident_broad" ,"top_5_or_tier"),
                  c("group_ident_broad" ,"top_5_or_tier","I(top_5_or_tier*standarderror_winsor)"),
                  c("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q", "log(1 + journal_impact)", "log(1 + num_cit)"),
                  c("group_ident_broad" ,"top_5_or_tier", "cbanker","I(cbanker*standarderror_winsor)","pub_year", "main_research_q", "log(1 + journal_impact)", "log(1 + num_cit)"),                  
                  c("group_ident_broad" ,"top_5_or_tier", "transformation", "cum"),
                  c("group_ident_broad" ,"top_5_or_tier", "lrir", "fx", "foreignir", "inflexp", "eglob", "find", "outpgap", "comprice"),
                  c("group_ident_broad","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes" ,"top_5_or_tier"),
                  c("group_ident_broad","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes" ,"top_5_or_tier","convent","top_5_or_tier"))

results_list<-list()

for (i in 1:length(equations)) {
  model <- meta_analysis(data, outvar = "unemp", se_option = "avg", periods = chosen_periods,
                         wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equations[[i]])
  results_list[[paste0("equation.", i)]] <- model
}


desired_period <- 12
index<-which(chosen_periods%in% desired_period)
index
# Filter the results_list for the specific period

modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Unemployment) period - ",desired_period), gof_map = NULL)



##################################################################################  country test   ####################################################################################
###################################### Output

# Explanatory variables for each formula (excluding "standarderror_winsor")
equations <- list(c("group_ident_broad" ,"top_5_or_tier","us","ea12", "cbanker"),
                  # c("group_ident_broad" ,"top_5_or_tier","I(top_5_or_tier*standarderror_winsor)"),
                  c("group_ident_broad" ,"top_5_or_tier","us","ea12", "cbanker","pub_year", "main_research_q"),               
                  c("group_ident_broad" ,"top_5_or_tier","us","ea12", "cbanker", "transformed", "cum"),
                  c("group_ident_broad" ,"top_5_or_tier","us","ea12", "cbanker", "lrir", "fx", "foreignir", "inflexp", "eglob", "find", "outpgap", "comprice"),
                  c("group_ident_broad","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes" ,"top_5_or_tier","us","ea12", "cbanker"),
                  c("group_ident_broad","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes" ,"top_5_or_tier","us","ea12", "cbanker","convent"))


results_list<-list()

for (i in 1:length(equations)) {
  model <- meta_analysis(data, outvar = "output", se_option = "avg", periods = chosen_periods,
                         wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equations[[i]])
  results_list[[paste0("equation.", i)]] <- model
}


desired_period <- 12
index<-which(chosen_periods%in% desired_period)
index
# Filter the results_list for the specific period

modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Output) period - ",desired_period), gof_map = NULL)


### inflation

results_list<-list()

for (i in 1:length(equations)) {
  model <- meta_analysis(data, outvar = "inflation", se_option = "avg", periods = chosen_periods,
                         wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equations[[i]])
  results_list[[paste0("equation.", i)]] <- model
}


desired_period <- 24
index<-which(chosen_periods%in% desired_period)
index
# Filter the results_list for the specific period

modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Inflation) period - ",desired_period), gof_map = NULL)


######################################################################## paper impact test ####################################################

equations <- list(c("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q"),
                  # c("group_ident_broad" ,"top_5_or_tier","I(top_5_or_tier*standarderror_winsor)"),
                  c("group_ident_broad" ,"log(1 + journal_impact)", "cbanker","pub_year", "main_research_q"),               
                  c("group_ident_broad" ,"log(1 + num_cit)", "cbanker","pub_year", "main_research_q"),
                  c("group_ident_broad" ,"log(1 + num_cit)","log(1 + journal_impact)","top_5_or_tier", "cbanker","pub_year", "main_research_q"))


results_list<-list()

for (i in 1:length(equations)) {
  model <- meta_analysis(data, outvar = "output", se_option = "avg", periods = chosen_periods,
                         wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equations[[i]])
  results_list[[paste0("equation.", i)]] <- model
}


desired_period <- 12
index<-which(chosen_periods%in% desired_period)
index
# Filter the results_list for the specific period

modelsummary::modelsummary(lapply(results_list, `[[`, index), output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE (Output) period - ",desired_period), gof_map = NULL)



######################################################################### best practice response #######################################################################

data$transformation<-as.factor(data$transformation)
data <- within(data, transformation <- relevel(transformation, ref = 3))

# as factor transformed
data$transformed<-as.factor(data$transformed)


# Explanatory variables for each formula (excluding "standarderror_winsor")
equation <- list("group_ident_broad" ,"top_5_or_tier", "cbanker","pub_year", "main_research_q", "transformed", "cum", "lrir", "fx", "foreignir", "inflexp", "eglob", "find", "outpgap", "comprice","lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "dsge", "varother", "panel", "bayes","convent")

###Output

chosen_periods<- seq(3,60, by = 3)


model <- meta_analysis(data, outvar = "output", se_option = "avg", periods = chosen_periods,
                         wins = 0.02, prec_weighted = TRUE, estimation = "PEESE", cluster_se = TRUE, mods = equation)

modelsummary::modelsummary(model, output = "gt", stars = TRUE, conf_level = 0.80, title = paste0("PEESE output"), gof_map = NULL)


# define best practice here
best_pract<-c(1,0,1,0,0,0,1,0,0,1,0,0,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,1)


results <- list()

model[[20]]<-NULL

# Loop through each model in the list
for (i in seq_along(model)) {
  # Extract coefficients as a matrix
  betahat <- as.matrix(coef(model[[i]]))
  
  # Perform matrix multiplication
  result <- best_pract %*% betahat
  
  # Store the result in the list
  results[[i]] <- result
}

# Print or return the results as needed
print(results)


df_results <- do.call(rbind, lapply(results, as.data.frame))
row.names(df_results)<-seq(3,57, by = 3)




######################################################################################################################################################################################################
############################################################################ Background information for regressions ##################################################################################
######################################################################################################################################################################################################



######################################################################### calculate mean and sd for Reg variables ###################################################

# Combine all variable names from the list into a unique vector
all_vars <- unique(unlist(equations))

# Select all these variables from the data frame
selected_df <- data %>%
  select(all_of(all_vars),key,model_id,outcome,period.month) %>% filter(period.month %in% seq(0,48, by = 3) & outcome %in% c("inflation","output")) %>% select(-key,-model_id,-period.month)



selected_df<-fastDummies::dummy_cols(selected_df,remove_most_frequent_dummy = FALSE,remove_selected_columns = TRUE)


results <- selected_df %>%
  summarise(across(
    where(is.numeric),
    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))
  )) %>%
  # Convert the summarised data frame into a long format using the last underscore
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "_(?!.*_)",   # Use the last underscore for splitting
    values_to = "value"
  ) %>%
  # Spread the 'stat' column back out into 'mean' and 'sd'
  pivot_wider(
    names_from = stat,
    values_from = value
  )


print(results,n=35)



############################################################################ plot density for specific explanatory variables #########################################################################


data %>% dplyr::select(key,model_id,tradegl:exrate) %>% group_by(key,model_id) %>% summarise(across(tradegl:exrate, ~ mean(.x, na.rm = TRUE))) %>% mutate(infl=infl,gdppc=log(gdppc)) %>% 
  pivot_longer(cols = tradegl:exrate, names_to = "variable", values_to = "value") %>% ggplot( aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Plots for All Variables", x = "Value", y = "Density") +
  theme_minimal()

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data %>% dplyr::select(key,model_id,num_cit,pub_year,journal_impact) %>% mutate(num_cit=log(1+num_cit),journal_impact=log(1+journal_impact)) %>% group_by(key,model_id) %>% summarise(across(num_cit:journal_impact, ~ mean(.x, na.rm = TRUE)))  %>% 
  pivot_longer(cols = num_cit:journal_impact, names_to = "variable", values_to = "value") %>% ggplot( aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Plots for All Variables", x = "Value", y = "Density") +
  theme_minimal()



#########################################################################  correlation plot #########################################################################



data<-data %>% filter(outcome=="output" & period.month==12)

data<-data %>% group_by(period.month) %>% mutate(StandardError=(SE.upper+SE.lower)/2) %>%
  mutate(standarderror_winsor=winsorizor(StandardError, c(0.02), na.rm = TRUE)) %>%
  mutate(mean.effect_winsor=winsorizor(mean.effect, c(0.02), na.rm = TRUE)) %>% 
  mutate(z_stat=abs(mean.effect/StandardError)) %>% 
  mutate(z_stat_winsor=abs(mean.effect_winsor/standarderror_winsor)) %>% 
  mutate(x=1:length(StandardError) / 100) %>% ungroup()


# Select the relevant variables
selected_vars <- data %>%
  select(key,model_id,mean.effect_winsor, standarderror_winsor, group_ident_broad, lp, vecm, dyn_ols, fvar, tvar, gvar, dsge, varother, panel, bayes, decomposition, convent, lrir, fx, foreignir, inflexp, eglob, find, outpgap, comprice,main_research_q,mean_year, transformation, cum, cbanker, pub_year, top_5_or_tier,rate_pers)#  regime, upr, lor, hike, cut,pure_rate_shock, outcome_measure, model_id, rid1, tradegl, infl, fingl, findev, cbi, gdppc, exrate



# Log-transform variables directly in the selection (if not already log-transformed)
selected_vars <- selected_vars %>% select(-key,-model_id)
  # mutate(log_observations = log(observations)) %>%  #,
  #        log_infl = log(infl),
  #        log_gdppc = log(gdppc)) %>%
 # , -infl, -gdppc  # Remove original columns if transformed


selected_vars<-fastDummies::dummy_cols(selected_vars,remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)# maybe exclude -`country_dev_Mixed or Unclassified`

colnames(selected_vars)[1:2]<-c("est","se")

# Compute correlation matrix
cor_matrix <- cor(selected_vars, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Install and load the corrplot package if you haven't already






png("correlation_plot.png", width = 1500, height = 1500)  # Width and height in pixels

# Create the correlation plot
# Create a correlation plot
corrplot(cor_matrix, 
         method = "color",      # Color-coded correlation matrix
         type = "lower",        # Show only the upper triangular matrix
         tl.col = "black",      # Text label color
         tl.cex = 1.5,          # Text label size
         addCoef.col = "black", # Color of the correlation coefficients
         number.cex = 1,
         #order = "hclust"
) 

# Close the PNG device
dev.off()













###################################################################################################################################################################################
###################################################################################### vif and old code ###########################################################################
###################################################################################################################################################################################


# +fexch#+real_output # only for output regression

# currently not included: rate_mean.effect (due to missing data),country_dev, conf, intrest_rate, external control variables. (periodicity does not really make sense the way it currently looks, we would need to replace the log a values) (real output does not make sense currently, we would need to check the non real ones again.) (initial shock size needs to be adjusted - size)

# small<-mean.effect_winsor ~standarderror_winsor+group_ident_broad+lp+vecm+dyn_ols+fvar+tvar+gvar+dsge+varother+panel+bayes+regime+upr+lor+hike+cut+decomposition+convent+cbanker+is_top_tier+is_top_5+journal_impact+num_cit+main_research_q
# 
# small<-mean.effect_winsor ~standarderror_winsor+group_ident_broad+cbanker+is_top_tier+is_top_5+log(1+journal_impact)+log(1+num_cit)
# 
# #lp+vecm+dyn_ols+fvar+tvar+gvar+dsge+varother+panel+bayes+convent+journal_impact+num_cit+main_research_q einstweilen raus
# 
# #colSums(is.na(data_period_winsor %>% select(standarderror_winsor,group_ident_broad,lp,vecm,dyn_ols,fvar,tvar,gvar,dsge,varother,cbanker,is_top_tier,is_top_5)))
# 
# 
# 
# external<- mean.effect_winsor ~tradegl+log(infl)+fingl+findev+cbi+log(gdppc)+exrate



data<-data %>% filter(outcome=="output" & quality_concern!=1)


periods <- c(3, 6, 12, 18, 24, 30, 36)
wins<-0.02

results_list<-list()
coef_test_data<-list()
confint_data<-list()
vif_list <- list()


for (x in periods) {
  print(paste("Processing period:", x))

  # Subset data for the current period
  data_period <- subset(data, period.month %in% x)

  data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
  data_period$precision <- 1 / data_period$StandardError

  # Winsorize data
  data_period <- apply_winsorization(data_period, wins)
  # Store the winsorized data
  data_period$variance_winsor <- data_period$standarderror_winsor^2

  # Calculate PrecVariance winsorised
  data_period$precvariance_winsor <- 1 / data_period$variance_winsor
  
  selected_vars <- data_period %>%
    select(key,model_id,standarderror_winsor, group_ident_broad, lp, vecm, dyn_ols, fvar, tvar, gvar, dsge, varother, panel, bayes, regime, upr, lor, hike, cut, decomposition, convent, pure_rate_shock, lrir, fx, foreignir, inflexp, eglob, find, outpgap, comprice, month, main_research_q, prefer, mean_year, observations, ea12, us, upper_middle, n_of_countries, outcome_measure, transformation, cum, interest_rate_short, cbanker, pub_year, is_top_tier, is_top_5, journal_impact, num_cit, model_id, rid1, tradegl, infl, fingl, findev, cbi, gdppc, exrate)#  regime, upr, lor, hike, cut,pure_rate_shock, outcome_measure, model_id, rid1, tradegl, infl, fingl, findev, cbi, gdppc, exrate
  
  
  
  # Log-transform variables directly in the selection (if not already log-transformed)
  selected_vars <- selected_vars %>%
     select(-key,-model_id) %>% 
    mutate(log_observations = log(observations),
          log_infl = log(infl),
          log_gdppc = log(gdppc)) %>% select(-observations, -infl, -gdppc)
  #  # Remove original columns if transformed
  
  
  selected_vars<-fastDummies::dummy_cols(selected_vars,remove_most_frequent_dummy = TRUE,remove_selected_columns = TRUE)
  # Calculate VIF for each variable
  vif_values <- car::vif(lm(data=selected_vars))
  # Print the VIF values
  vif_list[[paste0(x)]] <- vif_values


  # Calculate (precision-weighted) average
  # regwa <- lm(equation, data = selected_vars, weights = precvariance_winsor)#
  # results_list[[paste0(x, ".ols")]] <- regwa

  # coef_test_data[[paste0(x, ".ols")]]<-coef_test(equation, vcov = "CR0",
  #                                                cluster = data_period$key, test = "naive-t")
  # 
  # 
  # confint_data[[paste0(x, ".ols")]]<-confint(equation, level=0.95)
}

#modelsummary(results_list, output = "gt",vcov = 'cr0', cluster = data_period_winsor$key,stars = TRUE) # only works if cluster variable has the same number of observations across lists.
#modelsummary(results_list, output = "gt",stars = TRUE)

create_modelsummary_list_vif <- function(vif_element) {
  ti <- data.frame(
    term = names(vif_element),
    estimate = unname(vif_element),
    std.error = rep(NA, length(unname(vif_element)))
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

modelsummary(mod_list)
#length(names(vif_list[[1]]))

coef_test_data<-data.table::rbindlist(coef_test_data, fill = T,idcol = ".id")
coef_test_data


# 
# 
# library(modelsummary)
# 
# # Define the list of periods and initialize results list
# periods <- c(3, 6, 12, 18, 24, 30, 36)
# results_list <- list()
# 
# # Define the equations
# equations <- list(
#   small = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + decomposition + cbanker + is_top_tier + is_top_5 + pub_year,
#   small_alt = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + decomposition + cbanker + is_top_tier + is_top_5 + pub_year + convent + main_research_q,
#   small2 = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + decomposition + cbanker + log(1 + journal_impact) + log(1 + num_cit) + pub_year,
#   small_check = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + decomposition + cbanker + is_top_tier + is_top_5 + pub_year + transformation + cum,
#   ex = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + decomposition + cbanker + is_top_tier + is_top_5 + pub_year + tradegl + log(infl) + fingl + findev + cbi + log(gdppc) + exrate,
#   int = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + decomposition + cbanker + is_top_tier + is_top_5 + pub_year + lrir + fx + foreignir + inflexp + eglob + find + outpgap + comprice,
#   big = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + lp + vecm + dyn_ols + fvar + tvar + gvar + dsge + varother + panel + bayes + decomposition + cbanker + is_top_tier + is_top_5 + pub_year,
#   large = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + lp + vecm + dyn_ols + fvar + tvar + gvar + dsge + varother + panel + bayes + upr + lor + hike + cut + decomposition + cbanker + is_top_tier + is_top_5 + pub_year,
#   large_alt = mean.effect_winsor ~ standarderror_winsor + group_ident_broad + lp + vecm + dyn_ols + fvar + tvar + gvar + dsge + varother + panel + bayes + upr + lor + hike + cut + decomposition + cbanker + is_top_tier + is_top_5 + pub_year + convent + main_research_q
# )
# 
# # Loop over periods and equations
# for (x in periods) {
#   print(paste("Processing period:", x))
#   
#   # Subset data for the current period
#   data_period <- subset(data, period.month %in% x)
#   
#   data_period$StandardError <- (data_period$SE.upper + data_period$SE.lower) / 2
#   data_period$precision <- 1 / data_period$StandardError
#   
#   # Winsorize data
#   data_period_winsor <- data_period
#   data_period_winsor$standarderror_winsor <- winsorizor(data_period$StandardError, c(0.02), na.rm = TRUE)
#   data_period_winsor$mean.effect_winsor <- winsorizor(data_period$mean.effect, c(0.02), na.rm = TRUE)
#   data_period_winsor$precision_winsor <- 1 / data_period_winsor$standarderror_winsor
#   
#   # Calculate variance winsorised
#   data_period_winsor$variance_winsor <- data_period_winsor$standarderror_winsor^2
#   
#   # Calculate PrecVariance winsorised
#   data_period_winsor$precvariance_winsor <- 1 / data_period_winsor$variance_winsor
#   
#   # Fit models for each equation
#   for (name in names(equations)) {
#     formula <- equations[[name]]
#     model <- lm(formula, data = data_period_winsor, weights = precvariance_winsor)
#     results_list[[paste0(x, ".", name)]] <- model
#   }
# }
# 
# # Define the period you want to filter for
# desired_period <- 24
# 
# # Filter the results_list for the specific period
# filtered_results <- results_list[grep(paste0("^", desired_period, "\\."), names(results_list))]
# 
# # Summarize the filtered models
# modelsummary(filtered_results, output = "gt", stars = TRUE)
# 
# 
# 
# 
# sum(data$decomposition,na.rm = FALSE)
