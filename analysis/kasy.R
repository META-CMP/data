rm(list = ls())
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
source("data/data_prep.R")
source("analysis//MetaStudiesApp-master/metastudiesfunctions.r")
source("analysis//MetaStudiesApp-master/metastudiesplots.r")


# define outfar and horizon 
prd<-12
outvar<-"gdp"

#filter data
data_filtered <- data %>% filter(outcome==outvar & period.month==prd)

#create list for kasy test and plots
v = list()
metadata=as.data.frame(data_filtered %>% select(mean.effect,SE.upper,key))
v$X=metadata[,1]
v$sigma=metadata[,2]

#### Plots
# Kasy metastudies plot
metastudies_plot(v$X,v$sigma)

# Kasy histogram
z_histogram(v$X, v$sigma)


########## Estimation
#### results are not the same as in the paper but the same as in the app
#### standard errors do not seem to be clustered during the estimation. 

# DEFINE settings for estimation
cutoffs<-c(1,1.960)#(1,1.645, 1.960, 2.576)
symmetric<-FALSE#TRUE
modelmu<-"normal"#"t"

# set elements in list to defined values 
v$cutoffs=as.numeric(unlist(cutoffs))
v$symmetric=symmetric
v$modelmu=modelmu

# if non-symetric set cut offs to - x, 0, and + x
if (!v$symmetric) v$cutoffs= c(-rev(v$cutoffs), 0 ,v$cutoffs)

# Estimate Kasy test staistics
v$estimates=metastudies_estimation(v$X,v$sigma,v$cutoffs, v$symmetric, model= v$modelmu)

# Plot Kasy test statistics
estimates_plot(v$X,v$sigma,v$cutoffs, v$symmetric,v$estimates, model= v$modelmu)

# Print Kasy test staistics
estimatestable(v$estimates$Psihat, v$estimates$SE, v$cutoffs, v$symmetric, v$modelmu)
