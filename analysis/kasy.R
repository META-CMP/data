rm(list = ls())
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
load("data/preliminary_data_test.RData")
source("analysis//MetaStudiesApp-master/metastudiesfunctions.r")
source("analysis//MetaStudiesApp-master/metastudiesplots.r")


# define outfar and horizon 
prd<-24
outvar<-"output"

#filter data
data_filtered <- data %>% filter(outcome==outvar & period.month==prd & !quality_concern)




#create list for kasy test and plots
v = list()
metadata=as.data.frame(data_filtered %>% select(mean.effect,SE.upper,key))
v$X=metadata[,1]
v$sigma=metadata[,2]

#### Plots
# Kasy metastudies plot
#metastudies_plot(v$X,v$sigma)

# Kasy histogram
#z_histogram(v$X, v$sigma)


########## Estimation
#### results are not the same as in the paper but the same as in the app
#### standard errors do not seem to be clustered during the estimation. 

# DEFINE settings for estimation
cutoffs<-c(1)#(1,1.645, 1.960, 2.576)
symmetric<-FALSE#TRUE
modelmu<-"t"#"t"

# set elements in list to defined values 
v$cutoffs=as.numeric(unlist(cutoffs))
v$symmetric=symmetric
v$modelmu=modelmu

# if non-symetric set cut offs to - x, 0, and + x
if (!v$symmetric) v$cutoffs= c(-rev(v$cutoffs), 0 ,v$cutoffs)

# Estimate Kasy test staistics
v$estimates=metastudies_estimation(v$X,v$sigma,v$cutoffs, v$symmetric, model= v$modelmu)


estimates_plot<-function(cutoffs, symmetric, estimates, model="normal"){
  
  n=500
  Psihat=estimates$Psihat
  rangeZ=3
  dens=data.frame(z=seq(-rangeZ,rangeZ,length.out =n))
  shift=as.integer(model=="t")
  
  Tpowers=Tpowers_fun(dens$z,cutoffs,symmetric)
  betap=as.vector(c(Psihat[-(1:(2+shift))],  1))
  dens$p=Tpowers%*%betap
  
  if (model=="t") df=Psihat[3]
  else df=Inf
  
  dens$f=dt(((dens$z - Psihat[1])/ Psihat[2]), df=df)/Psihat[2]
  names(dens)[names(dens) == 'f'] <- 'density of true effect'
  names(dens)[names(dens) == 'p'] <- 'publication probability'
  dens<-dens[,-3]
  
  dens=melt(dens, id="z")
  ggplot(dens, aes(x=z, y=value)) +
    xlab("Z")+
    ylab("")+
    geom_line(size=2, color="blue") +
    facet_grid(variable ~ .,  scales = "free_y") +
    expand_limits(y = 0) +
    ggtitle(paste0(prd," months"))+
    scale_x_continuous(breaks =-3:3) +
    theme(#panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "grey95", colour = NA))
  
  
}

# Plot Kasy test statistics
p<-estimates_plot(v$cutoffs, v$symmetric,v$estimates, model= v$modelmu)

ggsave(filename = paste0("kasy ",prd, ".png"), plot = p, bg = "white", width = 4, height = 4, dpi = 150)

# Print Kasy test staistics
estimatestable(v$estimates$Psihat, v$estimates$SE, v$cutoffs, v$symmetric, v$modelmu)
