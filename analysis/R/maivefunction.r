# R code for MAIVE
# input as excel file:
#       estimates: bs
#       standard errors: sebs
#       number of observations: Ns
#       optional: study_id
#
# default option for MAIVE: MAIVE-PET-PEESE, unweighted, instrumented
#
# choices:
#       method= 1 FAT-PET, 2 PEESE, 3 PET-PEESE, 4 EK 
#       weighting = 0 no weights, 1 standard weights, 2 adjusted weights  
#       instrumenting = 1 yes, 0 no 
#       correlation at study level: 0 none, 1 fixed effects, 2 cluster
#       Anderson Rubin corrected confidence interval for weak instruments (only for unweighted MAIVE versions of PET, PEESE and PET-PEESE, not available for fixed effects): 0 no, 1 yes
#
# standard estimator: same option as for MAIVE but weighted by inverse variance and not instrumented     
#
# output:
#       MAIVE meta-estimate and standard error
#       Hausman type test: comparison between MAIVE and standard version
#       when instrumenting: heteroskedastic robust F-test of the first step
#       instrumented standard errors 


maive <- function(dat=dat,method=method,weight=weight,instrument=instrument,studylevel=studylevel,AR=AR) {

  if (!require('varhandle')) install.packages('varhandle'); library('varhandle')
  if (!require('pracma')) install.packages('pracma'); library('pracma')
  if (!require('sandwich')) install.packages('sandwich'); library('sandwich')
  
  methods <- c("PET","PEESE","PET-PEESE","EK")                              
  instrumented <- c("not instrumented","instrumented")                      
  weighted <- c("no weights","standardly weighted", "adjusted weights")     
  studylevelcorrelation <- c("none","study level dummies", "cluster")         

  if (studylevel==0){
    cluster<-0
    dummy<-0
  } else if (studylevel==1){
    cluster<-0
    dummy<-1
  } else if (studylevel==2) {
    cluster<-1
    dummy<-0
  }
  
  # AR not available for EK, for fixed effects, and for weighted
  if (method==4|studylevel==1|weight==1|weight==2|instrument==0) { 
    AR<-0
  }
  
  # extracting data from excel  
  dat = as.data.frame(dat)
  bs<-dat[,1]
  M<-length(bs)
  sebs<-dat[,2]
  Ns<-dat[,3]
  
  if (dim(dat)[2]==4){
    studyid<-dat[,4]
  } else {
    studyid<-(1:M)
    dummy<-0
    cluster<-0
  }  
    
  alpha_s <- 0.05 
  
  # create Dummies from studyid
    df<-data.frame(studyid)
    D <- to.dummy(df,"studyid")
    D <-D-matrix(colMeans(D),nrow=M,ncol=size(D)[2], byrow = TRUE)
    D <- D[,1:(dim(D)[2]-1)]
  
  # g=studyid if clustered and g=(1:M)' if not clustered (gives heteroskedastic robust SE)
    if (cluster==0){           
      g <- (1:M)    
    } else if (cluster==1){    
      g <- studyid           
    }

# (1) Instrumenting the variances with sample size allowing for a constant and including dummies
    invNs<- 1/Ns
    sebs2<- sebs^2
    Xiv<-matrix(c(ones(M,1)[,1],invNs),nrow=M)
    varreg1 <- lm(sebs2~ 0+Xiv) 
    dimiv<-2
  if (varreg1$coefficients[1]<0){
     Xiv<-invNs
     varreg1 <- lm(sebs2~ 0+Xiv) 
     dimiv<-1
  }                                           

  sebs2fit1 <- varreg1$fitted.values         

  # F-statistic of first step. heteroskedasticity and autocorrelation robust variance HAC
  F_hac <- (varreg1$coefficients[dimiv]^2 /vcovCL(varreg1, cluster = g)[dimiv,dimiv]) 

  #weight                                   
  if (weight==0){
      w <- ones(M,1)[,1]
  } else if (weight==1){
      w <- sebs       
  } else if (weight==2){
      w <- sebs2fit1^(1/2)
  }             
              
  #instrument
  if (instrument==0){
      x <- sebs      
      x2 <- sebs^2 
      F_hac <-"NA"
  } else if (instrument==1){
      x <- sebs2fit1^(1/2)  
      x2 <- sebs2fit1
      F_hac<-round(F_hac,3)
  }                          
                           
  #choose dependent variable and regressor
    y <- bs/w 
    x <- x 
    x2 <- x2
    X <- matrix(c(ones(M,1)[,1], x)/w,nrow=M)     
    X_d <- matrix(c(X, D/w), nrow=M)
    X2 <- matrix(c(ones(M,1)[,1], x2)/w,nrow=M)
    X2_d <- matrix(c(X2, D/w), nrow=M)         
  
  # baseline, i.e. chosen method, with chosen options of study-level correlation
  #  but with inverse-variance weighting and without instrumenting  
    y0 <- bs/sebs                                                               
    x0 <- sebs                                                                  
    x20 <- sebs ^2
    X0 <- matrix(c(ones(M,1)[,1], x0)/sebs, nrow=M)
    X0_d <- matrix(c(X0, D/sebs), nrow=M)          
    X20 <- matrix(c(ones(M,1)[,1], x20)/sebs, nrow=M)
    X20_d <- matrix(c(X20, D/sebs),  nrow=M)         
  
    if (dummy==0){                                            
      X <- X
      X0 <- X0
      X2 <- X2                                              
      X20 <- X20                                            
      cD <- ones(M,1)[,1]     
    } else if (dummy==1){                                          
      X <- X_d   
      X0 <- X0_d
      X2 <- X2_d                                                 
      X20 <- X20_d                                               
      cD <- matrix(c(ones(M,1)[,1],D),nrow=M)  # for EK stack constant and dummies         
    }
  
    cD<- cD/w                                                      
    cD0<- cD/sebs

    ones_w<-ones(M,1)[,1]/w
    ones_w0<-ones(M,1)[,1]/sebs                                                                   
                                                                     
  # Fixed effects (FE)                                                                 
    wis0 <- 1/(w^2)                                                      
    fe <- sum(bs*wis0)/sum(wis0)                                         
    varfe <- 1/sum(wis0) 
  # baseline
    wis00 <- 1/(sebs^2)                                                      
    fe0 <- sum(bs*wis00)/sum(wis00)                                         
    varfe0 <- 1/sum(wis00)
                                                                
  # WLS                                                              
    wlsreg <-lm(y~ 0+ cD )
    wls <- wlsreg$coefficients[1]
    wlsse <- sqrt(vcovCL(wlsreg,cluster=g)[1,1])
  # baseline 
    wlsreg0 <-lm(y0~ 0+ cD0 )
    wls0 <- wlsreg0$coefficients[1]
    wlsse0 <- sqrt(vcovCL(wlsreg0, cluster=g)[1,1])
                                                                                    
  # FAT-PET - MAIVE                                         
    fatpet <- lm(y~ 0+X) 
  # FAT-PET - baseline case
    fatpet0 <- lm(y0~ 0+X0) 

  # PEESE - MAIVE
    peese <- lm(y~0+X2)
  # PEESE - baseline case
    peese0 <- lm(y0~0+X20)
                                     
  # PET-PEESE - MAIVE                         
    if (abs(fatpet$coefficients[1]/sqrt(vcovCL(fatpet,cluster=g)[1,1]))>qt(1-alpha_s/2,M-dim(X)[2]-1)){
       petpeese <- peese                                                                             
    } else {                                                                                          
        petpeese <- fatpet                                                                            
    }                                                                                                 
  # PET-PEESE - baseline case                        
    if (abs(fatpet0$coefficients[1]/sqrt(vcovCL(fatpet0,cluster=g)[1,1]))>qt(1-alpha_s/2,M-dim(X0)[2]-1)){
        petpeese0 <- peese0
    } else {
        petpeese0 <- fatpet0
    }
                      
  # True effect variance - MAIVE
    Qfe0 <- sum(wlsreg$residuals*wlsreg$residuals)
    sigh2hat0 <- max(0,M*((Qfe0/(M-dim(wlsreg$model)[2]-1))-1)/sum(wis0))           
    sighhat0 <- sqrt(sigh2hat0)
  #True effect variance - baseline                      
    Qfe00 <- sum(wlsreg0$residuals*wlsreg0$residuals)
    sigh2hat00 <- max(0,M*((Qfe00/(M-dim(wlsreg0$model)[2]-1))-1)/sum(wis00))        
    sighhat00 <- sqrt(sigh2hat00)

  # Endogenous Kink (EK) Threshold- MAIVE
    if (petpeese$coefficients[1] > 1.96*sighhat0){    
        a0 <- (petpeese$coefficients[1]-1.96*sighhat0)*(petpeese$coefficients[1]+1.96*sighhat0)/(2*1.96*petpeese$coefficients[1])
    } else {   
        a0 <- 0
    }
  # Endogenous Kink (EK) Threshold - baseline
    if (petpeese0$coefficients[1] > 1.96*sighhat00){    
        a00 <- (petpeese0$coefficients[1]-1.96*sighhat00)*(petpeese0$coefficients[1]+1.96*sighhat00)/(2*1.96*petpeese0$coefficients[1])
    } else {   
        a00 <- 0
    }

  # EK - MAIVE                                      
    if (a0>min(x)  && a0<max(x)){ 
      xx_w=(x-a0)*(x>a0)/w
      ekreg <- lm(y~ 0+cD+xx_w)
    } else if (a0<min(x)){
      x_w=x/w
      ekreg <- lm(y~ 0+cD+x_w)
    } else if (a0>max(x)){
      ekreg <- lm(y~ 0+cD)
    }
    ek <- ekreg$coefficients[1]
    
  # EK - baseline
    if (a00>min(x0)  && a00<max(x0)){ 
      xx0_w=(x0-a00)*(x0>a00)/sebs
      ekreg0 <- lm(y0~ 0+cD0+xx0_w)
    } else if (a00<min(x0)){
      x0_w=x0/sebs
      ekreg0 <- lm(y0~ 0+cD0+x0_w )
    } else if (a00>max(x0)){
      ekreg0 <- lm(y0~ 0+cD0 )
    }
    ek0 <- ekreg0$coefficients[1]
    
  
  # Anderson and Rubin Confidence intervals     
  if (AR==1) {
    
  # PET without weights and with AR CI
    beta0=fatpet$coefficients[1]
    beta0se=sqrt(vcovCL(fatpet,cluster=g)[1,1])
    l0=beta0-5*beta0se
    u0=beta0+5*beta0se
    pr0=max(100,round(100/(u0-l0)))
    
    beta1=fatpet$coefficients[2]
    beta1se=sqrt(vcovCL(fatpet,cluster=g)[2,2])
    l1=beta1-5*beta1se
    u1=beta1+5*beta1se
    pr1=max(100,round(100/(u1-l1)))
    
    b0_long <- seq(l0,  u0, by=1/pr0)
    b1_long <- seq(l1,  u1, by=1/pr1)
    
    AR_0<-matrix(0,ncol=length(b1_long),  nrow=length(b0_long))  
    AR_1<-matrix(0,ncol=length(b1_long),  nrow=length(b0_long))  
  
    b0n<-0  
    for (b0 in b0_long) {
      b0n=b0n+1
      
      b1n<-0
    for (b1 in b1_long) {
      b1n=b1n+1
      bs_star<- bs-b0-b1*sebs
      model <- lm(bs_star~1+invNs) 
      
      # AR test statistic 
      Z<-matrix(c(ones(M,1)[,1], invNs), nrow=M)
      PZ=Z%*%inv(t(Z)%*%Z)%*%t(Z)
      MZ=diag(M)-PZ
      AR_0[b0n,b1n]=(M-2)*(t(bs_star)%*%PZ%*%bs_star)/(bs_star%*%MZ%*%bs_star)
      AR_1[b0n,b1n]=(AR_0[b0n,b1n]<5.99)
    }
  }
    
    b0_CI_AR_all <- b0_long[rowSums(AR_1) > 0]
    b1_CI_AR_all <- b1_long[colSums(AR_1) > 0]
    
    b0_CI_AR_PET <- c(b0_CI_AR_all[1], b0_CI_AR_all[length(b0_CI_AR_all)])
    b1_CI_AR_PET <- c(b1_CI_AR_all[1], b1_CI_AR_all[length(b1_CI_AR_all)])
    b0_CI_AR_PET=round(b0_CI_AR_PET,3)
  
  # PEESE without weights and with AR CI
    beta0=peese$coefficients[1]
    beta0se=sqrt(vcovCL(peese,cluster=g)[1,1])
    l0=beta0-5*beta0se
    u0=beta0+5*beta0se
    pr0=max(100,round(100/(u0-l0)))
      
    beta1=peese$coefficients[2]
    beta1se=sqrt(vcovCL(peese,cluster=g)[2,2])
    l1=beta1-5*beta1se
    u1=beta1+5*beta1se
    pr1=max(100,round(100/(u1-l1)))
    
    b0_long <- seq(l0,  u0, by=1/pr0)
    b1_long <- seq(l1,  u1, by=1/pr1)
    
    AR_0<-matrix(0,ncol=length(b1_long),  nrow=length(b0_long))  
    AR_1<-matrix(0,ncol=length(b1_long),  nrow=length(b0_long))  
      
    b0n<-0  
    for (b0 in b0_long) {
      b0n=b0n+1
      b1n<-0
    
    for (b1 in b1_long) {
      b1n=b1n+1
      bs_star<-bs-b0-b1*sebs^2
      model <- lm(bs_star~1+invNs) 
      
      # AR test statistic 
      Z<-matrix(c(ones(M,1)[,1], invNs), nrow=M)
      PZ=Z%*%inv(t(Z)%*%Z)%*%t(Z)
      MZ=diag(M)-PZ
      AR_0[b0n,b1n]=(M-2)*(t(bs_star)%*%PZ%*%bs_star)/(bs_star%*%MZ%*%bs_star)
      AR_1[b0n,b1n]=(AR_0[b0n,b1n]<5.99)
      
    }
  }
    
    b0_CI_AR_all <- b0_long[rowSums(AR_1) > 0]
    b1_CI_AR_all <- b1_long[colSums(AR_1) > 0]
    
    b0_CI_AR_PEESE <- c(b0_CI_AR_all[1], b0_CI_AR_all[length(b0_CI_AR_all)])
    b1_CI_AR_PEESE <- c(b1_CI_AR_all[1], b1_CI_AR_all[length(b1_CI_AR_all)])
    b0_CI_AR_PEESE=round(b0_CI_AR_PEESE,3)
     
    
    # PET-PEESE without weights and with AR CI
    # if PET does not contain 0, do PEESE. 
    if (b0_CI_AR_PET[1]>0) {
      b0_CI_AR_PP<-b0_CI_AR_PEESE 
    } else if (b0_CI_AR_PET[1]<=0) {
      b0_CI_AR_PP<-b0_CI_AR_PET
    }
  } else if (AR==0) {
    b0_CI_AR_PET= "NA"
    b0_CI_AR_PEESE= "NA"
    b0_CI_AR_PP= "NA"
  }  
    
  "RESULTS"
  if (method==1){
    "MAIVE-FAT-PET"
    beta=fatpet$coefficients[1]
    betase=sqrt(vcovCL(fatpet,cluster=g)[1,1])
    "Standard FAT-PET"
    beta0=fatpet0$coefficients[1]
    beta0se=sqrt(vcovCL(fatpet0,cluster=g)[1,1])
    "Hausman-type test"
    Hausman=(fatpet$coefficients[1]-fatpet0$coefficients[1])^2/(vcovCL(fatpet,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
    "AR-CI"
    b0_CI_AR=b0_CI_AR_PET
  } else if (method==2){       
    "MAIVE-PEESE"                  
    beta=peese$coefficients[1]    
    betase=sqrt(vcovCL(peese,cluster=g)[1,1]) 
    "Standard PEESE"
    beta0=peese0$coefficients[1]    
    beta0se=sqrt(vcovCL(peese0,cluster=g)[1,1])
    "Hausman-type test"
    Hausman=(peese$coefficients[1]-peese0$coefficients[1])^2/(vcovCL(peese,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
    "AR-CI"
    b0_CI_AR=b0_CI_AR_PEESE
  } else if (method==3){
    "MAIVE-PET-PEESE"                  
    beta=petpeese$coefficients[1]
    betase=sqrt(vcovCL(petpeese,cluster=g)[1,1])
    "Standard PET-PEESE"                  
    beta0=petpeese0$coefficients[1]
    beta0se=sqrt(vcovCL(petpeese0,cluster=g)[1,1])
    "Hausman-type test"
    Hausman=(petpeese$coefficients[1]-petpeese0$coefficients[1])^2/(vcovCL(petpeese,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
    "AR-CI"
    b0_CI_AR=b0_CI_AR_PP
  } else if (method==4){         
    "MAIVE-EK"                       
    beta=ekreg$coefficients[1]      
    betase=sqrt(vcovCL(ekreg,cluster=g)[1,1])   
    "Standard EK"                       
    beta0=ekreg0$coefficients[1]      
    beta0se=sqrt(vcovCL(ekreg0,cluster=g)[1,1])
    "Hausman-type test" # with variance of MAIVE in denominator (instead of the difference) hence is conservative
    Hausman=(ekreg$coefficients[1]-ekreg0$coefficients[1])^2/(vcovCL(ekreg,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
    "AR-CI"
    b0_CI_AR="NA"
  } 
  
  if (studylevel==1){
    "AR-CI"
    b0_CI_AR="NA"
  }
  
  if (weight==1||weight==2){
    "AR-CI"
    b0_CI_AR="NA"
  }

my_list <- list("beta"=round(beta,3), "SE"=round(betase,3),"F-test"=F_hac,"beta_standard"=round(beta0,3),"SE_standard"=round(beta0se,3),"Hausman"=round(Hausman,3), "Chi2"=round(Chi2,3), "SE_instrumented"=sebs2fit1^(1/2), "AR_CI"=b0_CI_AR)
return(my_list)
}

