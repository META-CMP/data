# This code is based on code provided by Maximilian Kasy (https://github.com/maxkasy/MetaStudiesApp/blob/master/metastudiesplots.r). 
# We customized his code for our specific purposes.

library(ggplot2)
library(reshape2)

#for drawing
critval=1.96

# Funnel plot function
metastudies_plot <- function(X, sigma, critvals){
  n = length(X)
  smallest_critval <- min(critvals)
  significant <- (abs(X/sigma) > smallest_critval)
  nooutlier = (sigma < 30*mean(sigma)) & (abs(X) < 30*mean(abs(X)))
  dat <- data.frame(X, sigma, as.factor(significant & nooutlier))
  names(dat) = c("xvar", "yvar", "significant")
  
  # Calculate range based on data
  rangeX = 1.1 * max(abs(X[nooutlier]))
  rangeY = 1.1 * max(sigma[nooutlier])
  
  dat <- dat[order(dat$significant),]
  
  p <- ggplot(dat, aes(x=xvar, y=yvar)) +
    xlab("X") +
    ylab(expression(sigma))
  
  # Add lines for each critical value
  for(crit in critvals) {
    p <- p + 
      geom_abline(intercept = 0, slope = 1/crit, color="grey", linetype="dashed") + 
      geom_abline(intercept = 0, slope = -1/crit, color="grey", linetype="dashed")
  }
  
  p <- p +
    geom_point(size = 4, aes(colour = significant,
                             fill = significant), alpha=min(.8, max(40/n, .3))) + 
    scale_colour_manual(values=c("grey50", "blue")) +
    scale_x_continuous(expand = c(0,0), limits = c(-rangeX, rangeX)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, rangeY)) +
    theme(legend.position="top",
          panel.background = element_rect(fill = "grey95", colour = NA))
  
  return(p)
}

z_histogram=function(X,sigma){
  Z=X/sigma
  n=length(Z)
  ll=floor(min(Z));
  uu=ceiling(max(Z));
  
    if (n>=30) {
      uu2<-ceiling(max((uu-.36)/.32,0))*.32+.36;
      ll2<-floor(min((ll+.36)/.32,0))*.32-.36;
      edges<-c(seq(from=ll2,
                   to=-0.36,
                   by=0.32), 0, seq(from=0.36,
                                    to=uu2,
                                    by=0.32));
    } else {
      uu2<-ceiling(max((uu-.68)/.64,0))*.64+.68;
      ll2<-floor(min((ll+.68)/.64,0))*.64-.68;
      edges<-c(seq(from=ll2,
                   to=-0.68,
                   by=0.64), 0, seq(from=0.68,
                                    to=uu2,
                                    by=0.64));
    }

  
  ggplot(data = as.data.frame(Z), aes(Z))+
    geom_histogram(aes(y = ..density..),
                   fill = 'blue',
                   breaks=edges)+
    geom_vline(xintercept =-1.96,color='grey')+
    geom_vline(xintercept =1.96, color='grey')+
    geom_vline(xintercept =-1,color='red')+
    geom_vline(xintercept =1,color='red')+
    xlab('Z')+
    ylab('Density')+
    xlim(c(min(edges)/4,max(edges)/2))+
    theme(#panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey95", colour = NA))
  
}


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

  dens=melt(dens, id="z")
  ggplot(dens, aes(x=z, y=value)) +
    xlab(paste("Z, ", intToUtf8(952)))+
    geom_line(size=2, color="blue") +
    facet_grid(variable ~ .,  scales = "free_y") +
    expand_limits(y = 0) +
    scale_x_continuous(breaks =-3:3) +
    theme(#panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "grey95", colour = NA))


}

# Plot only with 'publication probability'
estimates_plot_prob <- function(cutoffs, symmetric, estimates, model="normal", y_range=c(0,40), log_scale = FALSE){
  n = 500
  Psihat = estimates$Psihat
  rangeZ = 3
  dens = data.frame(z = seq(-rangeZ, rangeZ, length.out = n))
  shift = as.integer(model == "t")
  Tpowers = Tpowers_fun(dens$z, cutoffs, symmetric)
  betap = as.vector(c(Psihat[-(1:(2+shift))],  1))
  dens$p = Tpowers%*%betap
  names(dens)[names(dens) == 'p'] <- 'publication probability'
  
  p <- ggplot(dens, aes(x = z, y = `publication probability`)) +
    xlab(latex2exp::TeX("$z$")) +
    geom_line(size = 2, color = "blue") +
    # expand_limits(y = 0) +
    scale_x_continuous(breaks = -3:3) +
    theme(panel.background = element_rect(fill = "grey95", colour = NA))
  
  if (log_scale == TRUE) {
    p <- p + ylab("publication probability (log scale)") + scale_y_log10(limits = y_range)
  } else {
    p <- p + ylab("publication probability") + scale_y_continuous(limits = y_range)
  }
  
  p
  
}

# estimates_plot <- function(estimates, cutoffs, symmetric, model="normal") {
#   n <- 500
#   rangeZ <- 3
#   dens <- data.frame(z = seq(-rangeZ, rangeZ, length.out = n))
#   
#   Psihat <- estimates$Psihat
#   shift <- as.integer(model == "t")
#   
#   # Calculate p(z)
#   Tpowers <- Tpowers_fun(dens$z, cutoffs, symmetric)
#   betap <- as.vector(c(Psihat[-(1:(2+shift))], 1))
#   dens$p <- as.vector(Tpowers %*% betap)
#   
#   # Calculate f(z)
#   if (model == "t") {
#     df <- Psihat[3]
#     dens$f <- dt(((dens$z - Psihat[1]) / Psihat[2]), df = df) / Psihat[2]
#   } else {
#     dens$f <- dnorm(dens$z, mean = Psihat[1], sd = Psihat[2])
#   }
#   
#   # Create plotly traces
#   trace_p <- plot_ly(dens, x = ~z, y = ~p, type = 'scatter', mode = 'lines', name = 'Publication probability')
#   trace_f <- plot_ly(dens, x = ~z, y = ~f, type = 'scatter', mode = 'lines', name = 'Density of true effect')
#   
#   # Combine traces
#   fig <- subplot(trace_p, trace_f, nrows = 2, shareX = TRUE) %>%
#     layout(xaxis = list(title = "Z"),
#            yaxis = list(title = "Probability"),
#            yaxis2 = list(title = "Density"))
#   
#   return(fig)
# }