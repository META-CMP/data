rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


setwd("~/data")
#Load data by running data_prep script
source("analysis/data_prep.R")


data_back<-data

# load packages
library(tidyverse)# for data manipulation
library(dplyr)# for data manipulation
library(ggplot2) # for graphs
library(JWileymisc) # for Winsorization
library(gridExtra) # to make plot grids 
library(grid) # to make plot grids 


########################################## Functions

# Function to create funnel plots
create_funnel_plot <- function(data, period, winsorize = FALSE) {
  if (winsorize) {
    plot_title <- "(with Winsorization)"
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
    xlab(paste0("Mean effect size\n %-change of", outcome,"in response to 100bp monetary policy shock")) +
    ylab("Inverse of standard error (precision)") +
    ggtitle(paste("Funnel plots of effect of monetary policy on", outcome,"\n", "(", period, "months after the shock)", plot_title)) +
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

out<-'inflation'#c(gdp, inflation)
outcome<-"the price level" # c("output", "the price level")
data <- subset(data, outcome %in% out)

# out_measure<-'emp' #c(emp,une_rate)
# outcome<-"employment" #c(unemployment, employment)
# data <- subset(data, outcome_measure %in% out_measure)


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
  #data_period$StandardError <- ifelse(data_period$StandardError == 0, NA, data_period$StandardError)
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

# create funnel plots for non-winsorized data 
all_month<-grid.arrange(grobs = plot_list, ncol = 4,top = textGrob(paste0("Funnel plots of effect of monetary policy on ", outcome),gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob(paste0("%-change of ", outcome," in response to 100bp monetary policy shock"),gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months.png"), all_month, width = 30, height = 20, units = "cm")


# create funnel plots for winsorized data 
all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob(paste0("Funnel plots of effect of monetary policy on ", outcome," (with Winsorization)"),gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob(paste0("%-change of ", outcome," in response to 100bp monetary policy shock"),gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/funnel_plot_all_months_winsor.png"), all_month_winsor, width = 30, height = 20, units = "cm")