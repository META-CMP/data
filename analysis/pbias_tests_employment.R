setwd("~/data")

load("preliminary_data.RData")


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


split_list <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

data$real_output<-ifelse(split_list %in% c("rgdp","ip","rip","rgnp","rgap"),TRUE,NA)
data$real_output<-ifelse(split_list %in% c("gdp","gnp","gap"),FALSE,data$real_output)

# remove r from the outcome measure
split_list<-sub("^r", "", split_list)

data$outcome_measure<-split_list

data$outcome<-ifelse(data$outcome_measure %in% c("gdp","ip","gap","gnp"),"gdp",ifelse(data$outcome_measure %in% c("cpi","deflator","wpi","core","price_level"),"inflation","emp"))

unique(data$outcome_measure)

# check which outcome measures are how often available. 
data %>% group_by(outcome_measure) %>% summarise(count=n())

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
    xlab("Mean effect size\n %-change of employment in response to 100bp monetary policy shock") +
    ylab("Inverse of standard error (precision)") +
    ggtitle(paste("Funnel plots of effect of monetary policy on employment\n", "(", period, "months after the shock)", plot_title)) +
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




periods <- c(3, 6, 12, 18, 24, 30, 36, 48)

out<-'emp'
out_measure<-'emp'
data <- subset(data, outcome_measure %in% out_measure)

unique(data$period.month)


plot_list <- list()
plot_list_winsor <- list()

# Loop through periods and create plots
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
  ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_", x, "_months.png"), plot_funnel)
  plot_funnel <- create_funnel_plot_grid(data_period, x)
  plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)
all_month<-grid.arrange(grobs = plot_list, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on employment",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of employment in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_all_months.png"), all_month, width = 30, height = 20, units = "cm")
all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on employment (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("%-change of employment in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_all_months_winsor.png"), all_month_winsor, width = 30, height = 20, units = "cm")




out_measure<-'une_rate'
data <- subset(data, outcome_measure %in% out_measure)

unique(data$period.month)


plot_list <- list()
plot_list_winsor <- list()

# Loop through periods and create plots
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
  ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_", x, "_months.png"), plot_funnel)
  plot_funnel <- create_funnel_plot_grid(data_period, x)
  plot_list[[length(plot_list) + 1]]<-plot_funnel
  
  plot_funnel_winsor <- create_funnel_plot(data_period_winsor, x, winsorize = TRUE)
  ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_", x, "_months_winsor.png"), plot_funnel_winsor)
  plot_funnel_winsor <- create_funnel_plot_grid(data_period_winsor, x, winsorize = TRUE)
  plot_list_winsor[[length(plot_list_winsor) + 1]]<-plot_funnel_winsor
}

library(gridExtra)
library(grid)
all_month<-grid.arrange(grobs = plot_list, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on unemployment",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("pp. change of unemployment in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_all_months.png"), all_month, width = 30, height = 20, units = "cm")
all_month_winsor<-grid.arrange(grobs = plot_list_winsor, ncol = 4,top = textGrob("Funnel plots of effect of monetary policy on unemployment (with Winsorization)",gp=gpar(fontsize=20,font=2,lwd = 1.5)),bottom = textGrob("pp. change of employment in response to 100bp monetary policy shock",gp=gpar(fontsize=16,font=3)))
ggsave(paste0("./results/",out,"/plots/pbias/",out_measure,"/funnel_plot_all_months_winsor.png"), all_month_winsor, width = 30, height = 20, units = "cm")

