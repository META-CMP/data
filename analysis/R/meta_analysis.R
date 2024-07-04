#' Perform meta-analysis
#'
#' This function performs meta-analysis on the filtered data for a specified outcome variable,
#' standard error option, periods, winsorization parameter, and precision weighting option.
#'
#' @param data A data frame containing the necessary columns for analysis.
#' @param outvar A string specifying the outcome variable to filter the data.
#' @param se_option A string specifying the standard error option to use. Can be "avg", "lower", or "upper".
#' @param periods A numeric vector specifying the periods (in months) for which to perform the analysis.
#' @param wins A numeric value specifying the winsorization parameter.
#' @param prec_weighted A logical value indicating whether to use precision weighting.
#' @param estimation String specifying the meta analysis model, one of "Mean", "UWLS", "FAT-PAT", "PEESE", "EK".
#'
#' @return A list of model objects for each period.
#'
#' @import tidyverse
#' @import dplyr
#' @import JWileymisc
#'
#' @examples
#' library(tidyverse)
#' library(dplyr)
#' library(JWileymisc)
#' library(modelsummary)
#'
#' # Load the data
#' data_path <- "data/preliminary_data_test.RData"
#' load(data_path)
#'
#' # Calculate the average standard error and precision options
#' data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
#' data$precision.avg <- 1 / data$SE.avg
#' data$precision.lower <- 1 / data$SE.lower
#' data$precision.upper <- 1 / data$SE.upper
#'
#' # Perform meta-analysis
#' results1 <- meta_analysis(data, outvar = "gdp", se_option = "avg", periods = 1:60,
#'                          wins = 0.02, prec_weighted = FALSE, estimation = "PEESE", cluster_se = "sandwich")
#' results2 <- meta_analysis(data, outvar = "gdp", se_option = "avg", periods = 1:60,
#'                          wins = 0.02, prec_weighted = FALSE, estimation = "EK", cluster_se = "sandwich")
#'
#' # Display the results
#' modelsummary(results1, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "PEESE", gof_map = NULL)
#' modelsummary(results2, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "EK", gof_map = NULL)
#' 
#' @export
meta_analysis <- function(data, outvar, se_option, periods, wins, prec_weighted, estimation = "Mean", ap = FALSE, cluster_se = NULL) {
  # Subset data for the specified outcome variable
  data <- subset(data, outcome %in% outvar)
  
  # Create empty lists for results
  results_list <- list()
  
  # Define the equation to be estimated (not relevant for EK)
  if (estimation == "Mean") {
    equation <- mean.effect_winsor ~ 1
  } else if (estimation == "UWLS") {
    equation <- t.stat_winsor ~ precision_winsor - 1
  } else if (estimation == "FAT-PET") {
    equation <- mean.effect_winsor ~ standarderror_winsor
  } else if (estimation == "PEESE") {
    equation <- mean.effect_winsor ~ variance_winsor
  }
  
  for (x in periods) {
    # Subset data for the current period
    data_period <- subset(data, period.month %in% x)
    
    # Select the corresponding standard error and precision columns based on the se_option
    if (se_option == "avg") {
      data_period$StandardError <- data_period$SE.avg
      data_period$precision <- data_period$precision.avg
    } else if (se_option == "lower") {
      data_period$StandardError <- data_period$SE.lower
      data_period$precision <- data_period$precision.lower
    } else if (se_option == "upper") {
      data_period$StandardError <- data_period$SE.upper
      data_period$precision <- data_period$precision.upper
    }
    
    # Apply winsorization to the standard error, mean effect, and precision
    data_period <- apply_winsorization(data_period, wins)
    
    # Filter adequately powered if ap == TRUE
    if (ap == TRUE) {
      data_period$ap <- ifelse(data_period$standarderror_winsor <= abs(data_period$mean.effect_winsor)/2.8, 1, 0)
      data_period <- data_period %>% 
        filter(ap == 1)
    }
    
    # Calculate t.stat_winsor for UWLS
    if (estimation == "UWLS") {
      data_period$t.stat_winsor <- data_period$mean.effect_winsor / data_period$standarderror_winsor
    }
    
    # Calculate variance winsorized
    data_period$variance_winsor <- data_period$standarderror_winsor^2
    
    # Calculate PrecVariance winsorized
    data_period$precvariance_winsor <- 1 / data_period$variance_winsor
    
    # Run regression
    if (estimation != "EK") {
      
      weights <- if (prec_weighted) data_period$precvariance_winsor else NULL
      reg_result <- lm(equation, data = data_period, weights = weights)
      
    } else if (estimation == "EK") {
      
      data_EK <- data_period %>%  select(mean.effect_winsor, standarderror_winsor, key)
      est_EK <- EK(data=data_EK,verbose = FALSE)
      reg_result <- est_EK$model
      
    }
    
    # Correction of SEs - clustered by study (by key)
    if (!is.null(cluster_se)) {
      if (cluster_se == "sandwich") {
        vcov_cluster <- vcovCL(reg_result, cluster = ~key)
        reg_result <- coeftest(reg_result, vcov. = vcov_cluster)
      }
      if (cluster_se == "clubSandwich") {
        # Ensure 'key' is a factor
        data_period$key <- as.factor(data_period$key)
        
        # Apply clubSandwich correction
        corrected_results <- coef_test(reg_result, 
                                vcov = "CR0", 
                                cluster = data_period$key, 
                                test = "naive-t")
        
        # Update the reg_result object with corrected values
        reg_result$coefficients <- corrected_results$beta
        reg_result$std.error <- corrected_results$SE
        reg_result$t.value <- corrected_results$tstat
        reg_result$p.value <- corrected_results$p_val
      }
    }
    
    results_list[[paste0(x)]] <- reg_result
  }
  
  return(results_list)
}
