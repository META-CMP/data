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
#' @param estimation String specifying the meta analysis model, one of "Mean", "FAT-PAT" and "PEESE".
#'
#' @return A list of model summary objects for each period.
#'
#' @import tidyverse
#' @import dplyr
#' @import JWileymisc
#' @import clubSandwich
#' @import modelsummary
#'
#' @examples
#' library(tidyverse)
#' library(dplyr)
#' library(JWileymisc)
#' library(clubSandwich)
#' library(modelsummary)
#'
#' # Load the data
#' data_path <- "data/data_test_new.RData"
#' load(data_path)
#'
#' # Calculate the average standard error and precision options
#' data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
#' data$precision.avg <- 1 / data$SE.avg
#' data$precision.lower <- 1 / data$SE.lower
#' data$precision.upper <- 1 / data$SE.upper
#'
#' # Perform meta-analysis
#' results <- meta_analysis(data, outvar = "gdp", se_option = "avg", periods = c(3, 6, 12),
#'                          wins = 0.02, prec_weighted = FALSE)
#'
#' # Display the results
#' modelsummary(results, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "Title here", gof_map = NULL)
#'
#' @export
meta_analysis <- function(data, outvar, se_option, periods, wins, prec_weighted, estimation = "Mean", ap = FALSE) {
  # Subset data for the specified outcome variable
  data <- subset(data, outcome %in% outvar)
  
  # Create empty lists for results
  results_list <- list()
  
  # Define the equation to be estimated
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
    
    # Apply Winsorization to the standard error, mean effect, and precision
    data_period$standarderror_winsor <- winsorizor(data_period$StandardError, percentile = wins)
    data_period$mean.effect_winsor <- winsorizor(data_period$mean.effect, percentile = wins)
    data_period$precision_winsor <- winsorizor(data_period$precision, percentile = wins)
    
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
    weights <- if (prec_weighted) data_period$precvariance_winsor else NULL
    reg_result <- lm(equation, data = data_period, weights = weights)
    
    results_list[[paste0(x)]] <- reg_result
  }
  
  return(results_list)
}