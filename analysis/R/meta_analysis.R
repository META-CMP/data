#' Create equation for meta-analysis
#'
#' This helper function creates a formula object for use in meta-analysis models.
#' It combines a base formula with additional moderator variables if provided.
#'
#' @param base_formula A character string representing the base formula.
#' @param mods A character vector of moderator variable names, or NULL if no moderators.
#'
#' @return A formula object.
#'
#' @examples
#' create_equation("y ~ x", NULL)
#' create_equation("y ~ x", c("mod1", "mod2"))
#' create_equation("mean.effect_winsor ~ 1", c("cum", "iv"))
#'
#' @export
create_equation <- function(base_formula, mods) {
  if (is.null(mods)) {
    return(as.formula(base_formula))
  } else {
    return(as.formula(paste(base_formula, "+", paste(mods, collapse = " + "))))
  }
}

#' Perform meta-analysis
#'
#' This function performs meta-analysis on the filtered data for a specified outcome variable,
#' standard error option, periods, winsorization parameter, and precision weighting option.
#' It also supports multiple meta-regression with moderator variables.
#' 
#' @param data A data frame containing the necessary columns for analysis.
#' @param outvar A string specifying the outcome variable to filter the data.
#' @param se_option A string specifying the standard error option to use. Can be "avg", "lower", or "upper".
#' @param periods A numeric vector specifying the periods (in months) for which to perform the analysis.
#' @param wins A numeric value specifying the winsorization parameter.
#' @param prec_weighted A logical value indicating whether to use precision weighting.
#' @param estimation String specifying the meta analysis model, one of "Mean", "UWLS", "FAT-PAT", "PEESE", "EK".
#' @param mods A character vector of moderator variable names to include in the multiple meta-regression. If NULL, no moderators are included.
#' 
#' @return A list of model objects for each period.
#'
#' @import tidyverse
#' @import dplyr
#'
#' @examples
#' library(tidyverse)
#' library(dplyr)
#' library(JWileymisc)
#' library(modelsummary)
#' library(sandwich)
#' library(clubSandwich)
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
#' results1 <- meta_analysis(data, outvar = "gdp", se_option = "avg", periods = 1:20,
#'                          wins = 0.02, prec_weighted = TRUE, estimation = "FAT-PET", cluster_se = "sandwich", mods = NULL)
#' results2 <- meta_analysis(data, outvar = "gdp", se_option = "avg", periods = 1:20,
#'                          wins = 0.02, prec_weighted = FALSE, estimation = "EK", cluster_se = "sandwich")
#' # Example with moderators
#' result_with_mods <- meta_analysis(data, outvar = "gdp", se_option = "avg", 
#'                                   periods = 1:20, wins = 0.02, prec_weighted = TRUE, 
#'                                   estimation = "PEESE", cluster_se = "sandwich", mods = c("cum", "iv"))
#' # Display the results
#' modelsummary(results1, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "PEESE", gof_map = NULL)
#' modelsummary(results2, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "EK", gof_map = NULL)
#' modelsummary(result_with_mods, output = "gt", stars = TRUE, title = "PEESE with moderators", gof_map = NULL)
#' 
#' @export
meta_analysis <- function(data, outvar, se_option, periods, wins, prec_weighted, estimation = "Mean", ap = FALSE, cluster_se = NULL, EK_sig_threshold = 1.96, mods = NULL) {
  # Subset data for the specified outcome variable
  data <- subset(data, outcome %in% outvar)

  # Check which periods have data
  available_periods <- unique(data$period.month)
  valid_periods <- periods[periods %in% available_periods]

  if (length(valid_periods) == 0) {
    stop("No data available for any of the specified periods.")
  }

  if (length(valid_periods) < length(periods)) {
    warning(paste("Data is only available for the following periods:", 
                  paste(valid_periods, collapse = ", ")))
  }
  
  # Create empty lists for results
  results_list <- list()
  
  # Lookup table for base formulas
  base_formulas <- list(
    Mean = "mean.effect_winsor ~ 1",
    UWLS = "t.stat_winsor ~ precision_winsor - 1",
    "FAT-PET" = "mean.effect_winsor ~ standarderror_winsor",
    PEESE = "mean.effect_winsor ~ variance_winsor"
  )
  
  # Define the equation to be estimated (not relevant for EK)
  if (estimation != "EK") {
    base_formula <- base_formulas[[estimation]]
    if (is.null(base_formula)) {
      stop(paste("Unknown estimation method:", estimation))
    }
    equation <- create_equation(base_formula, mods)
  } else {
    # EK doesn't use the standard equation format
    equation <- NULL
  }
  
  for (x in valid_periods) {
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

      reg_weights <<- if (prec_weighted) data_period$precvariance_winsor else NULL
      # Note the use of "<<-". Defining reg_weights in the global environment here is necessary, otherwise 
      # it cannot be found by lm(). The reason is unclear. To avoid 
      # unintended effects, we remove it from the environment immediately after it 
      # was used by lm().
      reg_result <- lm(equation, data = data_period, weights = reg_weights)
      rm(reg_weights, envir = .GlobalEnv)
      
    } else if (estimation == "EK") {

      data_EK <- data_period %>% select(mean.effect_winsor, standarderror_winsor, key)
      est_EK <- EK(data=data_EK,verbose = FALSE, sig_threshold = EK_sig_threshold)
      reg_result <- est_EK$model

    }
    
    # Correction of SEs - clustered by study (by key)
    if (!is.null(cluster_se)) {
      data_period <<- data_period # Defining data_period in the global environment 
      # here is necessary, otherwise it cannot be found by sandwich::vcovCL(). The 
      # reason is unclear. To avoid unintended effects, we remove it from the environment 
      # immediately after it was used by sandwich::vcovCL. It is important that data_period 
      # is NOT defined globally before this point (secifically before the use of lm() above)!
      
      if (cluster_se == "sandwich") {
        vcov_cluster <- sandwich::vcovCL(reg_result, cluster = ~key)
        rm(data_period, envir = .GlobalEnv)
        reg_result <- lmtest::coeftest(reg_result, vcov. = vcov_cluster)
      }
      if (cluster_se == "clubSandwich") {
        # Ensure 'key' is a factor
        data_period$key <- as.factor(data_period$key)

        # Apply clubSandwich correction
        corrected_results <- clubSandwich::coef_test(reg_result,
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
