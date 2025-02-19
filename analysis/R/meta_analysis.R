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
#' @param first_period_wins_prec A numeric value specifying the winsorization  
#'   for StandardError and precision in the first period (period.month == 0). Default is NULL, in which case 
#'   the regular wins parameter is used.
#' @param first_period_wins_mean A numeric value specifying the winsorization  
#'   for mean.effect in the first period (period.month == 0). Default is NULL, in which case the regular 
#'   wins parameter is used.
#' @param prec_weighted A logical value indicating whether to use precision weighting.
#' @param ap A logical value indicating whether to filter for adequately powered studies. Default is FALSE.
#' @param ap_horizon A numeric value specifying the horizon (period.month) at which to evaluate adequate power.
#'   If NULL (default), power is evaluated separately for each period. If specified, power is evaluated
#'   at this horizon and entire IRFs (all periods for a model) are kept or dropped based on this evaluation.
#' @param ap_prec_weighted A logical value indicating whether to use precision weighting when calculating
#'   the true effect for adequate power evaluation. Default is TRUE.
#' @param ap_parameter Numeric value used in the threshold calculation for adequate power filtering. Default is 2.8.
#' @param estimation String specifying the meta analysis model, one of "Mean", "UWLS", "FAT-PAT", "PEESE", "EK", or "AK".
#' @param mods A character vector of moderator variable names to include in the multiple meta-regression. If NULL, no moderators are included.
#' @param hc_type A string specifying the type of Heteroskedasticity-Consistent (HC) Covariance Matrix Estimator to use. 
#'        Options are "HC0", "HC1", "HC2", or "HC3". Default is NULL, which uses "HC1" for lm objects and "HC0" otherwise.
#' @param return_lm A logical value indicating whether to store the original lm object as an attribute before applying clustered standard errors. Default is FALSE.
#' @param pred_data Optional data frame containing predictor values for which to calculate predictions. 
#'        Only works with estimation methods that use linear regression ("Mean", "UWLS", "FAT-PET", "PEESE").
#'        If provided with "EK" or "AK" methods, the function will throw an error.
#' @param pred_conf_level Confidence level for prediction intervals when pred_data is provided. Default is 0.89.
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
#' library(sandwich)
#'
#' # Load the data
#' data_path <- "data/preliminary_data_test_11072024.RData"
#' load(data_path)
#'
#' # Calculate the average standard error and precision options
#' data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
#' data$precision.avg <- 1 / data$SE.avg
#' data$precision.lower <- 1 / data$SE.lower
#' data$precision.upper <- 1 / data$SE.upper
#' # Renaming gdp to output
#' data$outcome <- ifelse(data$outcome == "gdp", "output", data$outcome)
#' # Perform meta-analysis
#' results1 <- meta_analysis(data, outvar = "output", se_option = "avg", periods = 1:20,
#'                          wins = 0.02, prec_weighted = TRUE, estimation = "FAT-PET", cluster_se = TRUE, mods = NULL)
#' results2 <- meta_analysis(data, outvar = "output", se_option = "avg", periods = 1:20,
#'                          wins = 0.02, prec_weighted = FALSE, estimation = "EK", cluster_se = TRUE)
#' AK_results <- meta_analysis(data, outvar = "output", se_option = "upper", periods = 1:3,
#'                          wins = 0, prec_weighted = FALSE, estimation = "AK", cluster_se = FALSE, AK_conf_level = 0.90)
#' # Example with moderators
#' result_with_mods <- meta_analysis(data, outvar = "output", se_option = "avg", 
#'                                   periods = 1:20, wins = 0.02, prec_weighted = TRUE, 
#'                                   estimation = "PEESE", cluster_se = TRUE, mods = c("cum", "iv"))
#' # Display the results
#' modelsummary::modelsummary(results1, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "PEESE", gof_map = NULL)
#' modelsummary::modelsummary(results2, output = "gt", stars = TRUE, statistic = c("se = {std.error}", "conf.int"), conf_level = 0.80, title = "EK", gof_map = NULL)
#' modelsummary::modelsummary(result_with_mods, output = "gt", stars = TRUE, title = "PEESE with moderators", gof_map = NULL)
#' modelsummary::modelsummary(AK_results, output = "gt", statistic = c("se = {std.error}", "conf.int"))
#' 
#' @export
meta_analysis <- function(data, outvar, se_option, periods, wins, first_period_wins_prec = NULL,
                          first_period_wins_mean = NULL, prec_weighted,
                          estimation = "Mean", ap = FALSE, ap_horizon = NULL, ap_prec_weighted = TRUE, 
                          ap_parameter = 2.8, cluster_se = FALSE, 
                          hc_type = NULL, EK_sig_threshold = 1.96, mods = NULL, 
                          cutoff_val = c(1.960), AK_symmetric = FALSE, 
                          AK_modelmu = "normal", AK_conf_level = 0.95, 
                          ak_plot = NULL, AK_plot_prob_y_range = c(0, 40), 
                          ak_prob_plot_log_scale = FALSE, return_lm = FALSE, 
                          pred_data = NULL, pred_conf_level = 0.68) {
  
  # Check pred_data for compatibility with estimation method
  if (!is.null(pred_data)) {
    if (estimation %in% c("EK", "AK")) {
      stop("You specified pred_data but predictions are only available for methods using linear regression ('Mean', 'UWLS', 'FAT-PET', 'PEESE'). 
                 The '", estimation, "' method is not supported for predictions.")
    }
  }
  
  # Subset data for the specified outcome variable
  data <- subset(data, outcome %in% outvar)
  
  # If ap is TRUE and ap_horizon is specified, do the AP filtering before the period loop
  if (ap && !is.null(ap_horizon)) {
    # First get the data for the specified horizon and apply winsorization
    horizon_data <- subset(data, period.month == ap_horizon)
    
    if (nrow(horizon_data) == 0) {
      stop(paste("No data available for specified ap_horizon:", ap_horizon))
    }
    
    # Apply standard error selection
    if (se_option == "avg") {
      horizon_data$StandardError <- horizon_data$SE.avg
      horizon_data$precision <- horizon_data$precision.avg
    } else if (se_option == "lower") {
      horizon_data$StandardError <- horizon_data$SE.lower
      horizon_data$precision <- horizon_data$precision.lower
    } else if (se_option == "upper") {
      horizon_data$StandardError <- horizon_data$SE.upper
      horizon_data$precision <- horizon_data$precision.upper
    }
    
    # Apply winsorization to horizon data
    current_wins_prec <- if (ap_horizon == 0) first_period_wins_prec else wins
    current_wins_mean <- if (ap_horizon == 0) first_period_wins_mean else wins
    horizon_data <- apply_winsorization(horizon_data, wins = wins,
                                        wins_prec = current_wins_prec,
                                        wins_mean = current_wins_mean)
    
    # Calculate estimate of "true" effect for the horizon
    est_true_effect <- if(ap_prec_weighted) {
      weighted.mean(horizon_data$mean.effect_winsor,
                    w = 1/(horizon_data$standarderror_winsor^2))
    } else {
      mean(horizon_data$mean.effect_winsor)
    }
    
    # Calculate threshold using the weighted mean
    waap_threshold <- abs(est_true_effect)/ap_parameter
    
    # Get the model IDs that pass the threshold
    selected_models <- horizon_data[horizon_data$standarderror_winsor <= waap_threshold,
                                    c("key", "model_id")]
    
    # Filter the original data to keep only the selected models
    data <- merge(data, selected_models, by = c("key", "model_id"))
    
    if (nrow(data) == 0) {
      stop("No adequately powered studies found based on the specified horizon")
    }
  }

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
  
  # Define the equation to be estimated (not relevant for EK & AK)
  if (!(estimation %in% c("EK", "AK"))) {
    base_formula <- base_formulas[[estimation]]
    if (is.null(base_formula)) {
      stop(paste("Unknown estimation method:", estimation))
    }
    equation <- create_equation(base_formula, mods)
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
    
    # Select appropriate winsorization levels
    if (x == 0) {
      current_wins_prec <- if (!is.null(first_period_wins_prec)) first_period_wins_prec else wins
      current_wins_mean <- if (!is.null(first_period_wins_mean)) first_period_wins_mean else wins
    } else {
      current_wins_prec <- NULL
      current_wins_mean <- NULL
    }
    data_period <- apply_winsorization(data_period, wins = wins, 
                                       wins_prec = current_wins_prec, 
                                       wins_mean = current_wins_mean)

    # Apply WAAP if ap == TRUE and ap_horizon is NULL
    if (ap == TRUE && is.null(ap_horizon)) {

      # Calculate estimate of "true" effect for this period
      est_true_effect <- if(ap_prec_weighted) {
        weighted.mean(data_period$mean.effect_winsor,
                      w = 1/(data_period$standarderror_winsor^2))
      } else if (ap_prec_weighted == FALSE) {
        mean(data_period$mean.effect_winsor)
      }
      # Calculate threshold using the weighted mean for this period
      waap_threshold <- abs(est_true_effect)/ap_parameter
      
      # Filter data for this period
      data_period <- data_period %>%
        filter(standarderror_winsor <= waap_threshold)
      
      # Check if we still have data after filtering
      if (nrow(data_period) == 0) {
        warning(paste("No adequately powered studies found for period", x))
        next  # Skip to next period
      }
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
    if (estimation == "EK") { # Bom & Rachinger (2019) EK method
      
      data_EK <- data_period %>% select(mean.effect_winsor, standarderror_winsor, key)
      est_EK <- EK(data=data_EK,verbose = FALSE, sig_threshold = EK_sig_threshold)
      reg_result <- est_EK$model
      
    } else if (estimation == "AK") { # Andrews & Kasy method
      
      data_AK <- data_period %>% select(mean.effect_winsor, standarderror_winsor, key)
      
      # AK cutoffs configuration
      AK_cutoffs <- as.numeric(unlist(cutoff_val))
      
      if (!AK_symmetric) {
        AK_cutoffs <- c(-rev(AK_cutoffs), 0, AK_cutoffs)
      }
      
      # Prepare clustering, if cluster_se == TRUE
      if (cluster_se == FALSE) {
        AK_cluster_ID <- NULL
      } else if (cluster_se == TRUE) {
        AK_cluster_ID <- data_AK$key
      }
      
      est_AK <- metastudies_estimation(X = data_AK$mean.effect_winsor, sigma = data_AK$standarderror_winsor, cutoffs = AK_cutoffs, symmetric = AK_symmetric, model = AK_modelmu, study_ID = AK_cluster_ID)
      
      # Storing results for modelsummary() compatibility
      table_AK <- estimatestable(est_AK$Psihat, est_AK$SE, AK_cutoffs, AK_symmetric, AK_modelmu)
      ti <- data.frame(
        term = colnames(table_AK),
        estimate = table_AK[1,],
        std.error = table_AK[2,])
      
      # Calculate confidence intervals using AK_conf_level (approximation of CIs with normality assumption)
      z_score <- qnorm((1 + AK_conf_level) / 2)
      ci_lower <- ti$estimate - z_score * ti$std.error
      ci_upper <- ti$estimate + z_score * ti$std.error
      ti$conf.low <- ci_lower
      ti$conf.high <- ci_upper
      
      # Calculate p-values
      ti$statistic <- ti$estimate / ti$std.error
      ti$p.value <- 2 * (1 - pnorm(abs(ti$statistic)))
      
      if (ak_plot == "pub_prob_only") {
        plot_AK <- estimates_plot_prob(cutoffs = AK_cutoffs, symmetric = AK_symmetric, estimates = est_AK, model = AK_modelmu, y_range = AK_plot_prob_y_range, log_scale = ak_prob_plot_log_scale)
      } else {
        plot_AK <- estimates_plot(cutoffs = AK_cutoffs, symmetric = AK_symmetric, estimates = est_AK, model = AK_modelmu)
      }
      
      gl <- data.frame(Num.Obs. = nrow(data_AK))
      reg_result <- list(
        tidy = ti,
        glance = gl,
        plot = plot_AK)
      class(reg_result) <- "modelsummary_list"
      
      warning("Note our custom calculation of confidence intervalls and p-values under normality assumptions for the AK method. We may want to check if this is adequate for their method.")
      
    } else { # Other methods

      reg_weights <<- if (prec_weighted) data_period$precvariance_winsor else NULL
      # Note the use of "<<-". Defining reg_weights in the global environment here is necessary, otherwise 
      # it cannot be found by lm(). The reason is unclear. To avoid 
      # unintended effects, we remove it from the environment immediately after it 
      # was used by lm().
      reg_result <- lm(equation, data = data_period, weights = reg_weights)
      rm(reg_weights, envir = .GlobalEnv)
      
    } 
    
    # Correction of SEs - clustered by study (by key)
    if (cluster_se == TRUE & estimation != "AK") {
      data_period <<- data_period # Defining data_period in the global environment 
      # here is necessary, otherwise it cannot be found by sandwich::vcovCL(). The 
      # reason is unclear. To avoid unintended effects, we remove it from the environment 
      # immediately after it was used by sandwich::vcovCL. It is important that data_period 
      # is NOT defined globally before this point (specifically before the use of lm() above)!
      
      vcov_cluster <- sandwich::vcovCL(reg_result, cluster = ~key, type = hc_type)
      rm(data_period, envir = .GlobalEnv)
      if (return_lm) {
        # Store original lm object before replacing it
        lm_object <- reg_result
      }
      reg_result <- lmtest::coeftest(reg_result, vcov. = vcov_cluster)
      if (return_lm) {
        # Add lm object as attribute
        attr(reg_result, "lm_object") <- lm_object
      }
        
    }
    
    results_list[[paste0(x)]] <- reg_result
    
  }
  
  # If predictions are requested, calculate them before returning
  if (!is.null(pred_data)) {
    predictions_list <- list()
    
    for (x in valid_periods) {
      # Get the model for this period
      reg_result <- results_list[[as.character(x)]]
      
      # Need to temporarily put data_period and reg_weights in global env for vcovCL
      data_period <<- subset(data, period.month %in% x)
      
      # Calculate weights as in the original code
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
      
      # Apply winsorization
      if (x == 0) {
        current_wins_prec <- if (!is.null(first_period_wins_prec)) first_period_wins_prec else wins
        current_wins_mean <- if (!is.null(first_period_wins_mean)) first_period_wins_mean else wins
      } else {
        current_wins_prec <- NULL
        current_wins_mean <- NULL
      }
      data_period <- apply_winsorization(data_period, wins = wins,
                                         wins_prec = current_wins_prec,
                                         wins_mean = current_wins_mean)
      data_period$variance_winsor <- data_period$standarderror_winsor^2
      data_period$precvariance_winsor <- 1 / data_period$variance_winsor
      
      # Set up data_period and weights in global environment
      data_period <<- data_period
      reg_weights <<- if (prec_weighted) data_period$precvariance_winsor else NULL
      
      # Re-fit the model to get vcov
      original_model <- lm(equation, data = data_period, weights = reg_weights)
      vcov_cluster <- sandwich::vcovCL(original_model, cluster = ~key, type = hc_type)
      
      # Clean up global environment
      rm(data_period, reg_weights, envir = .GlobalEnv)
      
      # Get model terms from the original model
      X <- model.matrix(delete.response(terms(original_model)), data = pred_data)
      
      # Calculate prediction and SE
      pred <- X %*% coef(reg_result)
      pred_se <- sqrt(X %*% vcov_cluster %*% t(X))
      
      # Calculate CI
      t_val <- qt((1 + pred_conf_level)/2, df = df.residual(original_model))
      ci_lower <- pred - t_val * pred_se
      ci_upper <- pred + t_val * pred_se
      
      predictions_list[[paste0(x)]] <- data.frame(
        period = x,
        predicted_value = as.numeric(pred),
        std_error = as.numeric(pred_se),
        ci_lower = as.numeric(ci_lower),
        ci_upper = as.numeric(ci_upper)
        
      )
    }
    
    return(list(
      models = results_list,
      predictions = predictions_list
    ))
  } else {
    # Original return if no predictions requested
    return(results_list)
  }
}