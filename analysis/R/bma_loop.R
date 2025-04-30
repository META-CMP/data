#' Run Bayesian Model Averaging Across Multiple Time Periods
#' 
#' @description
#' Applies Bayesian Model Averaging (BMA) to analyze impulse response functions (IRFs) 
#' as effect sizes across specified time periods. This function automates the process 
#' of running BMA for each time period in our meta-analysis data, handling factor variables, 
#' missing values, and producing both numerical results and interactive visualizations 
#' of how variable importance (PIP) and coefficients (Post Mean) and sign (Cond.Pos.Sign) 
#' change over time.
#' 
#' @param data A data frame containing variables for BMA and a column named 'period.month' 
#'   that identifies the time period for each observation.
#' @param periods A numeric vector specifying the time periods to analyze. 
#'   Default is \code{seq(0, 60, by = 3)}.
#' @param fixed.vars Character vector of variable names to be included in all models.
#'   Default is \code{NULL}.
#' @param weighted Logical indicating whether to use weighted least squares. 
#'   When TRUE, the input data must include a column named 'weight' containing 
#'   pre-calculated weights (typically 1/SE for inverse-variance weighting).
#'   These weights will be applied to all numeric variables before BMA estimation.
#'   Default is \code{FALSE}.
#' @param g_ The g-prior specification for Zellner's g prior. Default is \code{"UIP"} 
#'   (Unit Information Prior). See \code{\link[BMS]{bms}} for details.
#' @param mprior_ Model prior to use. Default is \code{"uniform"}. See \code{\link[BMS]{bms}} 
#'   for alternative options.
#' @param user.int_ Logical indicating whether to run BMA in interactive mode allowing 
#'   user to monitor progress. Default is \code{FALSE}.
#' @param nmodel_ Maximum number of top models to store. Default is 5000.
#' @param burn_ Number of burn-in iterations for MCMC sampling. Default is 100.
#' @param iter_ Number of iterations after burn-in for MCMC sampling. Default is 200.
#' @param mcmc_ MCMC sampler to use. Default is \code{"bd"} (birth-death sampler).
#'   See \code{\link[BMS]{bms}} for other options.
#' @param yprop2pip_ Logical indicating whether to proportion y-axis to Posterior Inclusion
#'   Probabilities (PIPs) in BMA image plots. Default is \code{TRUE}.
#' @param cex.axis_ Size of axis labels in BMA image plots. Default is 0.5.
#' 
#' @return 
#' A list with two components:
#' \itemize{
#'   \item \code{plots}: A named list containing three interactive plotly visualizations:
#'     \itemize{
#'       \item \code{PIP}: Posterior Inclusion Probabilities across time periods
#'       \item \code{Post Mean}: Posterior Mean coefficients with uncertainty ribbons
#'       \item \code{Cond.Pos.Sign}: Probability of positive coefficient signs across periods
#'     }
#'   \item \code{dataframes}: A named list with long-format data frames containing the raw data
#'     used to create the plots, facilitating further analysis or custom visualization.
#' }
#' 
#' @details
#' The function processes data period-by-period for impulse response functions:
#' 
#' \enumerate{
#'   \item For each period, it filters data and automatically handles data preparation:
#'     \itemize{
#'       \item Automatically creates dummy variables for factor/character columns
#'       \item Removes observations with missing values
#'       \item Performs BMA using the BMS package
#'     }
#'   \item Summarizes model results for each period and produces summary plots
#'   \item After processing all periods, creates time series visualizations showing:
#'     \itemize{
#'       \item How variable importance (PIP) changes over time
#'       \item How effect sizes (Post Mean) change over time, with uncertainty bands
#'       \item How the probability of positive effects (Cond.Pos.Sign) changes over time
#'     }
#'   \item Plays a completion sound using the beepr package when finished
#' }
#' 
#' This function is designed specifically for meta-analysis of impulse response functions
#' where the time dimension represents periods after an impulse or shock.
#' 
#' @note
#' The function requires the following packages: \code{dplyr}, \code{BMS}, \code{fastDummies}, 
#' \code{reshape2}, \code{plotly}, and \code{beepr}.
#' 
#' @examples
#' \dontrun{
#' # Assuming 'irf_data' contains impulse response data with a period.month column
#' # and variables representing different transmission mechanisms
#' 
#' library(dplyr)
#' library(BMS)
#' library(fastDummies)
#' library(reshape2)
#' library(plotly)
#' library(beepr)
#' 
#' # Run BMA across periods from 0 to 24 months in 3-month intervals
#' results <- bma_loop(
#'   data = irf_data,
#'   periods = seq(0, 24, by = 3),
#'   fixed.vars = c("country_fixed_effects"),
#'   g_ = "UIP",
#'   mprior_ = "random",
#'   nmodel_ = 10000,
#'   burn_ = 5000,
#'   iter_ = 10000
#' )
#' 
#' # View the PIP plot showing how variable importance changes over time
#' results$plots$PIP
#' 
#' # View how posterior means evolve over time with uncertainty bands
#' results$plots$`Post Mean`
#' 
#' # Extract dataframe with PIP values to use in custom visualizations
#' pip_data <- results$dataframes$PIP
#' head(pip_data)
#' }
#' 
#' @seealso 
#' \code{\link[BMS]{bms}} for details on the Bayesian Model Averaging implementation
#' 
#' @import dplyr BMS fastDummies reshape2 plotly beepr
#' @export
bma_loop <- function (data,
                      periods = seq(0, 60, by = 3),
                      fixed.vars = NULL,
                      weighted = FALSE,
                      g_ = "UIP",
                      mprior_ = "uniform",
                      user.int_ = FALSE,
                      nmodel_ = 5000,
                      burn_ = 1e2,
                      iter_ = 2e2,
                      mcmc_ = "bd",
                      yprop2pip_ = TRUE,
                      cex.axis_ = 0.5) {
  
  # Initialize a list to store error messages
  error_log <- list()
  
  # Placeholder to store results
  results_list <- list()
  
  # Initialize a list to store successful outputs
  bma_outputs <- list()
  
  # Store all variable names
  all_vars <- c()
  
  # Run BMA for each period
  for (i in periods) {
    tryCatch({
      # Filter data for out_var and the current period i
      data_bma <- data %>%
        filter(period.month == i) %>%
        select(-period.month) # Drop period.month, as it should not be used as a
      # regressor in the BMA
      
      # Check if there are any character or factor columns and create dummies
      char_factor_cols <- sapply(data_bma, function(x) is.character(x) || is.factor(x))
      if (any(char_factor_cols)) {
        # Automatically create dummies for factor and character variables.
        # This sets as reference group the most frequent level of each factor variable
        # and removes the original factor variable to only use the newly created dummies. 
        data_bma <- fastDummies::dummy_cols(data_bma,
                                            remove_most_frequent_dummy = TRUE,
                                            remove_selected_columns = TRUE)
      }
      
      # Apply weights after dummy creation if weighted=TRUE
      if (weighted) {
        # Check if weight column exists
        if (!"weight" %in% colnames(data_bma)) {
          stop("When weighted == TRUE, the input data must include a 'weight' column")
        }
        
        # Apply weights to all columns except weight itself
        data_bma <- data_bma %>%
          mutate(across(-weight, ~.*weight)) %>%
          select(-weight)  # Drop the weight column
      }
      
      # Test if any NA values in data_bma and remove these
      if (anyNA(data_bma)) {
        warning("Some of the selected variables contain NA values.
                Bayesian model averaging will ignore these observations.")
        data_bma <- data_bma[complete.cases(data_bma), ]
      }
      
      # BMS package requires a dataframe
      data_bma <- as.data.frame(data_bma)
      
      # Estimation
      bma_broad_output <- BMS::bms(data_bma,
                                   fixed.reg = fixed.vars,
                                   g = g_,
                                   mprior = mprior_,
                                   user.int = user.int_,
                                   nmodel = nmodel_,
                                   burn = burn_,
                                   iter = iter_ ,
                                   mcmc = mcmc_)
      
      # Store the successful output
      bma_outputs[[as.character(i)]] <- bma_broad_output
      
      # Get results with variable names
      results <- coef(bma_broad_output, include.constant = TRUE)
      results_list[[as.character(i)]] <- results
      
      # Update all_vars with new variable names
      all_vars <- union(all_vars, rownames(results))
      
      # Model summary
      print(paste("Period", i, "summary:"))
      print(summary(bma_broad_output))
      
      # Plot
      image(bma_broad_output,
            yprop2pip = yprop2pip_,
            cex.axis = cex.axis_
      )
      
    }, error = function(e) {
      # Store the error message along with the iteration info
      error_log[[length(error_log) + 1]] <- conditionMessage(e)
    })
  }
  
  # Check which periods were successfully processed
  successful_periods <- names(bma_outputs)
  print(paste("Successfully processed periods:", paste(successful_periods, collapse = ", ")))
  
  # Create data frames for each statistic
  stats <- c("PIP", "Post Mean", "Post SD", "Cond.Pos.Sign")
  result_dfs <- lapply(stats, function(stat) {
    df <- matrix(NA, nrow = length(all_vars), ncol = length(results_list), 
                 dimnames = list(all_vars, names(results_list)))
    for (period in names(results_list)) {
      period_results <- results_list[[period]]
      df[rownames(period_results), period] <- period_results[, stat]
    }
    as.data.frame(df)
  })
  names(result_dfs) <- stats
  
  # Create a list to store the long format dataframes
  df_long_list <- list()
  
  # Reshape and plot for each statistic
  plots <- lapply(c("PIP", "Post Mean", "Cond.Pos.Sign"), function(stat) {
    df <- result_dfs[[stat]]
    df$Variable <- rownames(df)
    
    df_long <- reshape2::melt(df, id.vars = "Variable", 
                              variable.name = "Period", value.name = stat)
    df_long$Period <- as.numeric(as.character(df_long$Period))
    
    # Store the long format dataframe
    df_long_list[[stat]] <<- df_long
    
    if (stat == "Post Mean") {
      # Add upper and lower bounds for Post Mean
      df_sd <- result_dfs[["Post SD"]]
      df_sd$Variable <- rownames(df_sd)
      df_sd_long <- reshape2::melt(df_sd, id.vars = "Variable", 
                                   variable.name = "Period", value.name = "Post SD")
      df_sd_long$Period <- as.numeric(as.character(df_sd_long$Period))
      
      df_long <- merge(df_long, df_sd_long, by = c("Variable", "Period"))
      df_long$Upper <- df_long$`Post Mean` + 1 * df_long$`Post SD`
      df_long$Lower <- df_long$`Post Mean` - 1 * df_long$`Post SD`
      
      # Update the stored long format dataframe for Post Mean
      df_long_list[[stat]] <<- df_long
      
      p <- plotly::plot_ly(df_long, x = ~Period, color = ~Variable) %>%
        add_ribbons(ymin = ~Lower, ymax = ~Upper, line = list(color = "transparent"), 
                    showlegend = FALSE, hoverinfo = "none") %>%
        add_lines(y = ~`Post Mean`, hoverinfo = "text",
                  text = ~paste("Variable:", Variable, 
                                "<br>Period:", Period, 
                                "<br>Post Mean:", round(`Post Mean`, 4),
                                "<br>Post SD:", round(`Post SD`, 4)))
    } else {
      p <- plotly::plot_ly(df_long, 
                           x = ~Period, 
                           y = as.formula(paste0("~`", stat, "`")),
                           color = ~Variable, 
                           type = 'scatter', 
                           mode = 'lines+markers',
                           hoverinfo = "text",
                           text = ~paste("Variable:", Variable, 
                                         "<br>Period:", Period, 
                                         "<br>", stat, ":", round(get(stat), 4)))
    }
    
    p %>% layout(
      title = paste("Change in BMA", stat, "across Periods"),
      xaxis = list(title = "Month"),
      yaxis = list(title = stat)
    )
  })
  names(plots) <- c("PIP", "Post Mean", "Cond.Pos.Sign")
  
  # Finish sound
  beepr::beep()
  
  # Return both the plots and the dataframes
  return(list(plots = plots, dataframes = df_long_list))
}