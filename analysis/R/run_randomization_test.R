#' Run Single Randomization Test for P-Hacking Analysis
#' 
#' @description
#' Performs a randomization test to detect discontinuities around statistical
#' thresholds following Brodeur et al. (2020). Tests if the proportion of 
#' statistics above the threshold differs from 0.5 within a specified window.
#'
#' @param data A data frame containing test statistics
#' @param statistic Character. Name of column containing test statistics
#' @param threshold Numeric. Statistical threshold to test around
#' @param delta Numeric. Half-width of window around threshold
#' @param group Optional character. Name of grouping variable
#' @param group_value Optional. Value of grouping variable to use
#'
#' @return A list containing:
#'   \item{proportion}{Proportion of statistics above threshold}
#'   \item{pvalue}{One-sided p-value from binomial test}
#'   \item{n}{Number of observations in window}
#'
#' @examples
#' \dontrun{
#' # Test for full sample
#' test <- run_randomization_test(data, "z_stat", threshold = 1.96, delta = 0.2)
#' 
#' # Test for specific group
#' test_group <- run_randomization_test(data, "z_stat", 
#'                                     threshold = 1.96, 
#'                                     delta = 0.2,
#'                                     group = "journal_type", 
#'                                     group_value = "top_5")
#' }
#'
#' @references 
#' Brodeur, A., Cook, N., & Heyes, A. (2020). Methods Matter: P-Hacking and 
#' Publication Bias in Causal Analysis in Economics. American Economic Review, 
#' 110(11), 3634-60.
#'
#' @export
run_randomization_test <- function(data,
                                   statistic,
                                   threshold,
                                   delta,
                                   group = NULL,
                                   group_value = NULL) {
  
  # Input validation
  if(!statistic %in% names(data)) {
    stop("statistic must be a column in data")
  }
  if(!is.null(group) && !group %in% names(data)) {
    stop("group must be a column in data")
  }
  
  # Filter data
  if(!is.null(group)) {
    if(is.null(group_value)) {
      stop("group_value must be specified when group is specified")
    }
    data <- data[data[[group]] == group_value,]
  }
  
  # Get data in window
  window_data <- data[[statistic]][
    data[[statistic]] >= (threshold - delta) & 
      data[[statistic]] <= (threshold + delta)
  ]
  
  # Count observations
  n_obs <- length(window_data)
  if(n_obs == 0) {
    warning("No observations in specified window")
    return(list(
      proportion = NA_real_,
      pvalue = NA_real_,
      n = 0
    ))
  }
  
  # Count successes (above threshold)
  n_success <- sum(window_data >= threshold, na.rm = TRUE)
  
  # Run binomial test
  test_result <- binom.test(n_success, n_obs, p = 0.5, 
                            alternative = "greater")
  
  # Return results
  return(list(
    proportion = n_success/n_obs,
    pvalue = test_result$p.value,
    n = n_obs
  ))
}
