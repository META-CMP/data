#' Apply Winsorization to Data
#'
#' This function applies winsorization to the StandardError, mean.effect, and precision
#' columns of a data frame. 
#' 
#'#' @param wins A numeric value between specifying the winsorization for all variables.
#'   Only used if both wins_prec and wins_mean are NULL. Default is 0.02 (2%).
#' @param wins_prec A numeric value between 0 and 0.5 specifying the winsorization percentile for 
#'   StandardError and precision. If not NULL, overrides wins parameter for these variables. Default is NULL.
#' @param wins_mean A numeric value between 0 and 0.5 specifying the winsorization percentile for 
#'   mean.effect. If not NULL, overrides wins parameter for this variable. Default is NULL.
#'
#' @return A data frame with additional columns: standarderror_winsor,
#'   mean.effect_winsor, and precision_winsor, containing the winsorized values.
#'
#' @importFrom JWileymisc winsorizor
#'
#' @examples
#' # Create a sample dataset
#' set.seed(123)
#' sample_data <- data.frame(
#'   StandardError = rnorm(100, mean = 1, sd = 0.2),
#'   mean.effect = rnorm(100, mean = 0, sd = 1),
#'   precision = 1 / rnorm(100, mean = 1, sd = 0.2)
#' )
#'
#' # Apply winsorization
#' winsorized_data <- apply_winsorization(sample_data, wins = 0.05)
#'
#' # Check the results
#' head(winsorized_data)
#'
#' @export
apply_winsorization <- function(data, wins = 0.02, wins_prec = NULL, wins_mean = NULL) {
  # Determine which winsorization level to use for each type
  prec_level <- if (!is.null(wins_prec)) wins_prec else wins
  mean_level <- if (!is.null(wins_mean)) wins_mean else wins
  
  # Apply winsorization with potentially different levels
  data$standarderror_winsor <- JWileymisc::winsorizor(data$StandardError, percentile = prec_level)
  data$mean.effect_winsor <- JWileymisc::winsorizor(data$mean.effect, percentile = mean_level)
  data$precision_winsor <- JWileymisc::winsorizor(data$precision, percentile = prec_level)
  
  return(data)
}


