#' Apply Winsorization to Data
#'
#' This function applies winsorization to the StandardError, mean.effect, and precision
#' columns of a data frame. 
#' 
#' @param data A data frame containing at least the following columns:
#'   StandardError, mean.effect, and precision.
#' @param wins A numeric value between 0 and 0.5 specifying the winsorization
#'   percentile. Default is 0.02 (2%).
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
apply_winsorization <- function(data, wins = 0.02) {
  data$standarderror_winsor <- JWileymisc::winsorizor(data$StandardError, percentile = wins)
  data$mean.effect_winsor <- JWileymisc::winsorizor(data$mean.effect, percentile = wins)
  data$precision_winsor <- JWileymisc::winsorizor(data$precision, percentile = wins)
  return(data)
}