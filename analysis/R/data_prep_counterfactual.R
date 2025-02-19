#' Prepare Test Statistic Data for Calibration and Plotting of Counterfactual 
#' Density
#' 
#' @description
#' Prepares data for the calibration and plotting of a non-central t distribution
#' to serve as counterfactual for p-hacking analysis following Brodeur et al. 
#' (2020). The returned data frame is the input for the \code{calibrate_counterfactual}
#' and \code{plot_counterfactual} functions.
#'
#' @param data A data frame containing effect size and standard error data
#' @param outvar Character. Name of outcome variable to analyze
#' @param periods Numeric vector. Time periods to include (default = 1:60)
#' @param se_option Character. Which standard error to use: "avg" (default),
#'        "upper", or "lower"
#' @param wins Numeric. Winsorization parameter for extreme values
#' @param only_negative Logical. Keep only negative effects (default = TRUE)
#' @param only_positive Logical. Keep only positive effects (default = FALSE)
#'
#' @return A data frame containing:
#'   \item{mean.effect}{Winsorized effect size estimates}
#'   \item{SE}{Selected and winsorized standard errors}
#'   \item{z_stat}{Absolute z-statistics (effect size / SE)}
#'   \item{Additional columns from input data frame}
#'
#' @details
#' The function processes effect size data through several steps:
#' 1. Filters data by outcome variable and time periods
#' 2. Winsorizes effect sizes and standard errors
#' 3. Calculates absolute z-statistics
#' 4. Optionally filters by effect direction
#'
#' Note that only_negative and only_positive cannot both be TRUE.
#'
#' @examples
#' \dontrun{
#' # Prepare data with average SE and negative effects
#' data_neg <- prep_effect_data(
#'   data = study_data,
#'   outvar = "employment",
#'   periods = 1:24,
#'   se_option = "avg",
#'   wins = 0.01,
#'   only_negative = TRUE
#' )
#' }
#'
#' @importFrom dplyr filter mutate case_when
#' @importFrom JWileymisc winsorizor
#'
#' @references 
#' Brodeur, A., Cook, N., & Heyes, A. (2020). Methods Matter: P-Hacking and 
#' Publication Bias in Causal Analysis in Economics. American Economic Review, 
#' 110(11), 3634-60.
#' 
#' @export
data_prep_counterfactual <- function (
    data = data,
    outvar = out_var,
    periods = 1:60,
    se_option = "avg",
    wins = wins_para,
    only_negative = TRUE,
    only_positive = FALSE
) {
  # Prepare data
  d_brodeur <- data %>% 
    filter(outcome == outvar, 
           period.month %in% periods) %>% 
    mutate(
      # Winsorization and SE option
      mean.effect = JWileymisc::winsorizor(mean.effect, percentile = wins),
      SE = case_when(
        se_option == "avg" ~ JWileymisc::winsorizor(SE.avg, percentile = wins),
        se_option == "upper" ~ JWileymisc::winsorizor(SE.upper, percentile = wins),
        se_option == "lower" ~ JWileymisc::winsorizor(SE.lower, percentile = wins)
      ),
      # Calculate z_stat (also absolute values)
      z_stat = mean.effect / SE,
      abs_z_stat = abs(mean.effect / SE)
    )
  
  # Extract only negative effects
  if (only_negative == TRUE & only_positive == FALSE) {
    d_brodeur <- d_brodeur %>% 
      filter(
        mean.effect <= 0
      )
  }
  # Extract only positive effects
  if (only_positive == TRUE & only_negative == FALSE) {
    d_brodeur <- d_brodeur %>% 
      filter(
        mean.effect >= 0
      )
  }
  return(d_brodeur)
}