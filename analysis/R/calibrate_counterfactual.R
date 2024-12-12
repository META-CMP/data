#' Calibrate Counterfactual Distribution for P-Hacking Analysis
#' 
#' @description
#' Calibrates a non-central t distribution to serve as counterfactual for 
#' p-hacking analysis following Brodeur et al. (2020). The calibration uses 
#' data above a threshold (default z=5) assuming this part of the distribution 
#' is free from p-hacking.
#'
#' @param data A data frame containing test statistics
#' @param statistic Character. Name of column containing test statistics
#' @param group Optional character. Name of grouping variable
#' @param group_value Optional. Value of grouping variable to use
#' @param threshold Numeric. Test statistics threshold above which to calibrate 
#' (default = 5 as in Brodeur et al. 2020)
#'
#' @return A list containing:
#'   \item{df}{Degrees of freedom of calibrated distribution}
#'   \item{ncp}{Non-centrality parameter of calibrated distribution}
#'   \item{fit_error}{Goodness of fit measure}
#'   \item{n}{Number of observations used}
#'   \item{empirical_mass}{Proportion of observations above threshold}
#'
#' @examples
#' \dontrun{
#' data(brodeur_data) # Available through the replication package (AER website)
#' 
#' # Calibrate for all data
#' cf <- calibrate_counterfactual(brodeur_data, statistic = "t")
#' 
#' # Calibrate by method
#' cf_did <- calibrate_counterfactual(brodeur_data, 
#'                                   statistic = "t",
#'                                   group = "method", 
#'                                   group_value = "DID")
#' }
#'
#' @references 
#' Brodeur, A., Cook, N., & Heyes, A. (2020). Methods Matter: P-Hacking and 
#' Publication Bias in Causal Analysis in Economics. American Economic Review, 
#' 110(11), 3634-60.
#'
#' @export
calibrate_counterfactual <- function(data, 
                                     statistic,
                                     group = NULL,
                                     group_value = NULL,
                                     threshold = 5) {
  
  # Input validation
  if(!statistic %in% names(data)) {
    stop("statistic must be a column in data")
  }
  
  if(!is.null(group) && !group %in% names(data)) {
    stop("group must be a column in data")
  }
  
  # Get relevant data
  if(is.null(group)) {
    method_data <- data[[statistic]]
    group_label <- "all data"
  } else {
    if(is.null(group_value)) {
      stop("group_value must be specified when group is specified")
    }
    method_data <- data[[statistic]][data[[group]] == group_value]
    group_label <- paste(group, "=", group_value)
  }
  
  # Calculate empirical mass above threshold
  n_total <- length(method_data)
  n_above <- sum(method_data > threshold)
  empirical_mass <- n_above / n_total
  
  # Initialize storage
  best_fit <- list(
    df = NA_integer_,
    ncp = NA_real_,
    fit_error = Inf,
    n = n_total,
    empirical_mass = empirical_mass
  )
  
  # For each df
  for(df in 1:10) {
    # Grid search over np values
    np_fits <- data.frame(
      np = seq(0, 3.5, by = 0.01),
      fit_diff = NA_real_
    )
    
    # Calculate fit for each np
    for(i in seq_along(np_fits$np)) {
      ncp <- np_fits$np[i]
      # Calculate survival function at threshold
      sdf_t <- 1 - (pt(threshold, df = df, ncp = ncp) - pt(0, df = df, ncp = ncp)) / 
        (1 - pt(0, df = df, ncp = ncp))
      np_fits$fit_diff[i] <- empirical_mass - sdf_t
    }
    
    # Find where difference crosses zero
    cross_idx <- which(diff(sign(np_fits$fit_diff)) != 0)
    if(length(cross_idx) > 0) {
      best_np <- np_fits$np[cross_idx[1]]
      
      # Evaluate full fit with these parameters
      t_seq <- sort(method_data)
      theoretical_cdf <- (pt(t_seq, df = df, ncp = best_np) - pt(0, df = df, ncp = best_np)) / 
        (1 - pt(0, df = df, ncp = best_np))
      empirical_cdf <- ecdf(method_data)(t_seq)
      
      total_fit_error <- mean(abs(theoretical_cdf - empirical_cdf))
      
      if(total_fit_error < best_fit$fit_error) {
        best_fit$df <- df
        best_fit$ncp <- best_np
        best_fit$fit_error <- total_fit_error
      }
    }
  }
  
  return(best_fit)
}