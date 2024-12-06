#' Run Multiple Randomization Tests for P-Hacking Analysis
#' 
#' @description
#' Runs multiple randomization tests across different thresholds and window sizes,
#' optionally comparing across groups. Implements the analysis framework from
#' Brodeur et al. (2020).
#'
#' @param data A data frame containing test statistics
#' @param statistic Character. Name of column containing test statistics
#' @param thresholds Numeric vector. Thresholds to test (default = c(1.65, 1.96, 2.58))
#' @param deltas Numeric vector. Window half-widths (default as in Brodeur et al.)
#' @param group Optional character. Name of primary grouping variable
#' @param group_comparison Optional character. Name of variable to compare across
#'
#' @return A data frame containing test results for all combinations
#'
#' @examples
#' \dontrun{
#' # Basic battery of tests
#' results <- randomization_test_battery(data, "z_stat")
#' 
#' # Compare across groups
#' results_by_type <- randomization_test_battery(
#'   data, "z_stat",
#'   group = "horizon",
#'   group_comparison = "journal_type"
#' )
#' }
#'
#' @references 
#' Brodeur, A., Cook, N., & Heyes, A. (2020). Methods Matter: P-Hacking and 
#' Publication Bias in Causal Analysis in Economics. American Economic Review, 
#' 110(11), 3634-60.
#'
#' @export
randomization_test_battery <- function(data,
                                       statistic,
                                       thresholds = c(1.65, 1.96, 2.58),
                                       deltas = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.075, 0.05),
                                       group = NULL,
                                       group_comparison = NULL) {
  
  # Get group values if specified
  group_values <- if(!is.null(group)) unique(data[[group]]) else "All"
  comparison_values <- if(!is.null(group_comparison)) unique(data[[group_comparison]]) else NULL
  
  # Create results storage
  results <- expand.grid(
    threshold = thresholds,
    delta = deltas,
    group = if(!is.null(group_values)) group_values else NA,
    comparison = if(!is.null(comparison_values)) comparison_values else NA,
    proportion = NA_real_,
    pvalue = NA_real_,
    n = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  # Run tests for all combinations
  for(i in 1:nrow(results)) {
    # Split by comparison group if specified
    if(!is.null(group_comparison)) {
      test_data <- data[data[[group_comparison]] == results$comparison[i],]
    } else {
      test_data <- data
    }
    
    # Run test
    test_result <- run_randomization_test(
      data = test_data,
      statistic = statistic,
      threshold = results$threshold[i],
      delta = results$delta[i],
      group = group,
      group_value = results$group[i]
    )
    
    # Store results
    results$proportion[i] <- test_result$proportion
    results$pvalue[i] <- test_result$pvalue
    results$n[i] <- test_result$n
  }
  
  return(results)
}
