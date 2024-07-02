#' Apply precision filtering to a dataset
#'
#' This function filters a dataset based on precision values according to specified criteria and method.
#'
#' @param data A data frame containing the dataset to be filtered.
#' @param precision_filter A string specifying the type of precision filter to apply. 
#'        Can be "Above", "Below", "Top Percentile", or "Bottom Percentile".
#' @param precision_col A string specifying the name of the column containing precision values.
#' @param threshold A numeric value specifying the threshold for "Above" and "Below" filters,
#'        or the percentile for "Top Percentile" and "Bottom Percentile" filters.
#' @param method A string specifying the filtering method. Can be "all" (default), "by_period", or "by_model".
#' @param model_precision A string specifying which precision value to use for the "by_model" method.
#'        Can be "min" (default), "max", or "avg".
#'
#' @return A filtered data frame.
#'
#' @import dplyr
#'
#' @examples
#' # Create a sample dataset
#' set.seed(123)
#' sample_data <- data.frame(
#'   key = rep(c("study1", "study2"), each = 60),
#'   model_id = rep(rep(1:3, each = 20), 2),
#'   period = rep(1:20, 6),
#'   mean.effect = rnorm(120, mean = 0, sd = 1),
#'   precision.avg = runif(120, 0, 100)
#' )
#'
#' # Apply different types of precision filters with different methods and model precision options
#' filtered_by_model_min <- precision_filter(sample_data, "Top Percentile", "precision.avg", 10, 
#'                                                 method = "by_model", model_precision = "min")
#' filtered_by_model_max <- precision_filter(sample_data, "Top Percentile", "precision.avg", 10, 
#'                                                 method = "by_model", model_precision = "max")
#' filtered_by_model_avg <- precision_filter(sample_data, "Top Percentile", "precision.avg", 10, 
#'                                                 method = "by_model", model_precision = "avg")
#'
#' # Check the results
#' nrow(filtered_by_model_min)
#' nrow(filtered_by_model_max)
#' nrow(filtered_by_model_avg)
#'
#' @export
precision_filter <- function(data, precision_filter, precision_col, threshold, method = "all", model_precision = "min") {
  
  required_cols <- c("key", "model_id", "period", "mean.effect", precision_col)
  if (!all(required_cols %in% names(data))) {
    missing_cols <- setdiff(required_cols, names(data))
    stop(paste("Required columns missing from the dataset:", paste(missing_cols, collapse = ", ")))
  }
  
  if (!method %in% c("all", "by_period", "by_model")) {
    stop("Invalid method. Must be 'all', 'by_period', or 'by_model'.")
  }
  
  if (!model_precision %in% c("min", "max", "avg")) {
    stop("Invalid model_precision. Must be 'min', 'max', or 'avg'.")
  }
  
  filter_data <- function(df, prec_col) {
    if (precision_filter %in% c("Above", "Below")) {
      if (is.na(as.numeric(threshold))) {
        stop("Threshold must be a numeric value for 'Above' and 'Below' filters.")
      }
      threshold <- as.numeric(threshold)
      if (precision_filter == "Above") {
        df %>% dplyr::filter(!!rlang::sym(prec_col) > threshold)
      } else if (precision_filter == "Below") {
        df %>% dplyr::filter(!!rlang::sym(prec_col) < threshold)
      }
    } else if (precision_filter %in% c("Top Percentile", "Bottom Percentile")) {
      if (is.na(as.numeric(threshold)) || threshold < 0 || threshold > 100) {
        stop("Threshold must be a numeric value between 0 and 100 for percentile filters.")
      }
      percentile <- as.numeric(threshold) / 100
      if (precision_filter == "Top Percentile") {
        precision_threshold <- stats::quantile(df[[prec_col]], 1 - percentile, na.rm = TRUE)
        df %>% dplyr::filter(!!rlang::sym(prec_col) >= precision_threshold)
      } else if (precision_filter == "Bottom Percentile") {
        precision_threshold <- stats::quantile(df[[prec_col]], percentile, na.rm = TRUE)
        df %>% dplyr::filter(!!rlang::sym(prec_col) <= precision_threshold)
      }
    } else {
      stop("Invalid precision_filter. Must be 'Above', 'Below', 'Top Percentile', or 'Bottom Percentile'.")
    }
  }
  
  if (method == "all") {
    data %>% filter_data(precision_col)
  } else if (method == "by_period") {
    data %>% 
      dplyr::group_by(period) %>% 
      dplyr::group_modify(~ filter_data(.x, precision_col)) %>% 
      dplyr::ungroup()
  } else if (method == "by_model") {
    model_precision_fn <- switch(model_precision,
                                 "min" = min,
                                 "max" = max,
                                 "avg" = mean)
    
    filtered_models <- data %>%
      dplyr::group_by(key, model_id) %>%
      dplyr::summarize(model_precision = model_precision_fn(!!rlang::sym(precision_col), na.rm = TRUE), 
                       .groups = "drop") %>%
      filter_data("model_precision") %>%
      dplyr::select(key, model_id)
    
    data %>%
      dplyr::inner_join(filtered_models, by = c("key", "model_id"))
  }
}