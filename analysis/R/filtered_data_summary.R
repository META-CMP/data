#' Generate Filtered Data Summary
#'
#' This function generates a summary of the filtered data, including the count of unique studies,
#' unique models, and total datapoints.
#'
#' @param data A data frame containing the filtered data.
#'
#' @return A data frame with two columns: "Metric" (containing the labels for the summary information)
#'         and "Count" (containing the corresponding counts).
#'
#' @examples
#' # Create an example dataset
#' example_data <- data.frame(
#'   key = c("Study1", "Study1", "Study1", "Study2", "Study2", "Study3"),
#'   model_id = c("Model1", "Model2", "Model3", "Model1", "Model2", "Model1"),
#'   value = rnorm(6)
#' )
#'
#' # Generate the filtered data summary for the example dataset
#' summary <- filtered_data_summary(example_data)
#' print(summary)
#'
#' # Load the actual data
#' data_path <- "data/preliminary_data.RData"
#' load(data_path)
#'
#' # Generate the filtered data summary for the actual data
#' summary <- filtered_data_summary(data)
#' print(summary)
#'
#' @export
filtered_data_summary <- function(data) {
  unique_studies <- length(unique(data$key))
  unique_models <- length(unique(paste(data$key, data$model_id)))
  total_datapoints <- nrow(data)
  
  data.frame(
    Metric = c("Studies", "Models", "Total datapoints"),
    Count = c(unique_studies, unique_models, total_datapoints)
  )
}