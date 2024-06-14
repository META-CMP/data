#' Plot model or study counts for each IRF period
#'
#' This function creates a bar plot showing the number of models or studies for each IRF period.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param plot_type A string specifying the type of plot to create. Can be "model" or "study".
#'
#' @return A plotly bar plot of model or study counts for each IRF period.
#'
#' @import plotly
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' library(plotly)
#' # Load the data
#' data_path <- "data/preliminary_data.RData"
#' load(data_path)
#'
#' # Create the model count plot
#' plot_model_study_counts(data, plot_type = "model")
#'
#' # Create the study count plot
#' plot_model_study_counts(data, plot_type = "study")
#'
#' @export
plot_model_study_counts <- function(data, plot_type = "model") {
  if (plot_type == "model") {
    counts <- data %>%
      group_by(period.month) %>%
      summarise(count = n_distinct(key, model_id))
    
    title <- "Number of Models per IRF Period (Distinct by Study)"
  } else if (plot_type == "study") {
    counts <- data %>%
      group_by(period.month) %>%
      summarise(count = n_distinct(key))
    
    title <- "Number of Studies per IRF Period"
  } else {
    stop("Invalid plot type. Choose either 'model' or 'study'.")
  }
  
  plot <- plot_ly(counts, x = ~period.month, y = ~count, type = 'bar') %>%
    layout(
      title = title,
      xaxis = list(title = "Period (Months)"),
      yaxis = list(title = "Count")
    )
  
  plot
}