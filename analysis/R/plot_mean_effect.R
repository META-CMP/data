#' Plot mean effect sizes
#'
#' This function creates a scatter plot of mean effect sizes with optional color coding by outcome or outcome_var.
#' The x-axis can be set to either the index of the models or the IRF periods.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param filter_outcome A string specifying the outcome to filter the data. If "All", no filtering is applied.
#' @param x_axis A string specifying the variable to use on the x-axis. Can be "model_index" or "period_month".
#'
#' @return A plotly scatter plot of mean effect sizes.
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
#' # Test with filter_outcome = "All" and x_axis = "model_index"
#' plot_model_index <- plot_mean_effect(data, filter_outcome = "All", x_axis = "model_index")
#' print(plot_model_index)
#'
#' # Test with filtering the data and x_axis = "period_month"
#' input.filter_outcome <- "inflation"
#' filtered_data <- data %>% filter(outcome == "inflation")
#' plot_period_month <- plot_mean_effect(filtered_data, filter_outcome = input.filter_outcome, x_axis = "period_month")
#' print(plot_period_month)
#'
#' @export
plot_mean_effect <- function(data, filter_outcome = "All", x_axis = "model_index") {
  if (filter_outcome == "All") {
    color_var <- ~outcome
  } else {
    color_var <- ~outcome_var
  }
  
  if (x_axis == "model_index") {
    x_var <- ~1:length(mean.effect)
    x_label <- "Index"
  } else {
    x_var <- ~period.month
    x_label <- "Period (Months)"
  }
  
  plot_ly(data,
          x = x_var,
          y = ~mean.effect,
          type = 'scatter',
          mode = 'markers',
          color = color_var,
          marker = list(opacity = 0.25),
          text = ~paste("Key:", key,
                        "<br>Model ID:", model_id,
                        "<br>Outcome Var:", outcome_var),
          hoverinfo = 'text') %>%
    layout(title = "Effect Sizes",
           xaxis = list(title = x_label),
           yaxis = list(title = "Effect"))
}
