#' Create funnel plots
#'
#' This function creates a funnel plot for a specific outcome variable and period, with optional Winsorization.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param outvar A string specifying the outcome variable to filter the data.
#' @param prd A numeric value specifying the period (in months) to filter the data.
#' @param se_option A string specifying the standard error option to use. Can be "avg", "lower", or "upper".
#' @param wins A numeric value specifying the Winsorization parameter.
#'
#' @return A plotly scatter plot of the funnel plot.
#'
#' @import plotly
#' @import dplyr
#' @import tidyr
#' @import JWileymisc
#' @import viridis
#'
#' @examples
#' library(dplyr)
#' library(plotly)
#' library(JWileymisc)
#' library(viridis)
#'
#' # Load the data
#' data_path <- "data/preliminary_data.RData"
#' load(data_path)
#'
#' # Calculate the average standard error and precision options
#' data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
#' data$precision.avg <- 1 / data$SE.avg
#' data$precision.lower <- 1 / data$SE.lower
#' data$precision.upper <- 1 / data$SE.upper
#'
#' # Create a funnel plot for the "emp" outcome variable at period 3, using average standard error and Winsorization of 0.1
#' funnel_plot <- create_funnel_plot(data, outvar = "emp", prd = 3, se_option = "avg", wins = 0.1)
#' print(funnel_plot)
#'
#' @export
create_funnel_plot <- function(data, outvar, prd, se_option = "avg", wins = 0.02) {
  # Filter the data for the specific period and outcome variable
  data_filtered <- data %>%
    filter(period.month == prd, outcome == outvar)
  
  # Select the corresponding standard error and precision columns based on the se_option
  if (se_option == "avg") {
    data_filtered$StandardError <- data_filtered$SE.avg
    data_filtered$precision <- data_filtered$precision.avg
  } else if (se_option == "lower") {
    data_filtered$StandardError <- data_filtered$SE.lower
    data_filtered$precision <- data_filtered$precision.lower
  } else if (se_option == "upper") {
    data_filtered$StandardError <- data_filtered$SE.upper
    data_filtered$precision <- data_filtered$precision.upper
  }
  
  # Apply Winsorization to the standard error, mean effect, and precision
  data_filtered$standarderror_winsor <- winsorizor(data_filtered$StandardError, c(wins), na.rm = TRUE)
  data_filtered$mean.effect_winsor <- winsorizor(data_filtered$mean.effect, c(wins), na.rm = TRUE)
  data_filtered$precision_winsor <- 1 / data_filtered$standarderror_winsor
  
  # Get the unique outcome variable values
  unique_outcomes <- unique(data_filtered$outcome_var)
  
  # Create a named vector of colors from the viridis palette
  palette_colors <- setNames(viridis(length(unique_outcomes)), unique_outcomes)
  
  # Create the funnel plot with Winsorized data and color points based on outcome_var
  plot_funnel <- plot_ly(data = data_filtered, x = ~mean.effect_winsor, y = ~precision_winsor,
                         color = ~outcome_var, colors = palette_colors, type = "scatter", mode = "markers",
                         marker = list(size = 10, opacity = 0.15),
                         hoverinfo = "text",
                         hovertext = ~paste("Key:", key,
                                            "<br>Model ID:", model_id,
                                            "<br>Outcome Var:", outcome_var)) %>%
    layout(title = paste("Funnel plot for", outvar, "at", prd, "months after the shock (Winsorized)"),
           xaxis = list(title = "Winsorized Effect Size"),
           yaxis = list(title = "Winsorized Inverse of Standard Error (Precision)"),
           shapes = list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                         line = list(color = "black", width = 1, dash = "dash")))
  
  # Return the funnel plot
  return(plot_funnel)
}