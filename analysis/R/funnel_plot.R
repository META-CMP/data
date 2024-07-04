#' Create funnel plots
#'
#' This function creates a funnel plot for a specific outcome variable and period, with optional Winsorization.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param outvar A string specifying the outcome variable to filter the data.
#' @param prd A numeric value specifying the period (in months) to filter the data.
#' @param se_option A string specifying the standard error option to use. Can be "avg", "lower", or "upper".
#' @param wins A numeric value specifying the Winsorization parameter.
#' @param legend If TRUE (default), the legend will be shown.
#' @param opac A numeric value specifying the opacity parameter for the plot.
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
#' # Create a funnel plot for the "gdp" outcome variable at period 3, using average standard error and Winsorization of 0.1 and opacity of 0.12
#' funnel_plot <- create_funnel_plot(data, outvar = "gdp", prd = 3, se_option = "avg", wins = 0.1, opac = 0.12)
#' print(funnel_plot)
#'
#' @export
create_funnel_plot <- function(data, outvar, prd, se_option = "avg", wins = 0.02, legend = TRUE, opac = 0.15, ap = FALSE) {
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
  data_filtered <- apply_winsorization(data_filtered, wins)
  
  # Filter adequately powered if ap == TRUE
  if (ap == TRUE) {
    data_filtered$ap <- ifelse(data_filtered$standarderror_winsor <= abs(data_filtered$mean.effect_winsor)/2.8, 1, 0)
    data_filtered <- data_filtered %>% 
      filter(ap == 1)
  }
  
  # Get the unique outcome variable values
  unique_outcomes <- unique(data_filtered$outcome_var)
  
  # Create a named vector of colors from the viridis palette
  palette_colors <- setNames(viridis(length(unique_outcomes)), unique_outcomes)
  
  # Create the funnel plot with Winsorized data and color points based on outcome_var
  plot_funnel <- plot_ly(data = data_filtered, 
                         x = ~mean.effect_winsor, 
                         y = ~precision_winsor,
                         color = ~outcome_var, 
                         colors = palette_colors, 
                         type = "scatter", 
                         mode = "markers",
                         marker = list(size = 10, opacity = opac),
                         hoverinfo = "text",
                         hovertext = ~paste("Key:", key,
                                            "<br>Model ID:", model_id,
                                            "<br>Outcome Var:", outcome_var)) %>%
    layout(title = paste(outvar, ",", prd, "months after shock"),
           xaxis = list(title = "Effect size"),
           yaxis = list(title = " Precision"),
           shapes = list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                         line = list(color = "black", width = 1, dash = "dash")),
           showlegend = legend)
  
  # Return the funnel plot
  return(plot_funnel)
}

