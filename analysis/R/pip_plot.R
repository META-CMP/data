#' Create a PIP (Posterior Inclusion Probability) plot
#'
#' This function creates a line plot of PIP values across periods for selected variables
#' from Bayesian Model Averaging (BMA) results.
#'
#' @param results A list containing BMA results, specifically including a dataframe
#'   named 'PIP' within a 'dataframes' list.
#' @param variables A character vector of variable names to be plotted.
#'
#' @return A plotly object representing the PIP plot.
#'
#' @examples
#' variables_of_interest <- c("SE2", "group_est_broad_lp_ardl")
#' pip_plot(results_output_bma_1, variables_of_interest)
pip_plot <- function(results, variables) {
  # Extract the PIP dataframe
  pip_df <- results$dataframes$PIP
  
  # Filter the dataframe for the target variables
  filtered_df <- pip_df %>%
    filter(Variable %in% variables)
  
  # Create and return the plot
  plot_ly(filtered_df, x = ~Period, color = ~Variable) %>%
    add_lines(y = ~PIP, hoverinfo = "text",
              text = ~paste("Variable:", Variable, 
                            "<br>Period:", Period, 
                            "<br>PIP:", round(PIP, 4))) %>%
    layout(title = "PIP across IRF periods for selected variables",
           xaxis = list(title = "Period"),
           yaxis = list(title = "PIP"))
}