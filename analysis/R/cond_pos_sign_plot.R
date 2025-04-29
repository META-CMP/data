#' Create a Conditional Positive Sign plot from IRF BMA results 
#'
#' This function creates a line plot of Conditional Positive Sign values across periods
#' for selected variables from Bayesian Model Averaging (BMA) results for an IRF dataset.
#'
#' @param results A list containing BMA results, specifically including a dataframe
#'   named 'Cond.Pos.Sign' within a 'dataframes' list.
#' @param variables A character vector of variable names to be plotted.
#'
#' @return A plotly object representing the Conditional Positive Sign plot.
#'
#' @examples
#' variables_of_interest <- c("SE2", "group_est_broad_lp_ardl")
#' cond_pos_sign_plot(results_output_bma_1, variables_of_interest)
cond_pos_sign_plot <- function(results, variables) {
  # Extract the Cond.Pos.Sign dataframe
  cps_df <- results$dataframes$`Cond.Pos.Sign`
  
  # Filter the dataframe for the target variables
  filtered_df <- cps_df %>%
    filter(Variable %in% variables)
  
  # Create and return the plot
  plot_ly(filtered_df, x = ~Period, color = ~Variable) %>%
    add_lines(y = ~`Cond.Pos.Sign`, hoverinfo = "text",
              text = ~paste("Variable:", Variable, 
                            "<br>Period:", Period, 
                            "<br>Cond.Pos.Sign:", round(`Cond.Pos.Sign`, 4))) %>%
    layout(title = "Conditional positive sign across IRF periods for selected variables",
           xaxis = list(title = "Period"),
           yaxis = list(title = "Cond.Pos.Sign"))
}
