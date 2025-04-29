#' Create a Post Mean plot from IRF BMA results 
#'
#' This function creates a line plot of Post Mean values across periods for selected
#' variables from Bayesian Model Averaging (BMA) results, including confidence intervals.
#'
#' @param results A list containing BMA results, specifically including a dataframe
#'   named 'Post Mean' within a 'dataframes' list.
#' @param variables A character vector of variable names to be plotted.
#'
#' @return A plotly object representing the Post Mean plot.
#'
#' @examples
#' variables_of_interest <- c("SE2", "group_est_broad_lp_ardl")
#' post_mean_plot(results_output_bma_1, variables_of_interest)
post_mean_plot <- function(results, variables) {
  # Extract the Post Mean dataframe
  post_mean_df <- results$dataframes$`Post Mean`
  
  # Filter the dataframe for the target variables
  filtered_df <- post_mean_df %>%
    filter(Variable %in% variables)
  
  # Create and return the plot
  plot_ly(filtered_df, x = ~Period, color = ~Variable) %>%
    add_ribbons(ymin = ~Lower, ymax = ~Upper, line = list(color = "transparent"), 
                showlegend = FALSE, hoverinfo = "none") %>%
    add_lines(y = ~`Post Mean`, hoverinfo = "text",
              text = ~paste("Variable:", Variable, 
                            "<br>Period:", Period, 
                            "<br>Post Mean:", round(`Post Mean`, 4),
                            "<br>Post SD:", round(`Post SD`, 4))) %>%
    layout(title = "Post Mean across IRF periods for selected variables",
           xaxis = list(title = "Period"),
           yaxis = list(title = "Post Mean"))
}