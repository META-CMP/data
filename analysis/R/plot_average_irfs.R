#' Plot average impulse response function (IRFs)
#'
#' This function creates a plot of an average IRF with confidence intervals.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param period_limit An integer specifying the maximum number of periods to include in the plot.
#'
#' @return A plotly plot of average IRFs.
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
#' # Test with default period_limit
#' plot_all_outcomes <- plot_average_irfs(data)
#' print(plot_all_outcomes)
#'
#' # Test with custom period_limit
#' period_limit <- 20
#' plot_period_limit <- plot_average_irfs(data, period_limit)
#' print(plot_period_limit)
#'
#' @export
plot_average_irfs <- function(data, period_limit = NULL, winsor = FALSE, wins_par = 0, corrected_irf,show_legend = T) {
  
  # Apply winsorization (if selected) to the CIs and mean effect
  if (winsor == TRUE) {
    data$approx.CI.lower_68 <- winsorizor(data$approx.CI.lower_68, percentile = wins_par)
    data$approx.CI.upper_68 <- winsorizor(data$approx.CI.upper_68, percentile = wins_par)
    data$approx.CI.lower_90 <- winsorizor(data$approx.CI.lower_90, percentile = wins_par)
    data$approx.CI.upper_90 <- winsorizor(data$approx.CI.upper_90, percentile = wins_par)
    data$approx.CI.lower_95 <- winsorizor(data$approx.CI.lower_95, percentile = wins_par)
    data$approx.CI.upper_95 <- winsorizor(data$approx.CI.upper_95, percentile = wins_par)
    data$mean.effect <- winsorizor(data$mean.effect, percentile = wins_par)
  }
  
  average_irf <- data %>%
    group_by(period.month) %>%
    summarise(
      avg_mean.effect = mean(mean.effect, na.rm = TRUE),
      avg_CI.lower_68 = mean(approx.CI.lower_68, na.rm = TRUE),
      avg_CI.upper_68 = mean(approx.CI.upper_68, na.rm = TRUE),
      avg_CI.lower_90 = mean(approx.CI.lower_90, na.rm = TRUE),
      avg_CI.upper_90 = mean(approx.CI.upper_90, na.rm = TRUE),
      avg_CI.lower_95 = mean(approx.CI.lower_95, na.rm = TRUE),
      avg_CI.upper_95 = mean(approx.CI.upper_95, na.rm = TRUE)
    )

  if (!is.null(period_limit)) {
    average_irf <- average_irf %>% filter(period.month <= period_limit)
  }
  
  plot <- average_irf %>%
    plot_ly(showlegend=show_legend) %>%
    add_ribbons(
      x = ~period.month,
      ymin = ~avg_CI.lower_68,
      ymax = ~avg_CI.upper_68,
      name = "68% CI",
      line = list(color = 'rgba(0,0,0,0)'),
      fillcolor = 'rgba(135,206,250,0.2)'
    ) %>%
    # add_ribbons(
    #   x = ~period.month,
    #   ymin = ~avg_CI.lower_90,
    #   ymax = ~avg_CI.upper_90,
    #   name = "90% CI",
    #   line = list(color = 'rgba(0,0,0,0)'),
    #   fillcolor = 'rgba(135,206,250,0.2)'
    # ) %>%
    add_ribbons(
      x = ~period.month,
      ymin = ~avg_CI.lower_95,
      ymax = ~avg_CI.upper_95,
      name = "95% CI",
      line = list(color = 'rgba(0,0,0,0)'),
      fillcolor = 'rgba(135,206,250,0.2)'
    ) %>%
    add_lines(
      x = ~period.month,
      y = ~avg_mean.effect,
      name = "Average effect",
      line = list(color = 'rgba(0,76,153,0.5)', width = 2)
    ) %>%
    layout(
      title = "Average IRF",
      xaxis = list(title = "Period (Months)"),
      yaxis = list(title = "Effect"),
      hovermode = "compare"
    )
  
  # Add corrected IRF with confidence bounds if provided
  if (!is.null(corrected_irf)) {
    plot <- plot %>%
      add_ribbons(
        data = corrected_irf,
        x = ~period,
        ymin = ~lower,
        ymax = ~upper,
        name = "Meta analysis CI",
        line = list(color = 'rgba(0,0,0,0)'),
        fillcolor = 'rgba(255,0,0,0.2)'
      )
    
    plot <- plot %>% 
      add_lines(
        data = corrected_irf,
        x = ~period,
        y = ~estimate,
        name = "Meta analysis",
        line = list(color = 'red', width = 3, dash = 'dash')
      )
    
  }
  
  plot
}