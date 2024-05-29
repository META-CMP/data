#' Plot average impulse response function (IRFs)
#'
#' This function creates a plot of an average IRF with confidence intervals.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param selected_outcome_var A string specifying the outcome_var variable to filter the data. If "All", no filtering is applied.
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
#' # Test with selected_outcome_var = "All" and default period_limit
#' plot_all_outcomes <- plot_average_irfs(data, selected_outcome_var = "All")
#' print(plot_all_outcomes)
#'
#' # Test with a specific outcome variable and custom period_limit
#' selected_outcome_var <- "log_m_gdp"
#' period_limit <- 20
#' plot_specific_outcome <- plot_average_irfs(data, selected_outcome_var, period_limit)
#' print(plot_specific_outcome)
#'
#' @export
plot_average_irfs <- function(data, selected_outcome_var = "All", period_limit = NULL) {
  if (selected_outcome_var == "All") {
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
  } else {
    average_irf <- data %>%
      filter(outcome_var == selected_outcome_var) %>%
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
  }
  
  if (!is.null(period_limit)) {
    average_irf <- average_irf %>% filter(period.month <= period_limit)
  }
  
  plot <- average_irf %>%
    plot_ly() %>%
    add_ribbons(
      x = ~period.month,
      ymin = ~avg_CI.lower_68,
      ymax = ~avg_CI.upper_68,
      name = "68% CI",
      line = list(color = 'rgba(0,0,0,0)'),
      fillcolor = 'rgba(135,206,250,0.2)'
    ) %>%
    add_ribbons(
      x = ~period.month,
      ymin = ~avg_CI.lower_90,
      ymax = ~avg_CI.upper_90,
      name = "90% CI",
      line = list(color = 'rgba(0,0,0,0)'),
      fillcolor = 'rgba(135,206,250,0.2)'
    ) %>%
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
      name = "Effect",
      line = list(color = 'rgba(0,76,153,0.5)', width = 2)
    ) %>%
    layout(
      title = if (selected_outcome_var == "All") "Average IRFs" else paste("IRFs for", selected_outcome_var),
      xaxis = list(title = "Period (Months)"),
      yaxis = list(title = "Effect"),
      hovermode = "compare"
    )
  
  plot
}