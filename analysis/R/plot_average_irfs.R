#' Plot average impulse response functions (IRFs) with confidence bounds
#'
#' Creates a plot of average impulse response functions (IRFs) from a meta-analysis with confidence bounds.
#' Supports two methods for uncertainty visualization: approximated confidence intervals
#' or standard error bounds. Can display both mean and median responses with their
#' respective uncertainty bounds.
#'
#' @param data A data frame containing the following required columns:
#'   \itemize{
#'     \item period.month: Time periods for the IRF
#'     \item mean.effect: Point estimates
#'     \item approx.CI.lower_68, approx.CI.upper_68: approximated 68% confidence interval bounds
#'     \item approx.CI.lower_95, approx.CI.upper_95: approximated 95% confidence interval bounds
#'     \item SE.upper, SE.lower: Upper and lower standard errors
#'   }
#' @param period_limit Integer. Maximum number of periods to include in the plot. 
#'   Default is NULL (all periods).
#' @param winsor Logical. Whether to apply winsorization to the data. Default is FALSE.
#' @param wins_par Numeric. Percentile for winsorization when winsor = TRUE. Default is 0.
#' @param corrected_irf Optional data frame containing meta-analysis corrections with columns:
#'   period, estimate, lower, upper.
#' @param show_legend Logical. Whether to display the plot legend. Default is TRUE.
#' @param show_median Logical. Whether to display median IRF and its bounds. Default is FALSE.
#' @param show_CIs Logical. Whether to display confidence intervals. Default is TRUE.
#' @param show_percentiles Logical. Whether to display percentile bands (5%-95% and 32%-68%). Default is FALSE.
#' @param return_data Logical. Whether to return the processed data along with the plot. 
#'   Default is FALSE.
#' @param ci_method Character. Method for computing confidence bounds. Must be either 
#'   "approx.CIs" (approximated CIs) or "avg.se" (average standard error bounds). Default is "approx.CIs".
#' @param se_multiplier Numeric. Multiplier for standard errors when ci_method = "avg.se". 
#'   Default is 1.
#'
#' @return If return_data = FALSE (default), returns a plotly object.
#'   If return_data = TRUE, returns a list containing:
#'   \itemize{
#'     \item plot: The plotly visualization
#'     \item data: The processed data frame used for plotting
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with approximated CIs
#' plot1 <- plot_average_irfs(data)
#'
#' # Using SE bounds with 2 standard errors
#' plot2 <- plot_average_irfs(data, 
#'                           ci_method = "avg.se", 
#'                           se_multiplier = 2)
#'
#' # Show both mean and median with period limit
#' plot3 <- plot_average_irfs(data,
#'                           period_limit = 24,
#'                           show_median = TRUE)
#'
#' # Apply winsorization and return processed data
#' result <- plot_average_irfs(data,
#'                            winsor = TRUE,
#'                            wins_par = 0.01,
#'                            return_data = TRUE)
#' }
#'
#' @import dplyr
#' @import plotly
#'
#' @export
plot_average_irfs <- function(data, period_limit = NULL, winsor = FALSE, wins_par = 0, corrected_irf, corrected_irf_color = 'red', corrected_irf_name = "Meta analysis CI", corrected_irf_show_CIs = TRUE, show_legend = TRUE, show_median = FALSE, return_data = FALSE, ci_method = "approx.CIs", se_multiplier = 1, show_CIs = TRUE, show_percentiles = FALSE) {
  
  # Validate ci_method parameter
  if (!ci_method %in% c("approx.CIs", "avg.se")) {
    stop("ci_method must be either 'approx.CIs' or 'avg.se'")
  }
  
  # Apply winsorization (if selected) to the CIs, mean effect, and SEs
  if (winsor == TRUE) {
    data <- data %>%
      group_by(period.month) %>%
      mutate(
        approx.CI.lower_68 = winsorizor(approx.CI.lower_68, percentile = wins_par),
        approx.CI.upper_68 = winsorizor(approx.CI.upper_68, percentile = wins_par),
        approx.CI.lower_90 = winsorizor(approx.CI.lower_90, percentile = wins_par),
        approx.CI.upper_90 = winsorizor(approx.CI.upper_90, percentile = wins_par),
        approx.CI.lower_95 = winsorizor(approx.CI.lower_95, percentile = wins_par),
        approx.CI.upper_95 = winsorizor(approx.CI.upper_95, percentile = wins_par),
        mean.effect = winsorizor(mean.effect, percentile = wins_par),
        SE.upper = winsorizor(SE.upper, percentile = wins_par),
        SE.lower = winsorizor(SE.lower, percentile = wins_par)
      ) %>%
      ungroup()
  }
  
  average_irf <- data %>%
    group_by(period.month) %>%
    summarise(
      # Mean calculations
      avg.effect = mean(mean.effect, na.rm = TRUE),
      avg_CI.lower_68 = mean(approx.CI.lower_68, na.rm = TRUE),
      avg_CI.upper_68 = mean(approx.CI.upper_68, na.rm = TRUE),
      avg_CI.lower_90 = mean(approx.CI.lower_90, na.rm = TRUE),
      avg_CI.upper_90 = mean(approx.CI.upper_90, na.rm = TRUE),
      avg_CI.lower_95 = mean(approx.CI.lower_95, na.rm = TRUE),
      avg_CI.upper_95 = mean(approx.CI.upper_95, na.rm = TRUE),
      avg_SE.upper = mean(SE.upper, na.rm = TRUE),
      avg_SE.lower = mean(SE.lower, na.rm = TRUE),
      # Median calculations
      median.effect = median(mean.effect, na.rm = TRUE),
      median_CI.lower_68 = median(approx.CI.lower_68, na.rm = TRUE),
      median_CI.upper_68 = median(approx.CI.upper_68, na.rm = TRUE),
      median_CI.lower_90 = median(approx.CI.lower_90, na.rm = TRUE),
      median_CI.upper_90 = median(approx.CI.upper_90, na.rm = TRUE),
      median_CI.lower_95 = median(approx.CI.lower_95, na.rm = TRUE),
      median_CI.upper_95 = median(approx.CI.upper_95, na.rm = TRUE),
      median_SE.upper = median(SE.upper, na.rm = TRUE),
      median_SE.lower = median(SE.lower, na.rm = TRUE),
      # Percentile calculations
      percentile_2.5 = quantile(mean.effect, 0.025, na.rm = TRUE),
      percentile_16 = quantile(mean.effect, 0.16, na.rm = TRUE),
      percentile_84 = quantile(mean.effect, 0.84, na.rm = TRUE),
      percentile_97.5 = quantile(mean.effect, 0.975, na.rm = TRUE)
    )
  
  # Calculate avg_SE-based bounds
  if (ci_method == "avg.se") {
    average_irf <- average_irf %>%
      mutate(
        avg_bound_upper_se = avg.effect + avg_SE.upper,
        avg_bound_lower_se = avg.effect - avg_SE.lower,
        median_bound_upper_se = median.effect + median_SE.upper,
        median_bound_lower_se = median.effect - median_SE.lower
      )
    if (se_multiplier != 1) {
      average_irf <- average_irf %>%
        mutate(
          avg_bound_upper_se_multiplier = avg.effect + (se_multiplier * avg_SE.upper),
          avg_bound_lower_se_multiplier = avg.effect - (se_multiplier * avg_SE.lower),
          median_bound_upper_se_multiplier = median.effect + (se_multiplier * median_SE.upper),
          median_bound_lower_se_multiplier = median.effect - (se_multiplier * median_SE.lower)
        )
    }
  }

  # Apply period limit if specified
  if (!is.null(period_limit)) {
    average_irf <- average_irf %>% filter(period.month <= period_limit)
  }
  
  # Create base plot
  plot <- average_irf %>%
    plot_ly(showlegend = show_legend)
  
  # Add confidence bounds based on selected method
  if (show_CIs == TRUE) {
    if (ci_method == "approx.CIs") {
      plot <- plot %>%
        add_ribbons(
          x = ~period.month,
          ymin = ~avg_CI.lower_68,
          ymax = ~avg_CI.upper_68,
          name = "Avg. 68% CI",
          showlegend = F,
          line = list(color = 'rgba(0,0,0,0)'),
          fillcolor = 'rgba(135,206,250,0.5)'
        ) %>%
        add_ribbons(
          x = ~period.month,
          ymin = ~avg_CI.lower_95,
          ymax = ~avg_CI.upper_95,
          name = "Avg. 95% CI",
          showlegend = F,
          line = list(color = 'rgba(0,0,0,0)'),
          fillcolor = 'rgba(135,206,250,0.1)'
        )
    } else {  # ci_method == "avg.se"
      plot <- plot %>%
        add_ribbons(
          x = ~period.month,
          ymin = ~avg_bound_lower_se,
          ymax = ~avg_bound_upper_se,
          name = "1 avg. SE bound",
          showlegend = F,
          line = list(color = 'rgba(0,0,0,0)'),
          fillcolor = 'rgba(135,206,250,0.5)'
        )
      
      if (se_multiplier != 1) {
        plot <- plot %>%
          add_ribbons(
            x = ~period.month,
            ymin = ~avg_bound_lower_se_multiplier,
            ymax = ~avg_bound_upper_se_multiplier,
            name = paste(se_multiplier, "avg. SE bounds"),
            line = list(color = 'rgba(0,0,0,0)'),
            fillcolor = 'rgba(135,206,250,0.1)'
          )
      }
    }
  }
  
  # Add median if requested
  if (show_median == TRUE) {
    if (show_CIs == TRUE) {
      # Add median bounds based on selected method
      if (ci_method == "approx.CIs") {
        plot <- plot %>%
          add_ribbons(
            x = ~period.month,
            ymin = ~median_CI.lower_95,
            ymax = ~median_CI.upper_95,
            name = "Median 95% CI",
            showlegend = F,
            line = list(color = 'rgba(0,0,0,0)'),
            fillcolor = 'rgba(255,0,0,0.07)'
          ) %>%
          add_ribbons(
            x = ~period.month,
            ymin = ~median_CI.lower_68,
            ymax = ~median_CI.upper_68,
            name = "Median 68% CI",
            showlegend = F,
            line = list(color = 'rgba(0,0,0,0)'),
            fillcolor = 'rgba(255,0,0,0.2)'
          )
      } else { # ci_method == "avg.se"
        plot <- plot %>%
          add_ribbons(
            x = ~period.month,
            ymin = ~median_bound_lower_se,
            ymax = ~median_bound_upper_se,
            name = "1 median SE bound",
            showlegend = F,
            line = list(color = 'rgba(0,0,0,0)'),
            fillcolor = 'rgba(255,0,0,0.2)'
          )
        
        if (se_multiplier != 1) {
          plot <- plot %>%
            add_ribbons(
              x = ~period.month,
              ymin = ~median_bound_lower_se_multiplier,
              ymax = ~median_bound_upper_se_multiplier,
              name = paste(se_multiplier, "median SE bounds"),
              showlegend = F,
              line = list(color = 'rgba(0,0,0,0)'),
              fillcolor = 'rgba(255,0,0,0.07)'
            )
        }
      }
    }
    # Add median line
    plot <- plot %>%
      add_lines(
        x = ~period.month,
        y = ~median.effect,
        name = "Median effect from literature",
        line = list(color = 'rgba(255,0,0,0.8)', width = 2)
      )
  }
  
  # Add percentiles if requested
  if (show_percentiles == TRUE) {
    plot <- plot %>%
      add_ribbons(
        x = ~period.month,
        ymin = ~percentile_2.5,
        ymax = ~percentile_97.5,
        name = "2.5%-97.5% percentile range",
        showlegend = F,
        line = list(color = 'rgba(0,0,0,0)'),
        fillcolor = 'rgba(147, 112, 219, 0.1)'
      ) %>%
      add_ribbons(
        x = ~period.month,
        ymin = ~percentile_16,
        ymax = ~percentile_84,
        name = "16%-84% percentile range",
        showlegend = F,
        line = list(color = 'rgba(0,0,0,0)'),
        fillcolor = 'rgba(147, 112, 219, 0.3)'
      )
  }
  
  # Add average line
  plot <- plot %>%
    add_lines(
      x = ~period.month,
      y = ~avg.effect,
      name = "Mean effect from literature",
      line = list(color = 'rgba(0,76,153,1)', width = 3)
    )
  
  # Add corrected IRF with confidence bounds if provided
  if (!is.null(corrected_irf)) {
    if (corrected_irf_show_CIs == TRUE) {
      plot <- plot %>%
        add_ribbons(
          data = corrected_irf,
          x = ~period,
          ymin = ~lower,
          ymax = ~upper,
          name = corrected_irf_name,
          line = list(color = corrected_irf_color, width = 3, dash = 'dot'),
          fillcolor = paste0('rgba(', paste(col2rgb(corrected_irf_color), collapse=','), ',0.5)')
        )
    }
    
    plot <- plot %>% 
      add_lines(
        data = corrected_irf,
        x = ~period,
        y = ~estimate,
        name = corrected_irf_name,
        line = list(color = corrected_irf_color, width = 3, dash = 'dot')
      )
    
  }
  
  # Add layout
  plot <- plot %>%
    layout(
      title = "Average IRF",
      xaxis = list(title = "Period (Months)"),
      yaxis = list(title = ""),
      hovermode = "compare"
    )
  
  # Optionally return the data alongside the plot
  if (return_data) {
    if (!is.null(corrected_irf)) {
      # Rename corrected_irf columns to avoid conflicts
      corrected_data <- corrected_irf %>%
        rename(
          corrected_estimate = estimate,
          corrected_lower = lower,
          corrected_upper = upper
        ) %>%
        rename(period.month = period)  # Align with average_irf column name
      
      # Join with average_irf
      combined_data <- left_join(average_irf, corrected_data, by = "period.month")
      return(list(plot = plot, data = combined_data))
    }
    
    return(list(plot = plot, data = average_irf))
  }
  
  plot
}