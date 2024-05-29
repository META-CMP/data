#' Plot study-specific impulse response functions (IRFs)
#'
#' This function creates a subplot of study-specific IRFs with optional approximated confidence intervals.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param study_keys A vector of study keys to include in the plot.
#' @param selected_model A string specifying the model ID to filter the data. If "All", no filtering is applied.
#' @param show_approx_bounds A logical value indicating whether to show approximated confidence bounds.
#'
#' @return A plotly subplot of study-specific IRFs.
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
#' # Test with specific study keys and selected_model = "All"
#' study_keys <- c("SU3IFG3P", "J7MUSSTN")
#' plot_all_models <- plot_study_irfs(data, study_keys, selected_model = "All",show_approx_bounds = FALSE)
#' print(plot_all_models)
#'
#' # Test with specific study keys and a selected model
#' selected_model <- "1"
#' plot_selected_model <- plot_study_irfs(data, study_keys, selected_model, show_approx_bounds = TRUE)
#' print(plot_selected_model)
#'
#' @export
plot_study_irfs <- function(data, study_keys, selected_model = "All", show_approx_bounds = FALSE) {
  filtered_data <- data %>% filter(key %in% study_keys)
  
  if (selected_model != "All") {
    filtered_data <- filtered_data %>% filter(model_id == selected_model)
  }
  
  plots_list <- list()
  
  for (current_outcome_var in unique(filtered_data$outcome_var)) {
    data_subset <- filtered_data %>% filter(outcome_var == current_outcome_var)
    
    plot <- data_subset %>%
      group_by(model_id) %>%
      plot_ly() %>%
      add_ribbons(
        x = ~period.month,
        ymin = ~CI.lower,
        ymax = ~CI.upper,
        hovertext = ~paste(key, model_id, sep = " - "),
        name = ~paste(model_id, outcome_var, "CI", sep = " - "),
        line = list(color = 'rgba(0,0,0,0.05)'),
        fillcolor = 'rgba(135,206,250,0.075)'
      )
    
    if (show_approx_bounds) {
      plot <- plot %>%
        add_ribbons(
          x = ~period.month,
          ymin = ~approx.CI.lower_68,
          ymax = ~approx.CI.upper_68,
          hovertext = ~paste(key, model_id, sep = " - "),
          name = ~paste(model_id, outcome_var, "CI 68%", sep = " - "),
          line = list(color = 'rgba(0,0,0,0)'),
          fillcolor = 'rgba(135,206,250,0.075)'
        ) %>%
        add_ribbons(
          x = ~period.month,
          ymin = ~approx.CI.lower_90,
          ymax = ~approx.CI.upper_90,
          hovertext = ~paste(key, model_id, sep = " - "),
          name = ~paste(model_id, outcome_var, "CI 90%", sep = " - "),
          line = list(color = 'rgba(0,0,0,0)'),
          fillcolor = 'rgba(135,206,250,0.075)'
        ) %>%
        add_ribbons(
          x = ~period.month,
          ymin = ~approx.CI.lower_95,
          ymax = ~approx.CI.upper_95,
          hovertext = ~paste(key, model_id, sep = " - "),
          name = ~paste(model_id, outcome_var, "CI 95%", sep = " - "),
          line = list(color = 'rgba(0,0,0,0)'),
          fillcolor = 'rgba(135,206,250,0.075)'
        )
    }
    
    plot <- plot %>%
      add_lines(
        x = ~period.month,
        y = ~mean.effect,
        hovertext = ~paste(key, model_id, sep = " - "),
        name = ~paste(model_id, outcome_var, "effect", sep = " - "),
        line = list(color = 'rgba(0,76,153,0.2)', width = 2)
      ) %>%
      layout(
        title = paste("IRFs for", current_outcome_var),
        xaxis = list(title = paste(current_outcome_var)),
        yaxis = list(title = "Mean (or median) effect"),
        hovermode = "compare"
      )
    
    plots_list[[current_outcome_var]] <- plot
  }
  
  subplot(plots_list, nrows = 1, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
    layout(
      title = "Impulse response functions by outcome variable",
      margin = list(t = 100)
    )
}