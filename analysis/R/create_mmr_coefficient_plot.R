create_mmr_coefficient_plot <- function(
    model_output,                              
    variable_name,                             
    conf_levels = c(0.67, 0.89, 0.97),              
    custom_title = NULL,                       
    base_color = "blue"                        
) {
  
  # Prepare the data
  plot_data <- map_dfr(conf_levels, function(x) {
    modelsummary::modelplot(model_output, conf_level = x, draw = FALSE) %>%
      mutate(conf_level = x)
  })
  
  # Filter for the specified variable and ensure model is numeric
  plot_data <- plot_data %>%
    filter(term == variable_name) %>%
    mutate(model = as.numeric(as.character(model)))
  
  # Generate colors based on number of confidence levels
  n_colors <- length(conf_levels)
  colors <- colorRampPalette(c("white", base_color))(n_colors + 1)[-1]
  names(colors) <- as.character(conf_levels)
  
  # Set title
  if (is.null(custom_title)) {
    title <- variable_name
  } else {
    title <- custom_title
  }
  
  # Create subtitle with CI information
  ci_text <- paste0("Confidence intervals: ", 
                    paste0(conf_levels * 100, "%", collapse = ", "))
  
  # Create the plot
  ggplot(plot_data, aes(x = model)) +
    # Add confidence intervals
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, 
                    fill = factor(conf_level)), alpha = 0.2) +
    # Add the point estimates
    geom_line(aes(y = estimate), color = "black", linewidth = 0.5) +
    geom_point(aes(y = estimate), color = "black", size = 1) +
    # Zero line for reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    # Customize appearance
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(x = "Month",
         y = "Coefficient Estimate",
         title = title,
         subtitle = ci_text) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, max(plot_data$model), by = 6))
}