#' Plot P-Hacking Evidence from Randomization Tests
#' 
#' @description
#' Creates a visualization showing evidence of p-hacking from randomization tests.
#' Shows proportion of significant results and significance of bunching.
#'
#' @param results Formatted table from format_randomization_table()
#' @param group_order Optional character vector. Order of groups in facets
#' @param threshold Numeric. The threshold value tested (for title)
#' @param facet_scales Character. "fixed", "free", "free_y", or "free_x"
#'
#' @return A ggplot object
#'
#' @export
plot_randomization_tests <- function(results,
                                   group_order = NULL,
                                   threshold = 1.96,
                                   facet_scales = "fixed",
                                   y_limits = c(0.3, 0.8)) {
  
  # Define fixed significance levels and their grayscale values
  sig_levels <- c("p < 0.01", "p < 0.05", "p < 0.1", "p ≥ 0.1")
  sig_colors <- c("black", "gray40", "gray70", "white")
  names(sig_colors) <- sig_levels
  
  # Prepare data
  plot_data <- results %>%
    filter(Statistic %in% c("Proportion", "P-value")) %>%
    tidyr::pivot_longer(
      cols = -c(Window, Statistic),
      names_to = "Group",
      values_to = "Value"
    ) %>%
    mutate(
      Window = as.numeric(gsub("±", "", Window)),
      Value = as.numeric(Value)
    ) %>%
    tidyr::pivot_wider(
      names_from = Statistic,
      values_from = Value
    ) %>%
    mutate(
      # Create significance indicator with fixed levels
      sig_level = case_when(
        `P-value` < 0.01 ~ "p < 0.01",
        `P-value` < 0.05 ~ "p < 0.05",
        `P-value` < 0.1 ~ "p < 0.1",
        TRUE ~ "p ≥ 0.1"
      ),
      sig_level = factor(sig_level, levels = sig_levels)
    )
  
  # Order groups if specified
  if(!is.null(group_order)) {
    plot_data$Group <- factor(plot_data$Group, levels = group_order)
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Window, y = Proportion)) +
    # Add reference line at 0.5
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    # Add line
    geom_line() +
    # Add points with fill based on significance
    geom_point(aes(fill = sig_level), 
               size = 3, 
               shape = 21,  # Filled circle with border
               color = "black",
               # Set size of points
               stroke = 0.05
               ) +
    # Facet by group
    facet_wrap(~Group, scales = facet_scales) +
    # Customize appearance
    scale_x_reverse(name = "Window Size") +
    scale_y_continuous(name = "Proportion Significant",
                       labels = scales::percent,
                       limits = y_limits) + 
    scale_fill_manual(values = sig_colors, 
                      breaks = sig_levels,  
                      drop = FALSE) + 
    # Add titles
    labs(
      title = paste("Evidence of P-Hacking Around z =", threshold),
      subtitle = "Point shading shows stat. significance of bunching (binomial test against > 50%)",
      fill = "Significance"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}
