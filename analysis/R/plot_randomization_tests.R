#' Plot P-Hacking Evidence from Randomization Tests
#' 
#' @description
#' Creates visualizations showing potential evidence of p-hacking from randomization tests.
#' The main plot shows the proportion of statistical significant results and statistical significance of bunching.
#' Optionally displays the number of observations across window sizes either as a 
#' combined plot or as separate plots.
#'
#' @param results data.frame. Formatted table from format_randomization_table()
#' @param group_order character vector. Optional. Order of groups in facets
#' @param threshold numeric. The threshold value tested (for title)
#' @param facet_scales character. One of "fixed", "free", "free_y", or "free_x"
#' @param y_limits numeric vector. Length 2. Y-axis limits for main plot
#' @param show_n logical. Whether to show the sample size plot. Default: FALSE
#' @param plot_type character. One of "combined" or "separate". How to display plots 
#'   when show_n = TRUE. Default: "combined"
#' @param n_plot_ratio numeric. Height ratio of N plot to main plot in combined view. 
#'   Default: 0.3
#'
#' @return If show_n = FALSE, returns a single ggplot object.
#'   If show_n = TRUE and plot_type = "combined", returns a patchwork object.
#'   If show_n = TRUE and plot_type = "separate", returns a list of two ggplot objects.
#'
#' @examples
#' \dontrun{
#' # Basic plot without N
#' plot_randomization_tests(results)
#' 
#' # Combined plot with N
#' plot_randomization_tests(results, show_n = TRUE)
#' 
#' # Separate plots
#' plots <- plot_randomization_tests(results, 
#'                                  show_n = TRUE, 
#'                                  plot_type = "separate")
#' plots$main_plot
#' plots$n_plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point 
#' @importFrom ggplot2 facet_wrap scale_x_reverse scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text margin
#' @importFrom dplyr filter mutate select rename
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom scales percent
#' @importFrom patchwork plot_layout
#'
#' @export
plot_randomization_tests <- function(results,
                                     group_order = NULL,
                                     threshold = 1.96,
                                     facet_scales = "fixed",
                                     y_limits = c(0.3, 0.8),
                                     show_n = FALSE,
                                     plot_type = c("combined", "separate"),
                                     n_plot_ratio = 0.3) {
  
  # Input validation
  plot_type <- match.arg(plot_type)
  
  if (!is.data.frame(results)) {
    stop("'results' must be a data frame")
  }
  
  if (!all(c("Window", "Statistic") %in% names(results))) {
    stop("'results' must contain 'Window' and 'Statistic' columns")
  }
  
  if (!is.numeric(n_plot_ratio) || n_plot_ratio <= 0 || n_plot_ratio >= 1) {
    stop("'n_plot_ratio' must be a number between 0 and 1")
  }
  
  # Define significance levels and colors
  sig_levels <- c("p < 0.01", "p < 0.05", "p < 0.1", "p ≥ 0.1")
  sig_colors <- c("black", "gray40", "gray70", "white")
  names(sig_colors) <- sig_levels
  
  # Prepare main plot data
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
      sig_level = case_when(
        `P-value` < 0.01 ~ "p < 0.01",
        `P-value` < 0.05 ~ "p < 0.05",
        `P-value` < 0.1 ~ "p < 0.1",
        TRUE ~ "p ≥ 0.1"
      ),
      sig_level = factor(sig_level, levels = sig_levels)
    )
  
  if (!is.null(group_order)) {
    plot_data$Group <- factor(plot_data$Group, levels = group_order)
  }
  
  # Create main plot
  main_plot <- ggplot(plot_data, aes(x = Window, y = Proportion)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    geom_line() +
    geom_point(aes(fill = sig_level), 
               size = 3, 
               shape = 21,
               color = "black",
               stroke = 0.05) +
    facet_wrap(~Group, scales = facet_scales) +
    scale_x_reverse(name = if(!show_n || plot_type == "separate") "Window Size" else NULL) +
    scale_y_continuous(name = "Proportion Significant",
                       labels = scales::percent,
                       limits = y_limits) + 
    scale_fill_manual(values = sig_colors, 
                      breaks = sig_levels,
                      drop = FALSE) + 
    labs(
      title = paste("Binomial test for p-hacking around z =", threshold),
      subtitle = "Point shading shows stat. significance of bunching (binomial test against > 50%)",
      fill = "Significance"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Return main plot if N plot not requested
  if (!show_n) {
    return(main_plot)
  }
  
  # Create N plot
  n_data <- results %>%
    filter(Statistic == "N") %>%
    mutate(
      Window = as.numeric(gsub("±", "", Window)),
      N = as.numeric(All)
    ) %>%
    select(Window, N)
  
  n_plot <- ggplot(n_data, aes(x = Window, y = N)) +
    geom_line() +
    geom_point(size = 1) +
    scale_x_reverse(name = "Window Size") +
    scale_y_continuous(name = "Number of\nObservations") +
    theme_minimal() +
    theme(
      plot.margin = margin(t = if(plot_type == "combined") 0 else 5),
      axis.title.y = element_text(size = if(plot_type == "combined") 8 else 10),
      axis.text = element_text(size = if(plot_type == "combined") 8 else 10)
    )
  
  # Return based on plot_type
  if (plot_type == "separate") {
    return(list(
      main_plot = main_plot,
      n_plot = n_plot
    ))
  } else {
    # Combine plots using patchwork
    combined_plot <- main_plot + n_plot +
      plot_layout(ncol = 1, heights = c(1, n_plot_ratio)) &
      theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5))
    
    return(combined_plot)
  }
}