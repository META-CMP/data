#' Create funnel plots
#'
#' This function creates a funnel plot for a specific outcome variable and period, with optional Winsorization.
#' It supports both standard funnel plots and Andrews & Kasy (AK) style plots.
#'
#' @param data A data frame containing the necessary columns for plotting.
#' @param outvar A string specifying the outcome variable to filter the data.
#' @param prd A numeric value specifying the period (in months) to filter the data.
#' @param se_option A string specifying the standard error option to use. Can be "avg", "lower", or "upper".
#' @param wins A numeric value specifying the Winsorization parameter.
#' @param legend If TRUE (default), the legend will be shown in the standard plot.
#' @param opac A numeric value specifying the opacity parameter for the plot.
#' @param ap If TRUE, only adequately powered studies are included.
#' @param type A string specifying the type of plot. Can be "standard" or "AK" (Andrews & Kasy style).
#' @param AK_critvals A numeric vector specifying critical values for the AK plot (default c(1.96, 2.58)).
#' @param AK_exclude_outliers If TRUE, applies additional outlier exclusion for AK plots (default FALSE).
#'
#' @return A plotly scatter plot of the funnel plot.
#'
#' @import plotly
#' @import dplyr
#' @import tidyr
#' @import JWileymisc
#' @import viridis
#'
#' @examples
#' library(dplyr)
#' library(plotly)
#' library(JWileymisc)
#' library(viridis)
#'
#' # Load the data
#' data_path <- "data/preliminary_data_test_11072024.RData"
#' load(data_path)
#'
#' # Calculate the average standard error and precision options
#' data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
#' data$precision.avg <- 1 / data$SE.avg
#' data$precision.lower <- 1 / data$SE.lower
#' data$precision.upper <- 1 / data$SE.upper
#'
#' # Create a standard funnel plot
#' funnel_plot <- create_funnel_plot(data, outvar = "gdp", prd = 24, se_option = "avg", wins = 0.01, opac = 0.12)
#' print(funnel_plot)
#'
#' # Create an Andrews & Kasy style plot
#' ak_plot <- create_funnel_plot(data, outvar = "gdp", prd = 24, se_option = "avg", wins = 0.01, type = "AK")
#' print(ak_plot)
#'
#' # Create an Andrews & Kasy style plot with additional outlier exclusion without winsorization
#' ak_plot_excluded <- create_funnel_plot(data, outvar = "gdp", prd = 24, se_option = "avg", wins = 0, type = "AK", AK_exclude_outliers = TRUE)
#' print(ak_plot_excluded)
#'
#' @export
create_funnel_plot <- function(data, outvar, prd, se_option = "avg", wins = 0.02, legend = TRUE, opac = 0.15, ap = FALSE, type = "standard", AK_critvals = c(1, 1.96, 2.58), AK_exclude_outliers = FALSE) {
  # Filter the data for the specific period and outcome variable
  data_filtered <- data %>%
    filter(period.month == prd, outcome == outvar)
  
  # Select the corresponding standard error and precision columns based on the se_option
  if (se_option == "avg") {
    data_filtered$StandardError <- data_filtered$SE.avg
    data_filtered$precision <- data_filtered$precision.avg
  } else if (se_option == "lower") {
    data_filtered$StandardError <- data_filtered$SE.lower
    data_filtered$precision <- data_filtered$precision.lower
  } else if (se_option == "upper") {
    data_filtered$StandardError <- data_filtered$SE.upper
    data_filtered$precision <- data_filtered$precision.upper
  }
  
  # Apply Winsorization to the standard error, mean effect, and precision
  data_filtered <- apply_winsorization(data_filtered, wins)
  
  # Filter adequately powered if ap == TRUE
  if (ap == TRUE) {
    data_filtered$ap <- ifelse(data_filtered$standarderror_winsor <= abs(data_filtered$mean.effect_winsor)/2.8, 1, 0)
    data_filtered <- data_filtered %>% 
      filter(ap == 1)
  }
  
  if (type == "standard") {
    # Create the standard funnel plot
    # Get the unique outcome variable values
    unique_outcomes <- unique(data_filtered$outcome_var)
    
    # Create a named vector of colors from the viridis palette
    palette_colors <- setNames(viridis(length(unique_outcomes)), unique_outcomes)
    
    # Create the funnel plot with Winsorized data and color points based on outcome_var
    plot_funnel <- plot_ly(data = data_filtered,
                           x = ~mean.effect_winsor,
                           y = ~precision_winsor,
                           color = ~outcome_var,
                           colors = palette_colors,
                           type = "scatter",
                           mode = "markers",
                           marker = list(size = 10, opacity = opac),
                           hoverinfo = "text",
                           hovertext = ~paste("Effect Size:", round(mean.effect_winsor, 3),
                                              "<br>Precision:", round(precision_winsor, 3),
                                              "<br>Key:", key,
                                              "<br>Model ID:", model_id,
                                              "<br>Outcome Var:", outcome_var)) %>%
      layout(title = paste(outvar, ",", prd, "months after shock"),
             xaxis = list(title = "Effect size"),
             yaxis = list(title = "Precision"),
             showlegend = legend)
    
  } else if (type == "AK") {
    # Create the Andrews and Kasy style funnel plot
    # Calculate significance based on the smallest critical value
    smallest_critval <- min(AK_critvals)
    data_filtered$significant <- abs(data_filtered$mean.effect_winsor / data_filtered$standarderror_winsor) > smallest_critval
    
    # Identify non-outliers (optional step)
    if (AK_exclude_outliers) {
      data_filtered$nooutlier <- (data_filtered$standarderror_winsor < 30 * mean(data_filtered$standarderror_winsor)) & 
        (abs(data_filtered$mean.effect_winsor) < 30 * mean(abs(data_filtered$mean.effect_winsor)))
      data_plot <- data_filtered %>% filter(nooutlier)
    } else {
      data_plot <- data_filtered
      data_plot$nooutlier <- TRUE  # All points considered non-outliers if exclusion is off
    }
    
    # Calculate range based on plotted data
    rangeX <- 1.1 * max(abs(data_plot$mean.effect_winsor))
    rangeY <- 1.1 * max(data_plot$standarderror_winsor)
    
    # Create the Andrews and Kasy style funnel plot
    plot_funnel <- plot_ly() %>%
      add_trace(
        data = data_plot,
        x = ~mean.effect_winsor,
        y = ~standarderror_winsor,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,
          color = ~ifelse(significant & nooutlier, "blue", "grey50"),
          opacity = opac
        ),
        hoverinfo = "text",
        hovertext = ~paste("Effect Size:", round(mean.effect_winsor, 3),
                           "<br>Standard Error:", round(standarderror_winsor, 3),
                           "<br>Significant:", significant,
                           "<br>Key:", key,
                           "<br>Model ID:", model_id,
                           "<br>Outcome Var:", outcome_var)
      )
    
    # Add lines for each critical value
    for (crit in AK_critvals) {
      plot_funnel <- plot_funnel %>%
        add_segments(x = -rangeX, xend = rangeX, y = -rangeX/crit, yend = rangeX/crit,
                     line = list(color = "grey", dash = "dash"), showlegend = FALSE) %>%
        add_segments(x = -rangeX, xend = rangeX, y = rangeX/crit, yend = -rangeX/crit,
                     line = list(color = "grey", dash = "dash"), showlegend = FALSE)
    }
    
    # Set layout
    plot_funnel <- plot_funnel %>%
      layout(
        title = paste(outvar, ",", prd, "months after shock"),
        xaxis = list(title = "Effect Size", range = c(-rangeX, rangeX)),
        yaxis = list(title = "Standard Error", range = c(0, rangeY)),
        showlegend = FALSE
      )
    
  } else {
    stop("Invalid plot type. Choose 'standard' or 'AK'.")
  }
  
  return(plot_funnel)
}