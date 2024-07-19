#' Create Z-score binned density plot
#'
#' This function prepares the data and creates a binned density plot of Z-scores using Plotly,
#' consistent with the data preparation steps used in the funnel plot function. The plot is inspired by 
#' Andrews and Kasy (2019) (see https://github.com/maxkasy/MetaStudiesApp/blob/master/metastudiesplots.r).
#' It uses adaptive bin widths and displays density rather than raw counts, providing a nuanced 
#' representation of the Z-score distribution. The use of density normalizes the counts.
#'
#' @param data A data frame containing the necessary columns for analysis.
#' @param outvar A string specifying the outcome variable to filter the data.
#' @param prd A numeric value specifying the period (in months) to filter the data.
#' @param se_option A string specifying the standard error option to use. Can be "avg", "lower", or "upper".
#' @param wins A numeric value specifying the winsorization parameter.
#' @param ap If TRUE, only adequately powered studies are included.
#' @param critvals A numeric vector of critical values for vertical lines (default c(1, 1.96)).
#'
#' @return A Plotly binned density plot of Z-scores.
#'
#' @import plotly
#' @import dplyr
#' @import JWileymisc
#'
#' @examples
#' library(dplyr)
#' library(plotly)
#' library(JWileymisc)
#'
#' # Load the data
#' data_path <- "data/preliminary_data_test_11072024.RData"
#' load(data_path)
#'
#' # Calculate the average standard error
#' data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
#'
#' # Create a Z-score binned density plot
#' z_plot <- create_z_histogram(data, outvar = "gdp", prd = 12, se_option = "avg", wins = 0.02, ap = FALSE)
#' print(z_plot)
#'
#' @export
create_z_histogram <- function(data, outvar, prd, se_option = "avg", wins = 0.02, ap = FALSE, critvals = c(1, 1.96)) {
  # Filter the data for the specific period and outcome variable
  data_filtered <- data %>%
    filter(period.month == prd, outcome == outvar)
  
  # Select the corresponding standard error column based on the se_option
  if (se_option == "avg") {
    data_filtered$StandardError <- data_filtered$SE.avg
  } else if (se_option == "lower") {
    data_filtered$StandardError <- data_filtered$SE.lower
  } else if (se_option == "upper") {
    data_filtered$StandardError <- data_filtered$SE.upper
  }
  
  # Apply Winsorization to the mean effect and standard error
  data_filtered <- apply_winsorization(data_filtered, wins)
  
  # Filter adequately powered if ap == TRUE
  if (ap == TRUE) {
    data_filtered$ap <- ifelse(data_filtered$standarderror_winsor <= abs(data_filtered$mean.effect_winsor)/2.8, 1, 0)
    data_filtered <- data_filtered %>% 
      filter(ap == 1)
  }
  
  # Calculate Z-scores
  Z <- data_filtered$mean.effect_winsor / data_filtered$standarderror_winsor
  n <- length(Z)
  ll <- floor(min(Z))
  uu <- ceiling(max(Z))
  
  if (n >= 30) {
    uu2 <- ceiling(max((uu - 0.36) / 0.32, 0)) * 0.32 + 0.36
    ll2 <- floor(min((ll + 0.36) / 0.32, 0)) * 0.32 - 0.36
    edges <- c(seq(from = ll2, to = -0.36, by = 0.32), 0, seq(from = 0.36, to = uu2, by = 0.32))
  } else {
    uu2 <- ceiling(max((uu - 0.68) / 0.64, 0)) * 0.64 + 0.68
    ll2 <- floor(min((ll + 0.68) / 0.64, 0)) * 0.64 - 0.68
    edges <- c(seq(from = ll2, to = -0.68, by = 0.64), 0, seq(from = 0.68, to = uu2, by = 0.64))
  }
  
  hist_data <- hist(Z, breaks = edges, plot = FALSE)
  
  plot_z_hist <- plot_ly() %>%
    add_bars(x = hist_data$mids, y = hist_data$density, 
             marker = list(color = 'blue', opacity = 0.6),
             name = "Z-score distribution") %>%
    layout(
      title = paste("Binned density -", outvar, ",", prd, "months after shock"),
      xaxis = list(title = "Effect / SE"),
      yaxis = list(title = "Density"),
      shapes = list(),
      plot_bgcolor = 'rgb(242, 242, 242)',
      paper_bgcolor = 'rgb(255, 255, 255)'
    )
  
  # Add vertical lines for critical values
  for (crit in critvals) {
    plot_z_hist <- plot_z_hist %>%
      add_segments(x = crit, xend = crit, y = 0, yend = max(hist_data$density),
                   line = list(color = 'red', width = 1.5, dash = 'dash'),
                   showlegend = FALSE) %>%
      add_segments(x = -crit, xend = -crit, y = 0, yend = max(hist_data$density),
                   line = list(color = 'red', width = 1.5, dash = 'dash'),
                   showlegend = FALSE)
  }
  
  return(plot_z_hist)
}