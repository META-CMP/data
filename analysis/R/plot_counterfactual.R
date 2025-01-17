#' Plot Test Statistics with Counterfactual Distribution
#' 
#' @description
#' Creates histogram of test statistics with kernel density and calibrated
#' counterfactual distribution following Brodeur et al. (2020).
#'
#' @param data A data frame containing test statistics
#' @param statistic Character. Name of column containing test statistics
#' @param group Optional character. Name of grouping variable
#' @param group_value Optional character. Value of grouping variable
#' @param calibration List. Output from calibrate_counterfactual()
#' @param breaks Numeric vector. Custom breaks for histogram
#' @param ylim Numeric vector. Y-axis limits
#' @param main Character. Plot title
#' @param significance Numeric vector. Significance levels to mark
#' @param show_params Logical. Show calibration parameters in legend
#' @param omit_cf Logical. Omit counterfactual distribution from plot
#' @param kernel_adjust Numeric. Adjustment factor for kernel density
#' @param kernel_bw Numeric. Bandwidth for kernel density
#' @param kernel_type Character. Kernel type for density estimation
#'
#' @return A plot (invisibly returns plot data)
#'
#' @examples
#' \dontrun{
#' data(brodeur_data) # Available through the replication package (AER website)
#' 
#' # Calibrate and plot for DID
#' cf_did <- calibrate_counterfactual(brodeur_data, 
#'                                   statistic = "t",
#'                                   group = "method", 
#'                                   group_value = "DID")
#' plot_counterfactual(brodeur_data, 
#'                     statistic = "t",
#'                     group = "method",
#'                     calibration = cf_did)
#' }
#' 
#'#' @references 
#' Brodeur, A., Cook, N., & Heyes, A. (2020). Methods Matter: P-Hacking and 
#' Publication Bias in Causal Analysis in Economics. American Economic Review, 
#' 110(11), 3634-60.
#' 
#' @export
plot_counterfactual <- function(data,
                                statistic,
                                group = NULL,
                                group_value = NULL,
                                calibration,
                                breaks = NULL,
                                ylim = c(0, 0.45),
                                xlims = c(0, 10),
                                main = NULL,
                                significance = c(1.645, 1.96, 2.576),
                                show_params = TRUE,
                                omit_cf = FALSE,
                                kernel_adjust = 0.5,
                                kernel_bw = 0.2,
                                kernel_type = "epanechnikov"
) {
  # Get relevant data
  if(is.null(group)) {
    plot_data <- data[[statistic]]
  } else {
    if(is.null(group_value)) {
      stop("group_value must be specified when group is specified")
    }
    plot_data <- data[[statistic]][data[[group]] == group_value]
  }
  
  # Set default title if not provided
  if(is.null(main)) {
    main <- if(!is.null(group)) group_value else ""
  }
  
  # Set default breaks if not provided
  if(is.null(breaks)) {
    breaks <- c(seq(0, 10, 0.1), max(plot_data))
  }
  
  # Set default title if not provided
  if(is.null(main)) {
    main <- if(!is.null(group)) group_value else ""
  }
  
  # Create base histogram
  hist(plot_data, 
       breaks = breaks,
       border = "grey",
       probability = TRUE,
       ylim = ylim,
       xlim = xlims,           
       main = main,
       xlab = "z-statistic",
       ylab = "Density")
  
  # Add kernel density of observed data
  lines(density(plot_data, 
                adjust = kernel_adjust, 
                bw = kernel_bw, 
                kernel = kernel_type,
                from = 0, 
                to = 10), 
        lwd = 2)
  
  # Add vertical lines at significance levels
  if(!is.null(significance)) {
    abline(v = significance, lty = "dotted")
  }
  
  # Add theoretical density
  z_seq <- seq(0, 10, 0.01)
  theoretical_dens <- dt(z_seq, 
                         df = calibration$df, 
                         ncp = calibration$ncp)
  if (omit_cf == FALSE) {
    lines(z_seq, theoretical_dens, 
          col = "red", 
          lty = 2, 
          lwd = 2)
  }
  
  # Create legend components
  legend_items <- c("Histogram", "Empirical Density")
  legend_colors <- c("grey", "black")
  legend_lines <- c(NA, 1)
  legend_pch <- c(22, NA)
  legend_pt_bg <- c("grey", NA)
  
  # Add legend components for counterfactual and parameters if not omitted
  if (!omit_cf) {
    legend_items <- c(legend_items, "Counterfactual")
    legend_colors <- c(legend_colors, "red")
    legend_lines <- c(legend_lines, 2)
    legend_pch <- c(legend_pch, NA)
    legend_pt_bg <- c(legend_pt_bg, NA)
    
    if (show_params) {
      legend_items <- c(legend_items,
                        paste0("df = ", calibration$df),
                        paste0("ncp = ", round(calibration$ncp, 2)))
      legend_colors <- c(legend_colors, "black", "black")
      legend_lines <- c(legend_lines, NA, NA)
      legend_pch <- c(legend_pch, NA, NA)
      legend_pt_bg <- c(legend_pt_bg, NA, NA)
    }
  }
  
  # Add legend
  legend("topright", 
         legend = legend_items,
         col = legend_colors,
         lty = legend_lines,
         pch = legend_pch,
         pt.bg = legend_pt_bg,
         bty = "n",
         pt.cex = 1.2)
  
  # Return plot data invisibly
  invisible(list(
    data = plot_data,
    breaks = breaks,
    density = density(plot_data, 
                      adjust = 0.9, 
                      bw = 0.2, 
                      kernel = "epanechnikov",
                      from = 0, 
                      to = 10),
    theoretical = data.frame(
      x = z_seq,
      y = theoretical_dens
    )
  ))
}