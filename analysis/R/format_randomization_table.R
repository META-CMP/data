#' Format Randomization Test Results into Publication-Ready Tables
#' 
#' @description
#' Formats the output from randomization_test_battery() into publication-ready
#' tables. Creates a row for proportion, p-value, and N for each window size.
#'
#' @param results Data frame. Output from randomization_test_battery()
#' @param group_comparison Optional character. Name of comparison group for split tables
#' @param group_order Optional character vector. Order of group columns in output
#' @param digits Integer. Number of decimal places (default = 3)
#'
#' @return A formatted data frame with three rows (proportion, p-value, N) per window
#'
#' @export
format_randomization_table <- function(results,
                                       group_comparison = NULL,
                                       group_order = NULL,
                                       digits = 3) {
  
  # Function to format one subtable
  format_subtable <- function(data) {
    # Get group values
    if(is.null(group_order)) {
      group_values <- unique(data$group)
    } else {
      # Validate group_order
      if(!all(group_order %in% unique(data$group))) {
        stop("All values in group_order must be present in the data")
      }
      group_values <- group_order
    }
    
    # Create separate wide tables for each statistic
    props <- data %>%
      select(delta, group, proportion) %>%
      tidyr::pivot_wider(
        names_from = group,
        values_from = proportion
      )
    
    pvals <- data %>%
      select(delta, group, pvalue) %>%
      tidyr::pivot_wider(
        names_from = group,
        values_from = pvalue
      )
    
    ns <- data %>%
      select(delta, group, n) %>%
      tidyr::pivot_wider(
        names_from = group,
        values_from = n
      )
    
    # Combine into final table with three rows per window
    final_table <- data.frame()
    for(i in seq_along(props$delta)) {
      # Create base data frame with window and statistic
      window_rows <- data.frame(
        Window = paste0("±", props$delta[i]),
        Statistic = c("Proportion", "P-value", "N")
      )
      
      # Add columns for each group value in specified order
      for(g in group_values) {
        window_rows[[g]] <- c(
          sprintf(paste0("%.", digits, "f"), props[[g]][i]),
          sprintf(paste0("%.", digits, "f"), pvals[[g]][i]),
          as.character(ns[[g]][i])
        )
      }
      
      final_table <- rbind(final_table, window_rows)
    }
    
    return(final_table)
  }
  
  # Split by comparison group if specified
  if(!is.null(group_comparison)) {
    tables <- split(results, results[[group_comparison]])
    formatted <- lapply(tables, format_subtable)
    return(formatted)
  } else {
    return(format_subtable(results))
  }
}
