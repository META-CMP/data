# Capping the maximum of period 0 precision to allow estimation in period 0 ---- 
## Create vectors of the columns we want to process ----

se_cols <- c("SE.avg", "SE.upper", "SE.lower")
precision_cols <- c("precision.avg", "precision.upper", "precision.lower")

for (outcome in unique(d_no_qc$outcome)) {
  # Subset data for this outcome where period.month == 1
  subset_data <- d_no_qc[d_no_qc$period.month == 1 & d_no_qc$outcome == outcome, ]
  
  # Process SE columns (check against minimums)
  for (col in se_cols) {
    winsorized_values <- winsorizor(subset_data[[col]], percentile = wins_para)
    min_value <- min(winsorized_values)
    
    # Only overwrite values that are below the minimum
    mask <- d_no_qc$period.month == 0 &
      d_no_qc$outcome == outcome &
      d_no_qc[[col]] < 1*min_value
    
    d_no_qc[mask, col] <- 1*min_value
  }
  
  # Process precision columns (check against maximums)
  for (col in precision_cols) {
    winsorized_values <- winsorizor(subset_data[[col]], percentile = wins_para)
    max_value <- max(winsorized_values)
    
    # Only overwrite values that are above the maximum
    mask <- d_no_qc$period.month == 0 &
      d_no_qc$outcome == outcome &
      d_no_qc[[col]] > 1*max_value
    
    d_no_qc[mask, col] <- 1*max_value
  }
}