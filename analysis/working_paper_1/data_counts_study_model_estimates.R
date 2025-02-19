# Counting studies, models and IRFs with summary tables

# Source the setup file ---- 
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Function to compute counts
get_counts <- function(df, outcome_var = NULL) {
  if (!is.null(outcome_var)) {
    df <- df[df$outcome == outcome_var, ]
  }
  
  # Return empty if no data matches filter
  if (nrow(df) == 0) {
    return(list(
      studies = 0,
      models = 0,
      irfs = 0,
      points = 0
    ))
  }
  
  # Count studies
  study_count <- length(unique(df$key))
  
  # Count models
  model_count <- nrow(unique(df[, c("key", "model_id")]))
  
  # Count IRFs (if outcome specified, this equals model count)
  if (!is.null(outcome_var)) {
    irf_count <- model_count  # For a specific outcome, each model has one IRF
  } else {
    irf_count <- nrow(unique(df[, c("key", "model_id", "outcome")]))
  }
  
  # Count point estimates
  point_count <- nrow(df)
  
  return(list(
    studies = study_count,
    models = model_count,
    irfs = irf_count,
    points = point_count
  ))
}

# Create main results table
create_table <- function() {
  # Define datasets and outcomes
  datasets <- list(
    "Full dataset" = data,
    "No quality concerns" = d_no_qc
  )
  
  outcomes <- c("overall", "output", "inflation", "emp", "unemp", "rate")
  
  # Initialize results table
  results <- data.frame(
    Dataset = character(),
    Outcome = character(),
    Studies = integer(),
    Models = integer(),
    IRFs = integer(),
    Points = integer(),
    stringsAsFactors = FALSE
  )
  
  # Fill table
  for (ds_name in names(datasets)) {
    df <- datasets[[ds_name]]
    
    # Overall counts
    counts <- get_counts(df)
    results <- rbind(results, data.frame(
      Dataset = ds_name,
      Outcome = "Overall",
      Studies = counts$studies,
      Models = counts$models,
      IRFs = counts$irfs,
      Points = counts$points,
      stringsAsFactors = FALSE
    ))
    
    # Outcome-specific counts
    for (out_var in outcomes[-1]) {  # Skip "overall"
      counts <- get_counts(df, out_var)
      results <- rbind(results, data.frame(
        Dataset = ds_name,
        Outcome = out_var,
        Studies = counts$studies,
        Models = counts$models,
        IRFs = counts$models,  # For specific outcome, IRFs = models
        Points = counts$points,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

# Create the focused table for selected outcomes
create_focused_table <- function() {
  # Filter the dataset for selected outcomes
  selected_outcomes <- c("output", "inflation", "rate")
  filtered_df <- d_no_qc[d_no_qc$outcome %in% selected_outcomes, ]
  
  # Get counts
  counts <- get_counts(filtered_df)
  
  # Create a single-row table
  results <- data.frame(
    Measure = c("Studies", "Models", "IRFs", "Point estimates"),
    Count = c(counts$studies, counts$models, counts$irfs, counts$points),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Generate and print the main summary table
results_table <- create_table()

# Print formatted table (using knitr if available)
if (requireNamespace("knitr", quietly = TRUE)) {
  cat("\nFormatted Main Table:\n")
  print(knitr::kable(results_table, format = "markdown"))
}

# Generate and print the focused table
cat("\n\nCounts for No Quality Concerns dataset filtered to output, inflation, and rate outcomes only:\n")
focused_table <- create_focused_table()

# Print formatted focused table
if (requireNamespace("knitr", quietly = TRUE)) {
  cat("\nFormatted Focused Table:\n")
  print(knitr::kable(focused_table, format = "markdown"))
}

# Save to files (optional)
write.csv(results_table, file = here::here("analysis/working_paper_1/tables/data_counts/summary_counts.csv"), 
          row.names = FALSE)
write.csv(focused_table, file = here::here("analysis/working_paper_1/tables/data_counts/wp_1_focused_counts.csv"), 
          row.names = FALSE)