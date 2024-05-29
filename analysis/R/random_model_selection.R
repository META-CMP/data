# random_model_selection.R

#' Randomly Select One Model Per Study
#'
#' This function randomly selects one model per study from a given dataset.
#' It ensures that only one model is selected for each study and verifies the selection.
#'
#' @param data A data frame containing the data. The data frame must include columns: `key`, `model_id`, and other relevant columns.
#' 
#' @return A data frame containing IRF data for the selected models.
#' 
#' @import dplyr
#' @examples
#' library(dplyr)
#' # Load the data 
#' data_path <- here::here("data/preliminary_data.RData")
#' load(data_path)
#' # Apply the function to the data
#' selected_data <- random_model_selection(data)
#' 
#' @export
random_model_selection <- function(data) {
  random_model_ids <- data %>%
    distinct(key, model_id) %>%
    group_by(key) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  random_model_data <- data %>%
    semi_join(random_model_ids, by = c("key", "model_id"))
  
  # Ensure that only one model per study is selected
  test_result <- random_model_data %>%
    group_by(key) %>%
    summarise(n_models = n_distinct(model_id)) %>%
    filter(n_models > 1)
  
  if (nrow(test_result) == 0) {
    message("Test passed: Only one model per study is selected.")
  } else {
    message("Test failed: More than one model per study is selected.")
    print(test_result)
    stop("Random model selection failed: More than one model per study is selected.")
  }
  
  # Verify the random selection process
  for (study in unique(data$key)) {
    selected_models <- random_model_data %>% filter(key == study) %>% pull(model_id) %>% unique()
    if (length(selected_models) != 1) {
      message(paste("Test failed for study:", study))
      stop(paste("Random model selection failed for study:", study))
    } else {
      message(paste("Test passed for study:", study, "with model:", selected_models))
    }
  }
  
  return(random_model_data)
}


