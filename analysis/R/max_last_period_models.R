#' Find models with maximum effect at the last period
#'
#' This function identifies models where the last period effect (absolute value) is equal to the maximum absolute effect across all periods,
#' indicating potentially "explosive" IRFs.
#'
#' @param data A data frame containing the necessary columns: key, model_id, period.month, and mean.effect.
#'
#' @return A data frame containing the models where the last period effect is equal to the maximum effect across all periods.
#'
#' @examples
#' library(dplyr)
#' # Create example dataset
#' example_data <- data.frame(
#'   key = c("A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C"),
#'   model_id = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5),
#'   period.month = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
#'   mean.effect = c(0.5, 0.7, 0.9, 0.8, 0.3, 0.6, 0.8, 0.9, 0.2, 0.4, 0.6, 0.8, 0.1, 0.3, 0.5, 0.7, -0.3, -0.5, -0.7, -0.7)
#' )
#'
#' # Apply the function to the example dataset
#' max_last_period_models <- find_max_last_period_models(example_data)
#'
#' # Print the result
#' print(max_last_period_models)
#'
#'
#' # Load the preliminary data
#' data_path <- "data/preliminary_data.RData"
#' load(data_path)
#'
#' # Apply the function to the preliminary data
#' max_last_period_models <- find_max_last_period_models(data)
#'
#' # Print the result
#' print(max_last_period_models)
#'
#' @import dplyr
#'
#' @export
find_max_last_period_models <- function(data) {
  last_period_effect <- data %>%
    group_by(key, model_id) %>%
    mutate(max_abs_effect = max(abs(mean.effect))) %>%
    filter(period.month == max(period.month)) %>%
    ungroup()
  
  max_last_period_models <- last_period_effect %>%
    filter(abs(mean.effect) == max_abs_effect)
  
  return(max_last_period_models)
}