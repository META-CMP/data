#' Filter data by a single country
#'
#' This function filters a data frame to include only rows where the list_of_countries column matches the specified country.
#'
#' @param data A data frame containing a list_of_countries column.
#' @param country A string specifying the country to filter by.
#'
#' @return A filtered data frame containing only rows where list_of_countries matches the specified country.
#' 
#' @import dplyr
#' 
#' @export
one_country <- function(data, country) {
  data %>%
    filter(list_of_countries == country)
}

#' Filter data by country presence
#'
#' This function filters a data frame to include only rows where the list_of_countries column contains the specified country.
#'
#' @param data A data frame containing a list_of_countries column.
#' @param country A string specifying the country to check for presence.
#'
#' @return A filtered data frame containing only rows where list_of_countries includes the specified country.
#' 
#' @import dplyr
#' 
#' @export
country_in_sample <- function(data, country) {
  data %>%
    filter(str_detect(list_of_countries, paste0("\\b", country, "\\b")))
}

#' Filter data by an exact group of countries
#'
#' This function filters a data frame to include only rows where the list_of_countries column contains exactly the specified group of countries.
#'
#' @param data A data frame containing a list_of_countries column.
#' @param countries A character vector specifying the countries to match exactly.
#'
#' @return A filtered data frame containing only rows where list_of_countries matches the specified group of countries exactly.
#' 
#' @import dplyr
#' 
#' @export
country_group_only <- function(data, countries) {
  pattern <- paste0("\\b(", paste(countries, collapse = "|"), ")\\b")
  data %>%
    filter(str_count(list_of_countries, pattern) == length(countries) &
             str_count(list_of_countries, "\\S+") == length(countries))
}

#' Filter data by the presence of a group of countries
#'
#' This function filters a data frame to include only rows where the list_of_countries column contains all the specified countries.
#'
#' @param data A data frame containing a list_of_countries column.
#' @param countries A character vector specifying the countries to check for presence.
#'
#' @return A filtered data frame containing only rows where list_of_countries includes all the specified countries.
#' 
#' @import dplyr
#' 
#' @export
country_group_in_sample <- function(data, countries) {
  pattern <- paste0("\\b(", paste(countries, collapse = "|"), ")\\b")
  data %>%
    filter(str_count(list_of_countries, pattern) >= length(countries))
}

#' Filter data by excluding specific countries
#'
#' This function filters a data frame to include only rows where the list_of_countries column does not contain any of the specified countries.
#'
#' @param data A data frame containing a list_of_countries column.
#' @param countries A character vector specifying the countries to exclude.
#'
#' @return A filtered data frame containing only rows where list_of_countries does not include any of the specified countries.
#' 
#' @import dplyr
#' 
#' @export
exclude_countries <- function(data, countries) {
  pattern <- str_c("\\b", str_c(countries, collapse = "\\b|\\b"), "\\b")
  data %>%
    filter(!str_detect(list_of_countries, pattern))
}