library(tidyverse)
library(here)

# This script extracts URLs, or item titles where no URL is included, 
# for all entries from our merged and duplicates-free initial results 
# that have no abstract or only a shortened abstract as it is retrieved 
# from Google Scholar.

## Step 1: Defining of helper functions

#' Filter entries without an abstract or with shortened Google Scholar abstract and write a CSV file.
#' 
#' This function filters a provided CSV file by searching for "…" in the "Abstract Note" column.
#' This will filter out entries containing an excerpted GS abstract or no abstract at all. 
#' Then the function writes the filtered data to a new CSV file. Note that is not impossible 
#' that some complete abstracts might also contain "…" as actual content, but these entries we 
#' would probably want to check anyway.
#'
#' @param filepath The filepath of the CSV file to be filtered
#' @param new_filepath The filepath of the new CSV file to be written
#'
#' @return The function returns the filtered dataframe and writes it to a new CSV file
#' @export
#'
#' @examples
#' filter_and_write_csv("data.csv", "example", "filtered_data.csv")

filter_gs_entries <- function(filepath, new_filepath) {
  df <- readr::read_csv(filepath)
  df_new <- df %>% 
    filter( grepl("…", df$`Abstract Note`) == TRUE | is.na(df$`Abstract Note`))
  write.csv(df_new, new_filepath, row.names = FALSE)
  View(df_new)
}

#' Filter entries that contain a complete abstract and write a CSV file.
#' 
#' This function filters a provided CSV file. It filters the complement to filter_gs_entries().
#' This will filter out entries NOT containing an excerpted GS abstract or NOT containing no abstract at all. 
#' Then the function writes the filtered data to a new CSV file.
#'
#' @param filepath The filepath of the CSV file to be filtered
#' @param new_filepath The filepath of the new CSV file to be written
#'
#' @return The function returns the filtered dataframe and writes it to a new CSV file
#' @export
#'
#' @examples
#' filter_and_write_csv("data.csv", "example", "filtered_data.csv")

filter_complete_entries <- function(filepath, new_filepath) {
  df <- readr::read_csv(filepath)
  df_new <- df %>% 
    filter( grepl("…", df$`Abstract Note`) == FALSE)
  df_new <- df_new %>%
    filter(!is.na(df_new$`Abstract Note`))
  write.csv(df_new, new_filepath, row.names = FALSE)
  View(df_new)
}

#' Extract URLs from a CSV file and save them in txt files
#'
#' @param filepath path of csv file containing URLs
#' @param col_name name of the column containing URLs
#' @param n number of URLs per txt file
#' @param dir directory to save the txt files
#' @return NULL, saves the txt files in the provided directory
#' @examples
#' extract_urls("path/to/csv/file.csv", "urls", 2, "path/to/save/txt/files")
#' # reads the csv file and creates txt files urls1.txt, urls2.txt in the path/to/save/txt/files directory containing 2 URLs each

extract_urls <- function(filepath, col_name, n, dir) {
  df <- read.csv(filepath)
  # create the directory if it does not exist
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  
  # get the number of txt files needed
  num_files <- ceiling(nrow(df) / n)
  
  for (i in 1:num_files) {
    # get the start and end indices for the current file
    start_index <- (i-1) * n + 1
    end_index <- min(i * n, nrow(df))
    
    # extract the URLs for the current file
    urls <- df[start_index:end_index, col_name]
    
    # save the URLs to a txt file
    file_name <- paste(dir, "/urls", i, ".txt", sep = "")
    writeLines(urls, file_name)
  }
}

#  This will filter out those entries from dataframe df that have no URLs
#
extract_titles_of_NA_urls <- function(filepath, col_name, n, dir) {
  df <- read.csv(filepath)
  # create the directory if it does not exist
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  
  # Filter entries with no URL
  df_NAs <- df %>%
    filter(is.na(df$Url)==TRUE)
  
  # get the number of txt files needed
  num_files <- ceiling(nrow(df_NAs) / n)
  
  for (i in 1:num_files) {
    # get the start and end indices for the current file
    start_index <- (i-1) * n + 1
    end_index <- min(i * n, nrow(df_NAs))
    
    # extract the titles for the current file
    titles <- df_NAs[start_index:end_index, col_name]
    
    # save the titles to a txt file
    file_name <- paste(dir, "/titles", i, ".txt", sep = "")
    writeLines(titles, file_name)
  }
  
  View(df_NAs)
}


## Step 2: Data processing 

# Export the entries that have no or shortened abstract.
filter_gs_entries(
  filepath = here::here("data/study search/database search/processed/preparation for abstract screening/merging of EL and GS results and duplicate removal/merged_EL_GS_no_duplicates.csv"), 
  new_filepath = here::here("data/study search/database search/processed/preparation for abstract screening/retrieving missing or incomplete abstracts/entries_without_complete_abstracts.csv")
  )

# Export the entries that have seemingly complete abstracts.
filter_complete_entries(
  filepath = here::here("data/study search/database search/processed/preparation for abstract screening/merging of EL and GS results and duplicate removal/merged_EL_GS_no_duplicates.csv"), 
  new_filepath = here::here("data/study search/database search/processed/preparation for abstract screening/retrieving missing or incomplete abstracts/entries_complete_abstracts.csv")
)

# Extract URLs of entries without complete abstract.
extract_urls(
  filepath = here::here("data/study search/database search/processed/preparation for abstract screening/retrieving missing or incomplete abstracts/entries_without_complete_abstracts.csv"),
  col_name = "Url", 
  n = 100, 
  dir = here::here("data/study search/database search/processed/preparation for abstract screening/retrieving missing or incomplete abstracts/URLs")
  )

# Extract titles of those entries without complete abstract that have no URL
extract_titles_of_NA_urls(
  filepath = here::here("data/study search/database search/processed/preparation for abstract screening/retrieving missing or incomplete abstracts/entries_without_complete_abstracts.csv"),
  col_name = "Title",
  n = 100, 
  dir = here::here("data/study search/database search/processed/preparation for abstract screening/retrieving missing or incomplete abstracts/titles")
)
