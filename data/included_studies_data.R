# Load required libraries
library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)

# Set path for study sets
directory <- here::here("data/study_search/database_search/processed/post_AS/packages_for_full_text_download")

# Get list of files in the directory
files <- list.files(directory, pattern = "^study_set_.*\\.xlsx$", full.names = TRUE)

# Function to read and process each file
read_and_process <- function(file_path) {
  df <- read_excel(file_path)
  
  # Ensure 'included' column exists and is numeric
  if (!"included" %in% names(df)) {
    warning(paste("File", file_path, "does not have an 'included' column. Skipping filtering."))
    return(df)
  }
  
  df <- df %>% 
    mutate(across(everything(), as.character)) %>%  # Convert all columns to character
    filter(included == "1")  # Filter after conversion
  
  return(df)
}

# Read and process all files, then combine them
papers <- files %>%
  map(read_and_process) %>%
  bind_rows()

papers$`publication year` <- as.integer(papers$`publication year`)

# Prepare Excel file for data collection of citations via Google Scholar
cg_data <- papers %>% select(key, author, title, `publication year`, `publication title`, `item type`)
cg_data$GS_link <- NA
cg_data$num_cit <- NA
cg_data$gs_date <- NA
cg_data$note <- NA

# Save the table as xlsx
write.xlsx(cg_data, file = "data/study_characteristics/citations_for_included_studies.xlsx")

# Prepare table for data collection of journal ranking and impact factor via resurchify
cg_data_journals <-data.frame(
  "publication title" = unique(cg_data$`publication title`),
  resurchify_link = NA, 
  journal_ranking = NA, 
  journal_impact = NA, 
  resurchify_date = NA,
  note = NA
) 
write.xlsx(cg_data_journals, file = "data/study_characteristics/sjr.xlsx")

