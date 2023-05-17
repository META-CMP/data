library(tidyverse)
library(openxlsx)
library(here)

# Importing screener 1 data (all)
s1_data <- read_csv(here("data/study search/database search/processed/post_AS/screener_1_data/asreview_dataset_all_abstract-screening-morpep-meta-cmp-screener-1.csv"))

# Importing screener 2 data (all)
s2_data <- read_csv(here("data/study search/database search/processed/post_AS/screener_2_data/asreview_dataset_all_abstract-screening-morpep-meta-cmp-screener-2.csv"))

## Some consistency tests before merging 
# Screener 1: only valid screener notes?
unique(s1_data$exported_notes_1)
paste('For screener 1, there is one "22", which is marked as relevant. No correction needed.')
View(s1_data %>% filter(exported_notes_1=="22")) # Inspecting the entry.
paste(s1_data %>% filter(exported_notes_1=="22") %>% select(`abstract note`)) # Inspecting the abstract
paste('For screener 1, there is one "00", which is marked as irrelevant. No correction needed.')
View(s1_data %>% filter(exported_notes_1=="00")) # Inspecting the entry.
paste(s1_data %>% filter(exported_notes_1=="00") %>% select(`abstract note`)) # Inspecting the abstract
paste("Exclusion seems valid.")
# Screener 2: only valid screener notes?
unique(s2_data$exported_notes_1)
paste('For screener 1, there is one "22", which is marked as relevant. No correction needed.')
View(s2_data %>% filter(exported_notes_1=="22")) # Inspecting the entry.
paste(s2_data %>% filter(exported_notes_1=="22") %>% select(`abstract note`)) # Inspecting abstract.

# Do screener 1 notes correctly correspond to in-/exclusion of studies?
inclusion_notes <- c("111", "112", "121", "211", "122", "212", "221", "222") # Index of entries that carry inclusion notes
s1_included_notes <- s1_data$exported_notes_1 %in% inclusion_notes 
sum(s1_data$included[s1_included_notes] != 1) 
paste("For screener 1, there is one entry that was excluded though it carries an inclusion note. Correction not necessary because one mistake will only marginally affect the sample.")
View(s1_data %>% filter(included == "0" & exported_notes_1 %in% inclusion_notes )) # Inspecting the entry
paste(s1_data %>% filter(included == "0" & exported_notes_1 %in% inclusion_notes ) %>% select(`abstract note`)) # Inspecting the abstract

exclusion_notes <- c("000", "001", "002", "010", "011", "012", "020", "021", "022", "100", "101", "102", "110", "120", "200", "201", "202", "210", "220", "3", "4") # Index of entries that carry exclusion notes
s1_excluded_notes <- s1_data$exported_notes_1 %in% exclusion_notes 
sum(s1_data$included[s1_excluded_notes] != 0) 
paste("For screener 1, there is one entry that is included though it carries an exclusion note. But no correction necessary, as the entry will be excluded in the full text screening phase.")
View(s1_data %>% filter(included == 1 & exported_notes_1 %in% exclusion_notes)) # Inspect the entry.
paste(s1_data %>% filter(included == 1 & exported_notes_1 %in% exclusion_notes ) %>% select(`abstract note`)) # Inspecting the abstract

# Do screener 2 notes correctly correspond to in-/exclusion of studies?
s2_included_notes <- s2_data$exported_notes_1 %in% inclusion_notes
sum(s2_data$included[s2_included_notes] != 1) 
paste("For screener 2, there are 3 entries that were excluded though they carries inclusion notes. Correction not necessary because this mistake will only marginally affect the sample.")
View(s2_data %>% filter(included == "0" & exported_notes_1 %in% inclusion_notes )) # Inspecting the entry
paste(s2_data %>% filter(included == "0" & exported_notes_1 %in% inclusion_notes ) %>% select(`abstract note`)) # Inspecting the abstract

s2_excluded_notes <- s2_data$exported_notes_1 %in% exclusion_notes
sum(s2_data$included[s2_excluded_notes] != 0) 
paste("No invalid inclusions for screener 2.")

## Merging the data
# Test that record id and key are matching for both s1_data and s2_data
all(sort(paste0(s1_data$record_id, s1_data$key)) == sort(paste0(s2_data$record_id, s2_data$key)))

# Merge s1_data and s2_data based on all variables that must have equal values, i.e. those that have not been created by ASReview
columns <- colnames(s1_data) # These are the column names of the dataframe (they are equal for s1_data and s2_data, see: 'sum(colnames(s1_data) != colnames(s2_data))')
matching_columns <- head(columns, -3) # These are the columns that have not been created by ASReview
merged_data <- merge(s1_data, s2_data, by = matching_columns, all = T, sort = F, suffixes = c(".s1",".s2")) # This merges the dataframes by all columns that must have equal values. Columns with separate values are retained.
# Test that merged data correctly matches original data
all(na.omit(merged_data[, c("record_id", "key", "included.s1", "exported_notes_1.s1")]) == na.omit(s1_data[, c("record_id", "key", "included", "exported_notes_1")])) # For screener 1, test that record_id, key, included and exported_notes_1 in the merged data matches the original entries
from_merged <- na.omit(merged_data[, c("record_id", "key", "included.s2", "exported_notes_1.s2")])
from_merged <- from_merged[order(from_merged$record_id),]
from_s2 <- na.omit(s2_data[, c("record_id", "key", "included", "exported_notes_1")])      
from_s2 <- from_s2[order(from_s2$record_id),]
all(from_merged == from_s2) # For screener 2, test that record_id, key, included and exported_notes_1 in the merged data matches the original entries (4 steps before are necessary, because "merged_data" is sorted as s1_data)

# Analyses of screening
s1_s2_agreement_data <- merged_data %>%   
  select(key, included.s1, included.s2, exported_notes_1.s1, exported_notes_1.s2) %>% 
  filter(!is.na(included.s1) | !is.na(included.s2))
paste(nrow(s1_s2_agreement_data), "entries were screened by either screener 1 or 2.", sum(s1_s2_agreement_data$included.s1, na.rm = T), "were included by screener 1.", sum(s1_s2_agreement_data$included.s2, na.rm = T), "were included by screener 2") 
paste(nrow(s1_s2_agreement_data %>% filter(included.s1==1 | included.s2==1)), "were marked as relevant by at least one screener.")
paste(nrow(s1_s2_agreement_data %>% filter(included.s1==1 & included.s2==1)), "were marked as relevant by both screeners in aggreement.")
# Screener 1 proportions of in-/exclusions notes
round(prop.table(table(s1_s2_agreement_data$exported_notes_1.s1)) * 100, 1)
barplot(prop.table(table(s1_s2_agreement_data$exported_notes_1.s1)) * 100)
title("Screener 1 notes (%)")
# Screener 2 proportions of in-/exclusions notes
round(prop.table(table(s1_s2_agreement_data$exported_notes_1.s2)) * 100, 1)
barplot(prop.table(table(s1_s2_agreement_data$exported_notes_1.s2)) * 100)
title("Screener 2 notes (%)")
# Agreement and overlap
# Included by both screeners
included_both <- s1_s2_agreement_data %>% 
  filter(included.s1 == 1 & included.s2 == 1)
paste(nrow(included_both), "entries were included by both screeners.")
# Excluded by both screeners 
excluded_both <- s1_s2_agreement_data %>% 
  filter(included.s1 == 0 & included.s2 == 0)
paste(nrow(excluded_both), "entries were excluded by both screeners.")
# Included by screener 1, excluded by screener 2
included_s1_excluded_s2 <- s1_s2_agreement_data %>% 
  filter(included.s1 == 1 & included.s2 == 0)
paste(nrow(included_s1_excluded_s2), "entries were included by screener 1 but excluded by screener 2.")
# Included by Screener 2, excluded by screener 1
included_s2_excluded_s1 <- s1_s2_agreement_data %>% 
  filter(included.s1 == 0 & included.s2 == 1)
paste(nrow(included_s2_excluded_s1), "entries were included by screener 2 but excluded by screener 1.")
# Screened by both screener 1 and 2
both <- s1_s2_agreement_data %>% 
  filter(!is.na(included.s2) & !is.na(included.s1))
paste(nrow(both), "entries were screened by both screeners. Of these,",
      sum(both$included.s1), ", i.e.", round(sum(both$included.s1)/nrow(both)*100,1), "%, were included by Screener 1 and", sum(both$included.s2), ", i.e.", round(sum(both$included.s2)/nrow(both)*100,1), "%, were included by Screener 2")
# Exclusively screened by Screener 1
only_s1 <- s1_s2_agreement_data %>% 
  filter(is.na(included.s2) & !is.na(included.s1))
paste(nrow(only_s1), "entries were screened exclusively by Screener 1. Of these,",
      sum(only_s1$included.s1), "were included, i.e.", round(sum(only_s1$included.s1)/nrow(only_s1)*100,1), "%")
# Exclusively screened by Screener 2
only_s2 <- s1_s2_agreement_data %>% 
  filter(is.na(included.s1) & !is.na(included.s2))
paste(nrow(only_s2), "entries were screened exclusively by Screener 2. Of these,",
      sum(only_s2$included.s2), "were included, i.e.", round(sum(only_s2$included.s2)/nrow(only_s2)*100,1), "%")

# Extracting relevant studies
relevant_entries <- merged_data %>% 
  filter(included.s1 == 1 | included.s2 == 1)
# Check that number of included in merged relevant entries matches number of included in original screener data
sum(relevant_entries$included.s1, na.rm = T) == sum(s1_data$included, na.rm = T)
sum(relevant_entries$included.s2, na.rm = T) == sum(s2_data$included, na.rm = T)

# Adding a bibtex key

#' Creates a Bibtex key from a dataframe with publication information.
#'
#' This function takes three inputs, year, author and
#' title, and creates a Bibtex key for each row in the dataframe. The Bibtex key has
#' the format: name-of-first-author_year_first-word-of-title.
#'
#' @param year A vector with the publication years.
#' @param author A vector with the authors' names.
#' @param title A vector with the publication titles.
#' @return A vector with the Bibtex keys.
create_bibtex_key <- function(year, author, title) {
  # Split author names and select first name
  first_author <- strsplit(author, ",")[[1]][1]
  # Remove non-alphanumeric characters from first word of title and convert to lowercase
  first_word <- tolower(gsub("[^[:alnum:]]", "", strsplit(title, " ")[[1]][1]))
  # Combine components to create Bibtex key
  paste0(first_author, "_", year, "_", first_word)
}
relevant_entries$BibtexKey <- apply(relevant_entries, 1, function(row) {
  create_bibtex_key(row["publication year"], row["author"], row["title"])
})
# Test uniqueness of bibtex keys:
if (length(unique(relevant_entries$BibtexKey)) == length(relevant_entries$BibtexKey)) {
  message("The generated Bibtex-keys are unique.")
} else {
  warning("There are ", length(relevant_entries$BibtexKey) - length(unique(relevant_entries$BibtexKey)), " duplicate Bibtex-keys.")
  View(as.data.frame(relevant_entries$BibtexKey[duplicated(relevant_entries$BibtexKey)]))
}

# Randomizing the order of the dataset
# Set the random seed
set.seed(364)
# Randomizing the dataframe
randomized_relevant_entries <- relevant_entries[sample(nrow(relevant_entries)),]

# Extract data for full text download
data_for_download <- randomized_relevant_entries %>% 
  select(record_id, key, author, title, `publication year`, `publication title`, issue, volume, `item type`, `abstract note`, doi, url, BibtexKey) %>% 
  mutate(most_recent_version_available = NA, not_available = NA, retracted = NA, duplicate = NA, PDF_source = NA, access_date = NA, personal_id = NA, master_bachelor_thesis = NA, notes_PDF_download = NA)

# Create packages for full text download
#' Save Dataframe to Multiple Excel Files
#' 
#' This function saves the entries of a dataframe into xlsx files with a maximum of 100 entries per file.
#' 
#' @param df The dataframe to be saved.
#' @param file_path The path and filename prefix for the output files.
#' 
#' @return None
#' 
#' @export
save_to_xlsx <- function(df, file_path) {
  
  # Get number of files to be saved
  n_files <- ceiling(nrow(df) / 100)
  
  # Split dataframe into chunks of 100 rows
  df_list <- split(df, rep(1:n_files, each=100, length.out=nrow(df)))
  
  # Save each chunk to a separate file
  for (i in seq_along(df_list)) {
    file_name <- paste0(file_path, "_", i, ".xlsx")
    write.xlsx(df_list[[i]], file_name, rowNames = FALSE)
  }
  
}
# save_to_xlsx(data_for_download, here("data/study search/database search/processed/post_AS/packages_for_full_text_download/study_set"))

