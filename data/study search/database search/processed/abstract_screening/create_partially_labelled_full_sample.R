## Merging labeled with unlabeled dataset for META-CMP
library(readr)
library(here)

# Import the complete unlabeled dataset
d_full <- read_csv(here("data/study search/database search/processed/abstract_screening/unlabelled_full_sample/unlabelled_full_sample.csv"))

length(unique(d_full$key)) == nrow(d_full) # Test if Key is unique
nrow(d_full) - length(unique(d_full$title)) # Number of duplicate titles
nrow(d_full) - length(unique(d_full$`abstract note`)) # Check for duplicate abstracts

# Add "included" column to the unlabeled dataset
d_full$included <- NA

# Import fully-labeled subset 
d_lab <- read_csv(here("data/study search/database search/processed/abstract_screening/1_percent_random_sample/asreview_dataset_all_morpep-meta-cmp-1-percent-random-sample-fully-labelled.csv"))

length(unique(d_lab$key)) == nrow(d_lab) # Test if Key is unique
nrow(d_lab) - length(unique(d_lab$title)) # Number of duplicate titles
nrow(d_lab) - length(unique(d_lab$abstract)) # Check for duplicate abstracts

# Deleting ASReview collumns (other than "inlcuded")
d_lab$record_id <- NULL 
d_lab$asreview_ranking <- NULL
d_lab$exported_notes_1 <- NULL

# Merging based on key
keys <- d_lab[,"key"]
keys <- keys$key
d_full_non_lab <- d_full[!d_full$key %in% keys, ] # Entries that are not in d_lab
d_full_lab <- rbind(d_lab, d_full_non_lab) # merged dataset including labeled entries
length(unique(d_full_lab$key)) == nrow(d_full_lab) # Test if Key is unique

# Writing the partially labeled full sample as .csv
write.csv(d_full_lab, here("data/study search/database search/processed/abstract_screening/partially_labeled_full_sample.csv"), row.names = FALSE)

