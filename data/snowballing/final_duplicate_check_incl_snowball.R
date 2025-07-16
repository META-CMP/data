# Script for final duplicate script of studies after snowballing
library(dplyr)
library(revtools) # For screening duplicates

# Set path for study sets
study_set_path <- here::here("data/study_search/database_search/processed/post_AS/packages_for_full_text_download")

## Read in xlsx files after the full text screening and the adjustment of the bibliographic information.
library(readxl)
file.list <- 1:27 # study_set_27 is obtained via snowballing. 
df.list <- lapply(file.list, function(x) read_excel(paste0(study_set_path, "/study_set_",x,".xlsx")))

# Combine all data frames into one
df <- bind_rows(df.list, .id = "id")

# Filter included studies
included <- df %>% filter(included==1)

# Screen duplicates in the "revtools" app for the xlsx files using title matches:
# use fuzzymatching for title, abstract and authors. We used the "stringdist" function and the "osa" method, by selcting a maximum distance of 5 and removed punctuation and made the letter to lower case. This way we identified 7 likely duplicated studies. 
screen_duplicates(as.data.frame(included))

# We then manually searched for first author surnames and checked for duplicate studies in the resulting csv file. 

test <- included %>% filter(grepl("Zhang",author, fixed = TRUE))

# This way we identified three more duplicated entries:
# Sousa_2013_the (10) is the published version of Gameiro_2010_monetary (9). Both papers are coded, we only use published version.
# Rabanal_2007_does (14) is the published version of Rabanal_2007_the (23). Both papers are coded, we only use published version.
# Karaca_2017_aggregate (1) is the published version of the 3rd chapter in the thesis Tugan_2014_three (8). Both papers are coded, we only use published version.
