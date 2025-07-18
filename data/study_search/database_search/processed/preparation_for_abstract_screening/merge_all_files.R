# Merge the five files:
#   
# 1. `entries_complete_abstracts.csv`
# 2. `abstracts_from_titles.csv`
# 3. `abstracts_from_URLs.csv`
# 4.  havranek_2013.csv
# 5.  Nguyen_2021_2020.csv

library(tidyverse)
library(here)
filepath1 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/entries_complete_abstracts.csv")
filepath2 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/abstracts_from_titles.csv")
filepath3 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/abstracts_from_URLs.csv")
filepath4 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/havranek_2013.csv")
filepath5 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/Nguyen_2021_2020.csv")


df1 <- readr::read_csv(filepath1)
df2 <- readr::read_csv(filepath2)
df3 <- readr::read_csv(filepath3)
df4 <- readr::read_csv(filepath4)
df5 <- readr::read_csv(filepath5)

df <- rbind(df1, df2, df3, df4, df5)


### Final checks 


### Missing abstract test

sum(is.na(df$`Abstract Note`)) # 621 missing abstracts.


## Former Test if there are further available abstracts for a random sample of 10%

# test<-cbind(df,runif(nrow(df)))
# look<-test[is.na(test$`Abstract Note`)&test$`runif(nrow(df))`<.09, ]
# 
# look$Url
# write.csv(look$Url,"urls_test",row.names = FALSE,fileEncoding =  "macroman")

## ~18% of those would have had an abstract available


### Google scholar abstract test

sum(grepl("…", df$`Abstract Note`) == TRUE) 
View(df[grepl("…", df$`Abstract Note`) == TRUE, ])# 25 studies do not include abbreviated Google Scholar abstracts, but for other reasons "..." # Look for abstracts with "..." which seem incomplete and save them as points_in_abstracts.csv




##################### Merge the addititional abstracts from the points_in_abstracts.csv file and check for duplicates using the revtools package:

filepath1 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/entries_complete_abstracts.csv")
filepath2 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/abstracts_from_titles.csv")
filepath3 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/abstracts_from_URLs.csv")
filepath4 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/havranek_2013.csv")
filepath5 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/Nguyen_2021_2020.csv")
filepath6 <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/points_in_abstracts.csv")

df1 <- readr::read_csv(filepath1)
df2 <- readr::read_csv(filepath2)
df3 <- readr::read_csv(filepath3)
df4 <- readr::read_csv(filepath4)
df5 <- readr::read_csv(filepath5)
df6 <- readr::read_csv(filepath6)
df <- rbind(df1, df2, df3, df4, df5, df6)



### Duplicates check work flow: 
library(revtools)

# Check whether the "Key" is uniquely defined:
matches <- find_duplicates(df,"Key")
data_unique <- extract_unique_references(df, matches)
# --> Key is uniquely defined

# Screen duplicates in the "revtools" app using perfect Title matches:
screen_duplicates(as.data.frame(df))

####### opened in app, screened for duplicates, and saved the files without the removed duplicates as "Final.csv"


####### LOAD the "Final.csv" file and conduct a second round of duplicates checks. 

# Second round of duplicates screening using fuzzdist matching with lowercases, the M ratio, and 0.05 as the maximum distance. #395 suggestions of duplicates
filepath <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/Final.csv")

df <- readr::read_csv(filepath)

screen_duplicates(as.data.frame(df)) # opened in app and screened for duplicates

# Save the final data.frame "Final_double_check.csv" directly in the app. 



#########################################################################


## Use the final duplicate free File:
filepath <- here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/retrieving_missing_or_incomplete_abstracts/Final_double_check.csv")

df <- readr::read_csv(filepath)



# Do a final round of checks:

### Missing abstract test

sum(is.na(df$`abstract note`)) # 609 missing abstracts.

# Number of entries with Title and abstract
nrow(df)-sum(is.na(df$`abstract note`)|is.na(df$title)) 


### Google scholar abstract test

sum(grepl("…", df$`abstract note`) == TRUE) # The 15 remaining studies do not include abbreviated Google Scholar abstracts, but for other reasons "..."
# View(df[grepl("…", df$`Abstract Note`) == TRUE, ])


### Final duplicate check (same title & same abstract)


duplicated_rows <- duplicated(df[,c("title", "abstract note")]) | duplicated(df[,c("title", "abstract note")], fromLast = TRUE) # CHECK those again?
sum(duplicated_rows) # Number of seeming duplicates based on title and abstract
View(df[duplicated_rows, ])



### Store the dataset as `unlabelled_full_sample.csv` without the remaining missing abstracts and titles:


df<-df[!(is.na(df$`abstract note`)|is.na(df$title)),]

write.csv(df, here::here("data/study_search/database_search/processed/abstract_screening/unlabelled_full_sample.csv"), row.names = FALSE)