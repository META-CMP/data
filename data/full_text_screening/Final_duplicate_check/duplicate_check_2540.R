setwd("~/data/data/study search/database search/processed/post_AS/packages_for_full_text_download")
getwd()
library(readxl)
file.list <- 1:26
df.list <- lapply(file.list, function(x) read_excel(paste0("study_set_",x,".xlsx")))

library(dplyr)
df <- bind_rows(df.list, .id = "id")


library(revtools)


setwd("~/data/data/full_text_screening/Final_duplicate_check")

# Screen duplicates in the "revtools" app for the xlsx files using title matches:
screen_duplicates(as.data.frame(df))

# saved as dup_free.csv.

df<-read.csv("dup_free.csv")


# Screen duplicates in the "revtools" app for the xlsx files using author matches:
screen_duplicates(as.data.frame(df))

#saved as dup_free_author.csv.

df<-read.csv("dup_free_author.csv")


# Screen duplicates in the "revtools" app for the xlsx files using title matches (with a change in the distance parameter):
screen_duplicates(as.data.frame(df))


df<-read.csv("dup_free_title2.csv")


# Screen duplicates in the "revtools" app for the xlsx files using abstract matches:
screen_duplicates(as.data.frame(df))


df<-read.csv("duplicate_free_final.csv")

test<-df %>% filter(grepl("Krusec",author, fixed = TRUE))


