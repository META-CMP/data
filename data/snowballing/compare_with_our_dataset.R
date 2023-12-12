library(dplyr)
library(tidyr)
library(readr)


setwd("~/data/data/snowballing")
getwd()



## read in xlsx files after the full text screening and the adjustment of the bibliographic information.
library(readxl)
file.list <- 1:26
df.list <- lapply(file.list, function(x) read_excel(paste0("study_set_",x,".xlsx")))


library(dplyr)
df_full_text <- bind_rows(df.list, .id = "id")



# read in snowballing results. 
df_snowballing <- read_csv("snowballing.csv")

colnames(df_snowballing)<-tolower(colnames(df_snowballing))

# ########################## Compare with full text screened excel files #######################


# Create data frames
df1 <- data.frame(df_snowballing %>% select(`publication year`,author,title))
df2 <- data.frame(df_full_text %>% select(`publication year`,author,title))

# Create single character string for information we would like to investigate
string1<-paste(df1$author,df1$publication.year,df1$title)
string2<-paste(df2$author,df2$publication.year,df2$title)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name_full<-adist(string1,string2, partial = TRUE, ignore.case = TRUE)

# To name the columns of this matrix with the respective names of our dataset
colnames(dist.name_full)<-string2

# get the best match by retrieving the minimum 
min_dist_full<-apply(dist.name_full, 1, min)

# get the study information for the closest match
min_cols_full <- apply(dist.name_full, 1, function(x) colnames(dist.name_full)[which(x == min(x))])

# list to data frame
data_frame_author <- as.data.frame(do.call(rbind, min_cols_full))

# Generate data frame for orignial string from the other meta studies, the distance measure and the string of our study 
matches <- cbind(string1,min_dist_full,data_frame_author)
colnames(matches)[3:4]<-c("Our_sample","Our_sample_option_2")



# Join to df_snowballing

df_snowballing$string1<-paste(df_snowballing$author,df_snowballing$`publication year`,df_snowballing$title)

df_snowballing<-df_snowballing %>% left_join(matches, by="string1")

# NA 2001 Monetary Policy Rules, Croushore, Dean 2012 Comment, Fuhrer, Jeffrey C. 2010 Inflation Persistence, Bernanke, Ben 1993 Credit in the macroeconomy, del Negro, Marco 2011 Bayesian Macroeconometrics, Taylor, John B. 2007 Housing and Monetary Policy, Melosi, Leonardo 2017 Signalling Effects of Monetary Policy # have lower values than 20, but are not accordingly matched. Thus we added them later to the df_snow_miss.xlsx file and did the same checks as for the other studies of this file. 

# filter those which do not have matches
df_snow_miss<- df_snowballing %>%  filter(min_dist_full>20)



# ########################## Compare with abstract screened dataset #######################

# Compare in a second step the remaining entries with the dataset which has been used for abstract screening
setwd("~/data")
merged_EL_GS_no_duplicates <- read_csv("data/study search/database search/processed/preparation for abstract screening/merging of EL and GS results and duplicate removal/merged_EL_GS_no_duplicates.csv")
colnames(merged_EL_GS_no_duplicates)<-tolower(colnames(merged_EL_GS_no_duplicates))

# Create data frames
df1 <- data.frame(df_snow_miss %>% select(`publication year`,author,title))
df2 <- data.frame(merged_EL_GS_no_duplicates %>% select(`publication year`,author,title))

# Create single character string for information we would like to investigate
string1<-paste(df1$publication.year,df1$title)
string2<-paste(df2$publication.year,df2$title)

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name_full<-adist(string1,string2, partial = FALSE, ignore.case = TRUE)

# To name the columns of this matrix with the respective names of our dataset
colnames(dist.name_full)<-string2

# get the best match by retrieving the minimum 
min_dist_full<-apply(dist.name_full, 1, min)

# get the study information for the closest match
min_cols_full <- apply(dist.name_full, 1, function(x) colnames(dist.name_full)[which(x == min(x))])

# list to data frame
data_frame_author <- as.data.frame(do.call(rbind, min_cols_full))

# Generate data frame for orignial string from the other meta studies, the distance measure and the string of our study 
matches <- cbind(string1,min_dist_full,data_frame_author)
colnames(matches)[3:4]<-c("Our_sample","Our_sample_option_2")


df_snow_miss$string1<-paste(df_snow_miss$`publication year`,df_snow_miss$title)

df_snow_miss<-df_snow_miss %>% left_join(matches, by="string1")

df_snow_miss<- df_snow_miss %>%  filter(min_dist_full>=5)



########################## save the remaining unmatched entries as an excel file for Title and abstract screening ###########################

library(xlsx)
setwd("~/data/data/snowballing")
#write.xlsx(df_snow_miss, 'df_snow_miss.xlsx')

sum(is.na(df_snow_miss$`abstract note`))

# manual screen of the remaining ones and exlcude studies already based on far off titles. Save this file as df_snow_miss.xlsx

# abstract screening for the remaining ones. Continue in df_snow_miss_abstract_screening.xlsx

# after the abstract screening 59 studies need to be full text screened which have benn saved in df_snowballing_full_text_screening



#### produce BibtexKey column for full text screening. 


test <- read_excel("df_snowballing_full_text_screening.xlsx")



create_bibtex_key <- function(year, author, title) {
  # Split author names and select first name
  first_author <- strsplit(author, ",")[[1]][1]
  # Remove non-alphanumeric characters from first word of title and convert to lowercase
  first_word <- tolower(gsub("[^[:alnum:]]", "", strsplit(title, " ")[[1]][1]))
  # Combine components to create Bibtex key
  paste0(first_author, "_", year, "_", first_word)
}

test$BibtexKey <- apply(test, 1, function(row) {
  create_bibtex_key(row["publication year"], row["author"], row["title"])
})

#write.xlsx(test, 'bibtexkey.xlsx')
