library(dplyr)
library(tidyr)
library(readr)

merged_EL_GS_no_duplicates <- read_csv("data/study_search/database_search/processed/preparation_for_abstract_screening/merging_of_EL_and_GS_results_and_duplicate_removal/merged_EL_GS_no_duplicates.csv")

Nguyen_2021 <- read_csv("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/Nguyen_2021.csv")
Nguyen_2020 <- read_csv("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/Nguyen_2020.csv")

#De grauwe do not list their studies, the others do not state their search strategy.

# Load the readxl package
library(readxl)


# Read in the third sheet of the Excel file
Havranek_2013 <- read_excel("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/Havranek_2013.xls", sheet = "studies")
Rusnak_2013 <- read_excel("data/study_search/database_search/processed/preparation_for_abstract_screening/studies_from_other_meta_studies/Rusnak_2013.xls", sheet = "studies")

# Havranek_2013 and Rusnak use the exact same sample of studies
sum(Rusnak_2013$Title==Havranek_2013$Title)





# ########################## NGUYEN 2020


# Create data frames
df1 <- data.frame(Nguyen_2020 %>% select(`Publication Year`,Author,Title))
df2 <- data.frame(merged_EL_GS_no_duplicates %>% select(`Publication Year`,Author,Title))

# Create single character string for information we would like to investigate
string1<-paste(df1$Publication.Year,df1$Author,df1$Title)
string2<-paste(df2$Publication.Year,df2$Author,df2$Title)

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

# The following observations are currently not covered in our dataset
additional_entries_nguyen2020<-matches[min_dist_full > 26, ]


########################## NGUYEN 2021

#Alternative by combininig year author and title
df1 <- data.frame(Nguyen_2021 %>% select(`Publication Year`,Author,Title))
df2 <- data.frame(merged_EL_GS_no_duplicates %>% select(`Publication Year`,Author,Title))


# Create single character string for information we would like to investigate
string1<-paste(df1$Publication.Year,df1$Author,df1$Title)
string2<-paste(df2$Publication.Year,df2$Author,df2$Title)

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

# The following observations are currently not covered in our dataset
additional_entries_nguyen2021<-matches[min_dist_full > 26, ]





########################## Rusnak_2013

#Alternative by combininig year author and title
df1 <- data.frame(Rusnak_2013 %>% select(Year,Study,Title))
df2 <- data.frame(merged_EL_GS_no_duplicates %>% select(`Publication Year`,Author,Title))


df1$Study<-vapply(strsplit(df1$Study,split = " [(]"), `[`, 1, FUN.VALUE=character(1))

# Create single character string for information we would like to investigate
string1<-paste(df1$Year,df1$Study,df1$Title)
string2<-paste(df2$Publication.Year,df2$Author,df2$Title)

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

# check these observations which seem different
additional_entries_havranek_runsak<-matches[min_dist_full > 25, ]

#ADDITIONAL to those 7 entries 12 studies have been found in Havranek 2013 and Rusnak 2013 which are not covered in our preliminary dataset: (The EFN study was not available)

# 2010 Forni and Gambetti The dynamic effects of monetary policy: A structural factor model approach they have a newer study ours is from 2008
# 2004 EFN Monetary transmission in acceding countries # ONLY APPENDIX FOUND AND NO ABSTRACT #NOT IN OUR DATABASE
# 1997 Bernanke et al. Systematic Monetary Policy and the Effects of Oil Price Shocks (HERE WE ONLY HAVE THE COMMENT BUT NOT THE ORIGINAL STUDY) - NO ABSTRACT
# 2009 Borys et al. The effects of monetary policy in the Czech Republic: an empirical study - MAYBE THERE EXCISTS A NEWER STUDY we only have 2008
# 2000 Shioji Identifying Monetary Policy Shocks in Japan
# 2010 Lange Regime-switching monetary policy in Canada
# 2009 Peersman and Straub Technology Shocks and Robust Sign Restrictions in a Euro Area SVAR - maybe newer version available (ours is from 2004)
# 2010 Banbura et al. Large Bayesian VARs
# 2008 Cespedes et al. Monetary Policy, Inflation and the Level of Economic Activity in Brazil after the Real Plan: Stylized Facts from SVAR Models (ours is from 2005)
# 2008 Andries Monetary policy transmission mechanism in Romania - a VAR approach
# 1992 Eichenbaum Comment on 'Interpreting the macroeconomic time series facts: The effects of monetary policy' NO ABSTRACT
# 1998 Leeper, Sims and Zha What does monetary policy do?
# 2008 Mertens Deposit rate ceilings and monetary transmission in the US



additional_entries_nguyen2020$covered_in_additional_entries_nguyen2021<-is.element(additional_entries_nguyen2020$string1,additional_entries_nguyen2021$string1)
MISSING<-rbind(additional_entries_nguyen2021,additional_entries_nguyen2020 %>% filter(covered_in_additional_entries_nguyen2021==FALSE) %>% select(-covered_in_additional_entries_nguyen2021,-V3,-V4))
