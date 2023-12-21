library(tidyr)

#################################### run this only once to set up the doublecheck files #########################

# # Set the working directory to "~/data/data/snowballing"
# setwd("~/data/data/snowballing")
# 
# 
# ## read in xlsx files after the full text screening of all studies 
# ## (including snowballed studies) and the adjustment of the bibliographic information, 
# ## do this only once to create an R file.
# library(readxl)
# 
# # Create a list of file indices from 1 to 27
# file.list <- 1:27
# 
# # Read each Excel file ("study_set_1.xlsx", ..., "study_set_27.xlsx") into a list of data frames
# df.list <- lapply(file.list, function(x) read_excel(paste0("study_set_", x, ".xlsx")))
# 
# library(dplyr)
# 
# # Combine all data frames in the list into one data frame (df_full_text)
# df_full_text <- bind_rows(df.list, .id = "id")
# 
# # Adjust the type of the 'doublecheck' column to logical
# df_full_text$doublecheck <- ifelse(df_full_text$doublecheck != 1 | is.na(df_full_text$doublecheck), FALSE, TRUE)
# 
# # Filter rows where 'included' is TRUE to only use inluded studies 
# df_full_text <- df_full_text %>% filter(included == TRUE)
# 
# # If 'reason_for_doublecheck' is NA, use the 'reason for doublecheck' column
# df_full_text$reason_for_doublecheck <- ifelse(is.na(df_full_text$reason_for_doublecheck),
#                                               df_full_text$`reason for doublecheck`,
#                                               df_full_text$reason_for_doublecheck)
# 
# # Create a new data frame 'doublechecks' containing rows where 'rid2' is not NA, thus containing already doublechecked studies.
# doublechecks <- df_full_text %>% filter(!is.na(rid2))
# 
# # Select specific columns from 'doublechecks' and rename some columns
# doublechecks <- doublechecks %>% select(key, `publication title`, BibtexKey, rid1, rid2, doublecheck, reason_for_doublecheck)
# 
# # Add a new column 'doublecheck_completed' with value 1 since all of theses studies have already been doublechecked.
# doublechecks$doublecheck_completed <- 1
# 
# # Rename columns 6 and 7 in 'doublechecks'
# colnames(doublechecks)[6:7] <- c("doublecheck_request", "reason_for_doublecheck_request")
# 
# # Set the working directory to "~/data/data/doublechecks"
# setwd("~/data/data/doublechecks")
# 
# library(readr)
# 
# # Write 'doublechecks' data frame to a CSV file named "assigned_doublechecks.csv"
# write_csv(doublechecks, "assigned_doublechecks.csv")
# 
# # Save the 'df_full_text' data frame as an RDS file
# saveRDS(df_full_text, "df_full_text")


##### actually do the doublecheck assignment


# load package

library(MetaExtractR)

setwd("~/data")

# assign doublechecks
assign_doublechecks("ph",10)
assign_doublechecks("sg",10)
assign_doublechecks("me",10)
assign_doublechecks("fp",10)
assign_doublechecks("dr",10)



