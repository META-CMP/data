# Journal ranking classification 

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringdist)
library(stringr)

#Load data by running data_prep script
# load("data/preliminary_data_01082024.RData")
load("data/final_join_json_irf_data.RData") # Load the data from final_join()

# Set path for study sets
study_set_path <- here::here("data/study_search/database_search/processed/post_AS/packages_for_full_text_download")

## Read in xlsx files after the full text screening and the adjustment of the bibliographic information.
library(readxl)
file.list <- 1:27 # study_set_27 is obtained via snowballing. 
df.list <- lapply(file.list, function(x) read_excel(paste0(study_set_path, "/study_set_",x,".xlsx")))

# Combine all data frames into one
df_full_text <- bind_rows(df.list, .id = "id")

# Filter included studies
df_full_bib <- df_full_text %>% filter(included==1)
#### get all studies of the data frame which are part of our final sample used for the meta analysis
# df_full_bib <- df_full_text %>% filter(key %in% unique(data %>% pull(key)))

# Define our list of top 50 journals which are present in our dataset

# Create a list of A+
top5 <- c(
  "Quarterly Journal of Economics",
  "The Quarterly Journal of Economics",
  "Journal of Political Economy",
  "American Economic Review",
  "The American Economic Review",
  "Econometrica",
  "Review of Economic Studies",
  "The Review of Economic Studies")

# Create a list of A journals
journals <- c(
  "Journal of Finance",
  "The Journal of Finance",
  "Review of Financial Studies",
  "Journal of Financial Economics",
  "American Economic Journal: Applied Economics",
  "American Economic Journal: Macroeconomics",
  "Journal of Economic Literature",
  "American Economic Journal: Economic Policy",
  "Review of Economics and Statistics",
  "The Review of Economics and Statistics",
  "Journal of Economic Perspectives",
  "Journal of Marketing",
  "Journal of Accounting and Economics",
  "Manufacturing and Service Operations Management",
  "Review of Corporate Finance Studies",
  "Journal of Consumer Research",
  "Annual Review of Economics",
  "Marketing Science",
  "Journal of the Royal Statistical Society. Series B Statistical Methodology",
  "Journal of Accounting Research",
  "American Political Science Review",
  "American Economic Journal: Microeconomics",
  "Journal of the European Economic Association",
  "Journal of Labor Economics",
  "Journal of Marketing Research",
  "Annual Review of Financial Economics",
  "Journal of Business and Economic Statistics",
  "Journal of Financial Intermediation",
  "Economic Journal",
  "The Economic Journal",
  "Management Science",
  "Journal of Public Economics",
  "Review of Asset Pricing Studies",
  "Brookings Papers on Economic Activity",
  "Journal of Monetary Economics",
  "Accounting Review",
  "Journal of the American Statistical Association",
  "Journal of International Business Studies JIBS",
  "PNAS – Proceedings of the National Academy of Sciences",
  "Review of Accounting Studies",
  "Review of Finance",
  "European Finance Review",
  "California Management Review",
  "Econometrics Journal",
  "The Econometrics Journal",
  "Operations Research",
  "International Organization",
  "Journal of International Economics",
  "Journal of Financial and Quantitative Analysis"
)

# Set max_dist_parameter
## The max_dist_param should remain at 3 so that all journals are assigned correctly. 
max_dist_param <- 3

# Function to match journals with fuzzy matching
fuzzy_match_journal <- function(journal, top_tier_journals, max_dist = 3) {
  # Convert to lowercase for consistent matching
  journal <- str_to_lower(journal)
  top_tier_journals <- str_to_lower(top_tier_journals)
  
  # Compute the string distances
  distances <- stringdist::stringdistmatrix(journal, top_tier_journals, method = "lv")
  
  # Find the minimum distance
  min_dist <- apply(distances, 1, min)
  
  # Check if the minimum distance is within the acceptable threshold
  is_match <- min_dist <= max_dist
  
  return(is_match)
}

# Apply the fuzzy matching function 
df_full_bib <- df_full_bib %>%
  rowwise() %>%
  mutate(is_top_tier = fuzzy_match_journal(`publication title`, journals,max_dist = max_dist_param),is_top_5 = fuzzy_match_journal(`publication title`, top5,max_dist = max_dist_param))

# Convert logical to numeric (0 or 1)
df_full_bib <- df_full_bib %>%
  mutate(is_top_tier = as.numeric(is_top_tier),is_top_5 = as.numeric(is_top_5))

# Checked all the entries, one needed to be corrected
df_full_bib$is_top_5 <- ifelse(df_full_bib$`publication title` == "Economica", 0, df_full_bib$is_top_5)

###################### Save this information to be merged in the final_data_preparation_working_paper_1.R file.
save(df_full_bib, file = "data/sjr.RData")





