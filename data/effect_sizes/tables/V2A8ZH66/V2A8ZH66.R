### table transformation 

#check study key:
key

# Define the subfolder names
subsubfolders <- c("gr_rgdp")








################## Table 14



# Load the readxl library for reading Excel files
library(readxl)

# Read the data from an Excel file into the 'data' variable
data <- readxl::read_xlsx(paste0(key, "/table14.xlsx"))

# Add a new column 'est' based on the values in the 'Quarter' column
# If 'Quarter' is NA, set 'est' to "se", otherwise set it to "mean"
data$est <- ifelse(is.na(data$Quarter), "se", "mean")

# Load the tidyr and dplyr libraries for data manipulation
library(tidyr)
library(dplyr)

# Convert 'data' to a data frame, fill missing values in the 'Quarter' column downward,
# convert the data from wide to long format, with 'DEU:SWE' columns representing 'country',
# and 'est' and 'value' columns representing the column headers and corresponding values respectively
data <- data %>% as.data.frame() %>% fill(Quarter, .direction = 'down') %>% 
  pivot_longer(DEU:SWE, names_to = "country") %>% 
  pivot_wider(names_from = "est", values_from = "value")

# Remove parentheses from 'se' values and convert them to numeric
data$se <- as.numeric(gsub('[()]', '', data$se))

# Replace '\u0002' character from 'mean' values with minus and convert them to numeric
data$mean <- as.numeric(gsub('\u0002', '-', data$mean))

# Add a new column 'model' by repeating the string "model_" followed by numbers from 1 to 12
data$model <- rep(paste0("model_", 1:12), 5)

# Perform calculations to add 'upper' and 'lower' columns based on 'mean' and 'se' columns,
# remove 'se' and 'country' columns, and convert the data from wide to long format with 'mean',
# 'upper', and 'lower' columns representing the column headers
data <- data %>% mutate(upper = mean + se, lower = mean - se) %>% 
  dplyr::select(!c(se, country)) %>% 
  pivot_longer(c(mean, upper, lower), names_to = "est")

# Load the purrr and readr libraries for working with data and files
library(purrr)
library(readr)

# Split the data into groups based on 'model' and 'est' columns,
# and write each group as a separate CSV file in the respective subfolders
data %>% 
  group_split(model, est) %>% 
  walk(~write.table(.x %>% select(!c(model, est)), 
                    paste0(key, "/", .x$model[1], "/", subsubfolders, "/", .x$est[1], ".csv"),
                    row.names = FALSE, col.names = FALSE, sep = ";", dec = ","))


#read.csv(paste0(key,"/","model_9","/",subsubfolders,"/","lower",".csv"),sep=";",dec = ",",header = FALSE)