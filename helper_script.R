# Helper script
library(MetaExtractR)

setwd("~/data")

key <- "AJTDIJCP"

# Create
MetaExtractR::create_json_file(
  key = key,
  rid = "me",
  codebook = "codebook.csv",
  folder_path = "data/full_text_screening/JSON_files"
)
file <- paste0("data/full_text_screening/JSON_files/", key, ".json")
file.edit(here::here(file))



library(countrycode)
# Countrycodes
countries <- c("Euro Area")
countrycode::countrycode(
  sourcevar = countries,
  origin = "country.name",
  destination = "iso2c"
)

# Parse
df <- MetaExtractR::parse_study_json(
  json_file = paste0(key, ".json"), 
  json_path = "data/full_text_screening/JSON_files",
  view_data = F)
View(df)

# Notes
df_notes <- strsplit(df$study_notes, ";")[[1]]
note_on_effect_size <- grep("quality_concern", df_notes, value = TRUE)
note_on_effect_size


#' Create JSON File
#'
#' This function generates a JSON file based on a codebook with variable 
#' names and variable categories, a study key, and a researcher ID.
#'
#' @param key Study key (character): A unique identifier for the study.
#' @param rid Researcher ID (character): A unique identifier for the researcher.
#' @param codebook File path (character, default = "codebook.xlsx"): The path to the codebook file in Excel format (.xlsx).
#' @param folder_path Folder path (character, default = "data/JSON_files"): The folder path where the JSON file will be saved.
#'
#' @details This function reads the codebook from the specified Excel file, 
#' checks for uniqueness of variable names, and converts the variable names 
#' to lowercase. It then creates a JSON file with variable categories as the 
#' main keys and variable names as subkeys, assigning NA values to each variable.
#' For variables that are indicated as "factor" in the codebook (in column "Type"),
#' the function automatically sets the variable's value to the options in the 
#' codebook (in column "Entries"). Additionally, it adds the study key and 
#' researcher ID to the generated JSON file.
#'
#' @examples
#' create_json_file(
#'  key = "dev", 
#'  rid = "fp", 
#'  folder_path = "/Users/franzprante/GitHub/MORPEP/META CMP/toy_data_extraction_dev/data/JSON_files", 
#'  codebook = "/Users/franzprante/GitHub/MORPEP/META CMP/data/codebook.xlsx")
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom janitor clean_names
#' @importFrom jsonlite prettify
#' @importFrom jsonlite toJSON
#' @importFrom here here
#' @importFrom utils file.edit
#'
#' @export
create_json_file <- 
  function(key,
           rid, 
           codebook = "codebook.csv",
           folder_path = "data/JSON_files"
  ) {
    file_path = here::here(folder_path, paste0(key, ".json"))
    
    # Check if the file already exists
    if (file.exists(file_path)) {
      stop("JSON file already exists for the given key.")
    }
    
    # Import the codebook
    codebook <- read.csv(here::here(codebook))
    # Exclude variables that are not needed for the JSON file
    codebook <- codebook[codebook$json == T,]
    # Test uniqueness of variable names in codebook
    if (length(unique(codebook$variable)) != length(codebook$variable)) {
      stop("Variables names in codebook are not unique.")
    }
    # Lower case variable names
    codebook <- janitor::clean_names(codebook)
    for (i in c("category", "variable")) {
      codebook[[i]] <- tolower(codebook[[i]])
    }
    
    df <- codebook
    
    # Initialize an empty list
    data <- list()
    
    # Iterate over unique categories
    for (category in unique(df$category)) {
      # Filter the dataframe for the current category
      category_df <- df[df$category == category, ]
      
      # Create a sublist for the current category
      sublist <- list()
      
      # Iterate over the rows of the filtered dataframe
      for (i in 1:nrow(category_df)) {
        # Get the variable for the current row
        variable <- category_df$variable[i]
        # If factor variable, get the value options, else NA
        if (category_df$type[i] == "factor") {
          value <- category_df$entries[i]
        } else {
          value <- NA
        }
        
        # Add the variable and assign factor options or NA value to the sublist
        sublist[[variable]] <- value
      }
      
      # Add the sublist to the main list
      data[[category]] <- sublist
    }
    
    # Add key and rid1
    data$general$key <- key
    data$general$rid1 <- rid
    data$general$rid2 <- "null"
    
    # Create the JSON string
    json_data <- jsonlite::prettify(
      jsonlite::toJSON(data, auto_unbox = TRUE)
    )
    
    # Write the JSON file
    write(json_data, file = file_path)
    
    # Print success message
    cat("JSON file", paste0(key, ".json"), "has been created.")
    
    # Open the JSON file for editing
    file.edit(file_path)
  }

# create_json_file(key = "dev", rid = "fp", codebook = codebook, folder_path = folder_path)
