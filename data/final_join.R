# Joining raw JSON coding and raw IRF extraction data from full text screening ----
library(MetaExtractR)

# Define problematic JSON files/studies
error_jsons <- c(
  "example_file",
  "V2A8ZH66", # no IRFs, only table
  "Q5FHEZNE" # contains table estimates as well. 
)

# Join JSON and IRF data and save as RData file
json.irf.join <- final_join(json_path = "data/full_text_screening/JSON_files",
                            irf_path = "data/effect_sizes/IRFs/",
                            only_json = FALSE,
                            ignore = error_jsons)
beepr::beep()
data <- json.irf.join
save(data, file = "data/final_join_json_irf_data.RData")