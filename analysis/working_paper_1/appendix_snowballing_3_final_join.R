# Appendix (editor point E3): copy of data/final_join.R for the studies of study_set_28.xlsx that
#   passed the full text screening. Only the JSON and IRF folders and the output file change.
#   Next: appendix_snowballing_4_journal_matching.R.

# Joining raw JSON coding and raw IRF extraction data from full text screening ----
library(MetaExtractR)

# Define problematic JSON files/studies
error_jsons <- c(
  "example_file"
)

# Join JSON and IRF data and save as RData file
json.irf.join <- final_join(json_path = "data/full_text_screening/JSON_files_appendix_snowballing",
                            irf_path = "data/effect_sizes/IRFs_appendix_snowballing/",
                            only_json = FALSE,
                            ignore = error_jsons)
beepr::beep()
data <- json.irf.join
save(data, file = "data/final_join_json_irf_data_appendix_snowballing.RData")