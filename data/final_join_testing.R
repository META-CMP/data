library(MetaExtractR)

error_jsons <- c(
  "example_file",
  "V2A8ZH66",# no IRFs
  "Q5FHEZNE", # contains table estimates as well. 
  "NADVD2AD" # Error: File '/Users/franzprante/GitHub/MORPEP/META_CMP/data/data/effect_sizes/IRFs/NADVD2AD/model_1/log_q_rgdp/mean.csv' does not exist or is non-readable. getwd()=='/Users/franzprante/GitHub/MORPEP/META_CMP/data'
)

json.irf.join <- final_join(json_path = "data/full_text_screening/JSON_files", irf_path = "data/effect_sizes/IRFs/", only_json = FALSE, ignore = error_jsons)

save(json.irf.join,file = "data/preliminary_data_14062024.RData")


