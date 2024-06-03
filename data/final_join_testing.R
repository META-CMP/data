library(MetaExtractR)

error_jsons <- c(
  "example_file",
  "V2A8ZH66",# no IRFs
  "Q5FHEZNE" # contains table estimates as well. 
)

setwd("~/data")


json.irf.join <- MetaExtractR::final_join(json_path = "data/full_text_screening/JSON_files", irf_path = "data/effect_sizes/IRFs/", only_json = FALSE, ignore = error_jsons)



data<-json.irf.join
#save(data,file = "preliminary_data.RData")


