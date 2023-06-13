# Effect size extraction for study IAFWL5F9
# Notes on effect size
df <- MetaExtractR::parse_study_json(
  json_file = "IAFWL5F9.json", 
  json_path = "data/full_text_screening/JSON_files",
  view_data = F)
df_notes <- strsplit(df$study_notes, ";")[[1]]
note_on_effect_size <- grep("effect_size", df_notes, value = TRUE)
note_on_effect_size
# Short-run coefficient and t-value from table 6
effect_size <- -0.075098
t_value <- 1.607218
se <- effect_size / t_value 
period <- 1 # See note_on_effect_size
data <- data.frame(
  key = "IAFWL5F9",
  effect_size = effect_size,
  se = se,
  period = period
)
write.csv(data, file = here::here("data/full_text_screening/effect_sizes/tables/IAFWL5F9/effect_data.csv"), row.names = F)
