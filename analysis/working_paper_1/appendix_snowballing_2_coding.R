# Helper script for the studies of the appendix (editor point E3, study_set_28)
#
# Same blocks as postscript_help.R and rename_folders.R. Only the folders change:
#   JSON files: data/full_text_screening/JSON_files_appendix_snowballing
#   IRFs:       data/effect_sizes/IRFs_appendix_snowballing
# so that data/final_join.R, which reads data/full_text_screening/JSON_files, never picks
# these studies up. They only enter the robustness check of the appendix (robust_23).
#
# How to run it, in this order, one study at a time:
#   0. Only the first time: run "library", "setwd" and "Folders" (creates the two folders).
#   1. Key: write the key of the study in key <- "..." (keys of the included studies below).
#   2. Create: creates <key>.json with rid1 = "dr" and opens it. Fill it in as in the main
#      sample and save it. If the JSON already exists, create_json_file() stops: run only
#      the two lines "file <- ..." and "file.edit(...)" to open it again.
#   3. Countrycodes: iso2c code of the country, to write it in the JSON.
#   4. Check: parse_study_json() reads the JSON and shows it with View(df). If it fails,
#      correct the JSON (step 2, second part) and run the check again.
#   5. Create folder structure: run "setwd" and "dir.create(mainFolder)", then only one
#      of the two variants:
#        variant 1, the same outcome variables in all models (numSubfolders, subsubfolders);
#        variant 2, different outcome variables by model (subfolderList).
#      It creates IRFs_appendix_snowballing/<key>/model_i/<outcome_var>/.
#   6. Outside R: screenshots and WebPlotDigitizer. In each folder model_i/<outcome_var>:
#      mean.csv, lower.csv, upper.csv, model_i_<outcome_var>.png and model_i_<outcome_var>.tar.
#   7. Rename folders (only if needed): renames the folder <outcome_var> and its .png and .tar
#      when the periodicity in the name is wrong (e.g. logdiff_q_cpi -> logdiff_a_cpi).
#      Set numSubfolders, subsubfolders (the current names) and periodicity (the new one),
#      and change "logdiff_q" in the three gsub() calls if the prefix to replace is another.
#   8. Join: final_join() joins the JSON and the IRFs of the study, to check them. It starts
#      with setwd("~/GitHub/data"), because steps 5 and 7 change the working directory.
#   Then go back to step 1 with the next key. When all included studies are coded, run
#   appendix_snowballing_3_final_join.R, _4_journal_matching.R, _5_data_preparation.R and
#   make_figure_correction_range_robust_23.R, in this order.
#
# Included studies of study_set_28 (key, BibtexKey, what to code):
#   AW6NQTYM  Coibion_2012_are          models 1-5: Fig. 2, columns 1-3; Fig. 4, columns 1-2 (rows of Fig. 2 mislabelled: log IP and log CPI)
#   9N5Y9K52  Ramey_2016_macroeconomic  models 1-4: Figs. 1, 2B, 2C, 3B (solid black lines; Figs. 2A and 3A not coded: replications)
#   6H59DTXI  Willems_2013_analyzing    models 1-10: right panels (US) of Figs. 1-3 and 6-12; dollarized countries not coded
#   CALC88QX  Christiano_2005_nominal   Fig. 1 (pp. 6-7): output, inflation, interest rate; shock at the asterisk (x = 3)
#   23Y8QPAL  Leeper_1996_what          models 1-3: Fig. 11; Fig. 13, column 1; Fig. 4, column 3 (Fig. 5 not coded: M1 shock)
#   4566G9CY  Santoro_2014_loss         models 1-6: Fig. 1, Fig. A2, Fig. A3; contraction (blue), expansion (green)
#   DCUIAAUA  Barnichon_2021_the        excluded: shock_size_unclear; no_comparable_effect_sizes (IRFs of averages over t to t+h, rescaled)
#   HT9XL53Z  Cloyne_2023_monetary      Online Appendix C (p. 10): Fig. C.2 IP, CPI, one-year rate; Fig. C.1 employment
#   9TQK54WH  Jeenas_2023_firm          Fig. A.15 (p. 75): output (panel b), fed funds rate (panel a)
#   ECWH377I  Rey_2015_dilemma          excluded: no_comparable_effect_sizes (transformation of GDP and deflator not reported)
#   46LKC57R  Olivei_2007_the           models 1-9: Fig. 1; Figs. 2-5 (Q1-Q4, benchmark); Figs. 6-9 (Q1-Q4, NSA data); 95% band
#   P7JNJ2KA  Iacoviello_2005_house     Fig. 1 (p. 741), row "shock to R": responses of R, pi and Y

library(MetaExtractR)

setwd("~/GitHub/data")

# Folders (only the first time)
dir.create("data/full_text_screening/JSON_files_appendix_snowballing")
dir.create("data/effect_sizes/IRFs_appendix_snowballing")

key <- "9N5Y9K52"

# Create
MetaExtractR::create_json_file(
  key = key,
  rid = "dr",
  codebook = "codebook.csv",
  folder_path = "data/full_text_screening/JSON_files_appendix_snowballing"
)


file <- paste0("data/full_text_screening/JSON_files_appendix_snowballing/", key, ".json")
file.edit(here::here(file))



library(countrycode)
# Countrycodes
countries <- c("United States")
countrycode::countrycode(
  sourcevar = countries,
  origin = "country.name",
  destination = "iso2c"
)


# Check
df <- MetaExtractR::parse_study_json(
  json_file = paste0(key, ".json"),
  json_path = "data/full_text_screening/JSON_files_appendix_snowballing",
  view_data = F)

View(df)


# create folder structure to save the IRF screenshots

setwd("~/GitHub/data/data/effect_sizes/IRFs_appendix_snowballing")
# Define the main folder name
mainFolder <- key

# Create the main folder
dir.create(mainFolder)

# Variant 1: the same outcome variables in all models
# Define the number of subfolders
numSubfolders <- 3

# Define the subfolder names
subsubfolders <- c("log_m_ip","lev_m_une_rate","log_m_cpi","rate")

# Create subfolders within the main folder
for (i in 1:numSubfolders) {
subfolder <- paste0("model_", i)
dir.create(file.path(mainFolder, subfolder))
# Create subsubfolders within each subfolder
for (subsubfolder in subsubfolders) {
  dir.create(file.path(mainFolder, subfolder, subsubfolder))
}
}

# Variant 2: different outcome variables by model
# Define the list of subfolder names
subfolderList <- list(
  "model_1" = c("log_m_ip", "lev_m_une_rate", "log_m_cpi","rate"),
  "model_2" = c("log_m_ip", "lev_m_une_rate", "log_m_cpi","rate"),
  "model_3" = c("log_m_ip", "lev_m_une_rate", "log_m_cpi","rate"),
  "model_4" = c("log_m_ip", "log_m_cpi","rate")
)


# Create subfolders within the main folder based on the list
for (model in names(subfolderList)) {
subfolder <- file.path(mainFolder, model)
dir.create(subfolder)

# Create subsubfolders within each subfolder
subsubfolders <- subfolderList[[model]]

for (subsubfolder in subsubfolders) {
  dir.create(file.path(subfolder, subsubfolder))
}
}


### Rename folders and pictures (only if needed)

setwd("~/GitHub/data/data/effect_sizes/IRFs_appendix_snowballing")

mainFolder <- key

numSubfolders <- 1
subsubfolders <- c("logdiff_q_cpi")
periodicity <- "_a_"

for (i in 1:numSubfolders) {
  subfolder <- paste0("model_", i)

  # Create subsubfolders within each subfolder
  for (subsubfolder in subsubfolders) {

    # Rename specific files within each subsubfolder
    oldFilePath <- file.path(mainFolder, subfolder, subsubfolder,
                             paste0(subfolder,"_",subsubfolder,".png"))
    newFilePath <- file.path(mainFolder, subfolder, subsubfolder,
                             paste0(subfolder,"_", gsub("logdiff_q", paste0("logdiff", periodicity), subsubfolder), ".png"))

    # Remove extra underscores in the file name
    newFilePath <- gsub("_+", "_", newFilePath)

    file.rename(oldFilePath, newFilePath)

    # Rename specific files within each subsubfolder
    oldFilePath <- file.path(mainFolder, subfolder, subsubfolder,
                             paste0(subfolder,"_",subsubfolder,".tar"))
    newFilePath <- file.path(mainFolder, subfolder, subsubfolder,
                             paste0(subfolder,"_", gsub("logdiff_q", paste0("logdiff", periodicity), subsubfolder), ".tar"))

    # Remove extra underscores in the file name
    newFilePath <- gsub("_+", "_", newFilePath)

    file.rename(oldFilePath, newFilePath)

    # Print the renamed file
    print(paste("Renamed file:", newFilePath))

    # Rename the subsubfolder itself
    oldSubsubfolderPath  <- file.path(mainFolder, subfolder, subsubfolder)
    newSubsubfolderPath <- file.path(mainFolder, subfolder,
                                     gsub("_+", "_", gsub("logdiff_q", paste0("logdiff", periodicity), subsubfolder)))
    file.rename(oldSubsubfolderPath, newSubsubfolderPath)

    # Print the renamed subsubfolder
    print(paste("Renamed subsubfolder:", newSubsubfolderPath))
  }
}


# Join (after the screenshots and WebPlotDigitizer)
setwd("~/GitHub/data")
json.irf.join <- MetaExtractR::final_join(json_path = "data/full_text_screening/JSON_files_appendix_snowballing", irf_path = "data/effect_sizes/IRFs_appendix_snowballing/", only_json = FALSE, investigate=key)

View(json.irf.join)
