# Creates study_set_28.xlsx: the snowballing references that were never screened (appendix, editor point E3)
#
# Why: the editor (GitHub issue #3 of META-CMP/red-revision, E3) asks why Coibion (2012) and
#   Iacoviello (2005) are not in the sample. Step 2 of data/snowballing/compare_with_our_dataset.R
#   compared the 406 snowball references with merged_EL_GS_no_duplicates.csv, the file of the
#   database search before the abstract screening, and dropped every reference that matched one
#   of its records (58 of the 324 references left after step 1). 28 of these 58 had never been
#   screened: 3 had been removed before the abstract screening for a missing abstract (Coibion
#   2012, Ball 1994, Sims 2002) and 25 were in ASReview but had not been labelled before the
#   stopping rule. Step 1 also dropped Romer and Romer (1994), not labelled before the stopping
#   rule either, by a false match with Romer and Romer (1989). In total, 29 references were never
#   screened (trace of 4 November 2026, matching the titles with the ASReview exports).
#   Wong (2021), the 30th reference of Franz's list, was screened: its ASReview record (record_id
#   7848) was excluded by screener 2 (note 220) and not seen by screener 1; step 2 matched it to a
#   second record of 2019 without a full abstract, which was removed before the screening.
# Row 32 (added on 4 November 2026, after the first run of this script): Olivei and Tenreyro (2007),
#   AER, record 46LKC57R of study_set_11.xlsx (Excel row 6). Both screeners labelled it relevant at
#   title and abstract, but in the full text screening it was excluded with the reason
#   "master thesis?" (rid1 ph), although master_bachelor_thesis = 0 and the paper is an AER article.
#   It is re-screened here. study_set_28.xlsx already existed, so Dani pastes this row from the row
#   file instead of running the script again.
# What: one row per reference (30) plus Iacoviello (2005), with the columns of study_set_27.xlsx,
#   so that the full text screening is recorded as in the other study sets: included (0/1),
#   reason_for_exclusion (codes of GitHub issue META-CMP/data#37, e.g. no_se,
#   no_relevant_effect_sizes, wrong_response_variable, wrong_shock_variable, effect_size_unclear,
#   no_comparable_effect_sizes, no_original_estimate, shock_size_unclear), rid1, rid2, ...
#   Hint columns at the end (only for the screening, not part of the study set format):
#   relevance_title_abstract (title and abstract judgement of notes_referee, Part C, Table 3),
#   group_franz (Franz's preliminary check), not_screened_because and snowball_step.
# Keys: the study keys are the Zotero keys of Snowballing.csv (column Key), as in study_set_27.
#   No key is generated. For Iacoviello (2005), the key of its AER record in the database search
#   (P7JNJ2KA, ASReview record_id 3092). make_zotero_key() at the end is only needed for a paper
#   without a Zotero key.
# record_id: row of the reference in Snowballing.csv; for Iacoviello, its ASReview record_id; for
#   Olivei and Tenreyro, its record_id in study_set_11 (986).
# PDFs: saved by BibtexKey in the OneDrive folder "Full text download/PDFs/appendix_snowballing",
#   online appendices as BibtexKey_appendix.pdf. The PDF columns are filled as in the Anleitung Full
#   Text Download (PDF_source, access_date, personal_id, notes_PDF_download with the codes
#   biblio_edit, biblio_edit_newer and pay): the published version (open access or through UAM) or,
#   if it could not be obtained, the latest working paper (most_recent_version_available = 0).
# Output: data/study_search/database_search/processed/post_AS/packages_for_full_text_download/study_set_28.xlsx
#   The script stops if the file already exists, so it never overwrites the screening.

# Load required libraries ----
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)

# Output file ----
study_set_path <- here::here("data/study_search/database_search/processed/post_AS/packages_for_full_text_download")
output_file <- paste0(study_set_path, "/study_set_28.xlsx")
if (file.exists(output_file)) {
  stop("study_set_28.xlsx already exists. Rename it first if you really want to create it again.")
}

# Read in the snowballing references and the database search ----
df_snowballing <- read_csv(here::here("data/snowballing/Snowballing.csv"))
colnames(df_snowballing) <- tolower(colnames(df_snowballing))
df_snowballing$record_id <- 1:nrow(df_snowballing) # Row in Snowballing.csv

merged_EL_GS_no_duplicates <- read_csv(here::here("data/study_search/database_search/processed/preparation_for_abstract_screening/merging_of_EL_and_GS_results_and_duplicate_removal/merged_EL_GS_no_duplicates.csv"))
colnames(merged_EL_GS_no_duplicates) <- tolower(colnames(merged_EL_GS_no_duplicates))

# Zotero keys of the 30 snowball references that were never screened ----
keys_never_screened <- c(
  "AW6NQTYM", # Coibion (2012), AEJ: Macroeconomics
  "I5DA9ZL3", # Jimenez, Ongena, Peydro and Saurina (2012), AER
  "79C4XIP5", # Lakdawala (2016), JEDC
  "YNIZB5V5", # Taylor (1993), Carnegie-Rochester
  "VJZLCB7P", # Andrade and Ferroni (2021), JME
  "YAMB46GA", # Piazzesi and Swanson (2008), JME
  "IJUXQH4K", # Kuttner (2018), JEP
  "ANSKI7DQ", # Jimenez, Ongena, Peydro and Saurina (2014), Econometrica
  "BRNRW6M6", # Rostagno et al. (2019), ECB WP
  "NTTMPAGJ", # Hartmann and Smets (2018), BPEA
  "KSISI7N2", # Sims (2002), BPEA
  "9N5Y9K52", # Ramey (2016), Handbook of Macroeconomics
  "PFF9ALRW", # Gornemann, Kuester and Nakajima (2021), Minneapolis Fed IWP
  "6H59DTXI", # Willems (2013), EER
  "JVUKBCKA", # Ottonello and Winberry (2020), Econometrica
  "CALC88QX", # Christiano, Eichenbaum and Evans (2005), JPE
  "Q5B29XSG", # Stock and Watson (2012), BPEA
  "23Y8QPAL", # Leeper, Sims and Zha (1996), BPEA
  "4566G9CY", # Santoro, Petrella, Pfajfar and Gaffeo (2014), JME
  "DCUIAAUA", # Barnichon and Mesters (2021), JME
  "PRJHLDWH", # Ball (1994), NBER volume
  "Z87XC3A6", # Benigno and Ricci (2011), AER
  "WA994JPN", # Sims and Zha (2006), Macroeconomic Dynamics
  "TIA7F45U", # Beaudry, Nam and Wang (2011), NBER WP
  "9LRT5YTQ", # Sastry (2022), SSRN
  "HT9XL53Z", # Cloyne, Ferreira, Froemel and Surico (2023), JEEA
  "9TQK54WH", # Jeenas (2023), working paper
  "ECWH377I", # Rey (2015), NBER WP
  "RI5T7ZIA", # Romer and Romer (1994), JME
  "VFDHXQ5G"  # Wong (2021), Princeton WP (in Franz's list; screened by screener 2 only, see the header)
)
key_iacoviello <- "P7JNJ2KA" # Iacoviello (2005), AER, record of the database search
key_olivei <- "46LKC57R" # Olivei and Tenreyro (2007), AER, record of study_set_11

# Bibliographic columns, as in study_set_27 ----
bib_columns <- c("record_id", "key", "author", "title", "publication year", "publication title",
                 "issue", "volume", "item type", "abstract note", "doi", "url")

df_never <- df_snowballing %>%
  filter(key %in% keys_never_screened) %>%
  select(all_of(bib_columns)) %>%
  mutate(across(everything(), as.character))

df_iacoviello <- merged_EL_GS_no_duplicates %>%
  filter(key == key_iacoviello) %>%
  mutate(record_id = 3092) %>% # ASReview record_id
  select(all_of(bib_columns)) %>%
  mutate(across(everything(), as.character))

df_olivei <- read_excel(paste0(study_set_path, "/study_set_11.xlsx")) %>%
  filter(key == key_olivei) %>%
  select(all_of(bib_columns)) %>%
  mutate(across(everything(), as.character))

study_set <- bind_rows(df_never, df_iacoviello, df_olivei)
study_set$record_id <- as.numeric(study_set$record_id)
study_set$`publication year` <- as.numeric(study_set$`publication year`)

# Test that all 32 studies were found
if (nrow(study_set) != 32) {
  stop("Expected 32 studies, found ", nrow(study_set), ". Check the keys.")
}

# Order as in keys_never_screened, then Iacoviello and Olivei and Tenreyro
study_set <- study_set[match(c(keys_never_screened, key_iacoviello, key_olivei), study_set$key), ]

# Adding a bibtex key ----
# Format of the project: name-of-first-author_year_first-word-of-title (see compare_with_our_dataset.R).
# The first author is taken before the first ";". The surname is the part before the comma
# ("Coibion, Olivier") or, if there is no comma, the last word ("Lawrence J. Christiano").
create_bibtex_key <- function(year, author, title) {
  # Select first author
  first_author <- trimws(strsplit(author, ";")[[1]][1])
  # Select surname
  if (grepl(",", first_author)) {
    surname <- strsplit(first_author, ",")[[1]][1]
  } else {
    surname <- tail(strsplit(first_author, " ")[[1]], 1)
  }
  # Remove non-alphanumeric characters from first word of title and convert to lowercase
  first_word <- tolower(gsub("[^[:alnum:]]", "", strsplit(title, " ")[[1]][1]))
  # Combine components to create Bibtex key
  paste0(surname, "_", year, "_", first_word)
}
study_set$BibtexKey <- mapply(create_bibtex_key, study_set$`publication year`, study_set$author, study_set$title)
# Olivei and Tenreyro keep their BibtexKey of study_set_11 (its PDF is saved under that name)
study_set$BibtexKey[study_set$key == key_olivei] <- "Olivei_2007_the"

# Test uniqueness of bibtex keys:
if (length(unique(study_set$BibtexKey)) == length(study_set$BibtexKey)) {
  message("The generated Bibtex-keys are unique.")
} else {
  warning("There are ", length(study_set$BibtexKey) - length(unique(study_set$BibtexKey)), " duplicate Bibtex-keys.")
}

# Corrections of the citation columns (biblio_edit and biblio_edit_newer in the Anleitung) ----
# Made after the Bibtex keys, which never change, with the data on the first page of the PDFs.
# biblio_edit: DOIs truncated or missing in Snowballing.csv (Willems, Santoro et al., Barnichon and
#   Mesters, Benigno and Ricci, Sims and Zha); volume and issue missing (Cloyne et al.).
# biblio_edit_newer: Sastry, whose published version (AEJ: Macroeconomics 2026) replaces the SSRN record.
study_set$doi[study_set$key == "6H59DTXI"] <- "10.1016/j.euroecorev.2013.03.005"
study_set$doi[study_set$key == "4566G9CY"] <- "10.1016/j.jmoneco.2014.07.009"
study_set$doi[study_set$key == "DCUIAAUA"] <- "10.1016/j.jmoneco.2020.04.005"
study_set$doi[study_set$key == "Z87XC3A6"] <- "10.1257/aer.101.4.1436"
study_set$doi[study_set$key == "WA994JPN"] <- "10.1017/S136510050605019X"
study_set$volume[study_set$key == "HT9XL53Z"] <- "21"
study_set$issue[study_set$key == "HT9XL53Z"] <- "6"
sastry <- study_set$key == "9LRT5YTQ"
study_set$`publication title`[sastry] <- "American Economic Journal: Macroeconomics"
study_set$`publication year`[sastry] <- 2026
study_set$volume[sastry] <- "18"
study_set$issue[sastry] <- "2"
study_set$`item type`[sastry] <- "journalArticle"
study_set$doi[sastry] <- "10.1257/mac.20220266"
study_set$url[sastry] <- "https://doi.org/10.1257/mac.20220266"
# biblio_edit_newer: Olivei and Tenreyro, whose record in study_set_11 (2006, no journal) is replaced
#   by the published version (AER 2007, 97(3): 636-663).
olivei <- study_set$key == key_olivei
study_set$`publication title`[olivei] <- "American Economic Review"
study_set$`publication year`[olivei] <- 2007
study_set$volume[olivei] <- "97"
study_set$issue[olivei] <- "3"
study_set$`item type`[olivei] <- "journalArticle"
study_set$doi[olivei] <- "10.1257/aer.97.3.636"
study_set$url[olivei] <- "https://doi.org/10.1257/aer.97.3.636"

# PDF retrieval (3 November 2026; Iacoviello on 9 October 2026) ----
# PDF_source: DOI link for the PDFs downloaded through UAM, otherwise the URL of the open copy.
# most_recent_version_available: 1 if the saved PDF is the published version (or the version of
# the record), 0 if it is an earlier working paper because the published version could not be
# obtained yet (Romer and Romer 1994 and Jeenas: interlibrary loan; Sims and Zha: Cambridge Core).
pdf_info <- tibble::tribble(
  ~key, ~PDF_source, ~most_recent_version_available, ~notes_PDF_download,
  "AW6NQTYM", "https://doi.org/10.1257/mac.4.2.1", 1, "Published version (AEJ: Macroeconomics 2012), through UAM. In the AEJ version the rows of Figure 2 are mislabelled: follow the text.",
  "I5DA9ZL3", "https://crei.cat/wp-content/uploads/2016/07/peydro_credit-supply.pdf", 1, "Published version (AER 2012).",
  "79C4XIP5", "https://doi.org/10.1016/j.jedc.2016.07.003", 1, "Published version (JEDC 2016), through UAM.",
  "YNIZB5V5", "https://web.stanford.edu/~johntayl/Papers/Discretion.PDF", 1, "Published version (Carnegie-Rochester Conference Series on Public Policy 1993).",
  "VJZLCB7P", "https://doi.org/10.1016/j.jmoneco.2020.06.002", 1, "Published version (JME 2021), through UAM.",
  "YAMB46GA", "https://sites.socsci.uci.edu/~swanson2/papers/fff.pdf", 1, "Published version (JME 2008).",
  "IJUXQH4K", "https://doi.org/10.1257/jep.32.4.121", 1, "Published version (JEP 2018), free on the AEA website.",
  "ANSKI7DQ", "https://repositori.upf.edu/handle/10230/43183", 1, "Published version (Econometrica 2014), UPF repository.",
  "BRNRW6M6", "https://www.ecb.europa.eu/pub/pdf/scpwps/ecb.wp2346~dd78042370.en.pdf", 1, "ECB WP 2346 (2019), the version of the record. An extended version was published as a book, Monetary Policy in Times of Crisis: A Tale of Two Decades of the European Central Bank (Oxford University Press, 2021), not saved.",
  "NTTMPAGJ", "https://www.brookings.edu/wp-content/uploads/2018/09/Hartmann-Smets_final-draft.pdf", 1, "BPEA final draft (Fall 2018), 146 pages as the published version.",
  "KSISI7N2", "https://www.brookings.edu/wp-content/uploads/2002/06/2002b_bpea_sims.pdf", 1, "Published version (BPEA 2002).",
  "9N5Y9K52", "https://econweb.ucsd.edu/~vramey/research/Shocks_HOM_Ramey_published_corrected.pdf", 1, "Published version (Handbook of Macroeconomics 2016, chapter 2), corrected copy on the author's website.",
  "PFF9ALRW", "https://www.minneapolisfed.org/institute/working-papers-institute/iwp50.pdf", 1, "Minneapolis Fed Institute WP 50 (2021), the version of the record; no journal version found.",
  "6H59DTXI", "https://doi.org/10.1016/j.euroecorev.2013.03.005", 1, "biblio_edit (DOI completed). Published version (EER 2013), through UAM.",
  "JVUKBCKA", "https://doi.org/10.3982/ECTA15949", 1, "Published version (Econometrica 2020), through UAM.",
  "CALC88QX", "https://faculty.wcas.northwestern.edu/yona/research/CEE2005.pdf", 1, "Published version (JPE 2005).",
  "Q5B29XSG", "https://www.brookings.edu/wp-content/uploads/2012/03/2012a_Stock.pdf", 1, "corr. Published version (BPEA 2012). The erratum (Tables 6-8, gasoline price data) is on the website of Mark Watson: Stock_2012_disentangling_erratum.pdf.",
  "23Y8QPAL", "https://www.brookings.edu/wp-content/uploads/1996/06/1996b_bpea_leeper_sims_zha_hall_bernanke.pdf", 1, "Published version (BPEA 1996), with the comments and discussion.",
  "4566G9CY", "https://doi.org/10.1016/j.jmoneco.2014.07.009", 1, "biblio_edit (DOI completed). Published version (JME 2014), through UAM.",
  "DCUIAAUA", "https://doi.org/10.1016/j.jmoneco.2020.04.005", 1, "biblio_edit (DOI completed). Published version (JME 2021), through UAM.",
  "PRJHLDWH", "https://www.nber.org/system/files/chapters/c8332/c8332.pdf", 1, "Published chapter (NBER volume Monetary Policy, 1994).",
  "Z87XC3A6", "https://doi.org/10.1257/aer.101.4.1436", 1, "biblio_edit (DOI added). Published version (AER 2011), through UAM.",
  "WA994JPN", "https://fraser.stlouisfed.org/files/docs/historical/frbatl/wp/frbatl_wp_1998-12.pdf", 0, "biblio_edit (DOI added). Atlanta Fed WP 98-12 (1998). The published version (Macroeconomic Dynamics 2006, a vintage article) could not be downloaded yet: Cambridge Core returned error 429 (too many requests).",
  "TIA7F45U", "https://www.nber.org/system/files/working_papers/w17651/w17651.pdf", 1, "NBER WP 17651 (2011), the version of the record.",
  "9LRT5YTQ", "https://doi.org/10.1257/mac.20220266", 1, "biblio_edit_newer (citation columns of the published version). Published version (AEJ: Macroeconomics 2026), through UAM.",
  "HT9XL53Z", "https://doi.org/10.1093/jeea/jvad009", 1, "biblio_edit (volume and issue added). Published version (JEEA 2023), through UAM.",
  "9TQK54WH", "https://bse.eu/sites/default/files/working_paper_pdfs/1409_0.pdf", 0, "pay. BSE WP 1409 (October 2023, revised May 2024). Published version (University of Chicago Press, DOI 10.1086/744130) not available through UAM: requested by interlibrary loan.",
  "ECWH377I", "https://www.nber.org/system/files/working_papers/w21162/w21162.pdf", 1, "NBER WP 21162 (2015), the version of the record. The 2013 Jackson Hole proceedings version is free at https://www.kansascityfed.org/documents/4575/2013Rey.pdf but could not be downloaded automatically.",
  "RI5T7ZIA", NA, 0, "pay. Not in open access (Google, Google Scholar) and not available through UAM: requested by interlibrary loan. JME 1994, 34(1), 75-88.",
  "VFDHXQ5G", "https://jfhoude.wiscweb.wisc.edu/wp-content/uploads/sites/769/2019/09/Arlene-Wong-refinancing.pdf", 0, "Draft of February 2019. The record is Princeton WP 2021-57 (March 2021), whose PDF link is broken.",
  "P7JNJ2KA", "Author's website (RED/papers_snow)", 1, "Published version (AER 2005).",
  "46LKC57R", "https://doi.org/10.1257/aer.97.3.636", 1, "biblio_edit_newer (citation columns of the published version). Published version (AER 2007), copied from the project folder PDFs/PDFs (Olivei_2007_the.pdf). The PDF_source in study_set_11 was LSE eprints 3742, which is CEP Discussion Paper 725 (May 2006)."
)
pdf_info$access_date <- as.Date(ifelse(pdf_info$key == "P7JNJ2KA", "2026-10-09",
                                       ifelse(pdf_info$key == "46LKC57R", "2026-11-04", "2026-11-03")))
pdf_info$not_available <- ifelse(pdf_info$key == "RI5T7ZIA", 1, 0)

# Hints for the full text screening ----
# relevance_title_abstract: notes_referee, Part C, Table 3 (title and abstract only, except Coibion).
# group_franz: Franz's preliminary eligibility check.
# not_screened_because and snowball_step: why the reference was never screened and which step
# of compare_with_our_dataset.R dropped it.
hints <- tibble::tribble(
  ~key, ~relevance_title_abstract, ~group_franz, ~not_screened_because, ~snowball_step,
  "AW6NQTYM", "Relevant (eligible in the full text)", "likely eligible", "missing abstract", 2,
  "I5DA9ZL3", "No: loan-level credit", "likely not eligible", "stopping rule", 2,
  "79C4XIP5", "No: policy preferences", "need a closer look", "stopping rule", 2,
  "YNIZB5V5", "No: policy rule", "likely not eligible", "stopping rule", 2,
  "VJZLCB7P", "Relevant: high-frequency shocks, euro area (forward-guidance news: check that it counts as conventional)", "need a closer look", "stopping rule", 2,
  "YAMB46GA", "No: financial markets", "likely not eligible", "stopping rule", 2,
  "IJUXQH4K", "No: unconventional policy, review", "likely not eligible", "stopping rule", 2,
  "ANSKI7DQ", "No: bank risk-taking", "likely not eligible", "stopping rule", 2,
  "BRNRW6M6", "No: review", "need a closer look", "stopping rule", 2,
  "NTTMPAGJ", "No: review", "likely not eligible", "stopping rule", 2,
  "KSISI7N2", "No: policy process", "likely not eligible", "missing abstract", 2,
  "9N5Y9K52", "Relevant: own estimates of monetary shocks", "likely eligible", "stopping rule", 2,
  "PFF9ALRW", "No: HANK model, distribution", "need a closer look", "stopping rule", 2,
  "6H59DTXI", "Relevant: price and output responses", "likely eligible", "stopping rule", 2,
  "JVUKBCKA", "No: firm-level investment", "likely not eligible", "stopping rule", 2,
  "CALC88QX", "Relevant: VAR responses and estimated model", "likely eligible", "stopping rule", 2,
  "Q5B29XSG", "Unclear: factor model, several shocks", "need a closer look", "stopping rule", 2,
  "23Y8QPAL", "Relevant: identified VARs", "likely eligible", "stopping rule", 2,
  "4566G9CY", "No: theoretical model", "likely not eligible", "stopping rule", 2,
  "DCUIAAUA", "No: inflation-unemployment trade-off statistic", "need a closer look", "stopping rule", 2,
  "PRJHLDWH", "No: disinflation episodes", "likely not eligible", "missing abstract", 2,
  "Z87XC3A6", "No: theory", "likely not eligible", "stopping rule", 2,
  "WA994JPN", "Relevant: identified VAR", "likely eligible", "stopping rule", 2,
  "TIA7F45U", "No: no monetary shock", "likely not eligible", "stopping rule", 2,
  "9LRT5YTQ", "No: beliefs about policy", "likely not eligible", "stopping rule", 2,
  "HT9XL53Z", "No: firm-level", "likely not eligible", "stopping rule", 2,
  "9TQK54WH", "No: firm-level", "need a closer look", "stopping rule", 2,
  "ECWH377I", "Unclear: mainly financial responses", "need a closer look", "stopping rule", 2,
  "RI5T7ZIA", "Unclear: narrative episodes; check bands and shock size", "need a closer look", "stopping rule; dropped at step 1 by a false match with Romer and Romer (1989)", 1,
  "VFDHXQ5G", "Not in Table 3. Abstract: household consumption and mortgage refinancing", "likely not eligible", "screened: excluded at title and abstract screening by screener 2 only (note 220); step 2 matched a 2019 record without a full abstract", 2,
  "P7JNJ2KA", "Excluded at title and abstract screening by both screeners (note 202). Full text: Section I VAR, Figure 1, 90% bands (one model)", NA, "screened: excluded at title and abstract screening", NA,
  "46LKC57R", "Relevant for both screeners at title and abstract. Full text: VAR with real GDP, GDP deflator and fed funds rate, 25 bp shocks, 95% and 80% bands (Figs. 1-13)", NA, "screened: full text in study_set_11 (Excel row 6), excluded with reason 'master thesis?' (rid1 ph), although master_bachelor_thesis = 0", NA
)

# Combine and add the empty columns of the study sets ----
study_set <- study_set %>%
  left_join(pdf_info, by = "key") %>%
  mutate(retracted = 0,
         duplicate = 0,
         personal_id = "DR",
         master_bachelor_thesis = 0,
         included = NA,
         reason_for_exclusion = NA,
         rid1 = NA,
         rid2 = NA,
         appendix = NA,
         doublecheck = NA,
         `reason for doublecheck` = NA,
         open_questions = NA,
         finished = NA,
         additional = 1,
         `relevant_code based on abstract and title screening` = NA) %>%
  left_join(hints, by = "key") %>%
  select(record_id, key, author, title, `publication year`, `publication title`, issue, volume,
         `item type`, `abstract note`, doi, url, BibtexKey, most_recent_version_available,
         not_available, retracted, duplicate, PDF_source, access_date, personal_id,
         master_bachelor_thesis, notes_PDF_download, included, reason_for_exclusion, rid1, rid2,
         appendix, doublecheck, `reason for doublecheck`, open_questions, finished, additional,
         `relevant_code based on abstract and title screening`,
         relevance_title_abstract, group_franz, not_screened_because, snowball_step)

# Print the keys and Bibtex keys ----
print(study_set %>% select(key, BibtexKey, most_recent_version_available), n = 32)

# Save the study set ----
options("openxlsx.dateFormat" = "dd-mm-yyyy") # Format of access_date in the Anleitung
write.xlsx(study_set, output_file, rowNames = FALSE)
message("study_set_28.xlsx created with ", nrow(study_set), " studies.")

# Zotero-style key (only for a paper without a Zotero key) ----
# Eight characters from the alphabet that Zotero uses for item keys (no 0, 1 or O). The function
# draws keys until it finds one that is not in used_keys.
make_zotero_key <- function(used_keys) {
  alphabet <- strsplit("23456789ABCDEFGHIJKLMNPQRSTUVWXYZ", "")[[1]]
  repeat {
    new_key <- paste(sample(alphabet, 8, replace = TRUE), collapse = "")
    if (!(new_key %in% used_keys)) {
      return(new_key)
    }
  }
}
# Example (not run): collect the keys already in use and draw a new one
# used_keys <- c(
#   sub("\\.json$", "", list.files(here::here("data/full_text_screening/JSON_files"))),
#   sub("\\.json$", "", list.files(here::here("data/full_text_screening/JSON_files_appendix_snowballing"))),
#   list.files(here::here("data/effect_sizes/IRFs")),
#   list.files(here::here("data/effect_sizes/IRFs_appendix_snowballing")),
#   unlist(lapply(1:28, function(x) read_excel(paste0(study_set_path, "/study_set_", x, ".xlsx"))$key)),
#   df_snowballing$key,
#   merged_EL_GS_no_duplicates$key
# )
# make_zotero_key(used_keys)
