# Preparing data for title and abstract screening

Here we outline the steps which were conducted on the data from the initial database search to prepare our machine-learning supported title and abstract screening process. We also store intermediate data files which were created during preparation.

## 1. De-duplication

After our initial database search, we merged the results from EconLit and Google Scholar and removed duplicates using the bibliographic software [Zotero](https://www.zotero.org/) (version: `6.0.21`) and the [Zotero Duplicates Merger](https://github.com/frangoud/ZoteroDuplicatesMerger) add-on (version: `v1.1.5`). We took care to preserve the existing complete abstracts from EconLit when merging the duplicates. See [here](merging_of_EL_and_GS_results_and_duplicate_removal) for the merged and duplicate-free results in `.csv` and `.ris` format.

- Entries after merging of EconLit and Google Scholar results: 18265
- Entries after removal of duplicates: 12272

## 2. Retrieving missing abstracts

We then applied the script [`extracting_URLs_script.R`](retrieving_missing_or_incomplete_abstracts/extracting_URLs_script.R) to the merged and duplicate-free dataset to extract the URLs (or, if no URL exists, the titles) of those 5572 [entries](retrieving_missing_or_incomplete_abstracts/entries_without_complete_abstracts.csv) that have no abstract or only an incomplete abstract (Google Scholar only extracts abbreviated abstracts and both Google Scholar and EconLit sometimes have missing abstracts). We then manually searched for these entries and retrieved their abstracts, if available, from the internet.

### Retrieving missing abstracts when only titles are available

To retrieve the abstracts for [73 entries](retrieving_missing_or_incomplete_abstracts/titles/titles1.txt) with only titles available, we conducted the following steps on February 21st and 22nd in 2023:

- We searched the exact title within the Google Scholar search and used parentheses to search for the exact phrase if the first few entries did not match the respective title.

- For entries where the title of an article matched a search result but a respective Google Scholar document link did not show the abstract, we used the following three options:  
  1. Tapped the "All 'X' versions" button in Google Scholar to screen for the respective abstracts of the relevant paper.
  2. Entered the title in a Google search to retrieve the abstracts for the relevant articles.
  3. Searched for the title in EconLit.
  
- If the Google Scholar search was unsuccessful to find a respective entry, we conducted options two and three of the previous point if the title of the paper seemed to fit our research question at a very basic level. 

This procedure provided us with corresponding abstracts for [25 studies](retrieving_missing_or_incomplete_abstracts/abstracts_from_titles.csv).

### Retrieving missing abstracts when URLs were available

A student assistant collected abstracts from the websites of bibliographic entries where possible based on the [URLs from entries without complete abstracts](retrieving_missing_or_incomplete_abstracts/URLs). The resulting [collections](retrieving_missing_or_incomplete_abstracts/RIS_files_from_URLs) with complete abstracts (where available) were then integrated using Zotero by following these steps:

#### 1st Step

First, we prepared a dataset, that contains those entries from the initial duplicate-free dataset that are not in [`entries_without_complete_abstracts.csv`](retrieving_missing_or_incomplete_abstracts/entries_without_complete_abstracts.csv) using a function in [`extracting_URLs_script.R`](retrieving_missing_or_incomplete_abstracts/extracting_URLs_script.R). The file was stored as [`entries_complete_abstracts.csv`](retrieving_missing_or_incomplete_abstracts/entries_complete_abstracts.csv).

#### 2nd Step

We imported the initial [`merged_EL_GS_no_duplicates.ris`](merging_of_EL_and_GS_results_and_duplicate_removal/merged_EL_GS_no_duplicates.ris) into Zotero to make Zotero know the `Key`s (the entry identifiers). Subsequently, we imported the [`abstracts_from_titles.ris`](retrieving_missing_or_incomplete_abstracts/abstracts_from_titles.ris) that we retrieved based on the titles of the entries that had no URLs. We then removed the remaining duplicates and entries with missing abstracts.

#### 3rd Step

We imported the completed [ris-files](retrieving_missing_or_incomplete_abstracts/RIS_files_from_URLs) of the student assistant as sub-collections into an empty new collection in Zotero. Subsequently, we deleted the [`merged_EL_GS_no_duplicates.ris`](merging_of_EL_and_GS_results_and_duplicate_removal/merged_EL_GS_no_duplicates.ris) collection (with items!) from Zotero. This procedure ensured that Zotero would have resolved any `Key` conflicts (identifier conflicts) between the original dataset and the `ris`-files from the student assistant. In a last step, we removed duplicated items directly in Zotero.

The merged collection of duplicate-free student `ris`-files is saved as [`abstracts_from_URLs.ris`](retrieving_missing_or_incomplete_abstracts/abstracts_from_URLs.ris) and [`abstracts_from_URLs.csv`](retrieving_missing_or_incomplete_abstracts/abstracts_from_URLs.csv).

## 3. Screening related existing meta-studies 

We complemented our search strategy by screening relevant meta-studies to identify studies that were not covered in our initial search. Specifically, we manually gathered all 43 Studies of Nguyen (2021) and 45 Studies of Nguyen (2020) and read the data of Havranek et al (2013) (available at: http://meta-analysis.cz/lags/lags.zip) and Rusnak et al (2013) (http://meta-analysis.cz/price_puzzle/puzzle.xls) directly into R using the `screen_for additional_studies_from_other_meta.R` script. 

For a more detailed explanation of how we screened for additional studies, please refer to the `screen_for additional_studies_from_other_meta.R` file. First, we identified 19 available studies from Havranek et al. (2013) and Rusnak et al. (2013) that were not yet in our database, added them to Zotero, and saved them as `havranek_2013.csv`. Second, we included 11 studies from Nguyen (2021) and Nguyen (2020) that were not covered in our initial search or the `havranek_2013.csv` file, and saved them as` Nguyen_2021_2020.csv`.

## 4. Final merge and final de-duplication

Finally, we merged 

1. `entries_complete_abstracts.csv`
2. `abstracts_from_titles.csv`
3. `abstracts_from_URLs.csv`
4. `havranek_2013.csv`
5. `Nguyen_2021_2020.csv`

to retrieve our dataset for a final round of checks conducted in[`merge_all_files.R`](). During this final check, we randomly screened for additional available abstracts when the abstract note was "NA." We also checked the abstracts for remaining "..." quotes and added 11 additional observations in `points_in_abstracts.csv` for which some information of the abstract was missing in the current dataset but full abstracts were available. Additionally, we conducted two more rounds of duplicate removal using the "revtools" package app in R. Finally, we removed entries without abstract or title and saved the resulting dataset for abstract screening as `unlabelled_full_sample.csv`.

__The resulting literature database contains 10,714 entries and was then screened for non-eligiblilty based on title and abstract.__

## References

Havranek, Tomas, and Marek Rusnak. 2013. “Transmission Lags of Monetary Policy: A Meta-Analysis.” International Journal of Central Banking 9 (4): 39–76. [https://ideas.repec.org/a/ijc/ijcjou/y2013q4a2.html](https://ideas.repec.org/a/ijc/ijcjou/y2013q4a2.html). 

Nguyen, Thi Mai Lan. 2020. “Output Effects of Monetary Policy in Emerging and Developing Countries: Evidence from a Meta-Analysis.” Emerging Markets Finance and Trade 56 (1): 68–85. [https://doi.org/10.1080/1540496X.2019.1601081](https://doi.org/10.1080/1540496X.2019.1601081).

Nguyen, Thi Mai Lan, Elissaios Papyrakis, and Peter A. G. van Bergeijk. 2021. “Publication Bias in the Price Effects of Monetary Policy: A Meta-Regression Analysis for Emerging and Developing Economies.” International Review of Economics & Finance 71 (January): 567–83. [https://doi.org/10.1016/j.iref.2020.09.010](https://doi.org/10.1016/j.iref.2020.09.010).

Rusnak, Marek, Tomas Havranek, and Roman Horvath. 2013. “How to Solve the Price Puzzle? A Meta-Analysis.” Journal of Money, Credit and Banking 45 (1): 37–70. [https://doi.org/10.1111/j.1538-4616.2012.00561.x]().

