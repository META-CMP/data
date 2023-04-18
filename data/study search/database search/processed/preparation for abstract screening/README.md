# Preparing data for title and abstract screening

Here we outline the steps which were conducted on the data from the initial database search to prepare our machine-learning supported title and abstract screening process. We also store intermediate data files which were created during preparation.

1. After our initial database search, we merged the results from EconLit and Google Scholar and removed duplicates using the bibliographic software [Zotero](https://www.zotero.org/) (version: 6.0.21) and the [Zotero Duplicates Merger](https://github.com/frangoud/ZoteroDuplicatesMerger) add-on (v1.1.5) for Zotero. We took care to preserve the existing complete abstracts from EconLit when merging the duplicates. See [here](https://github.com/META-CMP/data/tree/main/data/study%20search/database%20search/processed/preparation%20for%20abstract%20screening/merging%20of%20EL%20and%20GS%20results%20and%20duplicate%20removal) for the merged and duplicate-free results in `.csv` and `.ris` format.

- Entries after merging of EconLit and Google Scholar results: 18265
- Entries after removal of duplicates: 12272

2. We then applied the script [`extracting_URLs_script.R`](https://github.com/META-CMP/data/blob/main/data/study%20search/database%20search/processed/preparation%20for%20abstract%20screening/retrieving%20missing%20or%20incomplete%20abstracts/extracting_URLs_script.R) to the merged and duplicate-free dataset to extract the URLs (or, if no URL exists, the titles) of those 5572 [entries](https://github.com/META-CMP/data/blob/main/data/study%20search/database%20search/processed/preparation%20for%20abstract%20screening/retrieving%20missing%20or%20incomplete%20abstracts/entries_without_complete_abstracts.csv) that have no abstract or only an incomplete abstract (Google Scholar only extracts abbreviated abstracts and both Google Scholar and EconLit sometimes have missing abstracts). We then manually searched for these entries and retrieved their abstracts, if available, from the internet.

## Retrieving missing abstracts when only titles are available

This section describes the steps taken to retrieve missing abstracts for the 73 Zotero entries for which only titles were available. We conducted them on February 21st and 22nd in 2023.

- Using the exact title within the Google Scholar search, we screened the first few entries. When the title of those did not match the respective title, we used paranthesis to search for the exact phrase.

- When the title of an article matched a search result, but the Google Scholar citation did not have the option to retrieve the abstract, three options were used.  
  1. First, we tapped the "All 'X' versions" button in Google Scholar to screen for the respective abstracts of the relevant paper.
  2. Second, we entered the title in a Google search to retrieve the abstracts for the relevant articles.
  3. Third, we searched for the title in econlit.
  
- Option two and three of the last point were also conducted if the title of the paper seemed to fit our research question at a very basic level and none of the previous steps was successful. 

## Retrieving missing abstracts when urls are available

Based on the URLs and titles from entries without complete abstracts, a student assistant collected abstracts from the websites of the bibliographic entries, where possible. 

### How to merge these completed entries with the already complete entries?

#### 1st Step

First, we prepared a dataset, that contains those entries from the initial duplicate-free dataset that are not in "entries_without_complete_abstracts.csv". We added a function to `extracting_URLs_script.R` that did this. The file was stored as `entries_complete_abstracts.csv`.

#### 2nd Step

Afterward, we imported the initial "merged_EL_GS_no_duplicates.ris" into Zotero to make Zotero know the keys. Subsequently, we imported our ris-file that we retrieved based on the 73 titles of the entries that had no URLs. We removed the remaining duplicates and entries with missing abstracts. We exported this file to the GitHub repo as `abstracts_from_titles.ris` and `abstracts_from_titles.csv`.

#### 3rd Step

We imported the initial "merged_EL_GS_no_duplicates.ris" into Zotero to make Zotero know the keys. Afterwards, we created an empty new collection in Zotero. Next, we imported the ris-files of the student assistant one by one, and move them as a subcollection into the empty collection (drag and drop). Subsequently, we deleted the "merged_EL_GS_no_duplicates" collection (with items!) from Zotero. This procedure should ensure that Zotero would have resolved any "Key" conflicts between the original dataset and the ris-files from the student assistant. In a last step, we removed duplicated item directly in Zotero.

The total collection of duplicate free student ris-files is saved as `abstracts_from_URLs.ris` and `abstracts_from_URLs.csv` in the folder "retrieving missing or incomplete abstracts". 

For 24 studies, this procedure provided us with the corresponding abstracts for the respective papers. 


3. Our search strategy is supplemented by a screening of the existing meta-studies that are relevant for our research interest. From these meta-studies we add all studies to our database that were not yet covered by our search.

Therefore, we manually collected all 43 Studies of Nguyen (2021) and 45 Studies of Nguyen (2020) and read the data of Havranek et al (2013) (available at: http://meta-analysis.cz/lags/lags.zip) and Rusnak et al (2013) (http://meta-analysis.cz/price_puzzle/puzzle.xls) directly into R using the `code_to_read_in_data.R` script. 


4. Finally, we merged 

1. `entries_complete_abstracts.csv`
2. `abstracts_from_titles.csv`
3. `abstracts_from_URLs.csv`
4. `havranek_2013.csv`
5. `Nguyen_2021_2020.csv`


to retrieve our dataset for a last round of final checks conducted in [`merge_all_files.R`](). In this final checks we randomly screened for additional available abstracts when the abstract note was "NA", checked the abstracts for remaining "..." quotes, and conducted two additional rounds of duplicates removal using the app of the "revtools" Package in R. Afterward, we remove the entries without abstract or title and save the final dataset for abstract screening as `unlabelled_full_sample.csv`.


The resulting literature data base [containing 10714 entries] will be screened based on title and abstract.


