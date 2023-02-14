# Preparing data for title and abstract screening

Here we outline the steps which were conducted on the data from the intial database search to prepare our machine-learning supported title and abstract screening process. We also store intermediate data files which were created during preparation.

1. After our initial database search, we merged the results from EconLit and Google Scholar and removed duplicates using the bibliographic software [Zotero](https://www.zotero.org/) (version: 6.0.21) and the [Zotero Duplicates Merger](https://github.com/frangoud/ZoteroDuplicatesMerger) add-on (v1.1.5) for Zotero. We took care to preserve the existing complete abstracts from EconLit when merging the duplicates. See [here](https://github.com/META-CMP/data/tree/main/data/study%20search/database%20search/processed/preparation%20for%20abstract%20screening/merging%20of%20EL%20and%20GS%20results%20and%20duplicate%20removal) for the merged and duplicate-free results in `.csv` and `.ris` format.

- Entries after merging of EconLit and Google Scholar results: 18265
- Entries after removal of duplicates: 12272

2. We then applied the script [`extracting_URLs_script.R`](https://github.com/META-CMP/data/blob/main/data/study%20search/database%20search/processed/preparation%20for%20abstract%20screening/retrieving%20missing%20or%20incomplete%20abstracts/extracting_URLs_script.R) to the merged and duplicate-free dataset to extract the URLs (or, if no URL exists, the titles) of those 5572 [entries](https://github.com/META-CMP/data/blob/main/data/study%20search/database%20search/processed/preparation%20for%20abstract%20screening/retrieving%20missing%20or%20incomplete%20abstracts/entries_without_complete_abstracts.csv) that have no abstract or only an incomplete abstract (Google Scholar only extracts abbreviated abstracts and both Google Scholar and EconLit sometimes have missing abstracts). We then manually searched for these entries and retrieved their abstracts, if available, from the internet.

...

3. Finally, we merged 

...

to retrieve our final dataset for title and abstract screening. 
