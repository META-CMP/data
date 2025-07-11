# Abstract screening

Using [`draw_random_sample.R`](draw_random_sample.R), we first drew a [1% random sample](1_percent_random_sample/morpep-meta-cmp-1-percent-random-sample_unlabelled.csv) from the 10714 entries of the [unlabelled (unscreened) full sample of studies](unlabelled_full_sample.csv). We then conducted abstract screening using [ASReview](https://asreview.nl/) on this 1 % sample to pilot the selection process and to estimate the number of potentially relevant entries in the full sample for the calibration of our stopping rule (see [Sec. 3.6.1 of our pre-registration](https://osf.io/4jxk3) for details of the abstract screening and [Sec. 3.7.1](https://osf.io/4jxk3) for the stopping rules). The labbeled 1% sample provided also the prior information for the active learning-based abstract screening of the full sample. The results of this initial screening are stored as [morpep-meta-cmp-1-percent-random-sample-fully-labelled.asreview](1_percent_random_sample/morpep-meta-cmp-1-percent-random-sample-fully-labelled.asreview) and [asreview_dataset_all_morpep-meta-cmp-1-percent-random-sample-fully-labelled.csv](data/study_search/database_search/processed/abstract_screening/1_percent_random_sample/asreview_dataset_all_morpep-meta-cmp-1-percent-random-sample-fully-labelled.csv).

We then merged the 1% screened and labelled entries with the remaining entries from the full sample as [`partially_labeled_full_sample.csv`](partially_labelled_full_sample.csv). This file was then independently used by two screeners for the abstract screening process.

The identical start projects for abstract screening are:

- [`abstract-screening-morpep-meta-cmp-screener-1-start.asreview`](data/study_search/database_search/processed/abstract_screening/abstract-screening-morpep-meta-cmp-screener-1-start.asreview) for screener 1
- [`abstract-screening-morpep-meta-cmp-screener-2-start.asreview`](data/study_search/database_search/processed/abstract_screening/abstract-screening-morpep-meta-cmp-screener-2-start.asreview) for screener 2

After the abstract screening finished, the final files were stored in the respective folder as: 

- [`abstract-screening-morpep-meta-cmp-screener-1.asreview`](data/study_search/database_search/processed/post_AS/screener_1_data/abstract-screening-morpep-meta-cmp-screener-1.asreview) for screener 1
- [`abstract-screening-morpep-meta-cmp-screener-2.asreview`](data/study_search/database_search/processed/post_AS/screener_2_data/abstract-screening-morpep-meta-cmp-screener-2.asreview) for screener 2

The `.asreview` project files can be opened in [ASReview](https://asreview.nl/) to directly inspect the screening results from within the software. 

## Analyses of screening

Using [`testing_and_merging_of_AS_data.R`](/data/study_search/database_search/processed/post_AS/testing_and_merging_of_AS_data.R), we did some consistency checks and analysis of the screening as well as preparations for the retrieval of full-text PDFs and full text screening of the potentially eligible studies.

### Screener agreement and overlap

-   5826 entries were screened by either screener 1 or 2 (or both). 2034 were included by screener 1. 2148 were included by screener 2.
-   2540 were marked as relevant by at least one screener.
-   1642 entries were included by both screeners.
-   2500 entries were excluded by both screeners.
-   335 entries were included by screener 1 but excluded by screener 2.
-   477 entries were included by screener 2 but excluded by screener 1.
-   4954 entries were screened by both screeners. Of these, 1977 , i.e. 39.9 %, were included by Screener 1 and 2119 , i.e. 42.8 %, were included by Screener 2.
-   726 entries were screened exclusively by Screener 1. Of these, 57 were included, i.e. 7.9 %.
-   146 entries were screened exclusively by Screener 2. Of these, 29 were included, i.e. 19.9 %.

### Randomization and packages creation for full text download

-   Order of entries has been randomized
-   Data is split into a number `.xslx` files with max of 100 entries in each file.
-   These `.xslx` files contain only information relevant for retrieving PDFs and empty columns for notes regarding availability. These files were used for the retrieval of full text PDFs by student assistants following [these instructions](/data/study_search/database_search/processed/post_AS/full_text_download_HIWI_sheet.pdf).
