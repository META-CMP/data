# Abstract screening

We first draw a 1% random sample from the unlabelled (unscreened) full sample of studies. We then conducted abstract screening on this 1 % sample to estimate the number of potentially relevant entries in the full sample and to create prior information for the active learning-based abstract screening of the full sample. 

We then merged the 1% screened and labelled entries with the remaining entries from the full sample as `partially_labeled_full_sample.csv`. This file was then independently used by two screeners for the abstract screening process. 

The identical start projects for abstract screening are:

- [`abstract-screening-morpep-meta-cmp-screener-1-start.asreview`](data/study_search/database_search/processed/abstract_screening/abstract-screening-morpep-meta-cmp-screener-1-start.asreview) for screener 1
- [`abstract-screening-morpep-meta-cmp-screener-2-start.asreview`](data/study_search/database_search/processed/abstract_screening/abstract-screening-morpep-meta-cmp-screener-2-start.asreview) for screener 2

After the abstract screening finished, the final files were stored in the respective folder as: 

- [`abstract-screening-morpep-meta-cmp-screener-1.asreview`](data/study_search/database_search/processed/post_AS/screener_1_data/abstract-screening-morpep-meta-cmp-screener-1.asreview) for screener 1
- [`abstract-screening-morpep-meta-cmp-screener-2.asreview`](data/study_search/database_search/processed/post_AS/screener_2_data/abstract-screening-morpep-meta-cmp-screener-2.asreview) for screener 2

The `.asreview` project files can be opened in [ASReview](https://asreview.nl/) to directly inspect the screening results from within the software. 
