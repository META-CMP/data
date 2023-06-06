# Abstract screening

We first draw a 1% random sample from the unlabelled (unscreened) full sample of studies. We then conducted abstract screening on this 1 % sample to estimate the number of potentially relevant entries in the full sample and to create prior information for the active learning based abstract screening of the full sample. 

We then merged the 1% screened and labelled entries with the remaining entries from the full sample as `partially_labeled_full_sample.csv`. This file was then independently used by two screeners for the abstract screening process. 

The identical start projects for abstract screening are:

- `abstract-screening-morpep-meta-cmp-screener-1-start.asreview` for screener 1
- `abstract-screening-morpep-meta-cmp-screener-2-start.asreview` for screener 2

When abstract screening is finished, the final files are stored as: 

- `abstract-screening-morpep-meta-cmp-screener-1-end.asreview` for screener 1
- `abstract-screening-morpep-meta-cmp-screener-2-end.asreview` for screener 2