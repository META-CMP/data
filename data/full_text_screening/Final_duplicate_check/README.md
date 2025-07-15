# Final duplicate check 

After we split the studies randomly into [26 packages](data/study_search/database_search/processed/post_AS/packages_for_full_text_download) to facilitate the work sharing, we conducted one final round of duplicates checks. During these checks we aimed to find matches where we have two different versions of a paper in our data set. For example, a working paper version and a journal version of the same paper. In such cases we only kept the most recent and published version and created a duplicated note for the other item. Thus we made sure that we do not double code studies. We again used the `revtools` `R` package with "fuzzy matching" technique. 

The specific steps taken in `duplicate_check_2540.R` are the following:

- First, we screened duplicates in the `revtools` app for the `xlsx` files using fuzzy title matches and saved the resulting duplicate free data as `dup_free.csv`.

- Second, we screened the `dup_free.csv` data additionally by using fuzzy author matches and saved the resulting duplicate free data as `dup_free_author.csv`.

- Third, we screened the `dup_free_author.csv` data by using one more round of fuzzy title matches (with a lower matching threshold) and saved the resulting duplicate free data as `dup_free_title2.csv`.

- Fourth, we screened the `dup_free_title2.csv` data by using fuzzy abstract matches and saved the resulting duplicate free data as `duplicate_free_final.csv`.

In a last step, if we found a paper for which a more recent version was available in our dataset, we checked whether this more recent study is also listed in our dataset by screening for the first author name within `duplicate_free_final.csv`.


