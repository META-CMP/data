After we have split the data randomly in our Excel-files, we conducted one final round of duplicates checks. During these checks we aimed additionally to find mathes where we have to different versions of a paper in our data set. For example, the working paper and the published version two years afterward. In such cases we only keep the most recent and published version and note for the other item that it is a duplicated one. Thus we made sure that we do not double code a single study. Throughout the check used the r-package revtools and the fuzzy matching technique offered by the package. 

The step taken are the following: 

- First, we screened duplicates in the "revtools" app for the xlsx files using fuzzy title matches and saved the resulting duplicate free data as "dup_free.csv".

- Second, we screened the "dup_free.csv" data additionally by using fuzzy author matches and saved the resulting duplicate free data as "dup_free_author.csv".

- Third, we screened the "dup_free_author.csv" data by using one more round of fuzzy title matches (with a lower matching threshold) and saved the resulting duplicate free data as "dup_free_title2.csv".

- Fourth, we screened the "dup_free_title2.csv" data by using fuzzy abstract matches and saved the resulting duplicate free data as "duplicate_free_final.csv".


In a last step, if we accounted a working paper for which a more recent version is available in our dataset, we check whether this more recent study is also listed in our dataset by screening for the first author name within the "duplicate_free_final.csv".


