# Snowballing script to get references from the most recent eligible studies after the first round of full text screening.
library(dplyr)

# Set path for study sets
study_set_path <- here::here("data/study_search/database_search/processed/post_AS/packages_for_full_text_download")

## Read in xlsx files after the full text screening and the adjustment of the bibliographic information.
library(readxl)
file.list <- 1:26
df.list <- lapply(file.list, function(x) read_excel(paste0(study_set_path, "/study_set_",x,".xlsx")))
df <- bind_rows(df.list, .id = "id")

# Filter included studies which have been included and published in 2023.
test <- df %>% filter(included==1, `publication year`==2023)

# found 10 most recent studies by month of publication using the links: badinger, read, Hoesch, bauer, andersen, choi, forni, aruoba, read, burgard
test$author
test$newest <- c(1,1,0,0,1,0,1,1,1,0,1,0,1,0,0,1,1,0)
sum(test$newest) # Should be 10
test <- test %>% filter(newest==1)

# Further manual steps:

## Put the 6 dois into citation chaser and download 200 unique references: 10.1257/mac.20210035, 10.1111/1475-4932.12749, 10.1257/mac.20200068, 10.1086/723574, 10.1111/jofi.13262, 10.2139/ssrn.4320865 

## Directly download references ris files from https://ideas.repec.org/p/boe/boeewp/1024.html and https://ideas.repec.org/p/upf/upfgen/1742.html

## Import all those ris files into zotero and remove duplicates so that we came to 285 unique entries.

## Compare these reference with those in https://drive.google.com/file/d/1d-y6InWXYpQv7dVzx86yAaVOLoVXTgBW/view and econweb.umd.edu/~drechsel/papers/Aruoba_Drechsel.pdf

## Extract additional papers and thesis. But no newspaper articles, speeches, blog articles, comments on papers which have not been published, or descriptions of statistical packages and so on extracted from the references such as https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjmhYHumueCAxVThf0HHbR6AeYQFnoECBAQAQ&url=https%3A%2F%2Fstatic1.squarespace.com%2Fstatic%2F5e6033a4ea02d801f37e15bb%2Ft%2F5ee2cad59df0da103d0d30cb%2F1591921366423%2Ftalk_notes_new_measure_2.pdf&usg=AOvVaw3hAiK3fvN2tUNRR8gPeaTr&opi=89978449. But all paper and thesis like references. 

## First the Aruoba_2022_identifying paper additional references have been added, which increased the number of unique references to 318

## Afterward the Read_2022_the paper which lead to 331 unique references.  

## Afterward check Bauer_2022_a references which increased number of unique references to a total 341

## Check the two ideas repec papers # after Choi_2022_revisiting 359 and 371 after Debortoli_2020_asymmetric

## Control the remaining 5 ones which have been screened by citation chaser
###  Hoesch_2020_has to 372
###  Badinger_2020_measuring to 376
###  Burgard_2023_almost to 383
###  Andersen_2022_monetary to 391
###  Reserve Bank of Australia; Read_2023_estimating to 406

# Altogether, we extracted 406 references of those 10 most recent eligible studies. 