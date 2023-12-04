setwd("~/data/data/snowballing")
getwd()



## read in xlsx files after the full text screening and the adjustment of the bibliographic information.
library(readxl)
file.list <- 1:26
df.list <- lapply(file.list, function(x) read_excel(paste0("study_set_",x,".xlsx")))


library(dplyr)
df <- bind_rows(df.list, .id = "id")


# Filter studies which have been included and published in 2023.


test<-df %>% filter(included==1, `publication year`==2023)

# found 10 most recent studies by month of publication using the links: badinger, read, Hoesch, bauer, andersen, choi, forni, aruoba, read, burgard


test$author

test$newest<-c(1,1,0,0,1,0,1,1,1,0,1,0,1,0,0,1,1)

sum(test$newest)


test<-test %>% filter(newest==1)


# Put the 6 dois into citation chaser and download 200 unique references: 10.1257/mac.20210035, 10.1111/1475-4932.12749, 10.1257/mac.20200068, 10.1086/723574, 10.1111/jofi.13262, 10.2139/ssrn.4320865 

# directly download references ris files from https://ideas.repec.org/p/boe/boeewp/1024.html and https://ideas.repec.org/p/upf/upfgen/1742.html

# Import all those ris files into zotero and remove duplicates so that we came to 285 unique entries.

# compare these reference with those in https://drive.google.com/file/d/1d-y6InWXYpQv7dVzx86yAaVOLoVXTgBW/view and econweb.umd.edu/~drechsel/papers/Aruoba_Drechsel.pdf

# Extract additional papers and thesis. But no newspaper articles, speeches, blog articles, comments on papers which have not been published, or descriptions of statistical packages and so on extracted from the references such as https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjmhYHumueCAxVThf0HHbR6AeYQFnoECBAQAQ&url=https%3A%2F%2Fstatic1.squarespace.com%2Fstatic%2F5e6033a4ea02d801f37e15bb%2Ft%2F5ee2cad59df0da103d0d30cb%2F1591921366423%2Ftalk_notes_new_measure_2.pdf&usg=AOvVaw3hAiK3fvN2tUNRR8gPeaTr&opi=89978449. But all paper and thesis like references. 


# First the Aruoba_2022_identifying paper additional references have been added, which increased the number of unique references to 318

# afterward the Read_2022_the paper which lead to 331 unique references.  

# afterward check Bauer_2022_a references which increased number of unique references to a total 341

# check the two ideas repec papers # after Choi_2022_revisiting 359 and 371 after Debortoli_2020_asymmetric

# control the remaining 5 ones which have been screened by citation chaser
#  Hoesch_2020_has to 372
#  Badinger_2020_measuring to 376
#  Burgard_2023_almost to 383
#  Andersen_2022_monetary to 391
#  Reserve Bank of Australia; Read_2023_estimating to 406

# Thus altogether, we extracted 406 references of those 10 most recent eligible studies. 