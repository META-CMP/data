library(tidyverse)
library(here)

filepath <- here::here("unlabelled_full_sample.csv.csv")

df <- readr::read_csv(filepath)


set.seed(1234)
rand_df <- df[sample(nrow(df), size=107), ]

write.csv(rand_df, "random_sample_1p.csv", row.names = FALSE)
