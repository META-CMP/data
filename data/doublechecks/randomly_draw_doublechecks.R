library(dplyr)
library(tidyr)


setwd("~/data/data/snowballing")
getwd()



## read in xlsx files after the full text screening and the adjustment of the bibliographic information.
library(readxl)
file.list <- 1:26
df.list <- lapply(file.list, function(x) read_excel(paste0("study_set_",x,".xlsx")))


library(dplyr)
df_full_text <- bind_rows(df.list, .id = "id")
df_full_text$doublecheck<-ifelse(df_full_text$doublecheck!=1 | is.na(df_full_text$doublecheck),FALSE,TRUE)



################## Adapt these three settings:


#### Change researcher id accordingly ###########
researcher<-"me"

#### doublecheck only entries ###########
doublecheck_only<-FALSE


#### Put in the number of studies you want to draw: 
n_doublecheck<-10



# draw random sample
random_sample<-df_full_text %>% filter(rid1!=researcher & is.na(rid2) & doublecheck==doublecheck_only) %>% # filter to only consider relevant entries
                                slice_sample(n=n_doublecheck, replace = FALSE) # drawn n observations




