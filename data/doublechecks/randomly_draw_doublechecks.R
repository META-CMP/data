library(dplyr)
library(tidyr)


# setwd("~/data/data/snowballing")
# getwd()
# 
# 
# 
# ## read in xlsx files after the full text screening and the adjustment of the bibliographic information, do this only once to create r file.
# library(readxl)
# file.list <- 1:27
# df.list <- lapply(file.list, function(x) read_excel(paste0("study_set_",x,".xlsx")))
# 
# 
# library(dplyr)
# df_full_text <- bind_rows(df.list, .id = "id")
# # adjust type of doublecheck column
# df_full_text$doublecheck<-ifelse(df_full_text$doublecheck!=1 | is.na(df_full_text$doublecheck),FALSE,TRUE)
# 
# setwd("~/data/data/doublechecks")
# saveRDS(df_full_text,"df_full_text")

setwd("~/data/data/doublechecks")

#load data 
df_full_text<-readRDS("df_full_text")


################## Adapt these three settings:


#### Change researcher id accordingly ###########
researcher<-"me"

#### doublecheck only entries ###########
doublecheck_only<-FALSE


#### Put in the number of studies you want to draw: 
n_doublecheck<-10

#### round of double check draw per resercher
round<-1


sum(df_full_text$included, na.rm = TRUE)

# draw random sample
random_sample<-df_full_text %>% filter(rid1!=researcher & is.na(rid2) & doublecheck==doublecheck_only & included==1) %>% # filter to only consider relevant entries
                                slice_sample(n=n_doublecheck, replace = FALSE) # drawn n observations



df_full_text<-setdiff(df_full_text,random_sample)

saveRDS(df_full_text,"df_full_text")
saveRDS(random_sample,paste0("random_sample_",researcher,round))