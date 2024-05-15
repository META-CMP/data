library(MetaExtractR)

error_jsons <- c(
  "example_file",
  "V2A8ZH66",# no IRFs
  "Q5FHEZNE", # contains table estimates as well. 
  #"QBQF4W6S",# missing core responses, responses are now added
  "JHWGNGLX",
  "XB3JCPV7",
  "UGLCD22A",
  "V3YGD4RH",
  "L2TKIG44",
  "4WHAJ9ZC",
  "XE2RQA97",
  "Q9S2TXAI",
  "C87JBMDH",
  "DIKT4VT6",
  "AUUW3HH4",
  "5PA5TJLY",
  "P85XBWGQ",
  "4TYJQZIQ",
  "T6EA38XY",
  "8YK8NZTN",
  "7M3KH3J6",
  "87DL3IY2",
  "ZC9X48WY"# missing IRF data for many models. 
)

setwd("~/data")

json.irf.join <- MetaExtractR::final_join(json_path = "data/full_text_screening/JSON_files", irf_path = "data/effect_sizes/IRFs/", only_json = FALSE, ignore = error_jsons)

#save(data,file = "preliminary_data.RData")


library(dplyr)
library(tidyverse)

data<-json.irf.join


rate<-data %>% dplyr::filter(data$outcome_var=="rate")
other<-data %>% dplyr::filter(data$outcome_var!="rate")

rate<-rate %>% dplyr::select(key,model_id,period:SE.lower) %>% dplyr::select(-outcome_var)
#save(rate,file = "preliminary_rate_data.RData")

colnames(rate)[4:ncol(rate)] <- paste("rate_",colnames(rate)[4:ncol(rate)],sep="")

data<-other %>% dplyr::left_join(rate, by=c("key","model_id","period"))



library(stringi)

split_list<-stri_split_fixed(str = data$outcome_var, pattern = "_", n = 3)

# Extract nth element from each split list
data$transformation <- sapply(split_list, function(x) ifelse(1 <= length(x), x[1], NA))


split_list <- sapply(split_list, function(x) ifelse(3 <= length(x), x[3], NA))

data$real_output<-ifelse(split_list %in% c("rgdp","ip","rip","rgnp","rgap"),TRUE,NA)
data$real_output<-ifelse(split_list %in% c("gdp","gnp","gap"),FALSE,data$real_output)

# remove r from the outcome measure
split_list<-sub("^r", "", split_list)

data$outcome_measure<-split_list

data$outcome<-ifelse(data$outcome_measure %in% c("gdp","ip","gap","gnp"),"gdp",ifelse(data$outcome_measure %in% c("cpi","deflator","wpi","core","price_level"),"inflation","emp"))


#generate main research question dummy
fun <- function(x, y) {
  grepl(x, y, fixed = TRUE)
}

data$main_research_q<-mapply(fun, data$outcome, data$main)


library(zoo)


#data %>% filter(end=="Q4–2015")

data$start<-ifelse(data$start=="Q4–2000","Q4-2000",data$start)
data$end<-ifelse(data$end=="Q4–2015","Q4-2015",data$end)


data<-data %>%
  mutate(observations_calc = case_when(substr(start,1,1)=="Q" ~ ((as.yearqtr(end, format = "Q%q-%Y")-as.yearqtr(start, format = "Q%q-%Y"))*4+1)*as.numeric(n_of_countries),
                                       substr(start,1,1)!="Q" & grepl("-", start) == TRUE ~ ((as.yearmon(end, format = "%m-%Y")-as.yearmon(start, format = "%m-%Y"))*12+1)*as.numeric(n_of_countries),
                                       nchar(data$start)==4 ~ (as.numeric(end)-as.numeric(start)+1)*as.numeric(n_of_countries)))


data$samplesize<-as.numeric(data$samplesize)

unique(data$samplesize)

data$observations<-ifelse(is.na(data$samplesize), data$observations_calc, as.numeric(data$samplesize))




# set logical values to dummies.
data<-data %>% mutate_if(is.logical, as.numeric)  

#summary(data)
head(data)

#test<-data %>% filter(observations==1)
unique(data$study_notes)
unique(data$model_notes)

data$quality_concern<-grepl("quality_concern",data$study_notes)

data$quality_concern<-ifelse(grepl("quality_concern",data$model_notes),TRUE, data$quality_concern)


data$regime<-grepl("regime",data$study_notes)

data$regime<-ifelse(grepl("regime",data$model_notes),TRUE, data$regime)

sum(data$regime)
