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
