# Final join testing 

##### Only JSON
error_jsons <- c(
  "36YHG5BV", 
  "5GIIT7D9", 
  "73ASB2YH", 
  "8YK8NZTN", 
  "9DXGZWHB", 
  "A29KEVTA", 
  "AQPBXPMK", 
  "BZXQ7YNW", 
  "D48S8FM8", 
  "DG5TQDSF", 
  "DMN9K3YP", 
  "EV8Q8FDG", 
  "example_file", 
  "KM43WFQ8", 
  "NWBLK738", 
  "RETUUVG3", 
  "RNA92QK4", 
  "T3EZK9JC", 
  "UQPS6SGJ", 
  "V2A8ZH66", 
  "VE5FTFS7", 
  "WCBHBMQM", 
  "WEU39TYK", 
  "WRZ8Q2N8", 
  "Z2EIFGK7", 
  "ZCPHPCLN", 
  "ZMG2ZQ5V", 
  "ZV28P9HY"
  
  # Add further problem studies above
  )
json.join <- MetaExtractR::final_join(json_path = "data/full_text_screening/JSON_files", irf_path = NULL, only_json = TRUE, ignore = error_jsons)
# Get unique entries for JSON variables:
## JSON_VARIABLE_HERE
# all.JSON_VARIABLE_HERE <- as.data.frame(unique(json.join$JSON_VARIABLE_HERE)) #Replace JSON_VARIABLE_HERE with a specific variable code
all.size <- as.data.frame(unique(json.join$size)) # size
all.axis_trans <- as.data.frame(unique(json.join$axis_trans)) # axis_trans
all.inttype <- as.data.frame(unique(json.join$inttype)) # inttype
# ...




##### JSON and IRFs
assigned_irfs <- readr::read_csv("data/effect_sizes/assigned_irfs.csv")
extracted_irfs <- assigned_irfs$irf[complete.cases(assigned_irfs$included_in_dataset==1)]
# Ignore unextracted IRFs and erroneus JSON or IRF
keys <- sub("\\.json$", "", list.files("data/full_text_screening/JSON_files", pattern = "\\.json$"))
unextracted <- keys[!(keys %in% extracted_irfs)]
ignore <- c(error_jsons, unextracted, 
            "2LWMVPV7",  # IRF not extracted
            "34N9UJAA", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. 34N9UJAA model_2 Shock variable: lev_a_fed_funds Outcome: lev_q_rgap
            "3X48J8NE", # Probably issue with the csv format
            "4D2E8ML7", # Probably issue with the csv format
            "5F7IJHQM", # csv format 
            "5IUDWGEG", # model_2 lev_m_une_rate Check IRF data extraction in WebPlotDigitizer. Number of observations inconsistent: mean:49 upper:51 lower:49
            "5ULFVFYD", # 5ULFVFYD model_1 Shock variable: lev_a_ssr Outcome: logdiff_a_rip  Error in if (confidence_level <= 0 | confidence_level >= 1) { : missing value where TRUE/FALSE needed
            "652CDYWY", # Shock size in JSON unclear
            "6H5EMS5T", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. 6H5EMS5T model_1 Shock variable: fed_funds Outcome: lev_a_une
            "6INJPFY6", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "6YZLATYA", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "76SZ52IN", # Error in if (confidence_level <= 0 | confidence_level >= 1) { :   missing value where TRUE/FALSE needed
            "8LENTN8I", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. 8LENTN8I model_1 Shock variable: lev_a_fed_funds Outcome: log_q_gap
            "8VBXRY3I", # JSON periodicity inconsistent with IRF folder periodicity
            "96ST2TPE", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. 96ST2TPE model_1 Shock variable: lev_a_overnight_cashrate Outcome: levdiff_q_une_rate
            "9ACV5ELW", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. 9ACV5ELW model_1 Shock variable: lev_a_1_year_gov Outcome: lev_m_une_rate
            "9ECXEKL2", # Not exctracted
            "BJK49CEK", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "BKIN7DAE", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "BLXXM5HJ", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. BLXXM5HJ model_1 Shock variable: lev_a_repo Outcome: lev_q_une_rate
            "BWHKQVPU", # Error in if (confidence_level <= 0 | confidence_level >= 1) { : missing value where TRUE/FALSE needed
            "BXRNZWTJ", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. BXRNZWTJ model_1 Shock variable: 1_year_eonia Outcome: levdiff_q_une_rate
            "C5CP8A9I", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "C6UVA3Z2", # Error in if (confidence_level <= 0 | confidence_level >= 1) { :  missing value where TRUE/FALSE needed
            "CHGTHMWJ", # Error in d$CI.upper/shock_size : non-numeric argument to binary operator
            "FJ5LDSFN", # Error in d$CI.upper/shock_size : non-numeric argument to binary operator
            "FSUG2XYE", # JSON periodicity inconsistent with IRF folder periodicity
            "G968JAHL", # Error in FUN(left, right) : non-numeric argument to binary operator
            "GABJZ6WP", # JSON periodicity inconsistent with IRF folder periodicity
            "GBAXG3YQ", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. GBAXG3YQ model_11 Shock variable: lev_a_overnight_cashrate Outcome: lev_q_une_rate
            "GH5JFJE9", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. GH5JFJE9 model_1 Shock variable: lev_a_short_term_rate Outcome: lev_q_gap
            "GLY278ZB", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "GM9NF3YK", # Error in d$CI.upper/shock_size : non-numeric argument to binary operator
            "GXNQRTJ2", # Error in if (confidence_level <= 0 | confidence_level >= 1) { : missing value where TRUE/FALSE needed
            "GYW3RH4N", # JSON periodicity inconsistent with IRF folder periodicity
            "H55AC755", # JSON periodicity inconsistent with IRF folder periodicity
            "HB9TGTCM", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. HB9TGTCM model_1 Shock variable: lev_a_short_term_rate Outcome: lev_a_une_rate
            "I64723KV", # model_3 gr_m_cpi Check IRF data extraction in WebPlotDigitizer. Number of observations inconsistent: mean:48 upper:28 lower:48
            "IU3RD54B", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency.
            "LRVNFUKD", # model_7 log_m_ip Check IRF data extraction in WebPlotDigitizer. Number of observations inconsistent: mean:48 upper:47 lower:48
            "TTSTU35N", # Error in if (confidence_level <= 0 | confidence_level >= 1) { : missing value where TRUE/FALSE needed
            "VQXARSCQ", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. VQXARSCQ model_1 Shock variable: lev_a_ssr Outcome: lev_m_gap
            "WEL4CM26", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. WEL4CM26 model_1 Shock variable: lev_a_overnight_callrate Outcome: lev_q_une_rate
            "D9FTVUYP", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. D9FTVUYP model_1 Shock variable: lev_a_1_year_gov Outcome: lev_m_une_rate
            "DL3VAHYT", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. DL3VAHYT model_1 Shock variable: lev_a_fed_funds Outcome: lev_q_une_rate
            "DVZIAEVW", # Probably issue with the csv format
            "DWP2IL6B", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. DWP2IL6B model_1 Shock variable: lev_a_fed_funds Outcome: lev_q_une_rate
            "EN4JZ2YG", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. EN4JZ2YG model_1 Shock variable: lev_a_fed_funds Outcome: lev_q_une_rate
            "EPZD5AC9", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency. 
            "F7EUWDXG", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency. 
            "GGFP6GZF", # Check IRF data extraction in WebPlotDigitizer. Number of observations inconsistent: mean:49 upper:49 lower:48
            "IXUTM9DP", # Transformation case 1 or case 2, but periodicity of outcome variable does not match with data_frequency. 
            "J7MUSSTN", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. J7MUSSTN model_1 Shock variable: lev_a_fed_funds Outcome: lev_m_une_rate
            "JHWGNGLX", # 
            "JLZI3XBA", # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. JLZI3XBA model_1 Shock variable: lev_a_1_year_gov Outcome: lev_m_une_rate
            "JRBEAXPE", # Check IRF data extraction in WebPlotDigitizer. Number of observations inconsistent: mean:60 upper:59 lower:60
            "JT86GWFE" # Transformation case not specified for specification of dep and inttype. Check specification in JSON and update function if necessary. JT86GWFE model_2 Shock variable: lev_a_short_term_rate Outcome: lev_q_une
            
            # Add further problem studies above, ideally, paste the error message as a comment
)
json.irf.join <- MetaExtractR::final_join(json_path = "data/full_text_screening/JSON_files", irf_path = "data/effect_sizes/IRFs/", only_json = FALSE, ignore = ignore)
library(MetaExtractR)
json.irf.join <- MetaExtractR::final_join(json_path = "data/full_text_screening/JSON_files", irf_path = "data/effect_sizes/IRFs/", only_json = FALSE, investigate="3X48J8NE")

# Get IRF assignee:
# Get assignee (replace the key below)
assigned_irfs$assigned_to[assigned_irfs$irf=="GBAXG3YQ"]
