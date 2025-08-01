library(readr)
codebook <- read_csv("codebook.csv")

# Lowercase category and variable names (consistent with JSON files)
codebook$Category <- tolower(codebook$Category)
codebook$Variable <- tolower(codebook$Variable)

# Deleting outdated / irrelevant columns
codebook$Reference <- NULL
codebook$Motivation_theory <- NULL
codebook$Preference_ME <- NULL
codebook$Preference_FP <- NULL
codebook$Hypothese <- NULL

# Add column for coding instructions
codebook$Coding_instruction <- NA

# Update of "key"
codebook$Entries[codebook$Variable == "key"] <- 'e.g. "2LWMVPV7"'

# Update of "rid1", "rid2"
codebook$Entries[codebook$Variable == "rid1"] <- 'e.g. "me", "sg", "fp", "ph", "dr"'
codebook$Entries[codebook$Variable == "rid2"] <- 'e.g. "me", "sg", "fp", "ph", "dr"'

# Update of "study_notes"
codebook$Description[codebook$Variable == "study_notes"] <- "General notes on the study that are relevant for all models within the study."
codebook$Coding_instruction[codebook$Variable == "study_notes"] <- "https://github.com/META-CMP/data/issues/9"
codebook$Entries[codebook$Variable == "study_notes"] <- "See coding_instruction"

# Update of "model_notes"
codebook$Description[codebook$Variable == "model_notes"] <- "Model specific notes that are relevant only for a given model within the study."
codebook$Coding_instruction[codebook$Variable == "model_notes"] <- "https://github.com/META-CMP/data/issues/67"
codebook$Entries[codebook$Variable == "model_notes"] <- "See coding_instruction"

# Update of "figure"
codebook$Description[codebook$Variable == "figure"] <- "Indicates the number of the figure from which the impulse response is taken"
codebook$Coding_instruction[codebook$Variable == "figure"] <- "https://github.com/META-CMP/data/issues/46"
codebook$Entries[codebook$Variable == "figure"] <- "See coding_instruction"

# Update of "page"
codebook$Description[codebook$Variable == "page"] <- "Indicates the page on which the impulse response (figure or table) is displayed."
codebook$Coding_instruction[codebook$Variable == "page"] <- "https://github.com/META-CMP/data/issues/97"
codebook$Entries[codebook$Variable == "page"] <- "See coding_instruction"

# Update of "irf"
codebook$Description[codebook$Variable == "irf"] <- "Indicates if there is a graphical representation of the irfs in the study."
codebook$Coding_instruction[codebook$Variable == "irf"] <- "https://github.com/META-CMP/data/issues/71"

# Update of "cum"
codebook$Description[codebook$Variable == "cum"] <- "Indicates if the impulse response function represents cumulated effects."
codebook$Coding_instruction[codebook$Variable == "cum"] <- "https://github.com/META-CMP/data/issues/72"
codebook$source[codebook$Variable == "cum"] <- "study/screener"

# Update of "inttype"
codebook$Description[codebook$Variable == "inttype"] <- "Type of short term interest rate variable which is shocked."
codebook$Coding_instruction[codebook$Variable == "inttype"] <- "https://github.com/META-CMP/data/issues/6"
codebook$Entries[codebook$Variable == "inttype"] <- "See coding_instruction"

# Update of "size"
codebook$Description[codebook$Variable == "size"] <- "Shock size in basis points or percent."
codebook$Coding_instruction[codebook$Variable == "size"] <- "https://github.com/META-CMP/data/issues/52"
codebook$Entries[codebook$Variable == "size"] <- "See coding_instruction"

# Update of "axis_trans"
codebook$Description[codebook$Variable == "axis_trans"] <- "Note if the axis scale needs to be transformed to be a percentage scale."
codebook$Coding_instruction[codebook$Variable == "axis_trans"] <- "https://github.com/META-CMP/data/issues/62"
codebook$Entries[codebook$Variable == "axis_trans"] <- "See coding_instruction"
codebook$source[codebook$Variable == "axis_trans"] <- "study/screener"

# Update of "dep"
codebook$Description[codebook$Variable == "dep"] <- "Dependent (outcome) variable codes."
codebook$Coding_instruction[codebook$Variable == "dep"] <- "https://github.com/META-CMP/data/issues/10"
codebook$Entries[codebook$Variable == "dep"] <- "See coding_instruction"

# Update of "conf"
codebook$Description[codebook$Variable == "conf"] <- "Level of the confidence bounds or standard errors."
codebook$Coding_instruction[codebook$Variable == "conf"] <- "https://github.com/META-CMP/data/issues/98"
codebook$Entries[codebook$Variable == "conf"] <- "See coding_instruction"

# Update of "main"
codebook$Description[codebook$Variable == "main"] <- "The outcome variable representing the main research interest of the study."
codebook$Coding_instruction[codebook$Variable == "main"] <- "https://github.com/META-CMP/data/issues/17"

# Update of "prefer"
codebook$Description[codebook$Variable == "prefer"] <- "Indicates if the effect estimate based on the model specification is preferred in the study."
codebook$Coding_instruction[codebook$Variable == "prefer"] <- "https://github.com/META-CMP/data/issues/87"
codebook$source[codebook$Variable == "prefer"] <- "study/screener"

# Update of "iv"
codebook$Description[codebook$Variable == "iv"] <- "Indicates if an instrumental variable approch is used for identification."
codebook$Coding_instruction[codebook$Variable == "iv"] <- "https://github.com/META-CMP/data/issues/99"

# Update of "forecast_based"
codebook$Description[codebook$Variable == "forecast_based"] <- "Indicates whether a forecast-based strategy is used for shock identification."
codebook$Coding_instruction[codebook$Variable == "forecast_based"] <- "https://github.com/META-CMP/data/issues/41"

# Update of "nr"
codebook$Description[codebook$Variable == "nr"] <- "Indicates if a narrative strategy based on narrative records or sentiments of central bank statements is used for shock identification."
codebook$Coding_instruction[codebook$Variable == "nr"] <- "https://github.com/META-CMP/data/issues/41"

# Update of "event"
codebook$Description[codebook$Variable == "event"] <- "Indicates if an event study or a similar direct measurement of the monetary policy shock is used for shock identification."
codebook$Coding_instruction[codebook$Variable == "event"] <- "https://github.com/META-CMP/data/issues/100"

# Update of "chol"
codebook$Description[codebook$Variable == "chol"] <- 'Indicates if a recursive ("Cholesky") ordering is used for shock identification.'
codebook$Coding_instruction[codebook$Variable == "chol"] <- "https://github.com/META-CMP/data/issues/101"

# Update of "svar"
codebook$Description[codebook$Variable == "svar"] <- "Indicates if a non-recursive structural VAR approach is used for shock identification (no Cholesky)."
codebook$Coding_instruction[codebook$Variable == "svar"] <- "https://github.com/META-CMP/data/issues/102"

# Update of "signr"
codebook$Description[codebook$Variable == "signr"] <- "Indicates if a sign restriction approach is used for shock identification."
codebook$Coding_instruction[codebook$Variable == "signr"] <- "https://github.com/META-CMP/data/issues/86"

# Update of "hf"
codebook$Description[codebook$Variable == "hf"] <- "Indicates if a high frequency approach is used for shock identification."
codebook$Coding_instruction[codebook$Variable == "hf"] <- "https://github.com/META-CMP/data/issues/103"

# Update of "heteroskedas"
codebook$Description[codebook$Variable == "heteroskedas"] <- "Indicates if a heteroskedasticity-based approach is used for shock identification."
codebook$Coding_instruction[codebook$Variable == "heteroskedas"] <- "https://github.com/META-CMP/data/issues/78"

# Update of "longrun"
codebook$Description[codebook$Variable == "longrun"] <- "Indicates if shock identification is based on long-run restrictions."
codebook$Coding_instruction[codebook$Variable == "longrun"] <- "https://github.com/META-CMP/data/issues/104"

# Update of "idother"
codebook$Description[codebook$Variable == "idother"] <- "Indicates if the identification strategy does not correspond to one of the other variables in identification_strategy."
codebook$Coding_instruction[codebook$Variable == "idother"] <- "https://github.com/META-CMP/data/issues/69"

# Update of "var"
codebook$Description[codebook$Variable == "var"] <- "Indicates if a vector autoregression model (VAR) is used for estimation."
codebook$Coding_instruction[codebook$Variable == "var"] <- "https://github.com/META-CMP/data/issues/105"

# Update of "lp"
codebook$Description[codebook$Variable == "lp"] <- "Indicates if a local projections method is used for estimation."
codebook$Coding_instruction[codebook$Variable == "lp"] <- "https://github.com/META-CMP/data/issues/106"

# Update of "vecm"
codebook$Description[codebook$Variable == "vecm"] <- "Indicates if a vector error correction model is used for estimation."
codebook$Coding_instruction[codebook$Variable == "vecm"] <- "https://github.com/META-CMP/data/issues/107"

# Update of "dyn_ols"
codebook$Description[codebook$Variable == "dyn_ols"] <- "Indicates if a dynamic ols model, such as an autoregressive distributed lag model, is used for estimation."
codebook$Coding_instruction[codebook$Variable == "dyn_ols"] <- "https://github.com/META-CMP/data/issues/25"

# Update of "fvar"
codebook$Description[codebook$Variable == "fvar"] <- "Indicates if a factor-augmented VAR model is used for estimation."
codebook$Coding_instruction[codebook$Variable == "fvar"] <- "https://github.com/META-CMP/data/issues/81"

# Update of "tvar"
codebook$Description[codebook$Variable == "tvar"] <- "Indicates if a time-varying parameter VAR models is used for estimation."
codebook$Coding_instruction[codebook$Variable == "tvar"] <- "https://github.com/META-CMP/data/issues/108"

# Update of "gvar"
codebook$Description[codebook$Variable == "gvar"] <- "Indicates if a global VAR model (no panel) is used for estimation."
codebook$Coding_instruction[codebook$Variable == "gvar"] <- "https://github.com/META-CMP/data/issues/109"

# Update of "bayes"
codebook$Description[codebook$Variable == "bayes"] <- "Indicates if a Bayesian approach is used for estimation."
codebook$Coding_instruction[codebook$Variable == "bayes"] <- "https://github.com/META-CMP/data/issues/110"

# Update of "dsge"
codebook$Description[codebook$Variable == "dsge"] <- "Indicates if a dynamic stochastic general equilibrium (DSGE) model is used for estimation."
codebook$Coding_instruction[codebook$Variable == "dsge"] <- "https://github.com/META-CMP/data/issues/32"

# Update of "varother"
codebook$Description[codebook$Variable == "varother"] <- "Indicates if an other VAR approach is used for estimation."
codebook$Coding_instruction[codebook$Variable == "varother"] <- "https://github.com/META-CMP/data/issues/111"

# Update of "lor"
codebook$Description[codebook$Variable == "lor"] <- "Indicates that the effect is estimated for a 'downswing' or 'recession' regime."
codebook$Coding_instruction[codebook$Variable == "lor"] <- "https://github.com/META-CMP/data/issues/196"

# Update of "upr"
codebook$Description[codebook$Variable == "upr"] <- "Indicates that the effect is estimated for an 'upswing' or 'boom' regime."
codebook$Coding_instruction[codebook$Variable == "upr"] <- "https://github.com/META-CMP/data/issues/197"

# Update of "scr"
codebook$Description[codebook$Variable == "scr"] <- "Indicates that the effect is estimated for a supply-constrained regime."
codebook$Coding_instruction[codebook$Variable == "scr"] <- "https://github.com/META-CMP/data/issues/198"

# Update of "dcr"
codebook$Description[codebook$Variable == "dcr"] <- "Indicates that the effect is estimated for a demand-constrained regime."
codebook$Coding_instruction[codebook$Variable == "dcr"] <- "https://github.com/META-CMP/data/issues/199"

# Update of "hike"
codebook$Description[codebook$Variable == "hike"] <- "Indicates that the effect is estimated for an interest rate hike only."
codebook$Coding_instruction[codebook$Variable == "hike"] <- "https://github.com/META-CMP/data/issues/200"

# Update of "cut"
codebook$Description[codebook$Variable == "cut"] <- "Indicates that the effect is estimated for an interest rate cut only."
codebook$Coding_instruction[codebook$Variable == "cut"] <- "https://github.com/META-CMP/data/issues/201"

# Update of "annual"
codebook$Description[codebook$Variable == "annual"] <- "Annual data is used for effect estimation."
codebook$Coding_instruction[codebook$Variable == "annual"] <- "https://github.com/META-CMP/data/issues/15"

# Update of "quarter"
codebook$Description[codebook$Variable == "quarter"] <- "Quarterly data is used for effect estimation."
codebook$Coding_instruction[codebook$Variable == "quarter"] <- "https://github.com/META-CMP/data/issues/15"

# Update of "month"
codebook$Description[codebook$Variable == "month"] <- "Monthly data is used for effect estimation."
codebook$Coding_instruction[codebook$Variable == "month"] <- "https://github.com/META-CMP/data/issues/15"

# Update of "list_of_countries"
codebook$Description[codebook$Variable == "list_of_countries"] <- "ISO 2 codes of the countries for which the effect is estimated."
codebook$Coding_instruction[codebook$Variable == "list_of_countries"] <- "https://github.com/META-CMP/data/issues/65"
codebook$Entries[codebook$Variable == "list_of_countries"] <- "See coding_instruction"

# Update of "n_of_countries"
codebook$Description[codebook$Variable == "n_of_countries"] <- "Number of countries used in the estimation sample of the model."
codebook$Coding_instruction[codebook$Variable == "n_of_countries"] <- "https://github.com/META-CMP/data/issues/113"
codebook$Entries[codebook$Variable == "n_of_countries"] <- "See coding_instruction"

# Update of "panel"
codebook$Description[codebook$Variable == "panel"] <- "Indicates if a panel data method (e.g. panel VAR) is used for estimation."
codebook$Coding_instruction[codebook$Variable == "panel"] <- "https://github.com/META-CMP/data/issues/66"

# Update of "start"
codebook$Description[codebook$Variable == "start"] <- "Start date of the sample that is used for estimation."
codebook$Coding_instruction[codebook$Variable == "start"] <- "https://github.com/META-CMP/data/issues/42"
codebook$Entries[codebook$Variable == "start"] <- "See coding_instruction"

# Update of "end"
codebook$Description[codebook$Variable == "end"] <- "End date of the sample that is used for estimation."
codebook$Coding_instruction[codebook$Variable == "end"] <- "https://github.com/META-CMP/data/issues/42"
codebook$Entries[codebook$Variable == "end"] <- "See coding_instruction"

# Update of "samplesize"
codebook$Description[codebook$Variable == "samplesize"] <- "Sample size for estimation."
codebook$Coding_instruction[codebook$Variable == "samplesize"] <- "https://github.com/META-CMP/data/issues/22"
codebook$Entries[codebook$Variable == "samplesize"] <- "See coding_instruction"

# Update of "comprice"
codebook$Description[codebook$Variable == "comprice"] <- "Indicates that a commodity price variable is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "comprice"] <- "https://github.com/META-CMP/data/issues/88"

# Update of "outpgap"
codebook$Description[codebook$Variable == "outpgap"] <- "Indicates that a measure for the output gap is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "outpgap"] <- "https://github.com/META-CMP/data/issues/114"

# Update of "find"
codebook$Description[codebook$Variable == "find"] <- "Indicates that a variable for financial development is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "find"] <- "https://github.com/META-CMP/data/issues/54"

# Update of "eglob"
codebook$Description[codebook$Variable == "eglob"] <- "Indicates that a measure of economic globalisation is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "eglob"] <- "https://github.com/META-CMP/data/issues/55"

# Update of "cbind"
codebook$Description[codebook$Variable == "cbind"] <- "Indicates that central bank independence is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "cbind"] <- "https://github.com/META-CMP/data/issues/115"

# Update of "fexch"
codebook$Description[codebook$Variable == "fexch"] <- "Indicates that the exchange rate regime is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "fexch"] <- "https://github.com/META-CMP/data/issues/116"

# Update of "inflexp"
codebook$Description[codebook$Variable == "inflexp"] <- "Indicates that inflation expectations are included as a control variable."
codebook$Coding_instruction[codebook$Variable == "inflexp"] <- "https://github.com/META-CMP/data/issues/117"

# Update of "foreignir"
codebook$Description[codebook$Variable == "foreignir"] <- "Indicates that a foreign interest rate is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "foreignir"] <- "https://github.com/META-CMP/data/issues/89"

# Update of "fx"
codebook$Description[codebook$Variable == "fx"] <- "Indicates that a exchange rate is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "fx"] <- "https://github.com/META-CMP/data/issues/118"

# Update of "lrir"
codebook$Description[codebook$Variable == "lrir"] <- "Indicates that a long-run interest rate is included as a control variable."
codebook$Coding_instruction[codebook$Variable == "lrir"] <- "https://github.com/META-CMP/data/issues/119"

# Update of "pure_rate_shock"
codebook$Description[codebook$Variable == "pure_rate_shock"] <- "Indicates that the monetary policy shock is explicitly defined in terms of the interest rate."
codebook$Coding_instruction[codebook$Variable == "pure_rate_shock"] <- "https://github.com/META-CMP/data/issues/70" 

# Update of "convent"
codebook$Description[codebook$Variable == "convent"] <- "Indicates that the monetary policy shock is explicitly defined in terms of a conventional monetary policy shock."
codebook$Coding_instruction[codebook$Variable == "convent"] <- "https://github.com/META-CMP/data/issues/80"
codebook$Entries[codebook$Variable == "convent"] <- "true; false"

# Update of "decomposition"
codebook$Description[codebook$Variable == "decomposition"] <- "Indicates that the monetary policy shocks have been decomposed into different components."
codebook$Coding_instruction[codebook$Variable == "decomposition"] <- "https://github.com/META-CMP/data/issues/85"

# Update of "cbanker"
codebook$Description[codebook$Variable == "cbanker"] <- "Indicates that there is an institutional affiliation of one of the authors and/or the publication format with central banks."
codebook$Coding_instruction[codebook$Variable == "cbanker"] <- "https://github.com/META-CMP/data/issues/58"

# Update of "ext_fexch"
codebook$Description[codebook$Variable == "ext_fexch"] <- "Indicates the exchange rate regime of the country for which the model is estimated."
codebook$Type[codebook$Variable == "ext_fexch"] <- "factor"

# Update of "ext_infl"
codebook$Type[codebook$Variable == "ext_infl"] <- "numeric"

# Update of "ext_inflvar"
codebook$Type[codebook$Variable == "ext_inflvar"] <- "numeric"

# Update of "ext_topen"
codebook$Type[codebook$Variable == "ext_topen"] <- "numeric"

# Update of "ext_fopen"
codebook$Type[codebook$Variable == "ext_fopen"] <- "numeric"

# Update of "ext_gdppc"
codebook$Type[codebook$Variable == "ext_gdppc"] <- "numeric"

# Update of "ext_fdev"
codebook$Type[codebook$Variable == "ext_fdev"] <- "numeric"

# Update of "ext_cbind"
codebook$Type[codebook$Variable == "ext_cbind"] <- "numeric"

# Update of "ext_unem"
codebook$Type[codebook$Variable == "ext_unem"] <- "numeric"

# Update of "ext_supply"
codebook$Type[codebook$Variable == "ext_supply"] <- "numeric"

# Update of "citat"
codebook$Entries[codebook$Variable == "citat"] <- NA

# Update of "impact"
codebook$Entries[codebook$Variable == "impact"] <- NA

# Delete row where Variable is "snowball"
codebook <- codebook[codebook$Variable != "snowball", ]
  
# Update of "reason_for_exclusion"
codebook$Entries[codebook$Variable == "reason_for_exclusion"] <- "See coding_instruction"
codebook$Coding_instruction[codebook$Variable == "reason_for_exclusion"] <- "https://github.com/META-CMP/data/issues/37"

# Save 
write_csv(codebook, "codebook.csv")


