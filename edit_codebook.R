library(readr)
codebook <- read_csv("codebook.csv")

# Lowercase category and variable names (consistent with JSON files)
codebook$Category <- tolower(codebook$Category)
codebook$Variable <- tolower(codebook$Variable)

# Update of description
codebook$Description[codebook$Variable == "inttype"] <- "Type of short term interest rate variable which is shocked."
codebook$Description[codebook$Variable == "size"] <- "Shock size in basis points."
codebook$Description[codebook$Variable == "axis_trans"] <- "Note if the axis scale needs to be transformed to be a percentage scale."
codebook$Description[codebook$Variable == "dep"] <- "Dependent (outcome) variable codes."
codebook$Description[codebook$Variable == "conf"] <- "Level of the confidence bounds or standard errors."
codebook$Description[codebook$Variable == "main"] <- "The outcome variable representing the main research interest of the study."
codebook$Description[codebook$Variable == "prefer"] <- "Indicates if the effect estimate based on the model specification is preferred in the study."
codebook$Description[codebook$Variable == "iv"] <- "Indicates if an instrumental variable approch is used for identification."
codebook$Description[codebook$Variable == "forecast_based"] <- "Indicates whether a forecast-based strategy as in Romer and Romer (2004) is used for shock identification."
codebook$Description[codebook$Variable == "nr"] <- "Indicates if a narrative strategy based on narrative records or sentiments of central bank statements is used for shock identification (e.g. Romer and Romer 1989)."
codebook$Description[codebook$Variable == "event"] <- "Indicates if an event study or an other direct measurement of the monetary policy shock is used for shock identification."
codebook$Description[codebook$Variable == "chol"] <- 'Indicates if a recursive ("Cholesky") ordering is used for shock identification.'
codebook$Description[codebook$Variable == "svar"] <- "Indicates if a non-recursive structural VAR approach is used for shock identification (no Cholesky)."
codebook$Description[codebook$Variable == "signr"] <- "Indicates if a sign restriction approach is used for shock identification."
codebook$Description[codebook$Variable == "hf"] <- "Indicates if a high frequency approach is used for shock identification."
codebook$Description[codebook$Variable == "heteroskedas"] <- "Indicates if a heteroskedasticity-based approach is used for shock identification."
codebook$Description[codebook$Variable == "longrun"] <- "Indicates if shock identification is based on long-run restrictions."
codebook$Description[codebook$Variable == "idother"] <- "Indicates if the identification strategy does not correspond to one of the other variables in identification_strategy."
codebook$Description[codebook$Variable == "var"] <- "Indicates if a vector autoregression model (VAR) is used for estimation."
codebook$Description[codebook$Variable == "lp"] <- "Indicates if a local projections method is used for estimation."
codebook$Description[codebook$Variable == "vecm"] <- "Indicates if a vector error correction model is used for estimation."
codebook$Description[codebook$Variable == "dyn_ols"] <- "Indicates if a dynamic ols model, such as an autoregressive distributed lag model, is used for estimation."
codebook$Description[codebook$Variable == "fvar"] <- "Indicates if a factor-augmented VAR model is used for estimation."
codebook$Description[codebook$Variable == "tvar"] <- "Indicates if a time-varying parameter VAR models is used for estimation."
codebook$Description[codebook$Variable == "gvar"] <- "Indicates if a global VAR model (no panel) is used for estimation."
codebook$Description[codebook$Variable == "bayes"] <- "Indicates if a Bayesian approach is used for estimation."
codebook$Description[codebook$Variable == "dsge"] <- "Indicates if a dynamic stochastic general equilibrium (DSGE) model is used for estimation."
codebook$Description[codebook$Variable == "varother"] <- "Indicates if an other VAR approach is used for estimation."
codebook$Description[codebook$Variable == "lor"] <- "Indicates that the effect is estimated for a 'downswing' or 'recession' regime."
codebook$Description[codebook$Variable == "upr"] <- "Indicates that the effect is estimated for an 'upswing' or 'boom' regime."
codebook$Description[codebook$Variable == "scr"] <- "Indicates that the effect is estimated for a supply-constrained regime."
codebook$Description[codebook$Variable == "dcr"] <- "Indicates that the effect is estimated for a demand-constrained regime."
codebook$Description[codebook$Variable == "hike"] <- "Indicates that the effect is estimated for an interest rate hike only."
codebook$Description[codebook$Variable == "cut"] <- "Indicates that the effect is estimated for an interest rate cut only."
codebook$Description[codebook$Variable == "annual"] <- "Annual data are used for effect estimation."
codebook$Description[codebook$Variable == "quarter"] <- "Quarterly data are used for effect estimation."
codebook$Description[codebook$Variable == "month"] <- "Monthly data are used for effect estimation."
codebook$Description[codebook$Variable == "list_of_countries"] <- "Iso2 codes of the countries for which the effect is estimated."
codebook$Description[codebook$Variable == "panel"] <- "Indicates if a panel data method (e.g. panel VAR) is used for estimation."
codebook$Description[codebook$Variable == "start"] <- "Start date of the sample that is used for estimation."
codebook$Description[codebook$Variable == "end"] <- "End date the sample that is used for estimation."
codebook$Description[codebook$Variable == "samplesize"] <- "Sample size for estimation."
codebook$Description[codebook$Variable == "comprice"] <- "Indicates that a commodity price variable is included as a control variable."
codebook$Description[codebook$Variable == "outpgap"] <- "Indicates that a measure for the output gap is included as a control variable."
codebook$Description[codebook$Variable == "find"] <- "Indicates that a variable for financial development is included as a control variable."
codebook$Description[codebook$Variable == "eglob"] <- "Indicates that a measure of economic globalisation is included as a control variable."
codebook$Description[codebook$Variable == "cbind"] <- "Indicates that central bank independence is included as a control variable."
codebook$Description[codebook$Variable == "fexch"] <- "Indicates that the exchange rate regime is included as a control variable."
codebook$Description[codebook$Variable == "inflexp"] <- "Indicates that inflation expectations are included as a control variable."
codebook$Description[codebook$Variable == "foreignir"] <- "Indicates that a foreign interest rate is included as a control variable."
codebook$Description[codebook$Variable == "fx"] <- "Indicates that a exchange rate is included as a control variable."
codebook$Description[codebook$Variable == "lrir"] <- "Indicates that a long-run interest rate is included as a control variable."
codebook$Description[codebook$Variable == "pure_rate_shock"] <- "Indicates that the monetary policy shock is explicitly defined in terms of the interest rate."
codebook$Description[codebook$Variable == ""] <- ""
codebook$Description[codebook$Variable == ""] <- ""
codebook$Description[codebook$Variable == ""] <- ""
codebook$Description[codebook$Variable == ""] <- ""
codebook$Description[codebook$Variable == ""] <- ""

codebook$Description[codebook$Variable == ""] <- ""

View(codebook[,c("Variable", "Description")])


# Coding instructions
codebook$Coding_instruction <- NA
codebook$Coding_instruction[codebook$Variable == "study_notes"] <- ""
codebook$Coding_instruction[codebook$Variable == "model_notes"] <- ""
codebook$Coding_instruction[codebook$Variable == "figure"] <- ""
codebook$Coding_instruction[codebook$Variable == "page"] <- ""
codebook$Coding_instruction[codebook$Variable == "irf"] <- ""
codebook$Coding_instruction[codebook$Variable == "cum"] <- ""
codebook$Coding_instruction[codebook$Variable == "inttype"] <- "https://github.com/META-CMP/data/issues/6"
codebook$Coding_instruction[codebook$Variable == "size"] <- "https://github.com/META-CMP/data/issues/52"
codebook$Coding_instruction[codebook$Variable == "axis_trans"] <- "https://github.com/META-CMP/data/issues/62"
codebook$Coding_instruction[codebook$Variable == "dep"] <- "https://github.com/META-CMP/data/issues/10"
codebook$Coding_instruction[codebook$Variable == "conf"] <- "https://github.com/META-CMP/data/issues/98"
codebook$Coding_instruction[codebook$Variable == "main"] <- "https://github.com/META-CMP/data/issues/17"
codebook$Coding_instruction[codebook$Variable == "prefer"] <- "https://github.com/META-CMP/data/issues/87"
codebook$Coding_instruction[codebook$Variable == "iv"] <- "https://github.com/META-CMP/data/issues/99"
codebook$Coding_instruction[codebook$Variable == "forecast_based"] <- "https://github.com/META-CMP/data/issues/41"
codebook$Coding_instruction[codebook$Variable == "nr"] <- "https://github.com/META-CMP/data/issues/41"
codebook$Coding_instruction[codebook$Variable == "event"] <- "https://github.com/META-CMP/data/issues/100"
codebook$Coding_instruction[codebook$Variable == "chol"] <- "https://github.com/META-CMP/data/issues/101"
codebook$Coding_instruction[codebook$Variable == "svar"] <- "https://github.com/META-CMP/data/issues/102"
codebook$Coding_instruction[codebook$Variable == "signr"] <- "https://github.com/META-CMP/data/issues/86"
codebook$Coding_instruction[codebook$Variable == "hf"] <- "https://github.com/META-CMP/data/issues/103"
codebook$Coding_instruction[codebook$Variable == "heteroskedas"] <- "https://github.com/META-CMP/data/issues/78"
codebook$Coding_instruction[codebook$Variable == "longrun"] <- "https://github.com/META-CMP/data/issues/104"
codebook$Coding_instruction[codebook$Variable == "idother"] <- "https://github.com/META-CMP/data/issues/69"
codebook$Coding_instruction[codebook$Variable == "var"] <- "https://github.com/META-CMP/data/issues/105"
codebook$Coding_instruction[codebook$Variable == "lp"] <- "https://github.com/META-CMP/data/issues/106"
codebook$Coding_instruction[codebook$Variable == "vecm"] <- "https://github.com/META-CMP/data/issues/107"
codebook$Coding_instruction[codebook$Variable == "dyn_ols"] <- "https://github.com/META-CMP/data/issues/25"
codebook$Coding_instruction[codebook$Variable == "fvar"] <- "https://github.com/META-CMP/data/issues/81"
codebook$Coding_instruction[codebook$Variable == "tvar"] <- "https://github.com/META-CMP/data/issues/108"
codebook$Coding_instruction[codebook$Variable == "gvar"] <- "https://github.com/META-CMP/data/issues/109"
codebook$Coding_instruction[codebook$Variable == "bayes"] <- "https://github.com/META-CMP/data/issues/110"
codebook$Coding_instruction[codebook$Variable == "dsge"] <- "https://github.com/META-CMP/data/issues/32"
codebook$Coding_instruction[codebook$Variable == "varother"] <- "https://github.com/META-CMP/data/issues/111"
codebook$Coding_instruction[codebook$Variable == "lor"] <- "https://github.com/META-CMP/data/issues/196"
codebook$Coding_instruction[codebook$Variable == "upr"] <- "https://github.com/META-CMP/data/issues/197"
codebook$Coding_instruction[codebook$Variable == "scr"] <- "https://github.com/META-CMP/data/issues/198"
codebook$Coding_instruction[codebook$Variable == "dcr"] <- "https://github.com/META-CMP/data/issues/199"
codebook$Coding_instruction[codebook$Variable == "hike"] <- "https://github.com/META-CMP/data/issues/200"
codebook$Coding_instruction[codebook$Variable == "cut"] <- "https://github.com/META-CMP/data/issues/201"
codebook$Coding_instruction[codebook$Variable == "annual"] <- "https://github.com/META-CMP/data/issues/15"
codebook$Coding_instruction[codebook$Variable == "quarter"] <- "https://github.com/META-CMP/data/issues/15"
codebook$Coding_instruction[codebook$Variable == "month"] <- "https://github.com/META-CMP/data/issues/15"
codebook$Coding_instruction[codebook$Variable == "list_of_countries"] <- "https://github.com/META-CMP/data/issues/65"
codebook$Coding_instruction[codebook$Variable == "n_of_countries"] <- "https://github.com/META-CMP/data/issues/113"
codebook$Coding_instruction[codebook$Variable == "panel"] <- "https://github.com/META-CMP/data/issues/66"
codebook$Coding_instruction[codebook$Variable == "start"] <- "https://github.com/META-CMP/data/issues/42"
codebook$Coding_instruction[codebook$Variable == "end"] <- "https://github.com/META-CMP/data/issues/42"
codebook$Coding_instruction[codebook$Variable == "samplesize"] <- "https://github.com/META-CMP/data/issues/22"
codebook$Coding_instruction[codebook$Variable == "comprice"] <- "https://github.com/META-CMP/data/issues/88"
codebook$Coding_instruction[codebook$Variable == "outpgap"] <- "https://github.com/META-CMP/data/issues/114"
codebook$Coding_instruction[codebook$Variable == "find"] <- "https://github.com/META-CMP/data/issues/54"
codebook$Coding_instruction[codebook$Variable == "eglob"] <- "https://github.com/META-CMP/data/issues/55"
codebook$Coding_instruction[codebook$Variable == "cbind"] <- "https://github.com/META-CMP/data/issues/115"
codebook$Coding_instruction[codebook$Variable == "fexch"] <- "https://github.com/META-CMP/data/issues/116"
codebook$Coding_instruction[codebook$Variable == "inflexp"] <- "https://github.com/META-CMP/data/issues/117"
codebook$Coding_instruction[codebook$Variable == "foreignir"] <- "https://github.com/META-CMP/data/issues/89"
codebook$Coding_instruction[codebook$Variable == "fx"] <- "https://github.com/META-CMP/data/issues/118"
codebook$Coding_instruction[codebook$Variable == "lrir"] <- "https://github.com/META-CMP/data/issues/119"
codebook$Coding_instruction[codebook$Variable == "pure_rate_shock"] <- "https://github.com/META-CMP/data/issues/70" 
codebook$Coding_instruction[codebook$Variable == "convent"] <- ""
codebook$Coding_instruction[codebook$Variable == "decomposition"] <- ""

# Update of type

# Update of entries

# Update of references

# Update of Motivation_theory

# Update of json

# Update of source

# Deleting Preference_ME & Preference_FP
codebook$Preference_ME <- NULL
codebook$Preference_FP <- NULL

# Update of hypothesis
