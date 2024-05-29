library(here)
library(countrycode)

#trade globalisation
koftradegl <- fread(here("~/data/data/merge_external_data/Trade globalisation KOF.csv"), check.names = FALSE, header=TRUE)
#koftradegl <- melt(koftradegl, id.var="ccode")
#koftradegl$ccode <- countrycode(koftradegl$Country, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
#koftradegl <- select(koftradegl, ccode, variable, value)
colnames(koftradegl) <- c('ccode', 'year', 'tradeglobalisation')

#financial globalisation
koffinancialgl <- fread(here("~/data/data/merge_external_data/Financial globalisation KOF.csv"), check.names = FALSE, header=TRUE)
#koffinancialgl <- melt(koffinancialgl, id.var="ccode")
#koffinancialgl$ccode <- countrycode(koffinancialgl$Country, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
#koffinancialgl <- select(koffinancialgl, ccode, variable, value)
colnames(koffinancialgl) <- c('ccode', 'year', 'financialglobalisation')

#inflation
inflation <- fread(here("~/data/data/merge_external_data/Inflation World Bank.csv"), check.names = FALSE, header=TRUE)
inflation <- melt(inflation, id.var="ccode")
#inflation$ccode <- countrycode(inflation$Country, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
#inflation <- select(inflation, ccode, variable, value)
colnames(inflation) <- c('ccode', 'year', 'inflation')

koftradegl$year <- as.character(koftradegl$year)
inflation$year <- as.character(inflation$year)

data_merged <- left_join(koftradegl, inflation, by=c("ccode" = "ccode", "year" = "year"))

koffinancialgl$year <- as.character(koffinancialgl$year)

data_merged <- left_join(data_merged, koffinancialgl, by=c("ccode" = "ccode", "year" = "year"))

#financial development (domestic credit to private sector)
domesticcredit <- fread(here("~/data/data/merge_external_data/Domestic credit World Bank.csv"), check.names = FALSE, header=TRUE)
domesticcredit <- melt(domesticcredit, id.var="ccode")
#domesticcredit$ccode <- countrycode(domesticcredit$Country, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
#domesticcredit <- select(domesticcredit, ccode, variable, value)
colnames(domesticcredit) <- c('ccode', 'year', 'domesticcredit')

domesticcredit$year <- as.character(domesticcredit$year)

data_merged <- left_join(data_merged, domesticcredit, by=c("ccode" = "ccode", "year" = "year"))

#central bank independence
centralbankindependence <- fread(here("~/data/data/merge_external_data/Central bank independence Romelli.csv"), check.names = FALSE, header=TRUE)
#centralbankindependence <- melt(centralbankindependence, id.var="ccode")
#centralbankindependence$ccode <- countrycode(centralbankindependence$Country, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
#centralbankindependence <- select(centralbankindependence, ccode, variable, value)
colnames(centralbankindependence) <- c('year', 'ccode', 'centralbankindependence')

centralbankindependence$year <- as.character(centralbankindependence$year)
#centralbankindependence$ccode <- as.character(centralbankindependence$ccode)
#typeof(data_merged$ccode)

data_merged <- left_join(data_merged, centralbankindependence, by=c("ccode" = "ccode", "year" = "year"))

#GDPpc
GDPpc <- fread(here("~/data/data/merge_external_data/GDP per capita World Bank.csv"), check.names = FALSE, header=TRUE)
GDPpc <- melt(GDPpc, id.var="ccode")
#GDPpc$ccode <- countrycode(GDPpc$Country, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
#GDPpc <- select(GDPpc, ccode, variable, value)
colnames(GDPpc) <- c('ccode', 'year', 'GDPpc')

GDPpc$year <- as.character(GDPpc$year)
data_merged <- left_join(data_merged, GDPpc, by=c("ccode" = "ccode", "year" = "year"))

#Exchange rate regimes
Exchangerate <- fread(here("~/data/data/merge_external_data/Exchange rate data.csv"), check.names = FALSE, header=TRUE)
Exchangerate <- melt(Exchangerate, id.var="Year")
Exchangerate$ccode <- countrycode(Exchangerate$variable, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)
Exchangerate <- select(Exchangerate, ccode, Year, value)
colnames(Exchangerate) <- c('ccode', 'year', 'exchangerate')
Exchangerate$month <- rep(1:12, 15520)

typeof(Exchangerate$exchangerate)
Exchangerate$exchangerate <- as.double(Exchangerate$exchangerate)
Exchangerate$exchangerate <- as.numeric(Exchangerate$exchangerate)
typeof(Exchangerate$exchangerate)

#Exchangerate$ccode <- as.double(Exchangerate$ccode)
mean(Exchangerate$exchangerate, na.rm=TRUE)
AllExchangerate <- Exchangerate %>% 
  dplyr::group_by(ccode, year) %>% 
  dplyr::summarize(exratearr = mean(exchangerate, na.rm=TRUE))
colnames(AllExchangerate)
AllExchangerate$year <- as.character(AllExchangerate$year)

data_merged <- left_join(data_merged, AllExchangerate, by=c("ccode" = "ccode", "year" = "year"))

colnames(data_merged)<-c('ccode', 'year', 'tradegl', 'infl', 'fingl', 'findev', 'cbi', 'gdppc', 'exrate')


data_merged$ccode <- countrycode(data_merged$ccode, 'iso3c', 'iso2c') 

#getwd()
#write.csv(data_merged, file = "~/data/data/merge_external_data/external-data.csv")
