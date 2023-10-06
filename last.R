
## LIBRARIES 

library("haven")
library("readxl")
library(jtools)
library("dplyr")
library("plm")
library("psych")
library("openxlsx")
library("texreg")
library("stargazer")

## Data
final <- read_excel("~/1. Sustainability/Data/final.xlsx")
summary(final)


## Summary



## Panel data 

final<-pdata.frame(final, index=c("Ccode", "Year"), row.names = FALSE)

calculating_lags <- function(var1, lag) {
  class(var1)
  lag1<-lag(var1,k=lag)
}

eps<-final$EPS
market<-final$market
nonmarket<-final$nonmarket
technlogy<-final$technlogy

lag1<-calculating_lags(eps, 1)
summary(lag1)
final<-mutate(final, eps_1=lag1)
lag1<-calculating_lags(market, 1)
final<-mutate(final, market_1=lag1)
lag1<-calculating_lags(nonmarket, 1)
final<-mutate(final, nonmarket_1=lag1)
lag1<-calculating_lags(technlogy, 1)
final<-mutate(final, technology_1=lag1)

## LAGS 2 
lag1<-calculating_lags(eps, 2)
final<-mutate(final, eps_2=lag1)
lag1<-calculating_lags(market, 2)
final<-mutate(final, market_2=lag1)
lag1<-calculating_lags(nonmarket, 2)
final<-mutate(final, nonmarket_2=lag1)
lag1<-calculating_lags(technlogy, 2)
lag1<-final<-mutate(final, technology_2=lag1)

## LAGS 3

lag1<-calculating_lags(eps, 3)
final<-mutate(final, eps_3=lag1)
lag1<-calculating_lags(market, 3)
final<-mutate(final, market_3=lag1)
lag1<-calculating_lags(nonmarket, 3)
final<-mutate(final, nonmarket_3=lag1)
lag1<-calculating_lags(technlogy, 3)
final<-mutate(final, technology_3=lag1)

## LAGS 4

lag1<-calculating_lags(eps, 4)
final<-mutate(final, eps_4=lag1)
lag1<-calculating_lags(market, 4)
final<-mutate(final, market_4=lag1)
lag1<-calculating_lags(nonmarket, 4)
final<-mutate(final, nonmarket_4=lag1)
lag1<-calculating_lags(technlogy, 4)
final<-mutate(final, technology_4=lag1)

## LAGS 5

lag1<-calculating_lags(eps, 5)
final<-mutate(final, eps_5=lag1)
lag1<-calculating_lags(market, 5)
final<-mutate(final, market_5=lag1)
lag1<-calculating_lags(nonmarket, 5)
final<-mutate(final, nonmarket_5=lag1)
lag1<-calculating_lags(technlogy, 5)
final<-mutate(final, technology_5=lag1)

ankhaa<-describe(final)
print(ankhaa)
oecd<-filter(final, oecd==1)
describe(oecd)
dim(oecd)
dim(final)
de
#write.xlsx(ankhaa,"~/1. Sustainability/Data/result.xlsx",  asTable= TRUE, sheetName="decrdiptdsfsdfsdive", rowNames=TRUE)
final<-mutate(final, gdp=log(gdp))
final<-mutate(final, RD=log(RD))
final<-mutate(final, govRD=log(govRD))
## Estimation
#corr 
final<-mutate(final, Triadic=as.numeric(Triadic))
final<-mutate(final, nworkers=as.numeric(nworkers))
cor(final[c("market", "nonmarket", "technlogy", "EPS", "gdp", "stock",   "importGDP", "exportGDP", "govRD", "nworkers")]) 

# RD
fixed_rd1<-plm(RD~eps, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_rd1)
random_rd1<-plm(RD~eps, data=final, index=c("Ccode", "Year"), model="random")
summary(random_rd1)
fixed_rd2<-plm(RD~eps+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_rd2)
random_rd2<-plm(RD~eps+gdp+stock, data=final, index=c("Ccode", "Year"), model="random")
summary(random_rd2)
fixed_rd3<-plm(RD~eps+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_rd3)
fixed_rd4<-plm(RD~eps+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_rd4)
fixed_rd5<-plm(RD~eps+gdp+stock+exportGDP+nworkers+govRD+eps_1+eps_2+eps_3, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_rd5)
phtest(fixed_rd2, random_rd2)
phtest(fixed_rd1, random_rd1)
# patent
fixed_Triadic1<-plm(Triadic~eps, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_Triadic1)
random_Triadic1<-plm(Triadic~eps, data=final, index=c("Ccode", "Year"), model="random")
summary(random_Triadic1)
fixed_Triadic2<-plm(Triadic~eps+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_Triadic2)
fixed_Triadic3<-plm(Triadic~eps+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_Triadic3)
fixed_Triadic4<-plm(Triadic~eps+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_Triadic4)
random_Triadic4<-plm(Triadic~eps+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="random")
summary(random_Triadic4)
fixed_Triadic5<-plm(Triadic~eps+gdp+stock+exportGDP+nworkers+govRD+eps_1+eps_2+eps_3 +eps_4 +eps_5, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_Triadic5)

phtest(fixed_Triadic4, random_Triadic4)
phtest(fixed_Triadic1, random_Triadic1)

# etotal

fixed_etotal1<-plm(etotal~eps, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_etotal1)
fixed_etotal2<-plm(etotal~eps+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_etotal2)
fixed_etotal3<-plm(etotal~eps+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_etotal3)
fixed_etotal4<-plm(etotal~eps+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_etotal4)
fixed_etotal5<-plm(etotal~eps+gdp+stock+exportGDP+nworkers+govRD+eps_1+eps_2+eps_3+eps_4+eps_5, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_etotal5)
#Exporting restults
stargazer(fixed_rd1, fixed_rd2, fixed_rd3, fixed_rd4, fixed_rd5,df=FALSE, digits=2)

stargazer(fixed_Triadic1, fixed_Triadic2, fixed_Triadic3, fixed_Triadic4, df=FALSE, digits=1)
stargazer(fixed_Triadic5, df=FALSE, digits=1)
stargazer(fixed_etotal1, fixed_etotal2, fixed_etotal3, fixed_etotal4,df=FALSE, digits=1)
stargazer(fixed_etotal5,df=FALSE, digits=1)
#### NARROW 

# RD

narrow_rd1<-plm(RD~market+nonmarket, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_rd1)
narrow_rd2<-plm(RD~market+nonmarket+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_rd2)
narrow_rd3<-plm(RD~market+nonmarket+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_rd3)
narrow_rd4<-plm(RD~market+nonmarket+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_rd4)
narrow_rd5<-plm(RD~market+nonmarket+gdp+stock+exportGDP+nworkers+govRD+market_1+market_2+market_3+nonmarket_1+nonmarket_2+nonmarket_3, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_rd5)



phtest(fixed_rdn, random_rdn)
# patent

narrow_Triadic1<-plm(Triadic~market+nonmarket, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_Triadic1)
narrow_Triadic2<-plm(Triadic~market+nonmarket+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_Triadic2)
narrow_Triadic3<-plm(Triadic~market+nonmarket+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_Triadic3)
narrow_Triadic4<-plm(Triadic~market+nonmarket+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_Triadic4)
narrow_Triadic5<-plm(Triadic~market+nonmarket+gdp+stock+exportGDP+nworkers+govRD+market_1+market_2+market_3+nonmarket_1+nonmarket_2+nonmarket_3, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_Triadic5)





# etotal

narrow_etotal1<-plm(etotal~market+nonmarket, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_etotal1)
narrow_etotal2<-plm(etotal~market+nonmarket+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_etotal2)
narrow_etotal3<-plm(etotal~market+nonmarket+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_etotal3)
narrow_etotal4<-plm(etotal~market+nonmarket+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_etotal4)
narrow_etotal5<-plm(etotal~market+nonmarket+gdp+stock+exportGDP+nworkers+govRD+market_1+market_2+market_3+nonmarket_1+nonmarket_2+nonmarket_3, data=final, index=c("Ccode", "Year"), model="within")
summary(narrow_etotal5)
#exportig result
stargazer(narrow_rd1, narrow_rd2, narrow_rd3, narrow_rd4, narrow_rd5,df=FALSE, digits=1)

stargazer(narrow_Triadic1, narrow_Triadic2, narrow_Triadic3, narrow_Triadic4, df=FALSE, digits=1)
stargazer(narrow_Triadic5, df=FALSE, digits=1)
stargazer(narrow_etotal1, narrow_etotal2, narrow_etotal3, narrow_etotal4,df=FALSE, digits=1)
stargazer(narrow_etotal5,df=FALSE, digits=1)
# share of etotal

fixed_share1<-plm(shareofep~eps, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_share1)
fixed_share2<-plm(shareofep~eps+gdp+stock, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_share2)
fixed_share3<-plm(shareofep~eps+gdp+stock+exportGDP, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_share3)
fixed_share4<-plm(shareofep~eps+gdp+stock+exportGDP+nworkers+govRD, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_share4)
fixed_share5<-plm(shareofep~eps+gdp+stock+exportGDP+nworkers+govRD+eps_1+eps_2+eps_3+eps_4, data=final, index=c("Ccode", "Year"), model="within")
summary(fixed_share5)
stargazer(ankhaa)
