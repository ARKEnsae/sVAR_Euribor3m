############################### PACKAGE DOWNLOADING & LOADING ############################### 

#download packages

clean_eurostat_cache()
install.packages("package:ggplot2")
install.packages("ecb")
install.packages("eurostat")
install.packages("mFilter")
install.packages("tseries")
install.packages("forecast")
install.packages("tidyverse")
install.packages("TSstudio")
installed.packages("urca")
installed.packages("vars")

#load packages

library(ecb)
library(eurostat)
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

############################### DATA GATHERING & CLEANING ###############################  

#EURIBOR_3M#

euri<-get_eurostat(
  "irt_st_q",
  time_format="date",
  filters=list(geo="EA",int_rt="IRT_M3",sinceTimePeriod="2002Q1")
)
euribor <- ts(euri$values[1:24],start=c(str_sub(euri$time[1],1,4),1,1), freq=4)

#GDP#

gdp<-get_eurostat(
  "namq_10_gdp",
  time_format="date",
  filters=list(geo="EA", s_adj="NSA", na_item="B1GQ", unit="CLV10_MEUR",sinceTimePeriod = "2002Q1")
)
lgdp<-log(ts(gdp$values[1:24],start=c(str_sub(gdp$time[1],1,4),1,1), freq=4))

#Unemployment#

unem<-get_eurostat(
  "une_rt_q",
  time_format="date",
  filters=list(age = "Y15-74", geo="EA19",sex="T",s_adj = "NSA", unit="PC_ACT",sinceTimePeriod = "2002Q1")
)
unemp<-ts(unem$values[1:24],start=c(str_sub(unem$time[1],1,4),1,1), freq=4)

#inflation and underlying inflation (From ECB database)#

hicp <-get_data("ICP.M.U2.N.000000.4.ANR",
                filter = list(startPeriod ="2002-01",endPeriod="2007-12")
)

infex <-get_data("ICP.M.U2.N.XEF000.4.ANR",
                 filter = list(startPeriod ="2002-01",endPeriod="2007-12")
)
long<-nrow(infex)/3

#Function: Month to Quarter converter for ECB data#
monthly_to_quarterly <- function(month) {
  quarter<-matrix(0,long,1)
  for (v in (1:long)){
    quarter[v] = mean(month$obsvalue[3*v],month$obsvalue[3*v-1],month$obsvalue[3*v-2])
  }
  quarter<-ts(quarter,start=c(str_sub(hicp$obstime[1],1,4),1,1), freq=4)
  return(quarter)
}

hicpq<-monthly_to_quarterly(hicp)
infexq<-monthly_to_quarterly(infex)

############################### MODEL ###############################  

#Bunch the variables together#

matrix<-cbind(euribor,lgdp,unemp,hicpq,infexq)
colnames(matrix)<-cbind("EURIBOR_3M","GDP","unemployment","inflation","underinf")

#Select AIC-suggested lag#

lagselect <-VARselect(matrix,lag.max=17,type="both")
model<-VAR(matrix, p=lagselect$selection[1],type = "const")

###Forecast Error Impulse Response###

#response of Unemployment to EURIBOR#

forimp1 <- irf(model, impulse = "EURIBOR_3M", response = "unemployment",n.ahead = 8, ortho = FALSE, runs = 1000)

#response of GDP to EURIBOR#

forimp2 <- irf(model, impulse = "EURIBOR_3M", response = "GDP",n.ahead = 8, ortho = FALSE, runs = 1000)

#response of inflation to EURIBOR#

forimp3 <- irf(model, impulse = "EURIBOR_3M", response = "inflation",n.ahead = 8, ortho = FALSE, runs = 1000)

#response of underlying inflation to EURIBOR#

forimp4 <- irf(model, impulse = "EURIBOR_3M", response = "underinf",n.ahead = 8, ortho = FALSE, runs = 1000)

#draw plots

par(mfrow=c(2,2))
plot(forimp1)
plot(forimp2)
plot(forimp3)
plot(forimp4)

###Orthogonal Impulse Response###

#response of Unemployment to EURIBOR#

oir1 <- irf(model, impulse = "EURIBOR_3M", response = "unemployment",n.ahead = 8, ortho = TRUE, runs = 1000)

#response of GDP to EURIBOR#

oir2 <- irf(model, impulse = "EURIBOR_3M", response = "GDP",n.ahead = 8, ortho = TRUE, runs = 1000)

#response of inflation to EURIBOR#

oir3 <- irf(model, impulse = "EURIBOR_3M", response = "inflation",n.ahead = 8, ortho = TRUE, runs = 1000)

#response of underlying inflation to EURIBOR#

oir4 <- irf(model, impulse = "EURIBOR_3M", response = "underinf",n.ahead = 8, ortho = TRUE, runs = 1000)

#draw plots

par(mfrow=c(2,2))
plot(oir1)
plot(oir2)
plot(oir3)
plot(oir4)





