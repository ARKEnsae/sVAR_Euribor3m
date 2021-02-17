#download packages

clean_eurostat_cache()

install.packages("package:ggplot2")
install.packages("ecb")
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

#grab the data

##EURIBOR_3M

euri<-get_eurostat(
  "irt_st_q",
  time_format="date",
  filters=list(geo="EA",int_rt="IRT_M3",sinceTimePeriod="1997Q1")
)
euribor <- ts(euri$values,start=c(str_sub(euri$time[1],1,4),1,1), freq=4)

##GDP

gdp<-get_eurostat(
  "namq_10_gdp",
  time_format="date",
  filters=list(geo="EA", s_adj="NSA", na_item="B1GQ", unit="CLV10_MEUR",sinceTimePeriod = "1997Q1")
)
lgdp<-log(ts(gdp$values,start=c(str_sub(gdp$time[1],1,4),1,1), freq=4))

##unemployment

unem<-get_eurostat(
  "une_rt_q",
  time_format="date",
  filters=list(age = "Y15-74", geo="EA19",sex="T",s_adj = "NSA", unit="PC_ACT",sinceTimePeriod = "1997Q1")
)
unemp<-ts(unem$values,start=c(str_sub(unem$time[1],1,4),1,1), freq=4)

##inflation and underlying inflation (From ECB database)

hicp <-get_data("ICP.M.U2.N.000000.4.ANR",
                filter = list(startPeriod ="1997-01",endPeriod="2020-12")
)

infex <-get_data("ICP.M.U2.N.XEF000.4.ANR",
                filter = list(startPeriod ="1997-01",endPeriod="2020-12")
)
long<-nrow(infex)/3

###conversion of monthly inflation rates into quarterly inflation rates

hicpq<-matrix(0,long,1)
for (v in (1:long)){
 hicpq[v] = mean(hicp$obsvalue[3*v],hicp$obsvalue[3*v-1],hicp$obsvalue[3*v-2])
}
hicpq<-hicpq[-long,]
hicpq<-ts(hicpq,start=c(str_sub(hicp$obstime[1],1,4),1,1), freq=4)

infexq<-matrix(0,long,1)
for (v in (1:long)){
  infexq[v] = mean(infex$obsvalue[3*v],infex$obsvalue[3*v-1],infex$obsvalue[3*v-2])
}
infexq<-infexq[-long,]
infexq<-ts(infexq,start=c(str_sub(infex$obstime[1],1,4),1,1), freq=4)

#Put the variables together

matrix<-cbind(euribor,lgdp,unemp,hicpq,infexq)
matrix<-matrix[-long,]
colnames(matrix)<-cbind("GDP","EURIBOR_3M","unemployment","inflation","underinf")

#Lag selection

lagselect <-VARselect(matrix,lag.max=12,type="both")
lag<-min(lagselect$selection)

#Model

model<-VAR(matrix, p = lag, type = "both")
a.mat <- diag(5)
for (i in (1:5)){
for (v in (1:i)){
  a.mat[i,v]<-NA
}
}
SVARmodel <- SVAR(model,Amat=a.mat, estmethod = c("scoring","direct"))

#Impulse response

##response of Unemployment to EURIBOR

SVARimp1 <- irf(SVARmodel,impulse="EURIBOR_3M",response="GDP")

##response of GDP to EURIBOR

SVARimp2 <- irf(SVARmodel,impulse="EURIBOR_3M",response="unemployment")

##response of unemployment to EURIBOR

SVARimp3 <- irf(SVARmodel,impulse="EURIBOR_3M",response="inflation")

##response of inflation to EURIBOR

SVARimp4 <- irf(SVARmodel,impulse="EURIBOR_3M",response="underinf")

#draw plots

par(mfrow=c(2,2))
plot(SVARimp1)
plot(SVARimp2)
plot(SVARimp3)
plot(SVARimp4)

