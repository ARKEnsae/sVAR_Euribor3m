# clean_eurostat_cache()
# install.packages("package:ggplot2")
# install.packages("ecb")
# install.packages("eurostat")
# install.packages("mFilter")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("tidyverse")
# install.packages("TSstudio")
# installed.packages("urca")
# installed.packages("vars")

#load packages

# library(ecb)
# library(eurostat)
# library(urca)
library(vars)
# library(mFilter)
# library(tseries)
# library(TSstudio)
# library(forecast)
# library(tidyverse)

matrix <- readRDS("data/data.RDS")
matrix <- na.omit(matrix)

#Select AIC-suggested lag#

lagselect <-VARselect(matrix,lag.max=12,type="both")
lagselect$selection
p_retenu = 2
model<-VAR(matrix, p=p_retenu,type = "const")

###Forecast Error Impulse Response###

forimp <- irf(model, impulse = "EURIBOR_3M",
           response = c("unemployment","dlGDP","inflation","underinf"),
           n.ahead = 8, ortho = FALSE, runs = 1000)
plot(forimp,plot.type="multiple",
     mar.multi = c(.5, 4, .5, 4))

#response of Unemployment to EURIBOR#

forimp1 <- irf(model, impulse = "EURIBOR_3M", response = "unemployment",
               n.ahead = 8, ortho = FALSE, runs = 1000)

#response of dlGDP to EURIBOR#

forimp2 <- irf(model, impulse = "EURIBOR_3M", response = "dlGDP",
               n.ahead = 8, ortho = FALSE, runs = 1000)

#response of inflation to EURIBOR#

forimp3 <- irf(model, impulse = "EURIBOR_3M", response = "inflation",
               n.ahead = 8, ortho = FALSE, runs = 1000)

#response of underlying inflation to EURIBOR#

forimp4 <- irf(model, impulse = "EURIBOR_3M", response = "underinf",
               n.ahead = 8, ortho = FALSE, runs = 1000)

#draw plots

par(mfrow=c(2,2))
plot(forimp1)
plot(forimp2)
plot(forimp3)
plot(forimp4)

###Orthogonal Impulse Response###
oir <- irf(model, impulse = "EURIBOR_3M",
           response = c("unemployment","dlGDP","inflation","underinf"),
           n.ahead = 8, ortho = TRUE, runs = 1000)
plot(oir,plot.type="multiple",
     mar.multi = c(.5, 4, .5, 4))
#response of Unemployment to EURIBOR#

oir1 <- irf(model, impulse = "EURIBOR_3M", response = "unemployment",
            n.ahead = 8, ortho = TRUE, runs = 1000)

#response of dlGDP to EURIBOR#

oir2 <- irf(model, impulse = "EURIBOR_3M", response = "dlGDP",
            n.ahead = 8, ortho = TRUE, runs = 1000)

#response of inflation to EURIBOR#

oir3 <- irf(model, impulse = "EURIBOR_3M", response = "inflation",
            n.ahead = 8, ortho = TRUE, runs = 1000)

#response of underlying inflation to EURIBOR#

oir4 <- irf(model, impulse = "EURIBOR_3M", response = "underinf",
            n.ahead = 8, ortho = TRUE, runs = 1000)


#draw plots
plot(oir,plot.type="single")

par(mfrow=c(2,2))
plot(oir1,plot.type = "single")
plot(oir2,plot.type = "single")
plot(oir3,plot.type = "single")
plot(oir4,plot.type = "single")
??vars:::plot.varirf
