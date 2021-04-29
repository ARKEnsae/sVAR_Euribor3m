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
library(mFilter)

source("R/Z - Fonctions.R",encoding = "UTF-8")

data <- readRDS("data/data_UE.RDS")
data <- na.omit(data[,c("EURIBOR_3M", "lGDP","dlGDP",# "lfbcf", 
                            "U", "HICP", "underinf")])
data <- window(data, end = c(2018,4))

data = ts.union(data,
                  hpfilter(data[,"EURIBOR_3M"],freq = 1600)$cycle,
                  hpfilter(data[,"lGDP"],freq = 1600)$cycle,
                  hpfilter(data[,"U"],freq = 1600)$cycle,
                  hpfilter(data[,"HICP"],freq = 1600)$cycle,
                  hpfilter(data[,"underinf"],freq = 1600)$cycle)
colnames(data) <- c("EURIBOR_3M", "lGDP","dlGDP",# "lfbcf", 
                     "U", "HICP", "underinf",
                     paste(c("EURIBOR_3M", "lGDP",# "lfbcf", 
                             "U", "HICP", "underinf"),
                           "detrend",sep="_"))
library(ggfortify)
library(patchwork)
(autoplot(data[,c("EURIBOR_3M",  "EURIBOR_3M_detrend")]) +
autoplot(data[,c("lGDP","dlGDP",  "lGDP_detrend")])
) /
(
autoplot(data[,c("U",  "U_detrend")]) +
autoplot(data[,c("HICP",  "HICP_detrend")]) +
autoplot(data[,c("underinf",  "underinf_detrend")])
)

var_ordering1 = c("dlGDP",
                 "U", "underinf","HICP", "EURIBOR_3M")
var_ordering2 = c("dlGDP",
                 "U", "underinf","HICP", "EURIBOR_3M")
# var_retained = c("lGDP_detrend",# "lfbcf",
#                  "HICP",
#                  "U", "underinf", "EURIBOR_3M")
# var_retained = c("lGDP_detrend","U_detrend",
#                  "EURIBOR_3M_detrend","HICP_detrend", "underinf_detrend")
#Select AIC-suggested lag
lagselect <-VARselect(data[,var_ordering1],
                      lag.max=6,type="const")
# Tous les indicateurs suggèrent de retenir 2 lags
lagselect
p_retenu = 2
model <- VAR(data[,var_ordering1],
           p = p_retenu,type = "both")

Hmisc::rcorr(residuals(model))
# Pas d'autocorrélation dans les résidus
serial.test(model)

# Pas d'hétéroscédasticité dans les résidus
arch.test(model)
plot(stability(model))

# Pour récupérer le code latex du VAR :
cat(latexify_var(model,align = T, nb_dec = 2))


Bmat <- diag(nrow = 5)
Bmat[2,1] <- Bmat[3,1:2] <- 
    Bmat[4,(1:3)] <- Bmat[5,1:4] <- 
    diag(Bmat) <- NA
Bmat <- Bmat[-5,-5]

Bmat <- diag(nrow = 5)
Bmat[2,1] <- Bmat[3,1:2] <- 
    Bmat[4,(1:3)] <- Bmat[5,1:4] <- 
    diag(Bmat) <- NA
Bmat

Bmat2 <- diag(nrow = 5)
Bmat2[2,1] <- Bmat2[3,c(1)] <- 
    Bmat2[4,c(1, 3)] <- Bmat2[5,1:4] <- 
    diag(Bmat2) <- NA
Bmat2

Bmat3 <- diag(nrow = 5)
Bmat3[2,1] <- Bmat3[3,c(3)] <- 
    Bmat3[4,c(3,4)] <- Bmat3[5,1:4] <- 
    diag(Bmat3) <- NA
Bmat3

Bmat4 <- diag(nrow = 5)
Bmat4[2,1] <- Bmat4[3,c(3,4)] <- 
    Bmat4[4,c(3,4)] <- Bmat4[5,1:4] <- 
    diag(Bmat4) <- NA
Bmat4
SVAR(model,Bmat = Bmat4)

smodel1 <- SVAR(model,Bmat = Bmat)
smodel1$B
cat(latexify_mat(smodel1$B,nb_dec = 2))
smodel2 <- SVAR(model,Bmat = Bmat2)
smodel2$B
smodel3 <- SVAR(model,Bmat = Bmat3)

smodel4 <- SVAR(model,Bmat = Bmat4)

smodelbq <- BQ(model)
irf_1 <- irf(smodel1, impulse = "EURIBOR_3M_detrend",
           n.ahead = 20)
irf_2 <- irf(smodel2, impulse = "EURIBOR_3M_detrend",
             n.ahead = 20)
irf_3 <- irf(smodel3, impulse = "EURIBOR_3M_detrend",
             n.ahead = 20)
irf_bq <- irf(smodelbq, impulse = "EURIBOR_3M_detrend",,
             n.ahead = 20)
plot_irf(irf_1) + ggtitle("Choleski") 
plot_irf(irf_2) + ggtitle("Modèle 2") 
plot_irf(irf_3) + ggtitle("Modèle 3") 

plot_irf(irf_bq) + ggtitle("Blanchard Quah") 

