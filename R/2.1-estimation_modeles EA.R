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

source("R/Z - Fonctions.R",encoding = "UTF-8")

dataUE <- readRDS("data/data_UE.RDS")
dataUE <- na.omit(dataUE[,c("EURIBOR_3M", "lGDP","dlGDP",
                            "U", "HICP", "underinf")])
dataUE <- window(dataUE, end = c(2018,4))

p <- (plot_ts(dataUE, "EURIBOR_3M") +
          plot_ts(dataUE, "dlGDP"))/(
              plot_ts(dataUE, "U") + 
                  plot_ts(dataUE, "HICP") + 
                  plot_ts(dataUE, "underinf")
              )
p & theme_minimal()

var_ordering1 = c("dlGDP",
                 "U", "underinf","HICP", "EURIBOR_3M")
var_ordering2 = c("EURIBOR_3M", "dlGDP",
                 "U", "underinf","HICP")
# var_retained = c("lGDP_detrend",# "lfbcf",
#                  "HICP",
#                  "U", "underinf", "EURIBOR_3M")
# var_retained = c("lGDP_detrend","U_detrend",
#                  "EURIBOR_3M_detrend","HICP_detrend", "underinf_detrend")
#Select AIC-suggested lag
lagselect <-VARselect(dataUE[,var_ordering1],
                      lag.max=6,type="const")
# Tous les indicateurs suggèrent de retenir 2 lags
lagselect
p_retenu = 2

# L'ordre des variables n'aura pas d'impact sur les analyses du VAR
# mais uniquement dans la spécification du sVAR
model <- VAR(dataUE[,var_ordering1],
           p = p_retenu,type = "both")
model2 <- VAR(dataUE[,var_ordering2],
             p = p_retenu,type = "both")
# Pas d'autocorrélation dans les résidus
serial.test(model)

# Pas d'hétéroscédasticité dans les résidus
arch.test(model)
# Ni de problème de stabilité dans les coefficients
plot(stability(model))

# Pour récupérer le code latex du VAR :
cat(latexify_var(model,align = T, nb_dec = 2))
cat(latexify_mat(var(residuals(model)), nb_dec = 5))


Bmat_chol <- diag(nrow = 5)
Bmat_chol[2,1] <- Bmat_chol[3,1:2] <- 
    Bmat_chol[4,(1:3)] <- Bmat_chol[5,1:4] <- 
    diag(Bmat_chol) <- NA
Bmat_chol # Choleski

Bmat <- diag(nrow = 5)
Bmat[2,1] <- Bmat[3,c(1)] <- 
    Bmat[4,c(1, 3)] <- Bmat[5,1:4] <- 
    diag(Bmat) <- NA
Bmat # affiné

smodel1_chol <- SVAR(model,Bmat = Bmat_chol)
smodel1 <- SVAR(model,Bmat = Bmat) 
smodel1_chol$B # coefficients imposés à 0 proches de 0

smodel2 <- SVAR(model2,Bmat = Bmat_chol)

smodel1_bq <- BQ(model)
smodel2_bq <- BQ(model2)
irf_1 <- irf(smodel1, impulse = "EURIBOR_3M",
           n.ahead = 20)
irf_1_chol <- irf(smodel1_chol, impulse = "EURIBOR_3M",
             n.ahead = 20)
irf_1_bq <- irf(smodel1_bq, impulse = "EURIBOR_3M",
                  n.ahead = 20)

irf_2 <- irf(smodel2, impulse = "EURIBOR_3M",
             n.ahead = 20)
irf_2_bq <- irf(smodel2_bq, impulse = "EURIBOR_3M",
                n.ahead = 20)

plot_irf(irf_1_chol) + ggtitle("Choleski - supply") 
plot_irf(irf_1) + ggtitle("Matrice affinée - supply") # Quasiment même résultat

plot_irf(irf_1_bq) + ggtitle("Blanchard Quah decomposition- supply") 

plot_irf(irf_2) + ggtitle("Choleski - demand")
plot_irf(irf_2_bq) + ggtitle("Blanchard Quah decomposition - demand")

# decomposition de la variance
fevd <- fevd(smodel1, n.ahead = 20)
plot_fevd(fevd)



