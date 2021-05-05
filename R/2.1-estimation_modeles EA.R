library(vars)

source("R/Z - Fonctions.R",encoding = "UTF-8")

dataUE <- readRDS("data/data_UE.RDS")
dataUE <- na.omit(dataUE[,c("EURIBOR_3M","dlGDP",
                            "U", "HICP", "underinf")])
dataUE <- window(dataUE, end = c(2018,4))

round(sapply(dataUE, stationnarity_test), 3)
# Au seuil de 5 %, lorsque l'on enlève une tendance linéaire on trouve que
# toutes les séries stationnaires selon kpss et selon ADF.
# C'est pourquoi on effectue un VAR avec un modèle contenant constante + 
# tendance linéaire.

p <- (plot_ts(dataUE, "EURIBOR_3M") +
          plot_ts(dataUE, "dlGDP"))/(
              plot_ts(dataUE, "U") + 
                  plot_ts(dataUE, "HICP") + 
                  plot_ts(dataUE, "underinf")
              )
(p & theme_minimal() )+ 
    plot_annotation(title = 'European Area')

var_ordering1 = c("dlGDP",
                 "U", "underinf","HICP", "EURIBOR_3M")
var_ordering2 = c("EURIBOR_3M", "dlGDP",
                 "U", "underinf","HICP")

#Select AIC-suggested lag
lagselect <-VARselect(dataUE[,var_ordering1],
                      lag.max=6,type="const")
# Le critère de Schwartz suggère de ne retenir qu'un lag
lagselect
p_retenu = 1

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
cat(latexify_var(model,align = FALSE, nb_dec = 2, se=TRUE))
cat(latexify_mat(cor(residuals(model)), nb_dec = 2))

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

Bmat2 <- diag(nrow = 5)
Bmat2[2,1] <- Bmat2[3,c(1:2)] <- 
    Bmat2[5,4] <- 
    diag(Bmat2) <- NA
Bmat2 # affiné

smodel1_chol <- SVAR(model,Bmat = Bmat_chol)
smodel1 <- SVAR(model,Bmat = Bmat) 
smodel1_chol$B # coefficients imposés à 0 proches de 0
smodel1$B
cat(latexify_mat(smodel1$B, nb_dec = 3))

smodel2 <- SVAR(model2,Bmat = Bmat_chol)
smodel2
# les coefficients que l'on va imposer à 0 sont estimés à 0
# en utilisant la décomposition de choleski
# Les mêmes résultats sont donc trouvés entre les deux méthodes
smodel2 <- SVAR(model2,Bmat = Bmat2)
smodel2
cat(latexify_mat(smodel2$B, nb_dec = 3))

# Utilisation de Blanchard Quah, non commenté dans le rapport
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

plot_irf(irf_1_chol) + ggtitle("Choleski - GDP-led") 
# Quasiment même résultat comme attendu :
plot_irf(irf_1) + ggtitle("Matrice affinée - GDP-led")

# Non commenté dans le rapport
plot_irf(irf_1_bq) + ggtitle("Blanchard Quah decomposition - GDP-led") 

plot_irf(irf_2) + ggtitle("Matrice affinée - Euribor-led")
# Non commenté dans le rapport
plot_irf(irf_2_bq) + ggtitle("Blanchard Quah decomposition - Euribor-led")

# decomposition de la variance
# non utilisé dans le rapport
fevd <- fevd(smodel1, n.ahead = 20)
plot_fevd(fevd)


saveRDS(model, "data/models_EA/var_model.RDS")
saveRDS(irf_1, "data/models_EA/irf_1")
saveRDS(irf_1_chol, "data/models_EA/irf_1_chol")
saveRDS(irf_1_bq, "data/models_EA/irf_1_bq")
saveRDS(irf_2, "data/models_EA/irf_2")
saveRDS(irf_2_bq, "data/models_EA/irf_2_bq")
