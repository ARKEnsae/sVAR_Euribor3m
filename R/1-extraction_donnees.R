library(ecb)
library(eurostat)
library(zoo)

source("R/Z - Fonctions.R",encoding = "UTF-8")

############################### DATA GATHERING & CLEANING ###############################  

##################################
######### DONNEES UE #############
##################################
##EURIBOR_3M
euri<-get_eurostat(
    "irt_st_q",
    time_format="date",
    filters=list(geo="EA",int_rt="IRT_M3")
)
euribor <- ts(euri$values,start=c(substr(euri$time[1],1,4),1), freq=4)

##GDP

gdp<-get_eurostat(
    "namq_10_gdp",
    time_format="date",
    filters=list(geo="EA", s_adj="SCA", na_item="B1GQ", unit="CLV10_MEUR")
)  # La série désaisonnalisée n'est pas disponible au niveau agrégé
gdp <- log(ts(gdp$values,start=c(substr(gdp$time[1],1,4),1), freq=4))
dlgdp <- diff(gdp)


##unemployment
# Extraction du taux de chômage harmonisé pour les personnes de 15 à 74 ans,
# En ne faisant pas de distinction par sexe et en prenant le pourcentage dans la population active
unem<-get_eurostat(
    "une_rt_q",
    time_format="date",
    filters=list(age = "Y15-74", geo="EA19",sex="T",s_adj = "SA", unit="PC_ACT")
) 
unemp<-ts(unem$values,start=c(substr(unem$time[1],1,4),1), freq=4)

## inflation and underlying inflation (From ECB database)

hicp <-get_data("ICP.M.U2.N.000000.4.INX"
)
hicp<-ts(hicp$obsvalue,
         start=as.numeric(c(substr(hicp$obstime[1],1,4),
                 substr(hicp$obstime[1],6,7))),
         freq=12)
hicpq <- aggregate(as.zoo(hicp), yearqtr, mean)
hicpq <- as.ts(hicpq)
hicpq <- hicpq/lag(hicpq,-4)-1 # glissement annuel

infex <-get_data("ICP.M.U2.N.XEF000.4.INX"
)
infex<-ts(infex$obsvalue,
          start=as.numeric(c(substr(infex$obstime[1],1,4),
                             substr(infex$obstime[1],6,7))),
          freq=12)
infexq <- aggregate(as.zoo(infex), yearqtr, mean)
infexq <- as.ts(infexq)
infexq <- infexq/lag(infexq,-4)-1 # glissement annuel

data <- ts.union(euribor, dlgdp,gdp, unemp, hicpq, infexq)
colnames(data)<-cbind("EURIBOR_3M", "dlGDP","lGDP",
                      "U","HICP","underinf")
saveRDS(data, file="data/data_UE.RDS")

##################################
######### DONNEES FR #############
##################################

# Indice d'inflation sous-jacente - Base 2015 - Ensemble des ménages - 
# France métropolitaine - Ensemble
infex <- lectureBDM("001769686")
infexq <- aggregate(as.zoo(infex), yearqtr, mean)
infexq <- as.ts(infexq)
infexq <- infexq/lag(infexq,-4)-1 # glissement annuel

# Produit intérieur brut total - Volume aux prix de l'année précédente chaînés - 
# Série CVS-CJO aux prix de l'année précédente chaînés - Série CVS-CJO
gdp <- log(lectureBDM("010565708"))
dlgdp <- diff(gdp)

# Taux de chômage au sens du BIT - Ensemble - France métropolitaine - 
# Données CVS
unemp <- lectureBDM("001688526")

hicp <- lectureBDM("001759971")
hicpq <- aggregate(as.zoo(hicp), yearqtr, mean)
hicpq <- as.ts(hicpq)
hicpq <- hicpq/lag(hicpq,-4)-1 # glissement annuel

data <- ts.union(euribor, dlgdp,gdp, unemp, hicpq, infexq)
colnames(data)<-cbind("EURIBOR_3M", "dlGDP","lGDP",
                      "U","HICP","underinf")
saveRDS(data, file="data/data_FR.RDS")
