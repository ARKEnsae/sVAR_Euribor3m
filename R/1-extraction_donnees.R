library(ecb)
library(eurostat)
library(zoo)
library(RJDemetra)

############################### DATA GATHERING & CLEANING ###############################  

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
    filters=list(geo="EA", s_adj="NSA", na_item="B1GQ", unit="CLV10_MEUR")
)  # La série désaisonnalisée n'est pas disponible au niveau agrégé
gdp <- ts(gdp$values,start=c(substr(gdp$time[1],1,4),1), freq=4)
spec_sa <- x13_spec("RSA4c",
                    usrdef.outliersEnabled = TRUE,
                    usrdef.outliersDate = c("2020-01-01",
                                            "2020-04-01",
                                            "2020-07-01",
                                            "2020-10-01"),
                    usrdef.outliersType = rep("AO",4))
x13_gdp <- x13(gdp,spec_sa)
dlgdp <- diff(log(x13_gdp$final$series[,"sa"]))

##unemployment
# Extraction du taux de chômage harmonisé pour les personnes de 15 à 74 ans,
# En ne faisant pas de distinction par sexe et en prenant le pourcentage dans la population active
unem<-get_eurostat(
    "une_rt_q",
    time_format="date",
    filters=list(age = "Y15-74", geo="EA19",sex="T",s_adj = "SA", unit="PC_ACT")
) 
unemp<-ts(unem$values,start=c(substr(unem$time[1],1,4),1), freq=4)

##inflation and underlying inflation (From ECB database)

hicp <-get_data("ICP.M.U2.N.000000.4.ANR",
                filter = list(startPeriod ="1997-01")
)
hicp<-ts(hicp$obsvalue,start=c(1997,1), freq=12)
hicpq <- aggregate(as.zoo(hicp), yearqtr, mean)
hicpq <- as.ts(hicpq)

infex <-get_data("ICP.M.U2.N.XEF000.4.ANR",
                 filter = list(startPeriod ="1997-01")
)
infex<-ts(infex$obsvalue,start=1997, freq=12)
infexq <- aggregate(as.zoo(infex), yearqtr, mean)
infexq <- as.ts(infexq)


data <- ts.union(euribor, dlgdp, unemp, hicpq, infexq)
colnames(data)<-cbind("EURIBOR_3M", "dlGDP","unemployment","inflation","underinf")
saveRDS(data, file="data/data.RDS")
