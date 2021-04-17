library(ecb)
library(eurostat)
library(zoo)
library(insee)

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
gdp <- ts(gdp$values,start=c(substr(gdp$time[1],1,4),1), freq=4)
PP.test(matrix[,1])
dlgdp <- diff(log(gdp))

# FBCF
fbcf<-get_eurostat(
    "namq_10_gdp",
    time_format="date",
    filters=list(geo="EA", s_adj="SCA", na_item="P51G", unit="CLV10_MEUR")
) 
fbcf <- ts(fbcf$values,start=c(substr(fbcf$time[1],1,4),1), freq=4)
dfbcf <- diff(log(fbcf))

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


data <- ts.union(euribor, dlgdp,dfbcf, unemp, hicpq, infexq)
colnames(data)<-cbind("EURIBOR_3M", "dlGDP","dfbcf","unemployment","inflation","underinf")
saveRDS(data, file="data/data_UE.RDS")

##################################
######### DONNEES FR #############
##################################

# Attention, il les dates sont dans le sens inverse par rapport à avant
# il faut donc les trier avant de convertir en ts
infex <- insee::get_insee_idbank("001768593")
pib <- insee::get_insee_idbank("010565692")

# Personnellement je préfère l'utilisation de cette fonction "maison"
# où je fais automatiquement la transformation en ts()
lectureBDM <- function(idbank, ...)
{
    #On récupère les idbank et on supprime les éventuels espaces
    idbank<-gsub(" ","",c(idbank,unlist(list(...))))
    
    #Les url pour télécharger le(s) série(s)
    UrlData <- paste0("https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/",paste(idbank,collapse = "+"))
    
    tryCatch({
        dataBDM <- as.data.frame(rsdmx::readSDMX(UrlData,isURL = T),
                                 stringsAsFactors=TRUE)
    },error=function(e){
        stop(paste0("Il y a une erreur dans le téléchargement des données. Vérifier le lien\n",UrlData),
             call. = FALSE)
    })
    
    FREQ <- levels(factor(dataBDM$FREQ))
    
    if (length(FREQ)!=1)
        stop("Les séries ne sont pas de la même périodicité !")
    
    freq<-switch(FREQ
                 ,M=12
                 ,B=6
                 ,T=4
                 ,S=2
                 ,A=1)
    #On détermine le format de la colonne qui contient les dates en fonction de la fréquence
    sepDate<-switch(FREQ
                    ,M="-"
                    ,B="-B"
                    ,T="-Q"
                    ,S="-S"
                    ,A=" ")
    dataBDM <- reshape2::dcast(dataBDM,"TIME_PERIOD ~ IDBANK",value.var = "OBS_VALUE")
    dataBDM <- dataBDM[order(dataBDM$TIME_PERIOD),]
    
    #On récupère la première date
    dateDeb <- dataBDM$TIME_PERIOD[1]
    dateDeb <- regmatches(dateDeb,gregexpr(sepDate,dateDeb),invert=T)[[1]]
    dateDeb <- as.numeric(dateDeb)
    
    #On supprime la colonne des dates et on convertit les séries en numérique
    dataBDM$TIME_PERIOD <- NULL
    dataBDM <- apply(dataBDM,2,as.numeric)
    
    if(ncol(dataBDM) != length(idbank))
        warning(paste("Le ou les idbank suivant n'existent pas :",
                      paste(grep(paste(colnames(dataBDM),collapse="|"),idbank,value=T,invert = T),
                            collapse=", ")))
    if(ncol(dataBDM) > 1){
        # On a au moins 2 colonnes : on replace les colonnes dans le même ordre que les séries en entrée
        idbank <- idbank[idbank %in% colnames(dataBDM)] #On ne garde que les idbank présents dans la base
        dataBDM <- dataBDM[,idbank]
    }
    dataBDM <- ts(dataBDM,start=dateDeb,freq=freq)
    return(dataBDM)
}
# Indice d'inflation sous-jacente - Base 2015 - Ensemble des ménages - France métropolitaine - Ensemble
infex <- lectureBDM("001769686")
infexq <- aggregate(as.zoo(infex), yearqtr, mean)
infexq <- as.ts(infexq)
# Produit intérieur brut total - Volume aux prix de l'année précédente chaînés - Série CVS-CJO aux prix de l'année précédente chaînés - Série CVS-CJO
pib <- lectureBDM("010565708")
dlpib <- (log(pib))

# Pour avoir la doc associée : https://www.insee.fr/fr/statistiques/serie/010565708
# (changer le numéro de la fin pour changer de série)