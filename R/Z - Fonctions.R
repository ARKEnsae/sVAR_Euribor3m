if (!require(ggplot2)){
    install.packages(ggplot2)
    require(ggplot2)
}
if (!require(forcats)){
    install.packages(forcats)
    require(forcats)
}
if (!require(ggfortify)){
    install.packages(ggfortify)
    require(ggfortify)
}
if (!require(patchwork)){
    install.packages(patchwork)
    require(patchwork)
}
# On installe les packages suivants si non installés
packages <- c("reshape2", "fUnitRoots", "tseries", "vars",
              "ecb", "eurostat", "zoo")
install.packages(setdiff(packages, rownames(installed.packages())))  

#### Fonctions pour tracer les graphiques ####
plot_irf <- function(oir,
                     labeller = "label_parsed",
                     recode = c("Delta~y[t]"= "dlGDP",
                                "U[t]" = "U",
                                "pi[t]^core"="underinf",
                                "pi[t]"="HICP",
                                "R[t]" = "EURIBOR_3M")){
    data_plot <- Reduce(function(x,y) merge(x,y,by = c("Var1","Var2")),
                        list(reshape2::melt(oir$irf$EURIBOR_3M),
                             reshape2::melt(oir$Lower$EURIBOR_3M),
                             reshape2::melt(oir$Upper$EURIBOR_3M)))
    data_plot[,"Var1"] <- data_plot[,"Var1"] -1
    colnames(data_plot) <- c("x","variable","y","lower","upper")
    data_plot$variable <- factor(data_plot$variable,
                                 levels = c("dlGDP", "U", "underinf",
                                            "HICP", "EURIBOR_3M"),
                                 ordered = TRUE)
    data_plot$variable <- fct_recode(data_plot$variable, !!!recode)
    ggplot(data = data_plot,aes(x = x,y=y,ymin=lower,ymax=upper)) +
        geom_hline(yintercept=0,color="red")+
        geom_line(color = "darkblue")+
        geom_ribbon(alpha=0.1) +
        facet_wrap(vars(variable),
                   scales = "free", nrow = 2, strip.position = "top",
                   labeller = labeller)+
        theme_bw()+xlab("") + ylab("")   
}
plot_fevd <- function(fevd,
                      labeller = "label_parsed",
                      recode = c("Delta~y[t]"= "dlGDP",
                                 "U[t]" = "U",
                                 "pi[t]^core"="underinf",
                                 "pi[t]"="HICP",
                                 "R[t]" = "EURIBOR_3M")){
    data_plot <- Reduce(rbind,
                        lapply(names(fevd), function(x){
                            d <- reshape2::melt(fevd[[x]])
                            d$var_comp = x
                            d
                        }))
    colnames(data_plot) <- c("horizon","variable","y","decomp")
    data_plot$y <- 100 *data_plot$y
    data_plot$decomp <- factor(data_plot$decomp,
                               levels = c("dlGDP", "U", "underinf",
                                          "HICP", "EURIBOR_3M"),
                               ordered = TRUE)
    
    data_plot$variable = fct_recode(data_plot$variable, !!!recode)
    data_plot$decomp = fct_recode(data_plot$decomp, !!!recode)
    
    ggplot(data = data_plot, mapping = aes(x = horizon, y = y,
                                           fill = variable)) +
        geom_bar(stat = "identity")+
        facet_wrap(vars(decomp),
                   scales = "free", strip.position = "top",
                   labeller = labeller)+
        theme_bw()+xlab("Horizon") + ylab("Percentage") +
        scale_fill_viridis_d(
            breaks = levels(data_plot$variable),
            labels = unlist(label_parsed(levels(data_plot$variable))))
}
plot_ts <- function(data, x){
    rename_fun <- function(x){
        x <- sub("dlGDP", "Delta~y[t]" , x)
        x <- sub("underinf", "pi[t]^{core}" , x)
        x <- sub("HICP", "pi[t]" , x)
        x <- sub("EURIBOR_3M", "R[t]" , x)
        x <- sub("^U$", "U[t]" , x)
        x
    }
    autoplot(data[,x]) + 
        labs(title = label_parsed(rename_fun(x))[[1]][[1]])
}
########################################################

#### Fonctions pour extraire le code latex d'un VAR ####
latexify_var <- function(model, nb_dec = 1, align = FALSE,
                         se = FALSE) {
    rename_fun <- function(x){
        x <- sub("dlGDP", "\\Delta y" , x, fixed = TRUE)
        x <- sub("underinf", "\\pi^{core}" , x, fixed = TRUE)
        x <- sub("HICP", "\\pi" , x, fixed = TRUE)
        x <- sub("EURIBOR_3M", "R" , x, fixed = TRUE)
        x
    }
    # rename_fun = function(x) {gsub("_", "\\_", x,fixed = TRUE)}
    coefficients <- coef(model)
    names_var <- rename_fun(names(coefficients))
    
    coef_mats <- lapply(seq_len(model$p),function(lag){
        sapply(coefficients,function(x){
            x[grep(sprintf("\\.l%i$",lag),rownames(x)), "Estimate"]
        })
    })
    
    coef_mats_se <- lapply(seq_len(model$p),function(lag){
        if(se){
            sapply(coefficients,function(x){
                x[grep(sprintf("\\.l%i$",lag),rownames(x)), "Std. Error"]
            })
        }else{
            NULL
        }
    })
    X_tex = c(latexify_const_trend(model, se = se, nb_dec = nb_dec),
              sapply(seq_along(coef_mats), function(lag){
                  paste(latexify_mat(coef_mats[[lag]],
                                     se_mat = coef_mats_se[[lag]],
                                     nb_dec = nb_dec),
                        latexify_y(names_var, lag = lag)  
                  )
              })) 
    
    Y_tex = latexify_y(names_var, lag = 0)
    if (align){
        # Concaténation de la constante
        X_tex = c(paste(X_tex[1], X_tex[2], sep = " + "),
                  X_tex[-(1:2)])
        X_tex = paste(X_tex,
                      collapse = "\\nonumber \\\\ \n &+")
        res = sprintf("\\begin{align} \n%s &= %s + \\varepsilon_t \n\\end{align}",
                      Y_tex, X_tex)
    }else{
        X_tex = paste(X_tex,
                      collapse = "+")
        res = sprintf("%s = %s+ \\varepsilon_t\n",
                      Y_tex, X_tex)
    }
    return (res)
}

latexify_mat <- function(mat, se_mat = NULL, nb_dec = 1){
    mat = formatC(mat, digits = nb_dec, format = "f")
    if(!is.null(se_mat)){
        se_mat = formatC(se_mat, digits = nb_dec, format = "f")
        se_mat = apply(se_mat,2,
                       function(x) paste0("\\underset{(",x,")}"))
        mat = apply(mat,2,
                    function(x) paste0("{",x,"}"))
        mat[]= paste0(se_mat,mat)
    }
    
    mat = paste(apply(mat,1, paste, collapse = " & "), collapse = " \\\\\n")
    mat <- paste("\\begin{pmatrix}\n",mat,
                 "\n\\end{pmatrix}")
    mat
}
latexify_const_trend <- function(model, nb_dec = 1, se = FALSE){
    coefficients <- coef(model)
    const_trend <- t(sapply(coefficients,function(x){
        x[grep("(^const$)|(^trend$)",rownames(x)), "Estimate"]
    })) 
    if(se){
        const_trend_se <- t(sapply(coefficients,function(x){
            x[grep("(^const$)|(^trend$)",rownames(x)), "Std. Error"]
        }))  
    }else{
        const_trend_se <- NULL
    }
    
    if(model$type == "both"){
        # constante + tendance
        res <- latexify_mat(const_trend,
                            se_mat = const_trend_se,
                            nb_dec = nb_dec)
        res <- paste(res,
                     "\\begin{pmatrix} 1 \\\\ t \\end{pmatrix}")
    }
    if(model$type == "trend"){
        res <- latexify_mat(const_trend,
                            se_mat = const_trend_se,
                            nb_dec = nb_dec)
        res <- paste(res,
                     "\\begin{pmatrix} 1 \\end{pmatrix}")
    }
    if(model$type == "const"){
        res <- latexify_mat(const_trend,
                            se_mat = const_trend_se,
                            nb_dec = nb_dec)
        res <- paste(res,
                     "\\begin{pmatrix} t  \\end{pmatrix}")
    }
    if(model$type == "none"){
        res <- NULL
    }
    res
}
latexify_y <- function(vec, lag = 0){
    vec = sprintf("%s_{t%s}",vec,
                  ifelse(lag == 0,"",
                         sprintf(" - %i", lag)))
    paste("\\begin{pmatrix}\n",
          paste(vec,collapse = " \\\\\n"),
          "\n\\end{pmatrix}")
}
#############################################

#### Téléchargement données depuis BDM ####
lectureBDM <- function(idbank, ...)
{
    #On récupère les idbank et on supprime les éventuels espaces
    idbank<-gsub(" ","",c(idbank,unlist(list(...))))
    
    #Les url pour télécharger le(s) série(s)
    UrlData <- paste0("https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
                      paste(idbank,collapse = "+"))
    
    tryCatch({
        dataBDM <- as.data.frame(rsdmx::readSDMX(UrlData,isURL = T),
                                 stringsAsFactors=TRUE)
    },error=function(e){
        stop(paste0("Il y a une erreur dans le téléchargement des données.",
                    "Vérifier le lien\n",
                    UrlData),
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
    # On détermine le format de la colonne qui contient les dates
    # en fonction de la fréquence
    sepDate<-switch(FREQ
                    ,M="-"
                    ,B="-B"
                    ,T="-Q"
                    ,S="-S"
                    ,A=" ")
    dataBDM <- reshape2::dcast(dataBDM,"TIME_PERIOD ~ IDBANK",
                               value.var = "OBS_VALUE")
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
                      paste(grep(paste(colnames(dataBDM),collapse="|"),
                                 idbank,value=T,invert = T),
                            collapse=", ")))
    if(ncol(dataBDM) > 1){
        # On a au moins 2 colonnes : on replace les colonnes dans le même ordre 
        # que les séries en entrée
        
        # On ne garde que les idbank présents dans la base
        idbank <- idbank[idbank %in% colnames(dataBDM)]
        dataBDM <- dataBDM[,idbank]
    }
    dataBDM <- ts(dataBDM,start=dateDeb,freq=freq)
    return(dataBDM)
}

#### fonctions pour tester la stationarité des données ####
stationnarity_test <- function(x, detrend = TRUE){
    if(detrend){
        x_detrend = residuals(lm(x ~ time(x)))
    }else{
        x_detrend = x
    }
    
    kpss_test = tseries::kpss.test(x_detrend) # H_0 série stationnaire
    adf = adfTest_valid(x_detrend, kmax = 20, type = "nc") # H_0 racine unitaire
    result = c(kpss_test$p.value,
               adf@test$p.value)
    names(result) = c("KPSS test", "ADF test")
    result
}
Qtests <- function(series, k = 24, fitdf=0) {
    pvals <- apply(matrix(1:k), 1, FUN=function(l) {
        pval <- if (l<=fitdf) NA else Box.test(series,
                                               lag=l,
                                               type="Ljung-Box", 
                                               fitdf=fitdf)$p.value 
        return(c("lag"=l,"pval"=pval))
    })
    return(t(pvals))
}
# tests ADF jusqu’à ce que les résidus ne soient pas autocorrélés
adfTest_valid <- function(series, kmax,type){
    k <- 0
    noautocorr <- 0
    while (noautocorr==0){
        suppressWarnings({
            adf <- fUnitRoots::adfTest(series,lags=k,type=type)
        })
        
        pvals <- Qtests(adf@test$lm$residuals,
                        24,
                        fitdf=length(adf@test$lm$coefficients))[,2] 
        if (sum(pvals<0.05,na.rm=T) == 0) {
            noautocorr <- 1;
            adf <- fUnitRoots::adfTest(series,lags=k,type=type)
        }
        k <- k + 1
    }
    return(adf)
}