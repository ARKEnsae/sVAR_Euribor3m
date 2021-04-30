if (!require(ggplot2)){
    install.packages(ggplot2)
    require(ggplot2)
}
if (!require(forcats)){
    install.packages(forcats)
    require(forcats)
}
if (!require(ggfortify)){
    install.packages(forcats)
    require(forcats)
}
if (!require(patchwork)){
    install.packages(forcats)
    require(forcats)
}
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
                                 levels = c("dlGDP", "U", "underinf", "HICP", "EURIBOR_3M"),
                                 ordered = TRUE)
    data_plot$variable <- fct_recode(data_plot$variable, !!!recode)
    ggplot(data = data_plot,aes(x = x,y=y,ymin=lower,ymax=upper)) +
        geom_hline(yintercept=0,color="red")+
        geom_line(color = "darkblue")+
        geom_ribbon(alpha=0.1) +
        facet_wrap(vars(variable),
                   scales = "free", nrow = 2, strip.position = "top",
                   labeller = labeller)+
        theme_bw()+xlab("") + ylab("IRF")   
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
                                 levels = c("dlGDP", "U", "underinf", "HICP", "EURIBOR_3M"),
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
latexify_var <- function(model, nb_dec = 1, align = FALSE) {
    rename_fun <- function(x){
        x <- sub("dlGDP", "\\Delta y" , x,fixed = TRUE)
        x <- sub("underinf", "\\pi^{core}" , x,fixed = TRUE)
        x <- sub("HICP", "\\pi" , x,fixed = TRUE)
        x <- sub("EURIBOR_3M", "R" , x,fixed = TRUE)
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
    
    X_tex = c(latexify_const_trend(model, nb_dec = nb_dec),
              sapply(seq_along(coef_mats), function(lag){
                  paste(latexify_mat(coef_mats[[lag]], nb_dec = nb_dec),
                        latexify_y(names_var, lag = lag)  
                  )
              }))
    Y_tex = latexify_y(names_var, lag = 0)
    if (align){
        X_tex = paste(X_tex,
                      collapse = "\\nonumber \\\\ \n &+")
        res = sprintf("\\begin{align} \n%s &= %s + \\varepsilon_t \n\\end{align}",
                      Y_tex, X_tex)
        
    }else{
        X_tex = paste(X_tex,
                      collapse = "+")
        res = sprintf("$$\n %s = %s+ \\varepsilon_t\n$$",
                      Y_tex, X_tex)
    }
    return (res)
}

latexify_mat <- function(mat, nb_dec = 1){
    mat = formatC(mat, digits = nb_dec, format = "f")
    mat = paste(apply(mat,1, paste, collapse = " & "), collapse = " \\\\\n")
    mat <- paste("\\begin{pmatrix}\n",mat,
                 "\n\\end{pmatrix}")
    mat
}
latexify_const_trend <- function(model, nb_dec = 1){
    coefficients <- coef(model)
    const_trend <- t(sapply(coefficients,function(x){
        x[grep("(^const$)|(^trend$)",rownames(x)), "Estimate"]
    })) 
    if(model$type == "both"){
        # constante + tendance
        res <- latexify_mat(const_trend, nb_dec = nb_dec)
        res <- paste(res,
                     "\\begin{pmatrix} 1 \\\\ t \\end{pmatrix}")
    }
    if(model$type == "trend"){
        res <- latexify_mat(const_trend, nb_dec = nb_dec)
        res <- paste(res,
                     "\\begin{pmatrix} 1 \\end{pmatrix}")
    }
    if(model$type == "const"){
        res <- latexify_mat(const_trend, nb_dec = nb_dec)
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
