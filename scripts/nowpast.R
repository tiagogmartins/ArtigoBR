# faz o nowcasting pro passado (datas)
# usa a função PRTDB para cortar a base simulando o passado

nowpast <- function(datas, base, trans, delay, aggregate, method, p = 1, q = 1, r = 1, yAS = NULL){
  
  # base0 é a base cortada para uma data específica
  base0 <- list()
  
  # matriz para salvar os resultados das previsões
  acum4 <- NULL # acumulado 4 trimestres
  varQ <- NULL # variação trimestral
  varA <- NULL # variação trimestre do ano anterior
  nivel <- NULL # pib em nível
  
  for(i in 1:length(datas)){
    base0[[i]] <- PRTDB(mts = base, vintage = datas[i], delay = delay)
    
    # BRGDP (estacionário)
    brgdp <- month2qtr(base0[[i]][,"serie22099"])
    brgdpEst <- diff(diff(brgdp),4)
    
    # base X
    brgdp_position <- which(colnames(base0[[i]]) == "serie22099")
    X <- base0[[i]][,-brgdp_position]
    
    # painel balanceado
   if (method == "EM"){
     stationaryBase <- cbind(tryCatch(X[,trans == 0], error = function(e) NULL),
                             tryCatch(X[,trans == 1]/lag(X[,trans == 1], k = -1) - 1, error = function(e) NULL), 
                             tryCatch(diff(X[,trans == 2]), error = function(e) NULL), 
                             tryCatch((X[,trans == 3]/lag(X[,trans == 3], k = -12) - 1) - (lag(X[,trans == 3],-1)/lag(X[,trans == 3], k = -13) - 1), error = function(e) NULL), 
                             tryCatch(diff(X[,trans == 4],12) - diff(lag(X,-1)[,trans == 4],12), error = function(e) NULL) 
     )
     colnames(stationaryBase) <- colnames(X)[c(tryCatch(which(trans == 0), error = function(e) NULL),
                                               tryCatch(which(trans == 1), error = function(e) NULL),
                                               tryCatch(which(trans == 2), error = function(e) NULL),
                                               tryCatch(which(trans == 3), error = function(e) NULL),
                                               tryCatch(which(trans == 4), error = function(e) NULL))]
     XB <- stationaryBase[,colnames(X)]
     brgdpEst <- ts(c(na.omit(brgdpEst),NA,NA,NA), start = start(na.omit(brgdpEst)), freq = 4)
     
     XB <- tryCatch(XB[,!colSums(is.na(XB)) == nrow(XB)], error = function(e) XB)
   } else {
     XB <- Bpanel(X, trans = trans, aggregate = aggregate) 
     
   }
    
    # modelagem PIB
    now <- nowcast(y = brgdpEst, x = XB, q = q, r = r, p = p, method = method)
    
    # previsões
    backcst <- tryCatch(head(na.omit(now$yfcst[,3]),1), error = function(e) head(tail(now$yfcst[,3],3),1))
    nowcst <- tryCatch(tail(head(na.omit(now$yfcst[,3]),2),1), error = function(e) head(tail(now$yfcst[,3],2),1))
    forecst <- tryCatch(tail(head(na.omit(now$yfcst[,3]),3),1), error = function(e) tail(now$yfcst[,3],1))
    
    # previsões no nível
    backcstNivel <- backcst + c(tail(na.omit(lag(brgdp,-1) + lag(brgdp,-4) - lag(brgdp,-5)),1))
    nowcstNivel <- nowcst + c(tail(na.omit(backcstNivel + lag(brgdp,-3) - lag(brgdp,-4)),1))
    forecstNivel <- forecst + c(tail(na.omit(nowcstNivel + lag(brgdp,-2) - lag(brgdp,-3)),1))
    
    nivel <- cbind(nivel, ts(c(backcstNivel,nowcstNivel, forecstNivel), start = start(backcstNivel), freq = 4))
    
    # novo pib
    brgdpNovo <- ts(c(na.omit(brgdp), backcstNivel, nowcstNivel, forecstNivel), start = start(na.omit(brgdp)), freq = 4)
    brgdpNovo <- ts(c(96.84,100.12,107.56,104.31,100.13,104.88,109.49,108.21,101.14,106.46,109.88,106.66,101.92,106.04,109.20,108.97,
                      brgdpNovo), end = end(brgdpNovo), freq = 4)
    # ajuste sazonal
    m <- seas(brgdpNovo, transform.function = "auto", regression.aictest = c("td","easter"), 
              outlier.types = "all", x11 = "", pickmdl.method = "best", pickmdl.identify = "all",
              forecast.maxlead = 6, forecast.maxback = 0)
    brgdpAS <- tryCatch(ts(c(na.omit(month2qtr(base0[[i]][,"serie22109"])),tail(final(m),3)), end = end(final(m)), freq = 4),
                        error = function(e) ts(c(na.omit(yAS),tail(final(m),3)), end = end(final(m)), freq = 4))
    
    # previsão: variação trimestral (trimestre imediatamente anterior)
    brgdpVarQ <- tail((brgdpAS/lag(brgdpAS,-1)-1)*100,3)
    varQ <- cbind(varQ, brgdpVarQ)
    
    # previsão: acumulado quatro trimestres
    brgdpAcum <- ts(c((mean(head(tail(brgdpNovo,6),4))/mean(head(tail(brgdpNovo,10),4))-1)*100,
                      (mean(head(tail(brgdpNovo,5),4))/mean(head(tail(brgdpNovo,9),4))-1)*100,
                      (mean(tail(brgdpNovo,4))/mean(head(tail(brgdpNovo,8),4))-1)*100), start = start(backcstNivel), freq = 4)
    acum4 <- cbind(acum4, brgdpAcum)
    
    # previsão anual
    brgdpVarA <- ts(c(backcstNivel/lag(brgdp,-4)-1, nowcstNivel/lag(brgdp,-4)-1, forecstNivel/lag(brgdp,-4)-1),
                    start = start(backcstNivel), freq = 4)*100
    varA <- cbind(varA, brgdpVarA)
    
    message(datas[i])
  }
  colnames(acum4) = colnames(varQ) = colnames(varA) = colnames(nivel) <- as.character(datas)
  
  # output
  list(nivel = nivel, acum4 = acum4, varA = varA, varQ = varQ)
  
}


nowpast.plot <- function(out, y, yAS){
  
  options(warn=-1)
  # y <- month2qtr(base[,"serie22099"])
  # yAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)
  # out <- nowpast2008
  
  yVarQ <- (yAS/lag(yAS,-1)-1)*100
  yVarA <- (y/lag(y,-4)-1)*100
  ydata <- data.frame(y, ano = substr(as.Date(y),1,4))
  yMean <- aggregate(ydata$y, by = list(ydata$ano), FUN = mean, na.rm = T)
  yVarAcum <- (yMean/lag(yMean,-1)-1)*100
  
  nivel <- out$nivel
  varQ <- out$varQ
  varA <- out$varA
  acum4 <- out$acum4
  
  y <- window(y, start = start(nivel), end = end(nivel), freq = 4)
  yVarQ <- window(yVarQ, start = start(nivel), end = end(nivel), freq = 4)
  yVarA <- window(yVarA, start = start(nivel), end = end(nivel), freq = 4)
  yVarAcum <- window(yVarAcum, start = start(nivel)[1], end = end(nivel), freq = 1)
  
  
  names <- substr(as.Date(na.omit(y)),1,7)
  names <- gsub("-01","-Q1",names)
  names <- gsub("-04","-Q2",names)
  names <- gsub("-07","-Q3",names)
  names <- gsub("-10","-Q4",names)
  
  par(mar = c(3,3,3,2), mfrow = c(2,2))
  
  # nível
  nivel0 <- data.frame(t(window(nivel, end = end(na.omit(y)), freq = 4)))
  a <- barplot(c(na.omit(y)), ylim = c(0,200), main = "Nível", names.arg = names, border = "steelblue", col = "skyblue")
  for(i in 1:ncol(nivel0)){
    points(x = rep(a[i,1],sum(!is.na(nivel0[,i]))), y = nivel0[!is.na(nivel0[,i]),i], type = "l", col = "#CD0000")
    points(x = a[i,1], y = nivel0[max(which(!is.na(nivel0[,i]))),i], pch = 19, col = "#CD0000")
  }
  
  # variação trimestral (trimestre imediatamente anterior)
  varQ0 <- data.frame(t(window(varQ, end = end(na.omit(y)), freq = 4)))
  a <- barplot(c(na.omit(yVarQ)), ylim = c(min(varQ0, na.rm = T) -2, max(varQ0, na.rm = T) + 2), main = "Variação Trimestral\n(trimestre imediatamente anterior)", names.arg = names, border = "steelblue", col = "skyblue")
  for(i in 1:ncol(varQ0)){
    points(x = rep(a[i,1],sum(!is.na(varQ0[,i]))), y = varQ0[!is.na(varQ0[,i]),i], type = "l", col = "#CD0000")
    points(x = a[i,1], y = varQ0[max(which(!is.na(varQ0[,i]))),i], pch = 19, col = "#CD0000")
  }
  
  
  # variação anual
  varA0 <- data.frame(t(window(varA, end = end(na.omit(y)), freq = 4)))
  a <- barplot(c(na.omit(yVarA)), ylim = c(min(varA0, na.rm = T) -2, max(varA0, na.rm = T) + 2), main = "Variação Anual\n(trimestre do ano anterior)", names.arg = names, border = "steelblue", col = "skyblue")
  for(i in 1:ncol(varA0)){
    points(x = rep(a[i,1],sum(!is.na(varA0[,i]))), y = varA0[!is.na(varA0[,i]),i], type = "l", col = "#CD0000")
    points(x = a[i,1], y = varA0[max(which(!is.na(varA0[,i]))),i], pch = 19, col = "#CD0000")
  }
  
  # acumulado no ano
  acum40 <- acum4[grepl("-10-",as.Date(acum4)),]
  namesAcum <- paste0(substr(as.Date(acum4)[grepl("-10-",as.Date(acum4))],1,4),"-Q4")

  a <- barplot(c(na.omit(yVarAcum)), ylim = c(min(yVarAcum, na.rm = T) -2, max(yVarAcum, na.rm = T) + 2), 
               main = "Crescimento anual\n(média do ano contra média do ano anterior)", names.arg = namesAcum,
               border = "steelblue", col = "skyblue")
  
  for(i in 1:nrow(acum40)){
    points(x = rep(a[i,1],sum(!is.na(acum40[i,]))), y = acum40[i,!is.na(acum40[i,])], type = "l", col = "#CD0000")
    points(x = a[i,1], y = acum40[i,max(which(!is.na(acum40[i,])))], pch = 19, col = "#CD0000")
  }
  options(warn=0)
}

nowpast.plot2 <- function(out, y, yAS, type = 1){
  options(warn=-1)
  # y <- month2qtr(base[,"serie22099"])
  # yAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)
  # out <- nowpast2008
  
  yVarQ <- (yAS/lag(yAS,-1)-1)*100
  yVarA <- (y/lag(y,-4)-1)*100
  
  nivel <- out$nivel
  varQ <- out$varQ
  varA <- out$varA
  acum4 <- out$acum4
  
  y <- window(y, start = start(nivel), end = end(nivel), freq = 4)
  yVarQ <- window(yVarQ, start = start(nivel), end = end(nivel), freq = 4)
  yVarA <- window(yVarA, start = start(nivel), end = end(nivel), freq = 4)
  
  names <- substr(as.Date(na.omit(y)),1,7)
  names <- gsub("-01","-Q1",names)
  names <- gsub("-04","-Q2",names)
  names <- gsub("-07","-Q3",names)
  names <- gsub("-10","-Q4",names)
  
  if(type == 1){  # nível
    
    par(mar = c(3,3,3,2), mfrow = c(3,3))
    
    nivel0 <- data.frame(t(window(nivel, end = end(na.omit(y)), freq = 4)))
    colnames(nivel0) <- as.Date(window(nivel, end = end(na.omit(y)), freq = 4))
    
    titulos <- substr(colnames(nivel0),1,7)
    titulos <- gsub("-01","-Q1",titulos)
    titulos <- gsub("-04","-Q2",titulos)
    titulos <- gsub("-07","-Q3",titulos)
    titulos <- gsub("-10","-Q4",titulos)
    
    for(i in 1:ncol(nivel0)){
      plot(y = nivel0[!is.na(nivel0[,i]),i], x = 1:sum(!is.na(nivel0[,i])), type = "o", xaxt = "n", 
           ylim = c(min(c(y[i],nivel0[,i]), na.rm = T)-2, max(c(y[i],nivel0[,i]), na.rm = T)),
           col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nNível"), ylab = "")
      abline(h = y[i], lty = 3, col = "#CD0000")
      axis(1, labels = rownames(nivel0)[!is.na(nivel0[,i])], at = 1:sum(!is.na(nivel0[,i])))
    }
    
  }else if(type == 2){  # varQ
    
    par(mar = c(3,3,3,2), mfrow = c(3,3))
    
    varQ0 <- data.frame(t(window(varQ, end = end(na.omit(y)), freq = 4)))
    colnames(varQ0) <- as.Date(window(varQ, end = end(na.omit(y)), freq = 4))
    
    titulos <- substr(colnames(varQ0),1,7)
    titulos <- gsub("-01","-Q1",titulos)
    titulos <- gsub("-04","-Q2",titulos)
    titulos <- gsub("-07","-Q3",titulos)
    titulos <- gsub("-10","-Q4",titulos)
    
    for(i in 1:ncol(varQ0)){
      plot(y = varQ0[!is.na(varQ0[,i]),i], x = 1:sum(!is.na(varQ0[,i])), type = "o", xaxt = "n", 
           ylim = c(min(c(yVarQ[i],varQ0[,i]), na.rm = T)-2, max(c(yVarQ[i],varQ0[,i]), na.rm = T)),
           col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nVariação trimestral (trimestre imed. anterior)"), ylab = "")
      abline(h = yVarQ[i], lty = 3, col = "#CD0000")
      axis(1, labels = rownames(varQ0)[!is.na(varQ0[,i])], at = 1:sum(!is.na(varQ0[,i])))
    }
    
  }else if(type == 3){  # varA
    
    par(mar = c(3,3,3,2), mfrow = c(3,3))
    
    varA0 <- data.frame(t(window(varA, end = end(na.omit(y)), freq = 4)))
    colnames(varA0) <- as.Date(window(varA, end = end(na.omit(y)), freq = 4))
    
    titulos <- substr(colnames(varA0),1,7)
    titulos <- gsub("-01","-Q1",titulos)
    titulos <- gsub("-04","-Q2",titulos)
    titulos <- gsub("-07","-Q3",titulos)
    titulos <- gsub("-10","-Q4",titulos)
    
    for(i in 1:ncol(varA0)){
      plot(y = varA0[!is.na(varA0[,i]),i], x = 1:sum(!is.na(varA0[,i])), type = "o", xaxt = "n", 
           ylim = c(min(c(yVarA[i],varA0[,i]), na.rm = T)-2, max(c(yVarA[i],varA0[,i]), na.rm = T)),
           col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nVariação Anual (trimestre do ano anterior)"), ylab = "")
      abline(h = yVarA[i], lty = 3, col = "#CD0000")
      axis(1, labels = rownames(varA0)[!is.na(varA0[,i])], at = 1:sum(!is.na(varA0[,i])))
    }

  }
  options(warn=0)
}
  
nowpast.error <- function(out, y, yAS){
  options(warn=-1)
  # y <- month2qtr(base[,"serie22099"])
  # yAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)
  # out <- nowpast11
  
  nivel <- out$nivel
  varQ <- out$varQ
  varA <- out$varA
  acum4 <- out$acum4
  
  yVarQ <- (yAS/lag(yAS,-1)-1)*100
  yVarA <- (y/lag(y,-4)-1)*100
  ydata <- data.frame(y, ano = substr(as.Date(y),1,4))
  yMean <- aggregate(ydata$y, by = list(ydata$ano), FUN = mean, na.rm = T)
  yVarAcum <- (yMean/lag(yMean,-1)-1)*100
  
  y <- window(y, start = start(nivel), end = end(nivel), freq = 4)
  yVarQ <- window(yVarQ, start = start(nivel), end = end(nivel), freq = 4)
  yVarA <- window(yVarA, start = start(nivel), end = end(nivel), freq = 4)
  yVarAcum <- window(yVarAcum, start = start(nivel)[1], end = end(nivel)[1], freq = 1)
  
  rmse_nivel <- matrix(NA, nrow = length(y), ncol = 1)
  rmse_varQ <- matrix(NA, nrow = length(y), ncol = 1)
  rmse_varA <- matrix(NA, nrow = length(y), ncol = 1)
  rmse_varAcum <- matrix(NA, nrow = length(yVarAcum), ncol = 1)
  
  for(i in 1:length(y)){
    rmse_nivel[i,1] <- sqrt(mean((nivel[i,] - y[i])^2, na.rm = T))
    rmse_varQ[i,1] <- sqrt(mean((varQ[i,] - yVarQ[i])^2, na.rm = T))
    rmse_varA[i,1] <- sqrt(mean((varA[i,] - yVarA[i])^2, na.rm = T))
  }
  
  acum40 <- acum4[grepl("-10-",as.Date(acum4)),]
  for(i in 1:length(yVarAcum)){
    rmse_varAcum[i,1] <- sqrt(mean((acum40[i,] - yVarAcum[i])^2, na.rm = T))
  }
  
  rmse_nivel <- ts(rmse_nivel, start = start(y), freq = 4)
  rmse_varQ <- ts(rmse_varQ, start = start(y), freq = 4)
  rmse_varA <- ts(rmse_varA, start = start(y), freq = 4)
  rmse_varAcum <- ts(rmse_varAcum, start = start(yVarAcum), freq = 1)
  
  options(warn=0)
  
  # output
  list(trimestral = na.omit(cbind(nivel = rmse_nivel, varQ = rmse_varQ, varA = rmse_varA)),
       acumAno =  rmse_varAcum)
  
}
