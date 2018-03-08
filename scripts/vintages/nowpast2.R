# faz o nowcasting pro passado (datas)
# usa a função PRTDB para cortar a base simulando o passado

nowpast <- function(datas, trans, delay, aggregate, method, p = 1, q = 1, r = 1, yAS = NULL){
  
  # base0 é a base cortada para uma data específica
  base0 <- list()
  
  # matriz para salvar os resultados das previsões
  acum4 <- NULL # acumulado 4 trimestres
  varQ <- NULL # variação trimestral
  varA <- NULL # variação trimestre do ano anterior
  nivel <- NULL # pib em nível
  
  for(i in 1:length(datas)){
    base0[[i]] <- ts(read.csv2(paste0("./scripts/vintages/vintage",datas[i],".csv"))[,-1],start=c(2000,1),freq=12)
    
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


nowpast.plot <- function(out, y, yAS, yVarA, yVarQ, yAcumAno){
  
  options(warn=-1)
  # y = pibs[,"pib"]
  # yAS = pibs[,"pibAS"]
  # yVarA = pibs[,"pibVarA"]
  # yVarQ = pibs[,"pibVarQ"]
  # yAcumAno = pibAcum
  # out <- nowpastSM10
  
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
  
  par(mfrow = c(2,2), mar = c(4,3,3,3))
  # nível
  nivel0 <- data.frame(t(window(nivel, end = end(na.omit(y)), freq = 4)))
  a <- barplot(c(na.omit(y)), ylim = c(0,200), main = "Nível", border = "steelblue", col = "skyblue") #  names.arg = names,
  text(a,-8,  srt = 60, adj= 1, xpd = TRUE,
       labels = names, cex=0.9)
  for(i in 1:ncol(nivel0)){
    if(sum(!is.na(nivel0[,i])) > 28){ n <- 28}else{n <- sum(!is.na(nivel0[,i]))}
    points(x = rep(a[i,1],n), y = tail(nivel0[!is.na(nivel0[,i]),i],28), type = "l", col = "#CD0000")
    points(x = a[i,1], y = nivel0[max(which(!is.na(nivel0[,i]))),i], pch = 19, col = "#CD0000")
  }
  
  # variação trimestral (trimestre imediatamente anterior)
  varQ0 <- data.frame(t(window(varQ, end = end(na.omit(y)), freq = 4)))
  a <- barplot(c(na.omit(yVarQ)), ylim = c(min(varQ0, na.rm = T) -2, max(varQ0, na.rm = T) + 2), main = "Variação Trimestral\n(trimestre imediatamente anterior)", border = "steelblue", col = "skyblue")
  text(a,min(varQ0, na.rm = T) -1,  srt = 60, adj= 1, xpd = TRUE,
       labels = names, cex=0.9)
  for(i in 1:ncol(varQ0)){
    if(sum(!is.na(varQ0[,i])) > 28){ n <- 28}else{n <- sum(!is.na(varQ0[,i]))}
    points(x = rep(a[i,1],n), y = tail(varQ0[!is.na(varQ0[,i]),i],28), type = "l", col = "#CD0000")
    points(x = a[i,1], y = varQ0[max(which(!is.na(varQ0[,i]))),i], pch = 19, col = "#CD0000")
  }
  
  # variação anual
  names <- substr(as.Date(na.omit(yVarA)),1,7)
  names <- gsub("-01","-Q1",names)
  names <- gsub("-04","-Q2",names)
  names <- gsub("-07","-Q3",names)
  names <- gsub("-10","-Q4",names)
  
  varA0 <- data.frame(t(window(varA, start = start(yVarA), end = end(na.omit(y)), freq = 4)))
  a <- barplot(c(na.omit(yVarA)), ylim = c(min(varA0, na.rm = T) -2, max(varA0, na.rm = T) + 2), main = "Variação Anual\n(trimestre do ano anterior)", border = "steelblue", col = "skyblue")
  text(a,-10,  srt = 60, adj= 1, xpd = TRUE, labels = names, cex=0.9)
  for(i in 1:ncol(varA0)){
    if(sum(!is.na(varA0[,i])) > 28){ n <- 28}else{n <- sum(!is.na(varA0[,i]))}
    points(x = rep(a[i,1],n), y = tail(varA0[!is.na(varA0[,i]),i],28), type = "l", col = "#CD0000")
    points(x = a[i,1], y = varA0[max(which(!is.na(varA0[,i]))),i], pch = 19, col = "#CD0000")
  }
  
  # acumulado no ano
  # dataAcum <- table(substr(as.Date(acum4),1,4))
  # dataAcum <- as.numeric(names(dataAcum[dataAcum == 4]))
  # acum40 <- window(acum4, start = c(dataAcum[1],1), freq = 4)
  
  acum40 <- acum4[grepl("-10-",as.Date(acum4)),]
  namesAcum <- substr(as.Date(acum4)[grepl("-10-",as.Date(acum4))],1,4)
  yAcumAno <- window(yAcumAno, start = as.numeric(namesAcum[1]), freq = 1)
  namesAcum <- namesAcum[namesAcum %in% substr(as.Date(yAcumAno),1,4)]
  
  a <- barplot(c(yAcumAno), ylim = c(min(yAcumAno, na.rm = T) -2, max(yAcumAno, na.rm = T) + 2), 
               main = "Crescimento anual\n(média do ano contra média do ano anterior)", names.arg = namesAcum,
               border = "steelblue", col = "skyblue")
  
  for(i in 1:length(namesAcum)){
    if(sum(!is.na(acum40[i,])) > 28){ n <- 28}else{n <- sum(!is.na(acum40[i,]))}
    points(x = rep(a[i,1],n), y = tail(acum40[i,!is.na(acum40[i,])],28), type = "l", col = "#CD0000")
    points(x = a[i,1], y = acum40[i,max(which(!is.na(acum40[i,])))], pch = 19, col = "#CD0000")
  }
  
  options(warn=0)
}

nowpast.plot2 <- function(out, y, yAS, yVarA, yVarQ, yAcumAno, type = 1){
  options(warn=-1)
  # y = pibs[,"pib"]
  # yAS = pibs[,"pibAS"]
  # yVarA = pibs[,"pibVarA"]
  # yVarQ = pibs[,"pibVarQ"]
  # yAcumAno = pibAcum
  # out <- nowpastSM10
  
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
      plot(y = tail(nivel0[!is.na(nivel0[,i]),i],28), x = 1:length(tail(nivel0[!is.na(nivel0[,i]),i],28)), type = "o", xaxt = "n", xlab = "",
           ylim = c(min(c(y[i],nivel0[,i]), na.rm = T)-2, max(c(y[i],nivel0[,i]), na.rm = T)),
           col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nNível"), ylab = "")
      abline(h = y[i], lty = 3, col = "#CD0000")
      axis(1, labels = tail(rownames(nivel0)[!is.na(nivel0[,i])],28), at = 1:length(tail(nivel0[!is.na(nivel0[,i]),i],28)))
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
      plot(y = tail(varQ0[!is.na(varQ0[,i]),i],28), x = 1:length(tail(varQ0[!is.na(varQ0[,i]),i],28)), type = "o", xaxt = "n", xlab = "",
           ylim = c(min(c(yVarQ[i],varQ0[,i]), na.rm = T)-2, max(c(yVarQ[i],varQ0[,i]), na.rm = T)),
           col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nVariação trimestral (trimestre imed. anterior)"), ylab = "")
      abline(h = yVarQ[i], lty = 3, col = "#CD0000")
      axis(1, labels = tail(rownames(varQ0)[!is.na(varQ0[,i])],28), at = 1:length(tail(varQ0[!is.na(varQ0[,i]),i],28)))
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
      plot(y = tail(varA0[!is.na(varA0[,i]),i],28), x = 1:length(tail(varA0[!is.na(varA0[,i]),i],28)), type = "o", xaxt = "n", xlab = "",
           ylim = c(min(c(yVarA[i],varA0[,i]), na.rm = T)-2, max(c(yVarA[i],varA0[,i]), na.rm = T)),
           col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nVariação Anual (trimestre do ano anterior)"), ylab = "")
      abline(h = yVarA[i], lty = 3, col = "#CD0000")
      axis(1, labels = tail(rownames(varA0)[!is.na(varA0[,i])],28), at = 1:length( tail(varA0[!is.na(varA0[,i]),i],28)))
    }

  }
  options(warn=0)
}
  
nowpast.error <- function(out, y, yAS, yVarQ, yVarA, yAcumAno){
  options(warn=-1)
  # y = pibs[,"pib"]
  # yAS = pibs[,"pibAS"]
  # yVarA = pibs[,"pibVarA"]
  # yVarQ = pibs[,"pibVarQ"]
  # yAcumAno = pibAcumAno
  # out <- nowpastSM10

  nivel <- out$nivel
  varQ <- out$varQ
  varA <- out$varA
  acum4 <- out$acum4
  
  
  yOK <- window(y, start = start(nivel), freq = 4)#, end =  as.yearmon(tri)[length(tri)]
  yASOK <- window(yAS, start =  start(nivel), freq = 4)#, end =  as.yearmon(tri)[length(tri)], freq = 4)
  yVarAOK <- window(yVarA, start = start(nivel), freq = 4)#, end =  as.yearmon(tri)[length(tri)], freq = 4)
  yVarQOK <- window(yVarQ, start = start(nivel), freq = 4)#, end =  as.yearmon(tri)[length(tri)], freq = 4)
  
  datas_ano <- as.Date(gsub("-01-","-10-",as.Date(yAcumAno)))
  acum4OK <- acum4[as.Date(acum4) %in% datas_ano,]
  acum4OK <- ts(acum4OK, start = as.numeric(substr(as.Date(acum4)[as.Date(acum4) %in% datas_ano][1],1,4)), freq = 1)
  yAcumAnoOK <- window(yAcumAno, start = start(acum4OK), end = end(acum4OK), freq = 1)
  
  # diferença entre previsão e o valor do PIB
  difNivel <- (nivel - yOK)^2
  difVarQ <- (varQ - yVarQOK)^2
  difVarA <- (varA - yVarAOK)^2
  difAcum4 <- (acum4OK - yAcumAnoOK)^2
  
  difNivelOK <- matrix(NA, ncol = 39, nrow = nrow(difNivel))
  difVarQOK <- matrix(NA, ncol = 39, nrow = nrow(difNivel))
  difVarAOK <- matrix(NA, ncol = 39, nrow = nrow(difNivel))
  difAcumAnoOK <- matrix(NA, ncol = 39, nrow = nrow(difAcum4))
  
  for(i in 1:nrow(difNivel)){
    pos_inicial <- min(which(!is.na(difNivel[i,])))
    pos_final <- max(which(!is.na(difNivel[i,])))
    n <- pos_inicial:pos_final
    if(length(n) > 39){
      n <- sort(seq(max(n), length.out = 39, by = -1))
    }
    if(length(n) < 39){
      difNivelOK[i,] <- c(rep(NA, 39 - length(n)), difNivel[i,n])
      difVarQOK[i,] <- c(rep(NA, 39 - length(n)), difVarQ[i,n])
      difVarAOK[i,] <- c(rep(NA, 39 - length(n)), difVarA[i,n])
    }else{
      difNivelOK[i,] <- c(difNivel[i,n])
      difVarQOK[i,] <- c(difVarQ[i,n])
      difVarAOK[i,] <- c(difVarA[i,n])
    }
  }
  
  for(i in 1:nrow(difAcum4)){
    pos_inicial <- min(which(!is.na(difAcum4[i,])))
    pos_final <- max(which(!is.na(difAcum4[i,])))
    n <- pos_inicial:pos_final
    if(length(n) > 39){
      n <- sort(seq(max(n), length.out = 39, by = -1))
    }
    if(length(n) < 39){
      difAcumAnoOK[i,] <- c(rep(NA, 39 - length(n)), difAcum4[i,n])
    }else{
      difAcumAnoOK[i,] <- c( difAcum4[i,n])
    }
  }
  
  rmseNivel <- apply(difNivelOK, MARGIN = 2, FUN = function(x) sqrt(mean(x, na.rm = T)))
  rmseVarQ <- apply(difVarQOK, MARGIN = 2, FUN = function(x) sqrt(mean(x, na.rm = T)))
  rmseVarA <- apply(difVarAOK, MARGIN = 2, FUN = function(x) sqrt(mean(x, na.rm = T)))
  rmseAcum <- apply(difAcumAnoOK, MARGIN = 2, FUN = function(x) sqrt(mean(x, na.rm = T)))
  
  par(mfrow = c(2,2), mar = c(4,3,3,3))
  
  a <- barplot(tail(rmseNivel,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Nível", ylim = c(0,7))
  rect(a[21,1],0,a[28,1],7,col="#F0F8FF",lty=0)
  rect(a[9,1],0,a[20,1],7,col="#9BC4E2",lty=0)
  rect(a[1,1],0,a[8,1],7,col="#F0F8FF",lty=0)
  text(mean(a[4:5,1]),6.5, "Forecasting")
  text(mean(a[14:15,1]),6.5, "Nowcasting")
  text(mean(a[24:25,1]),6.5, "Backcasting")
  barplot(tail(rmseNivel,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Nível", add = T, ylim = c(0,7))
  
  a <- barplot(tail(rmseVarQ,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Variação Trimestral\n(trimestre imediatamente anterior)", ylim = c(0,2))
  rect(a[21,1],0,a[28,1],2,col="#F0F8FF",lty=0)
  rect(a[9,1],0,a[20,1],2,col="#9BC4E2",lty=0)
  rect(a[1,1],0,a[8,1],2,col="#F0F8FF",lty=0)
  text(mean(a[4:5,1]),1.85, "Forecasting")
  text(mean(a[14:15,1]),1.85, "Nowcasting")
  text(mean(a[24:25,1]),1.85, "Backcasting")
  barplot(tail(rmseVarQ,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Variação Trimestral\n(trimestre imediatamente anterior)", add = T, ylim = c(0,2))
  
  a <- barplot(tail(rmseVarA,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Variação Anual\n(trimestre do ano anterior)", ylim = c(0,4))
  rect(a[21,1],0,a[28,1],4,col="#F0F8FF",lty=0)
  rect(a[9,1],0,a[20,1],4,col="#A4D3EE",lty=0)
  rect(a[1,1],0,a[8,1],4,col="#F0F8FF",lty=0)
  text(mean(a[4:5,1]),3.75, "Forecasting")
  text(mean(a[14:15,1]),3.75, "Nowcasting")
  text(mean(a[24:25,1]),3.75, "Backcasting")
  barplot(tail(rmseVarA,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Variação Anual\n(trimestre do ano anterior)", add = T, ylim = c(0,4))
  
  a <- barplot(tail(rmseAcum,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Crescimento anual\n(média do ano contra média do ano anterior)", ylim = c(0,1.4))
  rect(a[21,1],0,a[28,1],1.4,col="#F0F8FF",lty=0)
  rect(a[9,1],0,a[20,1],1.4,col="#A4D3EE",lty=0)
  rect(a[1,1],0,a[8,1],1.4,col="#F0F8FF",lty=0)
  text(mean(a[4:5,1]),1.3, "Forecasting")
  text(mean(a[14:15,1]),1.3, "Nowcasting")
  text(mean(a[24:25,1]),1.3, "Backcasting")
  barplot(tail(rmseAcum,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Crescimento anual\n(média do ano contra média do ano anterior)", add = T, ylim = c(0,1.4))
  
  options(warn=0)
  
  # output
  list(nivel = rmseNivel, varQ = rmseVarQ, varA = rmseVarA, acumAno = rmseAcum)
  
}
