# faz o nowcasting pro passado (datas)
# usa a função PRTDB para cortar a base simulando o passado

nowpast <- function(datas, base, trans, delay, aggregate, method, p = 1, q = 1, r = 1){
  
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
     stationaryBase <- cbind(X[,trans == 0], X[,trans == 1]/lag(X[,trans == 1], k = -1) - 1, diff(X[,trans == 2]),
                             (X[,trans == 3]/lag(X[,trans == 3], k = -12) - 1) - (lag(X[,trans == 3],-1)/lag(X[,trans == 3], k = -13) - 1),
                             diff(X[,trans == 4],12) - diff(lag(X,-1)[,trans == 4],12)
     )
     colnames(stationaryBase) <- colnames(X)[c(which(trans == 0),which(trans == 1),which(trans == 2),which(trans == 3),which(trans == 4)) ]
     XB <- stationaryBase[,colnames(X)]
     
   } else {
     XB <- Bpanel(X, trans = trans, aggregate = aggregate) 
     
   }
    
    
    
    # modelagem PIB
    now <- nowcast(y = brgdpEst, x = XB, q = q, r = r, p = p, method = method)
    
    # previsões
    backcst <- head(na.omit(now$yfcst[,3]),1)
    nowcst <- tail(head(na.omit(now$yfcst[,3]),2),1)
    forecst <- tail(head(na.omit(now$yfcst[,3]),3),1)
    
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
    brgdpAS <- ts(c(na.omit(month2qtr(base0[[i]][,"serie22109"])),tail(final(m),3)), end = end(final(m)), freq = 4)
    
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


nowpast.plot <- function(out){
  
  par(mar = c(2,2,2,2), mfrow = c(2,2))
  
  nivel <- out$nivel
  varQ <- out$varQ
  varA <- out$varA
  acum4 <- out$acum4
  
  # nível
  nivel0 <- window(nivel, end = end(na.omit(pib)), freq = 4)
  a <- barplot(c(window(pib, start = c(2015,4), freq = 4)), ylim = c(0,200), main = "nivel")
  points(x = rep(a[1,1],10), y = c(nivel0[1,1:10]), type = "l", col = "red")
  points(x = a[1,1], y = nivel0[1,10], pch = 19)
  points(x = rep(a[2,1],23), y = c(nivel0[2,1:23]), type = "l", col = "red")
  points(x = a[2,1], y = nivel0[2,23], pch = 19)
  points(x = rep(a[3,1],23), y = c(nivel0[3,1:23]), type = "l", col = "red")
  points(x = a[3,1], y = nivel0[3,23], pch = 19)
  points(x = rep(a[4,1],39), y = c(nivel0[4,11:49]), type = "l", col = "red")
  points(x = a[4,1], y = nivel0[4,49], pch = 19)
  points(x = rep(a[5,1],39), y = c(nivel0[5,11:49]), type = "l", col = "red")
  points(x = a[5,1], y = nivel0[5,49], pch = 19)
  points(x = rep(a[6,1],39), y = c(nivel0[6,11:49]), type = "l", col = "red")
  points(x = a[6,1], y = nivel0[6,49], pch = 19)
  points(x = rep(a[7,1],39), y = c(nivel0[7,50:88]), type = "l", col = "red")
  points(x = a[7,1], y = nivel0[7,88], pch = 19)
  points(x = rep(a[8,1],39), y = c(nivel0[8,50:88]), type = "l", col = "red")
  points(x = a[8,1], y = nivel0[8,88], pch = 19)
  
  # variação trimestral (trimestre imediatamente anterior)
  varQ0 <- window(varQ, end = end(na.omit(pib)), freq = 4)
  a <- barplot(c(window(pibVarQ, start = c(2015,4), freq = 4)), ylim = c(-5,5), main = "varQ")
  points(x = rep(a[1,1],10), y = c(varQ0[1,1:10]), type = "l", col = "red")
  points(x = a[1,1], y = varQ0[1,10], pch = 19)
  points(x = rep(a[2,1],23), y = c(varQ0[2,1:23]), type = "l", col = "red")
  points(x = a[2,1], y = varQ0[2,23], pch = 19)
  points(x = rep(a[3,1],23), y = c(varQ0[3,1:23]), type = "l", col = "red")
  points(x = a[3,1], y = varQ0[3,23], pch = 19)
  points(x = rep(a[4,1],39), y = c(varQ0[4,11:49]), type = "l", col = "red")
  points(x = a[4,1], y = varQ0[4,49], pch = 19)
  points(x = rep(a[5,1],39), y = c(varQ0[5,11:49]), type = "l", col = "red")
  points(x = a[5,1], y = varQ0[5,49], pch = 19)
  points(x = rep(a[6,1],39), y = c(varQ0[6,11:49]), type = "l", col = "red")
  points(x = a[6,1], y = varQ0[6,49], pch = 19)
  points(x = rep(a[7,1],39), y = c(varQ0[7,50:88]), type = "l", col = "red")
  points(x = a[7,1], y = varQ0[7,88], pch = 19)
  points(x = rep(a[8,1],39), y = c(varQ0[8,50:88]), type = "l", col = "red")
  points(x = a[8,1], y = varQ0[8,88], pch = 19)
  
  # variação
  varA0 <- window(varA, end = end(na.omit(pib)), freq = 4)
  a <- barplot(c(window(pibVarA, start = c(2015,4), freq = 4)), ylim = c(-10,5), main = "varA")
  points(x = rep(a[1,1],10), y = c(varA0[1,1:10]), type = "l", col = "red")
  points(x = a[1,1], y = varA0[1,10], pch = 19)
  points(x = rep(a[2,1],23), y = c(varA0[2,1:23]), type = "l", col = "red")
  points(x = a[2,1], y = varA0[2,23], pch = 19)
  points(x = rep(a[3,1],23), y = c(varA0[3,1:23]), type = "l", col = "red")
  points(x = a[3,1], y = varA0[3,23], pch = 19)
  points(x = rep(a[4,1],39), y = c(varA0[4,11:49]), type = "l", col = "red")
  points(x = a[4,1], y = varA0[4,49], pch = 19)
  points(x = rep(a[5,1],39), y = c(varA0[5,11:49]), type = "l", col = "red")
  points(x = a[5,1], y = varA0[5,49], pch = 19)
  points(x = rep(a[6,1],39), y = c(varA0[6,11:49]), type = "l", col = "red")
  points(x = a[6,1], y = varA0[6,49], pch = 19)
  points(x = rep(a[7,1],39), y = c(varA0[7,50:88]), type = "l", col = "red")
  points(x = a[7,1], y = varA0[7,88], pch = 19)
  points(x = rep(a[8,1],39), y = c(varA0[8,50:88]), type = "l", col = "red")
  points(x = a[8,1], y = varA0[8,88], pch = 19)
}

nowpast.error <- function(out, y, yAS){
  
  # y <- month2qtr(base[,"serie22099"])
  # yAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)
  # out <- nowpast11
  
  nivel <- out$nivel
  varQ <- out$varQ
  varA <- out$varA
  acum4 <- out$acum4
  
  yVarQ <- (yAS/lag(yAS,-1)-1)*100
  yVarA <- (y/lag(y,-4)-1)*100
  
  y <- window(y, start = start(nivel), end = end(nivel), freq = 4)
  yVarQ <- window(yVarQ, start = start(nivel), end = end(nivel), freq = 4)
  yVarA <- window(yVarA, start = start(nivel), end = end(nivel), freq = 4)
  
  rmse_nivel <- matrix(NA, nrow = length(y), ncol = 1)
  rmse_varQ <- matrix(NA, nrow = length(y), ncol = 1)
  rmse_varA <- matrix(NA, nrow = length(y), ncol = 1)
  
  for(i in 1:length(y)){
    rmse_nivel[i,1] <- sqrt(mean((nivel[i,] - y[i])^2, na.rm = T))
    rmse_varQ[i,1] <- sqrt(mean((varQ[i,] - yVarQ[i])^2, na.rm = T))
    rmse_varA[i,1] <- sqrt(mean((varA[i,] - yVarA[i])^2, na.rm = T))
    
  }
  rmse_nivel <- ts(rmse_nivel, start = start(y), freq = 4)
  rmse_varQ <- ts(rmse_varQ, start = start(y), freq = 4)
  rmse_varA <- ts(rmse_varA, start = start(y), freq = 4)
  
  # output
  na.omit(cbind(nivel = rmse_nivel, varQ = rmse_varQ, varA = rmse_varA))
  
  
  
}