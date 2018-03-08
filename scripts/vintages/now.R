now <- function(base, trans, aggregate, method, r, p, q, yAS = NULL){
  options(warn=-1)
  
  # BRGDP (estacionário)
  brgdp <- month2qtr(base[,"serie22099"])
  brgdpEst <- diff(diff(brgdp),4)
  
  # base X
  brgdp_position <- which(colnames(base) == "serie22099")
  X <- base[,-brgdp_position]
  
  if(method == "EM"){
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
  }else{
    XB <- Bpanel(X, trans = trans, aggregate = aggregate) 
    # séries que foram excluídas da base
    st_fora <- colnames(X)[!colnames(X) %in% colnames(XB)]
    message(length(st_fora), " séries não foram utilizadas:")
    message(paste0(st_fora, collapse = ", "))
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
  nivel <- ts(c(backcstNivel,nowcstNivel, forecstNivel), start = start(backcstNivel), freq = 4)
  
  
  # novo pib
  brgdpNovo <- ts(c(na.omit(brgdp), backcstNivel, nowcstNivel, forecstNivel), start = start(na.omit(brgdp)), freq = 4)
  brgdpNovo <- ts(c(96.84,100.12,107.56,104.31,100.13,104.88,109.49,108.21,101.14,106.46,109.88,106.66,101.92,106.04,109.20,108.97,
                    brgdpNovo), end = end(brgdpNovo), freq = 4)
  # ajuste sazonal
  m <- seas(brgdpNovo, transform.function = "auto", regression.aictest = c("td","easter"), 
            outlier.types = "all", x11 = "", pickmdl.method = "best", pickmdl.identify = "all",
            forecast.maxlead = 6, forecast.maxback = 0)
  brgdpAS <- tryCatch(ts(c(na.omit(month2qtr(base[,"serie22109"])),tail(final(m),3)), end = end(final(m)), freq = 4),
                      error = function(e) ts(c(na.omit(yAS),tail(final(m),3)), end = end(final(m)), freq = 4))
  
  # previsão: variação trimestral (trimestre imediatamente anterior)
  brgdpVarQ <- tail((brgdpAS/lag(brgdpAS,-1)-1)*100,3)
 
  
  # previsão: acumulado quatro trimestres
  brgdpAcum <- ts(c((mean(head(tail(brgdpNovo,6),4))/mean(head(tail(brgdpNovo,10),4))-1)*100,
                    (mean(head(tail(brgdpNovo,5),4))/mean(head(tail(brgdpNovo,9),4))-1)*100,
                    (mean(tail(brgdpNovo,4))/mean(head(tail(brgdpNovo,8),4))-1)*100), start = start(backcstNivel), freq = 4)
  
  # previsão anual
  brgdpVarA <- ts(c(backcstNivel/lag(brgdp,-4)-1, nowcstNivel/lag(brgdp,-4)-1, forecstNivel/lag(brgdp,-4)-1),
                  start = start(backcstNivel), freq = 4)*100


  options(warn=0)
  # output
  na.omit(cbind(nivel = nivel, nivelAS = brgdpAS, varQ = brgdpVarQ, varA = brgdpVarA, acum4 = brgdpAcum))

}