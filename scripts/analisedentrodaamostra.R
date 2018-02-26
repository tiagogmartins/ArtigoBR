# diretório
setwd("./scripts/")

# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)

# base
base <- ts(read_excel("base2000.xlsx")[,-1], start = c(2000,1), freq = 12)
legenda <- data.frame(read_excel("base2000.xlsx", sheet = 2)  )
pib <- month2qtr(base[,"serie22099"])
pibAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)
pibVarQ <- (pibAS/lag(pibAS,-1)-1)*100
pibVarA <- (pib/lag(pib,-4)-1)*100
# fazer previsão do PIB toda sexta-feira do ano
# aproximadamente 4 vezes por mês
# guardar previsões do trimestre atual, anterior e próximo

datas <- seq.Date(as.Date("2016-01-01"),as.Date("2017-12-31"), by = 7)

# base0 é a base cortada para uma data específica
base0 <- NULL

# matriz para salvar os resultados das previsões
acum4 <- NULL # acumulado 4 trimestres
varQ <- NULL # variação trimestral
varA <- NULL # variação trimestre do ano anterior
nivel <- NULL # pib em nível


for(i in 1:length(datas)){
  base0 <- PRTDB(mts = base, vintage = datas[i], delay = legenda$delay.em.semanas.depois.do.fim.no.período*7)
  
  # BRGDP (estacionário)
  brgdp <- month2qtr(base0[,"serie22099"])
  brgdpEst <- diff(diff(brgdp),4)
  
  # base X
  brgdp_position <- which(colnames(base0) == "serie22099")
  X <- base0[,-brgdp_position]
  
  # painel balanceado
  XB <- Bpanel(X, trans = legenda$transf, aggregate = T)
  
  # modelagem PIB
  now <- nowcast(y = brgdpEst, x = XB, q = 1, r = 1, p = 1, method = "2sq")
  
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
  brgdpAS <- ts(c(na.omit(month2qtr(base0[,"serie22109"])),tail(final(m),3)), end = end(final(m)), freq = 4)
  
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

View(varQ)
View(varA)
View(acum4)
View(nivel)

# nível
nivel0 <- window(nivel, end = end(na.omit(pib)), freq = 4)
a <- barplot(c(window(pib, start = c(2015,4), freq = 4)), ylim = c(0,200))
a
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
a <- barplot(c(window(pibVarQ, start = c(2015,4), freq = 4)), ylim = c(-5,5))
a
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
#barplot(c(varQ[6,]))

# variação
varA0 <- window(varA, end = end(na.omit(pib)), freq = 4)
a <- barplot(c(window(pibVarA, start = c(2015,4), freq = 4)), ylim = c(-10,5))
a
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


barplot(nivel[6,], ylim = c(100,200))

rmseNivel <- abs(nivel - pib)

for(i in 1:8){
  barplot(c(rmseNivel[i,]))
  Sys.sleep(1)
}

abline(h = 161.72)

