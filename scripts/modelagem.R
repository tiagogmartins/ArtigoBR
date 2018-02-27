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

# BRGDP (estacionário)
brgdp <- month2qtr(base[,"serie22099"])
brgdpEst <- diff(diff(brgdp),4)
# brgdpEst2 <- (brgdp/lag(brgdp,-12) - 1) - (lag(brgdp,-1)/lag(brgdp,-13) - 1)
# plot(brgdp)
# ts.plot(diff(brgdp))
# ts.plot((brgdp/lag(brgdp,-1)-1)*100)
# ts.plot(diff(brgdp),(brgdp/lag(brgdp,-1)-1)*100)
# ts.plot(brgdpEst)
# ts.plot(brgdpEst2)

# base X
brgdp_position <- which(colnames(base) == "serie22099")
X <- base[,-brgdp_position]

# painel balanceado
XB <- Bpanel(X, trans = legenda$transf, aggregate = T)
# séries que foram excluídas da base
colnames(X)[!colnames(X) %in% colnames(XB)]

# modelagem PIB
now <- nowcast(y = brgdpEst, x = XB, q = 2, r = 2, p = 2, method = "2sq")


summary(now$reg)
nowcast.plot(now, type = "fcst")
nowcast.plot(now, type = "eigenvalues")
nowcast.plot(now, type = "eigenvectors")
nowcast.plot(now, type = "factors")

# previsão próximo trimestre
fcst <- head(na.omit(now$yfcst[,3]),1)
fcst_nivel <- fcst + c(tail(na.omit(lag(brgdp,-1) + lag(brgdp,-4) - lag(brgdp,-5)),1))
(fcst_nivel/lag(brgdp,-4) - 1)*100

# novo pib
brgdpNovo <- ts(c(na.omit(brgdp),fcst_nivel), start = start(na.omit(brgdp)), freq = 4)
brgdpNovo <- ts(c(96.84,100.12,107.56,104.31,100.13,104.88,109.49,108.21,101.14,106.46,109.88,106.66,101.92,106.04,109.20,108.97,
                  brgdpNovo), end = end(brgdpNovo), freq = 4)
# ajuste sazonal
m <- seas(brgdpNovo, transform.function = "auto", regression.aictest = c("td","easter"), 
          outlier.types = "all", x11 = "", pickmdl.method = "best", pickmdl.identify = "all",
          forecast.maxlead = 6, forecast.maxback = 0)
summary(m)
qs(m)
brgdpAS <- final(m)

# previsão: variação trimestral (trimestre imediatamente anterior)
(brgdpAS/lag(brgdpAS,-1)-1)*100

# previsão: acumulado no ano
(mean(tail(brgdpNovo,4))/mean(head(tail(brgdpNovo,8),4))-1)*100



# selecting and transforming y  


# selecting and transforming x 
trans <- legenda$transf[-brgdp_position]
stationaryBase <- cbind(X[,trans == 0], X[,trans == 1]/lag(X[,trans == 1], k = -1) - 1, diff(X[,trans == 2]),
                        (X[,trans == 3]/lag(X[,trans == 3], k = -12) - 1) - (lag(X[,trans == 3],-1)/lag(X[,trans == 3], k = -13) - 1),
                        diff(X[,trans == 4],12) - diff(lag(X,-1)[,trans == 4],12)
                        )
colnames(stationaryBase) <- colnames(X)[c(which(trans == 0),which(trans == 1),which(trans == 2),which(trans == 3),which(trans == 4)) ]
stationaryBase <- stationaryBase[,colnames(X)]
nowEM <- nowcast(y = brgdpEst, x = stationaryBase, q = 3, r = 3, p = 1, method = "EM")


# # X estacionário para o EM
# trans <- legenda[-brgdp_position,"transf"]
# XEst <- cbind(X[,trans == 0], X[,trans == 1]/lag(X[,trans == 1], k = -1) - 1, diff(X[,trans == 2]),
#               (X[,trans == 3]/lag(X[,trans == 3], k = -12) - 1) - (lag(X[,trans == 3],-1)/lag(X[,trans == 3], k = -13) - 1),
#               diff(X[,trans == 4],12) - diff(lag(X,-1)[,trans == 4],12)
# )
# colnames(XEst) <- colnames(X)[c(which(trans == 0),which(trans == 1),which(trans == 2),which(trans == 3),which(trans == 4)) ]
# XEst <- XEst[,colnames(X)]
# nowEM <- nowcast(y = brgdpEst, x = XEst[,1:130], q = 1, r = 1, p = 1, method = "EM")