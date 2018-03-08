# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)
library(zoo)
source('./scripts/vintages/nowpast2.R', encoding = 'UTF-8')

# base
legenda <- data.frame(read_excel("./scripts/vintages/legenda.xlsx"))
pibs <- ts(read.csv2("./scripts/vintages/primeira_vintage_PIB.csv")[,-1], start = c(1996,1), freq = 4)
pibAcum <- ts(as.numeric(as.character(read.csv2("./scripts/vintages/PIB_AcumuladoAno.csv")[,-1])), start = c(1996), freq = 1)

# fazer previsão do PIB toda sexta-feira do ano
# aproximadamente 4 vezes por mês
# guardar previsões do trimestre atual, anterior e próximo

datas <- seq.Date(as.Date("2014-11-07"),as.Date("2017-12-31"), by = 7)
           
# nowpastSM1 <- nowpast(datas = datas,  delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                       r = 1, p = 1, q = 1, aggregate = F, method = "2sm")
# nowpastSM2 <- nowpast(datas = datas,  delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 2, p = 2, q = 1, aggregate = F, method = "2sm")
# nowpastSM3 <- nowpast(datas = datas, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 3, p = 3, q = 1, aggregate = F, method = "2sm")
# nowpastSM4 <- nowpast(datas = datas,  delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 2, p = 1, q = 2, aggregate = F, method = "2sm")
# nowpastSM5 <- nowpast(datas = datas,  delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 3, p = 1, q = 3, aggregate = F, method = "2sm")
# nowpastSM6 <- nowpast(datas = datas, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 3, p = 1, q = 2, aggregate = F, method = "2sm")
# nowpastSM7 <- nowpast(datas = datas,  delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 3, p = 1, q = 1, aggregate = F, method = "2sm")
# nowpastSM8 <- nowpast(datas = datas, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 3, p = 2, q = 2, aggregate = F, method = "2sm")
# nowpastSM9 <- nowpast(datas = datas,  delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 3, p = 3, q = 3, aggregate = F, method = "2sm")
# nowpastSM10 <- nowpast(datas = datas, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                        r = 2, p = 2, q = 2, aggregate = F, method = "2sm")
# nowpastSM11 <- nowpast(datas = datas, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                        r = 2, p = 1, q = 1, aggregate = F, method = "2sm")
# 
# saveRDS(nowpastSM1,"./scripts/vintages/nowpastSM1IAE4.rds")
# saveRDS(nowpastSM2,"./scripts/vintages/nowpastSM2IAE4.rds")
# saveRDS(nowpastSM3,"./scripts/vintages/nowpastSM3IAE4.rds")
# saveRDS(nowpastSM4,"./scripts/vintages/nowpastSM4IAE4.rds")
# saveRDS(nowpastSM5,"./scripts/vintages/nowpastSM5IAE4.rds")
# saveRDS(nowpastSM6,"./scripts/vintages/nowpastSM6IAE4.rds")
# saveRDS(nowpastSM7,"./scripts/vintages/nowpastSM7IAE4.rds")
# saveRDS(nowpastSM8,"./scripts/vintages/nowpastSM8IAE4.rds")
# saveRDS(nowpastSM9,"./scripts/vintages/nowpastSM9IAE4.rds")
# saveRDS(nowpastSM10,"./scripts/vintages/nowpastSM10IAE4.rds")
# saveRDS(nowpastSM11,"./scripts/vintages/nowpastSM11IAE4.rds")

nowpastSM1 <- readRDS("./scripts/vintages/nowpastSM1IAE4.rds")
nowpastSM2 <- readRDS("./scripts/vintages/nowpastSM2IAE4.rds")
nowpastSM3 <- readRDS("./scripts/vintages/nowpastSM3IAE4.rds")
nowpastSM4 <- readRDS("./scripts/vintages/nowpastSM4IAE4.rds")
nowpastSM5 <- readRDS("./scripts/vintages/nowpastSM5IAE4.rds")
nowpastSM6 <- readRDS("./scripts/vintages/nowpastSM6IAE4.rds")
nowpastSM7 <- readRDS("./scripts/vintages/nowpastSM7IAE4.rds")
nowpastSM8 <- readRDS("./scripts/vintages/nowpastSM8IAE4.rds")
nowpastSM9 <- readRDS("./scripts/vintages/nowpastSM9IAE4.rds")
nowpastSM10 <- readRDS("./scripts/vintages/nowpastSM10IAE4.rds")
nowpastSM11 <- readRDS("./scripts/vintages/nowpastSM11IAE4.rds")
         

nowpast.plot(out = nowpastSM1, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM2, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM3, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM4, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM5, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM6, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM7, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM8, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM9, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM10, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.plot(out = nowpastSM11, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)

nowpast.error(out = nowpastSM1, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM2, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM3, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM4, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM5, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM6, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM7, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM8, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
error9 <-nowpast.error(out = nowpastSM9, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
error10 <- nowpast.error(out = nowpastSM10, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)
nowpast.error(out = nowpastSM11, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum)


sqrt(mean(error9$nivel))
sqrt(mean(error9$varQ))
sqrt(mean(error9$varA))
sqrt(mean(error9$acumAno))

sqrt(mean(error10$nivel))
sqrt(mean(error10$varQ))
sqrt(mean(error10$varA))
sqrt(mean(error10$acumAno))

nowpast.plot2(nowpastSM10, y = pibs[,"pib"], yAS = pibs[,"pibAS"], 
              yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcum,
              type = 2)

# GRÁFICO DAS VARIAÇÕES --------------------------------

y = pibs[,"pib"]
yAS = pibs[,"pibAS"]
yVarA = pibs[,"pibVarA"]
yVarQ = pibs[,"pibVarQ"]
yAcumAno = pibAcum
out <- nowpastSM10

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

par(mfrow = c(1,2), mar = c(4,3,3,3))

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
# GRÁFICO DO RMSE ------------------------------

y = pibs[,"pib"]
yAS = pibs[,"pibAS"]
yVarA = pibs[,"pibVarA"]
yVarQ = pibs[,"pibVarQ"]
yAcumAno = pibAcumAno
out <- nowpastSM10

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

par(mfrow = c(1,2), mar = c(4,3,3,3))

a <- barplot(tail(rmseVarQ,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Variação Trimestral\n(trimestre imediatamente anterior)", cex.main =1.2, ylim = c(0,2))
rect(a[21,1],0,a[28,1],2,col="#F0F8FF",lty=0)
rect(a[9,1],0,a[20,1],2,col="#9BC4E2",lty=0)
rect(a[1,1],0,a[8,1],2,col="#F0F8FF",lty=0)
text(mean(a[4:5,1]),1.85, "Forecasting")
text(mean(a[14:15,1]),1.85, "Nowcasting")
text(mean(a[24:25,1]),1.85, "Backcasting")
barplot(tail(rmseVarQ,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", add = T, ylim = c(0,2))

a <- barplot(tail(rmseVarA,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", main = "Variação Anual\n(trimestre do ano anterior)", cex.main =1.2, ylim = c(0,3))
rect(a[21,1],0,a[28,1],3,col="#F0F8FF",lty=0)
rect(a[9,1],0,a[20,1],3,col="#A4D3EE",lty=0)
rect(a[1,1],0,a[8,1],3,col="#F0F8FF",lty=0)
text(mean(a[4:5,1]),2.75, "Forecasting")
text(mean(a[14:15,1]),2.75, "Nowcasting")
text(mean(a[24:25,1]),2.75, "Backcasting")
barplot(tail(rmseVarA,28), names.arg = -28:(-1), border = "#A52A2A", col = "#EEB4B4", add = T, ylim = c(0,4))


# GRÁFICO DENTRO DE CADA TRISMESTRE --------------------------

y = pibs[,"pib"]
yAS = pibs[,"pibAS"]
yVarA = pibs[,"pibVarA"]
yVarQ = pibs[,"pibVarQ"]
yAcumAno = pibAcumAno
out <- nowpastSM10

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

par(mar = c(3,3,3,2), mfrow = c(2,2))
  
nivel0 <- data.frame(t(window(nivel, end = end(na.omit(y)), freq = 4)))
colnames(nivel0) <- as.Date(window(nivel, end = end(na.omit(y)), freq = 4))
  
titulos <- substr(colnames(nivel0),1,7)
titulos <- gsub("-01","-Q1",titulos)
titulos <- gsub("-04","-Q2",titulos)
titulos <- gsub("-07","-Q3",titulos)
titulos <- gsub("-10","-Q4",titulos)
  
for(i in c(11,12,13,14)){
  plot(y = tail(nivel0[!is.na(nivel0[,i]),i],28), x = 1:28, type = "o", xaxt = "n", 
       ylim = c(min(c(y[i],nivel0[,i]), na.rm = T)-2, max(c(y[i],nivel0[,i]), na.rm = T)),
       col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nNível"), ylab = "")
  abline(h = y[i], lty = 3, col = "#CD0000")
  axis(1, labels = tail(rownames(nivel0)[!is.na(nivel0[,i])],28), at = 1:28)
}
  
