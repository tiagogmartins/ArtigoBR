

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
PIBfocus<- data.frame(read_excel("./scripts/vintages/Séries de estatísticas.xls",skip = 1))[,-1]
nowpastSM10 <- readRDS("./scripts/vintages/nowpastSM10IAE4.rds")
# GRÁFICO DENTRO DE CADA TRISMESTRE --------------------------

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

par(mar = c(3,3,3,2), mfrow = c(2,2))

varA0 <- data.frame(t(window(varA, end = end(na.omit(y)), freq = 4)))
colnames(varA0) <- as.Date(window(varA, end = end(na.omit(y)), freq = 4))

titulos <- substr(colnames(varA0),1,7)
titulos <- gsub("-01","-Q1",titulos)
titulos <- gsub("-04","-Q2",titulos)
titulos <- gsub("-07","-Q3",titulos)
titulos <- gsub("-10","-Q4",titulos)

for(i in c(11,12,13,14)){
  plot(y = tail(varA0[!is.na(varA0[,i]),i],28), x = 1:28, type = "o", xaxt = "n", 
       #ylim = c(min(c(y[i],varA0[,i]), na.rm = T)-2, max(c(y[i],varA0[,i]), na.rm = T)),
       col = "steelblue", lwd = 2, main = paste0(titulos[i],"\nVariação Anual"), ylab = "")
  points(y=tail(na.omit(PIBfocus[,i-10]),28),x=1:28)
  abline(h = yVarA[i], lty = 3, col = "#CD0000")
  axis(1, labels = tail(rownames(varA0)[!is.na(varA0[,i])],28), at = 1:28)
  legend("bottomright",legend = c("modelo","Focus","Valor real PIB"),lty = c(1,NA,3), pch=c(1,1,NA), col=c("steelblue",1,2),bty = "n",lwd = c(2,1,1))
}

