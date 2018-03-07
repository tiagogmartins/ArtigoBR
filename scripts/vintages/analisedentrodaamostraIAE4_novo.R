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
pibAcumAno <- ts(as.numeric(as.character(read.csv2("./scripts/vintages/PIB_AcumuladoAno.csv")[,-1])), start = c(1996), freq = 1)

# fazer previsão do PIB toda sexta-feira do ano
# aproximadamente 4 vezes por mês
# guardar previsões do trimestre atual, anterior e próximo

datas <- seq.Date(as.Date("2014-11-07"),as.Date("2017-12-31"), by = 7)
           
# nowpastSM10 <- nowpast(datas = datas, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf,
#                       r = 2, p = 2, q = 2, aggregate = F, method = "2sm")
# 
# saveRDS(nowpastSM10,"./scripts/vintages/nowpastSM10IAE4.rds")


nowpastSM10 <- readRDS("./scripts/vintages/nowpastSM10IAE4.rds")

nowpast.plot(nowpastSM10, y = pibs[,"pib"], yAS = pibs[,"pibAS"],
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcumAno)
nowpast.plot2(nowpastSM10, y = pibs[,"pib"], yAS = pibs[,"pibAS"],
             yVarA = pibs[,"pibVarA"], yVarQ = pibs[,"pibVarQ"], yAcumAno = pibAcumAno, type = 2)


colMeans(nowpast.error(out = nowpastSM10, y = pib, yAS = pibAS)$trimestral)
mean(nowpast.error(out = nowpastSM10, y = pib, yAS = pibAS)$acumAno)
nowpast.plot(nowpastSM10, y = pib, yAS = pibAS)
nowpast.plot2(nowpastSM10, y = pib, yAS = pibAS, type = 1)
nowpast.plot2(nowpastSM10, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastSM10, y = pib, yAS = pibAS, type = 3)



# > colMeans(nowpast.error(out = nowpastSM10, y = pib, yAS = pibAS)$trimestral)
# nivel     varQ     varA 
# 3.323588 1.809466 2.096761 

# MODELO SM10 GRÁFICOS SEPARADOS ---------------

par(mfrow = c(1,3), mar = c(4,2,3,2))
out <- nowpastSM10
y <- pib
yAS <- pibAS
yVarQ <- pibVarQ2#(yAS/lag(yAS,-1)-1)*100
yVarA <- pibVarA2#(y/lag(y,-4)-1)*100 
ydata <- data.frame(y, ano = as.numeric(substr(as.Date(y),1,4)))
ydata <- ydata[ydata$ano %in% as.numeric(names(table(ydata$ano)[table(ydata$ano) == 4])),]
yMean <- aggregate(ydata$y, by = list(ydata$ano), FUN = mean, na.rm = F)
yMean <- ts(yMean$x, start = yMean$Group.1[1], freq = 1)
yVarAcum <- na.omit((yMean/lag(yMean,-1)-1)*100)

nivel <- window(out$nivel, start = c(2015,1), freq = 4)
varQ <- window(out$varQ, start = c(2015,1), freq = 4)
varA <- window(out$varA, start = c(2015,1), freq = 4)
acum4 <- window(out$acum4, start = c(2015,1), freq = 4)

y <- window(y, start = start(nivel), end = end(nivel), freq = 4)
yVarQ <- window(yVarQ, start = start(nivel), end = end(nivel), freq = 4)
yVarA <- window(yVarA, start = start(nivel), end = end(nivel), freq = 4)

names <- substr(as.Date(na.omit(y)),1,7)
names <- gsub("-01","-Q1",names)
names <- gsub("-04","-Q2",names)
names <- gsub("-07","-Q3",names)
names <- gsub("-10","-Q4",names)

# nível
nivel0 <- data.frame(t(window(nivel, end = end(na.omit(y)), freq = 4)))
a <- barplot(c(na.omit(y)), ylim = c(0,200), main = "Nível", border = "steelblue", col = "skyblue") #  names.arg = names,
text(a,-8,  srt = 60, adj= 1, xpd = TRUE,
     labels = names, cex=0.9)
for(i in 1:ncol(nivel0)){
  points(x = rep(a[i,1],sum(!is.na(nivel0[,i]))), y = nivel0[!is.na(nivel0[,i]),i], type = "l", col = "#CD0000")
  points(x = a[i,1], y = nivel0[max(which(!is.na(nivel0[,i]))),i], pch = 19, col = "#CD0000")
}

# variação trimestral (trimestre imediatamente anterior)
varQ0 <- data.frame(t(window(varQ, end = end(na.omit(y)), freq = 4)))
a <- barplot(c(na.omit(yVarQ)), ylim = c(min(varQ0, na.rm = T) -2, max(varQ0, na.rm = T) + 2), main = "Variação Trimestral\n(trimestre imediatamente anterior)", border = "steelblue", col = "skyblue")
text(a,min(varQ0, na.rm = T) -2,  srt = 60, adj= 1, xpd = TRUE,
     labels = names, cex=0.9)
for(i in 1:ncol(varQ0)){
  points(x = rep(a[i,1],sum(!is.na(varQ0[,i]))), y = varQ0[!is.na(varQ0[,i]),i], type = "l", col = "#CD0000")
  points(x = a[i,1], y = varQ0[max(which(!is.na(varQ0[,i]))),i], pch = 19, col = "#CD0000")
}

# variação anual
names <- substr(as.Date(na.omit(yVarA)),1,7)
names <- gsub("-01","-Q1",names)
names <- gsub("-04","-Q2",names)
names <- gsub("-07","-Q3",names)
names <- gsub("-10","-Q4",names)

varA0 <- data.frame(t(window(varA, start = start(yVarA), end = end(na.omit(yVarA)), freq = 4)))
a <- barplot(c(na.omit(yVarA)), ylim = c(min(varA0, na.rm = T) -2, max(varA0, na.rm = T) + 2), main = "Variação Anual\n(trimestre do ano anterior)", border = "steelblue", col = "skyblue")
text(a,-10,  srt = 60, adj= 1, xpd = TRUE, labels = names, cex=0.9)
for(i in 1:ncol(varA0)){
  points(x = rep(a[i,1],sum(!is.na(varA0[,i]))), y = varA0[!is.na(varA0[,i]),i], type = "l", col = "#CD0000")
  points(x = a[i,1], y = varA0[max(which(!is.na(varA0[,i]))),i], pch = 19, col = "#CD0000")
}

# acumulado no ano
# dataAcum <- table(substr(as.Date(acum4),1,4))
# dataAcum <- as.numeric(names(dataAcum[dataAcum == 4]))
# acum40 <- window(acum4, start = c(dataAcum[1],1), freq = 4)

acum40 <- acum4[grepl("-10-",as.Date(acum4)),]
namesAcum <- substr(as.Date(acum4)[grepl("-10-",as.Date(acum4))],1,4)
yVarAcum <- window(yVarAcum, start = as.numeric(namesAcum[1]), freq = 1)
namesAcum <- namesAcum[namesAcum %in% substr(as.Date(yVarAcum),1,4)]

a <- barplot(c(yVarAcum), ylim = c(min(yVarAcum, na.rm = T) -2, max(yVarAcum, na.rm = T) + 2), 
             main = "Crescimento anual\n(média do ano contra média do ano anterior)", names.arg = namesAcum,
             border = "steelblue", col = "skyblue")

for(i in 1:length(namesAcum)){
  points(x = rep(a[i,1],sum(!is.na(acum40[i,]))), y = acum40[i,!is.na(acum40[i,])], type = "l", col = "#CD0000")
  points(x = a[i,1], y = acum40[i,max(which(!is.na(acum40[i,])))], pch = 19, col = "#CD0000")
}
