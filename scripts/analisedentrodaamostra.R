# diretório
setwd("./scripts/")

# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)
library(zoo)
source('nowpast.R', encoding = 'UTF-8')

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

datas <- seq.Date(as.Date("2008-01-04"),as.Date("2017-12-31"), by = 7)

# nowpastSQ1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 1, p = 1, q = 1, aggregate = T, method = "2sq")
# nowpastSM1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 1, p = 1, q = 1, aggregate = F, method = "2sm")
# # nowpastEM1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                     r = 1, p = 1, q = 1, aggregate = T, method = "EM")
# 
# 
# nowpastSQ2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 2, p = 2, q = 1, aggregate = T, method = "2sq")
# nowpastSM2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 2, p = 2, q = 1, aggregate = F, method = "2sm")
# # nowpastEM2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                     r = 2, p = 2, q = 1, aggregate = T, method = "EM")
# 
# 
# nowpastSQ3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 3, p = 3, q = 1, aggregate = T, method = "2sq")
# nowpastSM3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 3, p = 3, q = 1, aggregate = F, method = "2sm")
# # nowpastEM3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                     r = 3, p = 3, q = 1, aggregate = T, method = "EM")
# 
# 
# nowpastSQ4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 2, p = 1, q = 2, aggregate = T, method = "2sq")
# nowpastSM4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 2, p = 1, q = 2, aggregate = F, method = "2sm")
# # nowpastEM4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                     r = 2, p = 1, q = 2, aggregate = T, method = "EM")
# 
# 
# nowpastSQ5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 1, q = 3, aggregate = T, method = "2sq")
# nowpastSM5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 3, p = 1, q = 3, aggregate = F, method = "2sm")
# # nowpastEM5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                     r = 3, p = 1, q = 3, aggregate = F, method = "EM")
# 
# 
# nowpastSQ6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                     r = 3, p = 1, q = 2, aggregate = T, method = "2sq")
# nowpastSM6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 1, q = 2, aggregate = F, method = "2sm")
# # nowpastEM6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                      r = 3, p = 1, q = 2, aggregate = F, method = "Em")
# 
# 
# nowpastSQ7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 1, q = 1, aggregate = T, method = "2sq")
# nowpastSM7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 1, q = 1, aggregate = F, method = "2sm")
# # nowpastEM7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                      r = 3, p = 1, q = 1, aggregate = F, method = "EM")
# 
# 
# nowpastSQ8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 2, q = 2, aggregate = T, method = "2sq")
# nowpastSM8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 2, q = 2, aggregate = F, method = "2sm")
# # nowpastEM8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                      r = 3, p = 2, q = 2, aggregate = F, method = "EM")
# 
# 
# nowpastSQ9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 3, q = 3, aggregate = T, method = "2sq")
# nowpastSM9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 3, p = 3, q = 3, aggregate = F, method = "2sm")
# # nowpastEM9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                      r = 3, p = 3, q = 3, aggregate = F, method = "EM")
# 
# 
# nowpastSQ10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 2, p = 2, q = 2, aggregate = T, method = "2sq")
# nowpastSM10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 2, p = 2, q = 2, aggregate = F, method = "2sm")
# # nowpastEM10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                      r = 2, p = 2, q = 2, aggregate = F, method = "EM")
# 
# 
# nowpastSQ11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 2, p = 1, q = 1, aggregate = T, method = "2sq")
# nowpastSM11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
#                      r = 2, p = 1, q = 1, aggregate = F, method = "2sm")
# #nowpastEM11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
# #                     r = 2, p = 1, q = 1, aggregate = F, method = "EM")

# saveRDS(nowpastSQ1,"nowpastSQ1.rds")
# saveRDS(nowpastSQ2,"nowpastSQ2.rds")
# saveRDS(nowpastSQ3,"nowpastSQ3.rds")
# saveRDS(nowpastSQ4,"nowpastSQ4.rds")
# saveRDS(nowpastSQ5,"nowpastSQ5.rds")
# saveRDS(nowpastSQ6,"nowpastSQ6.rds")
# saveRDS(nowpastSQ7,"nowpastSQ7.rds")
# saveRDS(nowpastSQ8,"nowpastSQ8.rds")
# saveRDS(nowpastSQ9,"nowpastSQ9.rds")
# saveRDS(nowpastSQ10,"nowpastSQ10.rds")
# saveRDS(nowpastSQ11,"nowpastSQ11.rds")
# saveRDS(nowpastSM1,"nowpastSM1.rds")
# saveRDS(nowpastSM2,"nowpastSM2.rds")
# saveRDS(nowpastSM3,"nowpastSM3.rds")
# saveRDS(nowpastSM4,"nowpastSM4.rds")
# saveRDS(nowpastSM5,"nowpastSM5.rds")
# saveRDS(nowpastSM6,"nowpastSM6.rds")
# saveRDS(nowpastSM7,"nowpastSM7.rds")
# saveRDS(nowpastSM8,"nowpastSM8.rds")
# saveRDS(nowpastSM9,"nowpastSM9.rds")
# saveRDS(nowpastSM10,"nowpastSM10.rds")
# saveRDS(nowpastSM11,"nowpastSM11.rds")


nowpastSQ1 <- readRDS("nowpastSQ1.rds")
nowpastSQ2 <- readRDS("nowpastSQ2.rds")
nowpastSQ3 <- readRDS("nowpastSQ3.rds")
nowpastSQ4 <- readRDS("nowpastSQ4.rds")
nowpastSQ5 <- readRDS("nowpastSQ5.rds")
nowpastSQ6 <- readRDS("nowpastSQ6.rds")
nowpastSQ7 <- readRDS("nowpastSQ7.rds")
nowpastSQ8 <- readRDS("nowpastSQ8.rds")
nowpastSQ9 <- readRDS("nowpastSQ9.rds")
nowpastSQ10 <- readRDS("nowpastSQ10.rds")
nowpastSQ11 <- readRDS("nowpastSQ11.rds")
nowpastSM1 <- readRDS("nowpastSM1.rds")
nowpastSM2 <- readRDS("nowpastSM2.rds")
nowpastSM3 <- readRDS("nowpastSM3.rds")
nowpastSM4 <- readRDS("nowpastSM4.rds")
nowpastSM5 <- readRDS("nowpastSM5.rds")
nowpastSM6 <- readRDS("nowpastSM6.rds")
nowpastSM7 <- readRDS("nowpastSM7.rds")
nowpastSM8 <- readRDS("nowpastSM8.rds")
nowpastSM9 <- readRDS("nowpastSM9.rds")
nowpastSM10 <- readRDS("nowpastSM10.rds")
nowpastSM11 <- readRDS("nowpastSM11.rds")


nowpast.plot(nowpastSQ1, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM1, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM1,  y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ2,  y = pib, yAS = pibAS)
nowpast.plot(nowpastSM2,  y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM2,  y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ3,  y = pib, yAS = pibAS)
nowpast.plot(nowpastSM3,  y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM3, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ4,  y = pib, yAS = pibAS)
nowpast.plot(nowpastSM4, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM4, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ5, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM5, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM5, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ6, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM6, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM6, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ7, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM7, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM7, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ8, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM8, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM8, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ9, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM9, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM9, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ10, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM10, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM10, y = pib, yAS = pibAS)
nowpast.plot(nowpastSQ11, y = pib, yAS = pibAS)
nowpast.plot(nowpastSM11, y = pib, yAS = pibAS)
#nowpast.plot(nowpastEM11, y = pib, yAS = pibAS)

colMeans(nowpast.error(out = nowpastSQ1, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM1, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM1, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ2, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM2, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM2, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ3, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM3, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM3, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ4, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM4, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM4, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ5, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM5, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM5, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ6, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM6, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM6, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ7, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM7, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM7, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ8, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM8, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM8, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ9, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM9, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM9, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ10, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM10, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM10, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpastSQ11, y = pib, yAS = pibAS)$trimestral)
colMeans(nowpast.error(out = nowpastSM11, y = pib, yAS = pibAS)$trimestral)
#colMeans(nowpast.error(out = nowpastEM11, y = pib, yAS = pibAS))

colMeans(nowpast.error(out = nowpastSM8, y = pib, yAS = pibAS)$trimestral)
mean(nowpast.error(out = nowpastSM8, y = pib, yAS = pibAS)$acumAno)
nowpast.plot(nowpastSM8, y = pib, yAS = pibAS)
nowpast.plot2(nowpastSM8, y = pib, yAS = pibAS, type = 1)
nowpast.plot2(nowpastSM8, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastSM8, y = pib, yAS = pibAS, type = 3)

colMeans(nowpast.error(out = nowpastSM9, y = pib, yAS = pibAS)$trimestral)
mean(nowpast.error(out = nowpastSM9, y = pib, yAS = pibAS)$acumAno)
nowpast.plot(nowpastSM9, y = pib, yAS = pibAS)
nowpast.plot2(nowpastSM9, y = pib, yAS = pibAS, type = 1)
nowpast.plot2(nowpastSM9, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastSM9, y = pib, yAS = pibAS, type = 3)

colMeans(nowpast.error(out = nowpastSM10, y = pib, yAS = pibAS)$trimestral)
mean(nowpast.error(out = nowpastSM10, y = pib, yAS = pibAS)$acumAno)
nowpast.plot(nowpastSM10, y = pib, yAS = pibAS)
nowpast.plot2(nowpastSM10, y = pib, yAS = pibAS, type = 1)
nowpast.plot2(nowpastSM10, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastSM10, y = pib, yAS = pibAS, type = 3)




# MODELO SM10 GRÁFICOS SEPARADOS ---------------

out <- nowpastSM10
y <- pib
yAS <- pibAS
yVarQ <- (yAS/lag(yAS,-1)-1)*100
yVarA <- (y/lag(y,-4)-1)*100
ydata <- data.frame(y, ano = as.numeric(substr(as.Date(y),1,4)))
ydata <- ydata[ydata$ano %in% as.numeric(names(table(ydata$ano)[table(ydata$ano) == 4])),]
yMean <- aggregate(ydata$y, by = list(ydata$ano), FUN = mean, na.rm = F)
yMean <- ts(yMean$x, start = yMean$Group.1[1], freq = 1)
yVarAcum <- na.omit((yMean/lag(yMean,-1)-1)*100)

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

# nível
nivel0 <- data.frame(t(window(nivel, end = end(na.omit(y)), freq = 4)))
a <- barplot(c(na.omit(y)), ylim = c(0,200), main = "", border = "steelblue", col = "skyblue") #  names.arg = names,
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

varA0 <- data.frame(t(window(varA, start = start(yVarA), end = end(na.omit(y)), freq = 4)))
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
