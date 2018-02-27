# diretório
setwd("./scripts/")

# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)
library(zoo)
source('C:/Users/daiane.mattos/Dropbox/Github/ArtigoBR/scripts/nowpast.R', encoding = 'UTF-8')

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

nowpast1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 1, p = 1, q = 1, aggregate = T, method = "2sq")
nowpast.plot(nowpast1)

nowpast2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 2, q = 1, aggregate = T, method = "2sq")
nowpast.plot(nowpast2)

nowpast3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 3, q = 1, aggregate = T, method = "2sq")
nowpast.plot(nowpast3)

nowpast4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 1, p = 1, q = 1, aggregate = F, method = "2sm")
nowpast.plot(nowpast4)

nowpast5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 2, q = 1, aggregate = F, method = "2sm")
nowpast.plot(nowpast5)

nowpast6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 3, q = 1, aggregate = F, method = "2sm")
nowpast.plot(nowpast6)

nowpast7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 1, q = 2, aggregate = F, method = "2sm")
nowpast.plot(nowpast7)

nowpast8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 1, q = 3, aggregate = F, method = "2sm")
nowpast.plot(nowpast8)

nowpast9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 1, q = 2, aggregate = T, method = "2sq")
nowpast.plot(nowpast9)

nowpast10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 1, q = 3, aggregate = T, method = "2sq")
nowpast.plot(nowpast10)

nowpast11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 1, q = 2, aggregate = F, method = "2sm")
nowpast.plot(nowpast11)

nowpast12 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 1, aggregate = F, method = "2sm")
nowpast.plot(nowpast12)

nowpast13 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 2, q = 2, aggregate = F, method = "2sm")
nowpast.plot(nowpast13)

nowpast14 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 3, q = 3, aggregate = F, method = "2sm")
nowpast.plot(nowpast14)

nowpast15 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 2, q = 2, aggregate = F, method = "2sm")
nowpast.plot(nowpast15)

nowpast16 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 1, q = 1, aggregate = F, method = "2sm")
nowpast.plot(nowpast16)


nowpast.plot(nowpast4)
nowpast.plot(nowpast5)
nowpast.plot(nowpast6)
nowpast.plot(nowpast7)
nowpast.plot(nowpast8)
nowpast.plot(nowpast11)
nowpast.plot(nowpast12)
nowpast.plot(nowpast13)
nowpast.plot(nowpast14)
nowpast.plot(nowpast15)
nowpast.plot(nowpast16)

colMeans(nowpast.error(out = nowpast4, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast5, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast6, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast7, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast8, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast11, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast12, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast13, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast14, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast15, y = pib, yAS = pibAS))
colMeans(nowpast.error(out = nowpast16, y = pib, yAS = pibAS))



View(varQ)
View(varA)
View(acum4)
View(nivel)





na.omit(data.frame(varA0[4,]))

"2016 Q3"

View(base0[[which(datas == as.Date("2016-06-03"))]])
View(base0[[which(datas == as.Date("2016-06-10"))]])

tail(base0[[which(datas == as.Date("2016-06-03"))]][,51:60])
tail(base0[[which(datas == as.Date("2016-06-10"))]][,51:60])

barplot(nivel[6,], ylim = c(100,200))

rmseNivel <- abs(nivel - pib)

for(i in 1:8){
  barplot(c(rmseNivel[i,]))
  Sys.sleep(1)
}

abline(h = 161.72)

