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

datas <- seq.Date(as.Date("2016-01-01"),as.Date("2017-12-31"), by = 7)

nowpastSQ1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 1, p = 1, q = 1, aggregate = T, method = "2sq")
nowpastSM1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 1, p = 1, q = 1, aggregate = F, method = "2sm")
nowpastEM1 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 1, p = 1, q = 1, aggregate = T, method = "EM")


nowpastSQ2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 2, q = 1, aggregate = T, method = "2sq")
nowpastSM2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 2, q = 1, aggregate = F, method = "2sm")
nowpastEM2 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 2, q = 1, aggregate = T, method = "EM")


nowpastSQ3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 3, q = 1, aggregate = T, method = "2sq")
nowpastSM3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 3, q = 1, aggregate = F, method = "2sm")
nowpastEM3 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 3, q = 1, aggregate = T, method = "EM")


nowpastSQ4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 1, q = 2, aggregate = T, method = "2sq")
nowpastSM4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 1, q = 2, aggregate = F, method = "2sm")
nowpastEM4 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 2, p = 1, q = 2, aggregate = T, method = "EM")


nowpastSQ5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 3, aggregate = T, method = "2sq")
nowpastSM5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 1, q = 3, aggregate = F, method = "2sm")
nowpastEM5 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 1, q = 3, aggregate = F, method = "EM")


nowpastSQ6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                    r = 3, p = 1, q = 2, aggregate = T, method = "2sq")
nowpastSM6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 2, aggregate = F, method = "2sm")
nowpastEM6 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 2, aggregate = F, method = "Em")

nowpastSQ7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 1, aggregate = T, method = "2sq")
nowpastSM7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 1, aggregate = F, method = "2sm")
nowpastEM7 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 1, q = 1, aggregate = F, method = "EM")


nowpastSQ8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 2, q = 2, aggregate = T, method = "2sq")
nowpastSM8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 2, q = 2, aggregate = F, method = "2sm")
nowpastEM8 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 2, q = 2, aggregate = F, method = "EM")


nowpastSQ9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 3, q = 3, aggregate = T, method = "2sq")
nowpastSM9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 3, q = 3, aggregate = F, method = "2sm")
nowpastEM9 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 3, p = 3, q = 3, aggregate = F, method = "EM")


nowpastSQ10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 2, q = 2, aggregate = T, method = "2sq")
nowpastSM10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 2, q = 2, aggregate = F, method = "2sm")
nowpastEM10 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 2, q = 2, aggregate = F, method = "EM")


nowpastSQ11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 1, q = 1, aggregate = T, method = "2sq")
nowpastSM11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 1, q = 1, aggregate = F, method = "2sm")
nowpastEM11 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 1, q = 1, aggregate = F, method = "EM")


data.frame(nowpast16$varA[6,])


nowpast.plot(nowpast4, y = pib, yAS = pibAS)
nowpast.plot(nowpast5, y = pib, yAS = pibAS)
nowpast.plot(nowpast6,  y = pib, yAS = pibAS)
nowpast.plot(nowpast7,  y = pib, yAS = pibAS)
nowpast.plot(nowpast8,  y = pib, yAS = pibAS)
nowpast.plot(nowpast11,  y = pib, yAS = pibAS)
nowpast.plot(nowpast12,  y = pib, yAS = pibAS)
nowpast.plot(nowpast13,  y = pib, yAS = pibAS)
nowpast.plot(nowpast14, y = pib, yAS = pibAS)
nowpast.plot(nowpast15,  y = pib, yAS = pibAS)
nowpast.plot(nowpast16, y = pib, yAS = pibAS)

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



datas <- seq.Date(as.Date("2008-01-04"),as.Date("2017-12-31"), by = 7)

nowpast17 <- nowpast(datas = datas, base = base, delay = legenda$delay.em.semanas.depois.do.fim.no.período*7, trans = legenda$transf, 
                     r = 2, p = 2, q = 2, aggregate = F, method = "2sm")
nowpast.plot(nowpast17, y = pib, yAS = pibAS)
