# diretório
setwd("./scripts")

# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)
library(zoo)
source("nowpast.R", encoding = "utf-8")

# base
base <- ts(read_excel("BaseshortBR.xlsx")[,-1], start = c(2000,1), freq = 12)
legenda <- data.frame(read_excel("BaseshortBR.xlsx", sheet = 2))
brgdp_position <- which(colnames(base) == "serie22099")
pib <- month2qtr(base[,"serie22099"])
pibAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)

# datas
datas <- seq.Date(as.Date("2008-01-04"),as.Date("2017-12-31"), by = 7)
#datas <- seq.Date(as.Date("2008-08-01"),as.Date("2016-12-31"), by = 7)

# nowpastEM1 <- nowpast(datas = datas, base = base, delay = legenda$Delay[-brgdp_position]*7, trans = legenda$transf[-brgdp_position], 
#                     r = 1, p = 1, q = 1, aggregate = T, method = "EM", yAS = pibAS)
# saveRDS(nowpastEM1, "nowpastEM1.rds")
# nowpastEM2 <- nowpast(datas = datas, base = base, delay = legenda$Delay[-brgdp_position]*7, trans = legenda$transf[-brgdp_position], 
#                       r = 2, p = 2, q = 2, aggregate = T, method = "EM", yAS = pibAS)
# saveRDS(nowpastEM2, "nowpastEM2.rds")
# nowpastEM3 <- nowpast(datas = datas, base = base, delay = legenda$Delay[-brgdp_position]*7, trans = legenda$transf[-brgdp_position], 
#                       r = 2, p = 1, q = 2, aggregate = T, method = "EM", yAS = pibAS)
# saveRDS(nowpastEM3, "nowpastEM3.rds")
# nowpastEM4 <- nowpast(datas = datas, base = base, delay = legenda$Delay[-brgdp_position]*7, trans = legenda$transf[-brgdp_position], 
#                       r = 2, p = 1, q = 1, aggregate = T, method = "EM", yAS = pibAS)
# saveRDS(nowpastEM4, "nowpastEM4.rds")
# nowpastEM5 <- nowpast(datas = datas, base = base, delay = legenda$Delay[-brgdp_position]*7, trans = legenda$transf[-brgdp_position], 
#                       r = 2, p = 2, q = 1, aggregate = T, method = "EM", yAS = pibAS)
# saveRDS(nowpastEM5, "nowpastEM5.rds")


nowpastEM1 <- readRDS("nowpastEM1.rds")
nowpastEM2 <- readRDS("nowpastEM2.rds")
nowpastEM3 <- readRDS("nowpastEM3.rds")
nowpastEM4 <- readRDS("nowpastEM4.rds")
nowpastEM5 <- readRDS("nowpastEM5.rds")

nowpast.plot(nowpastEM1, y = pib, yAS = pibAS)
nowpast.plot(nowpastEM2, y = pib, yAS = pibAS)
nowpast.plot(nowpastEM3, y = pib, yAS = pibAS)
nowpast.plot(nowpastEM4, y = pib, yAS = pibAS)
nowpast.plot(nowpastEM5, y = pib, yAS = pibAS)

nowpast.plot2(nowpastEM1, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastEM2, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastEM3, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastEM4, y = pib, yAS = pibAS, type = 2)
nowpast.plot2(nowpastEM5, y = pib, yAS = pibAS, type = 2)

na.omit(data.frame(t(nowpastEM5$varQ)[,10]))
na.omit(data.frame(t(nowpastEM4$varQ)[,10]))
na.omit(data.frame(t(nowpastEM3$varQ)[,10]))
na.omit(data.frame(t(nowpastEM2$varQ)[,10]))
na.omit(data.frame(t(nowpastEM1$varQ)[,10]))
