# diretório
setwd("./scripts/")

# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)
source("nowpast.R", encoding = "utf-8")

# base
base <- ts(read_excel("BaseshortBR.xlsx")[,-1], start = c(2000,1), freq = 12)
legenda <- data.frame(read_excel("BaseshortBR.xlsx", sheet = 2))
brgdp_position <- which(colnames(base) == "serie22099")
pib <- month2qtr(base[,"serie22099"])
pibAS <- ts(read.csv2("pib_dessazonalizado22109.csv")[,-1], start = c(1996,1), freq = 4)

# datas
datas <- seq.Date(as.Date("2008-01-04"),as.Date("2016-12-31"), by = 7)

nowpastEM1 <- nowpast(datas = datas, base = base, delay = legenda$Delay[-brgdp_position]*7, trans = legenda$transf[-brgdp_position], 
                    r = 1, p = 1, q = 1, aggregate = T, method = "EM", yAS = pibAS)

nowpast.plot(nowpastEM1, y = pib, yAS = pibAS)


