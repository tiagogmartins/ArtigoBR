
# pacotes necessários
library(readxl)
library(nowcasting)
library(forecast)
library(seasonal)
source("./scripts/vintages/now.R", encoding = "utf-8")

# base
base <- ts(read_excel("./scripts/vintages/base2000IAE3.xlsx")[,-1], start = c(2000,1), freq = 12)
legenda <- data.frame(read_excel("./scripts/vintages/base2000IAE3.xlsx", sheet = 2))
hoje <- now(base = base, trans = legenda$transf, aggregate = F, method = "2sm", r = 2, p = 2, q = 2)
hoje
