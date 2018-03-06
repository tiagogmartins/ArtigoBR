# script para recorte da base
setwd("./scripts")

# base
base <- ts(read_excel("base2000IAE3.xlsx")[,-1], start = c(2000,1), freq = 12)
 legenda <- data.frame(read_excel("base2000IAE3.xlsx", sheet = 2)  )
 delay <- legenda$delay.em.semanas.depois.do.fim.no.período*7
# trans <- legenda$transf
# r = p = q = 2
# aggregate = F
# method = "2sm"

# base0 é a base cortada para uma data específica
base0 <- NULL

# matriz para salvar os resultados das previsões
acum4 <- NULL # acumulado 4 trimestres
varQ <- NULL # variação trimestral
varA <- NULL # variação trimestre do ano anterior
nivel <- NULL # pib em nível

datas <- seq.Date(as.Date("2014-11-07"),as.Date("2017-12-31"), by = 7)

for(i in 1:length(datas)){
  base0 <- PRTDB(mts = base, vintage = datas[i], delay = delay)
  data <- data.frame(data = as.Date(base0), base0)
  colnames(data)[-1] <- colnames(base0)
  write.csv2(data, paste0("./vintages/vintage",datas[i],".csv"), quote = F, row.names = F, na = "")
}

# lista de arquivos
vintagesPIB <- list.files("vintages/PIB_OBSERVADO/")
x <- data.frame(read_excel(paste0("./vintages/PIB_OBSERVADO/",vintagesPIB[1]), sheet = 1))
