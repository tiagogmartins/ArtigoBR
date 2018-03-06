
library(nowcasting)
library(zoo)
library(readxl)

source("./scripts/vintages/arrumacao.R",encoding = "UTF-8")


vintage <-paste0( "./scripts/vintages/vintage",seq.Date(as.Date("2015-05-29"),as.Date("2017-12-31"), by = 7),".csv")

datas<-seq.Date(as.Date("2015-05-29"),as.Date("2017-12-31"), by = 7)

base<-rep(NA,length(datas))

base[datas<as.Date("2015-08-25")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q1.xls"

base[datas<as.Date("2015-11-27") & datas>=as.Date("2015-08-25")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q2.xls"
base[datas<as.Date("2016-02-26") & datas>=as.Date("2015-11-27")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q3.xls"
base[datas<as.Date("2016-05-27") & datas>=as.Date("2016-02-26")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q4.xls"
base[datas<as.Date("2016-08-26") & datas>=as.Date("2016-05-27")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q1.xls"
base[datas<as.Date("2016-11-25") & datas>=as.Date("2016-08-26")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q2.xls"
base[datas<as.Date("2017-03-03") & datas>=as.Date("2016-11-25")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q3.xls"
base[datas<as.Date("2017-05-26") & datas>=as.Date("2017-03-03")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q4.xls"
base[datas<as.Date("2017-08-25") & datas>=as.Date("2017-05-26")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2017Q1.xls"
base[datas<as.Date("2017-12-01") & datas>=as.Date("2017-08-25")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2017Q2.xls"
base[datas>=as.Date("2017-12-01")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2017Q3.xls"

for(i in 1:length(datas)){
  arrumacao(vintage[i],base[i])
  
}


