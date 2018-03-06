
library(nowcasting)
library(zoo)
library(readxl)

source("./scripts/vintages/arrumacao.R",encoding = "UTF-8")


vintage <-paste0( "./scripts/vintages/vintage",seq.Date(as.Date("2015-06-12"),as.Date("2017-12-31"), by = 7),".csv")

datas<-seq.Date(as.Date("2015-06-12"),as.Date("2017-12-31"), by = 7)

datas<as.Date("2015-09-11")

base<-rep(NA,length(datas))

base[datas<as.Date("2015-09-11")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q1.xls"

base[datas<as.Date("2015-12-11") & datas>=as.Date("2015-09-11")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q2.xls"
base[datas<as.Date("2016-03-11") & datas>=as.Date("2015-12-11")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q3.xls"
base[datas<as.Date("2016-06-10") & datas>=as.Date("2016-03-11")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2015Q4.xls"
base[datas<as.Date("2016-09-09") & datas>=as.Date("2016-06-10")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q1.xls"
base[datas<as.Date("2016-12-09") & datas>=as.Date("2016-09-09")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q2.xls"
base[datas<as.Date("2017-03-17") & datas>=as.Date("2016-12-09")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q3.xls"
base[datas<as.Date("2017-06-09") & datas>=as.Date("2017-03-17")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2016Q4.xls"
base[datas<as.Date("2017-09-08") & datas>=as.Date("2017-06-09")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2017Q1.xls"
base[datas<as.Date("2017-12-15") & datas>=as.Date("2017-09-08")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2017Q2.xls"
base[datas>=as.Date("2017-12-15")]<-"./scripts/vintages/PIB_OBSERVADO/PIB2017Q3.xls"

for(i in 1:length(datas)){
  arrumacao(vintage[i],base[i])
  
}


