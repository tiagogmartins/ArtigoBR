


arrumacao<- function(vintage,base){
  A <- read.csv2(vintage)
  
  B<- ts(A[,-1],start = c(2000,01), freq=12)
  
  C<- data.frame(read_excel(base,sheet="Série Encadeada",skip = 3))[,"PIB"]
  
  E<- data.frame(read_excel(base,sheet="Série com Ajuste Sazonal",skip = 3))[,"PIB"]
  
  G<- ts(E, start = c(1996,1),freq=4)
  
  H<- window(qtr2month(G),start=c(2000,1),freq=12)
  
  window(B[,"serie22109"],end=end(H),freq=12)<-H
  
  
  CI<- na.omit(ts(C, start = c(1996,1),freq=4))
  
  
  D<- window(qtr2month(CI),start=c(2000,1),freq=12)
  
  
  window(B[,"serie22099"],end=end(D),freq=12)<-D
  
  
  data <- data.frame(data = as.Date(B), B)
  colnames(data)[-1] <- colnames(B)
  write.csv2(data, vintage, quote = F, row.names = F, na = "")
  
  
}

  
  
