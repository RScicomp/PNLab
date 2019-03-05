


install.packages("rlist")
library(rlist)


file <- paste(getwd(),"/ExcelDocs/",sep="")

pars <-list.files(file, pattern = "\\.xlsm", full.names = FALSE)
asdpars <- vector("character")
for (i in 1: length(pars)){
  df<-read_excel(paste(file,pars[i],sep=""), sheet = 1)
  df<-read_excel(paste(file,pars[i],sep=""), sheet = 1)
  if (df[6,2] == "ASD"){
    asdpars[i] <- pars[i]
  }
}               
typpars <- vector("character")
for (i in 1: length(pars)){
  df<-read_excel(paste(file,pars[i],sep=""), sheet = 1)
  df<-read_excel(paste(file,pars[i],sep=""), sheet = 1)
  if (df[6,2] == "ASD"){
    typpars[i] <- pars[i]
  }
}     