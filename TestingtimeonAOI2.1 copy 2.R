#_____Required librairies_______#
require(openxlsx)
require(dplyr)
require(rowr)
#require(data.table)

#_____Remove current workspace and load workspace with Master data______#
rm(list = ls())
load(paste(getwd(), "Master RDA files_RG/Masters.rda", sep = "/"))
#load(paste(getwd(), "Heatmapdata.rda", sep = "/"))
#_____Excel Writing functions______#
writetoExcel<- function(dataframes, file, conditions){
  #WriteXLS(data, file, condition, row.names = TRUE)
  #write.xlsx(data,file, sheetName = condition, row.names=TRUE, append = TRUE)
  wb <- createWorkbook()
  
  for (i in 1: length(conditions)){
    addWorksheet(wb, conditions[i])
  }
  #browser()
  for (j in 1: length(dataframes)){
    #browser()
    writeDataTable(wb, conditions[[j]], as.data.frame(dataframes[[j]]), rowNames = TRUE) 
    
  }
  saveWorkbook(wb, file, overwrite = TRUE)
  
}
writetoExcelone <- function(dataframe, file, sheetname, tablename){
  wb <- loadWorkbook(file)
  getTables(wb,sheet=sheetname)
  removeTable(wb=wb, sheet =sheetname, table = tablename)
  writeDataTable(wb,sheet = sheetname, dataframe, tableName = tablename,rowNames = TRUE)
  saveWorkbook(wb, file = file, overwrite = TRUE)
}
replaceNAWithMeans <- function(data){
  for(i in 1:ncol(data)){
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
  }
  return(data)
}

writetoCSV <- function(dataframes, conditions){
  file = paste(getwd(), "/Condition CSV files_RG", sep = "")
  for (j in 1: length(conditions)){
    write.csv(dataframes[[j]], file = paste(file, paste(conditions[[j]], ".csv", sep = ""), sep ="/"), row.names= TRUE)
  }
}
writetoCSVone <- function(dataframe, condition){
  file = paste(getwd(), "/Condition CSV files_RG", sep = "")
  write.csv(dataframe, file = paste(file, paste(condition, ".csv", sep = ""), sep ="/"), row.names= TRUE)
}

#_____Participant list functions_______#
asdparticipants <- function(){
  pars <- FF1[1,]
  colnames(pars) <- FF1[1,]
  pars<-unique(unlist(pars[,grep("A0",pars)]))
  return(pars)
}
typparticipants <- function(){
  pars <- FF1[1,]
  list <- unique(unlist(pars[1,]))
  list <- unlist(lapply(list, function(x) if(!grepl("A0",x)) x else NA))
  list <- list[!is.na(list)]
  
  return(list)
}

#_____Remove Correct and Incorrect Data______#
#_____Note removeI actually keeps Incorrect data and vice versa for removeC______#
removeI<- function(dataframe){
  newdataframe = dataframe[,grep("Incorrect", dataframe[2,])]
  return(newdataframe)
}
removeC<- function(dataframe){
  newdataframe = dataframe[,is.na(unlist(dataframe[2,]))]
  return(newdataframe)
}

#_____Pull stats already present from Master data______#
incorrectstats <- function(dataframe1, dataframe2){
  pars <- asdparticipants()
  incorrect <- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    incorrect <- cbind(combine(dataframe1,dataframe2, pars[i]),incorrect)
  }
  return(incorrect)
}
incorrectcounter <- function(dataframe){

  pars <- append(typparticipants(),asdparticipants())
  a<-table(as.vector(unlist(dataframe[1,])))/3
  c<-as.data.frame(a)
  rownames(c) <- as.vector(c$Var1)
  c$Var1 <-  NULL
  colnames(c) <- c("No. Incorrect")
  return(c)
}
incorrectstatstyp <- function(dataframe1, dataframe2){
  pars <- typparticipants()
  incorrect <- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    incorrect <- cbind(combine(dataframe1,dataframe2, pars[i]),incorrect)
  }
  return(incorrect)
}
correctstats <- function(dataframe1,dataframe2){
  pars <- asdparticipants()
  correct<- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    correct <- cbind(combine(dataframe1,dataframe2, pars[i]),correct)
    #browser()
  }
  return(correct)
}
correctstatstyp <- function(dataframe1,dataframe2){
  pars <- typparticipants()
  correct<- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    correct <- cbind(combine(dataframe1,dataframe2, pars[i]),correct)
    #browser()
  }
  return(correct)
}
allstats <- function(dataframe1, dataframe2){
  pars <- asdparticipants()
  all <- combine(dataframe1,dataframe2, pars[1])
  for (i in 2: length(pars)){
    all <- cbind(combine(dataframe1,dataframe2,pars[i]),all)
  }
  return(all)
}
allstatstyp <- function(dataframe1, dataframe2){
  pars <- typparticipants()
  all <- combine(dataframe1,dataframe2, pars[1])
  for (i in 2: length(pars)){
    all <- cbind(combine(dataframe1,dataframe2,pars[i]),all)
  }
  return(all)
}
parstats <- function(dataframe, name){
  #browser()
  newdataframe = dataframe[,grep(name, dataframe[1,])]
  return(newdataframe)
}
combine <- function(dataframe1, dataframe2, name){
  # browser()
  newdataframe= cbind(parstats(dataframe1, name), parstats(dataframe2,name))
  return(newdataframe)
}
addparticipant <- function(dataframe, name,AOIno,AOI){
  
  # browser()
  newdataframe<- parstats(dataframe, name)
  if(ncol(newdataframe) == 0){
    countsum <- NA
    countavg <- NA
    countstd <- NA
    timesum <- NA
    timeavg <- NA
    timestd <- NA
  }else{
    countseq <- seq(2,ncol(newdataframe),3)
    timeseq <- seq(3,ncol(newdataframe),3)
    #browser()
    counts <- newdataframe[,countseq]
    times <- newdataframe[,timeseq]
    if(ncol(newdataframe) == 3){
      
      #browser()
      count <- append(unlist(counts[AOIno+2]),unlist(counts[AOIno+3]))
      time <- append(unlist(times[AOIno+2]),unlist(times[AOIno+3]))
    }else{
      count <- append(unlist(counts[AOIno+2,]),unlist(counts[AOIno+3,]))
      time <- append(unlist(times[AOIno+2,]),unlist(times[AOIno+3,]))
    }
    #browser()
    countsum <- sum(as.numeric(count))
    timesum <- sum(as.numeric(time))
    countavg <- mean(as.numeric(count))
    timeavg <- mean(as.numeric(time))
    countstd <- sd(as.numeric(count))
    timestd <- sd(as.numeric(time))
  }
  df <- data.frame(countsum,countavg,countstd,timesum,timeavg,timestd)
  rownames(df) <- name
  colnames(df) <- c(paste(AOI,"Count",sep=""), paste(AOI, "Average", sep = ""), paste(AOI, "STD", sep = ""), paste("TimeSpenton", AOI, sep =""), paste("AvgTimeSpenton", AOI, sep=""), paste("STDTimeSpenton", AOI, sep =""))
  
  return(df)  
}
addparticipantLR <- function(dataframe, name,AOIno,AOI){
  
  #browser()
  newdataframe<- parstats(dataframe, name)
  if(ncol(newdataframe) == 0){
    countsum <- NA
    countavg <- NA
    countstd <- NA
    timesum <- NA
    timeavg <- NA
    timestd <- NA
  }else{
    countseq <- seq(2,ncol(newdataframe),3)
    timeseq <- seq(3,ncol(newdataframe),3)
    #browser()
    counts <- newdataframe[,countseq]
    times <- newdataframe[,timeseq]
    if(ncol(newdataframe) == 3){
      
      count <- unlist(counts[AOIno+2])
      time <- unlist(times[AOIno+2])
    }else{
      count <- unlist(counts[AOIno+2,])
      time <- unlist(times[AOIno+2,])
    }
    #browser()
    countsum <- sum(as.numeric(count))
    timesum <- sum(as.numeric(time))
    countavg <- mean(as.numeric(count))
    timeavg <- mean(as.numeric(time))
    countstd <- sd(as.numeric(count))
    timestd <- sd(as.numeric(time))
  }
  df <- data.frame(countsum,countavg,countstd,timesum,timeavg,timestd)
  rownames(df) <- name
  colnames(df) <- c(paste(AOI,"Count",sep=""), paste(AOI, "Average", sep = ""), paste(AOI, "STD", sep = ""), paste("TimeSpenton", AOI, sep =""), paste("AvgTimeSpenton", AOI, sep=""), paste("STDTimeSpenton", AOI, sep =""))
  
  return(df)  
}

parstatsall <- function(name, condition){
  cbind(addparticipant(condition,name,1,"Eye"),addparticipant(condition, name, 3, "Nose"), addparticipant(condition, name,5,"Mouth"), addparticipant(condition,name,7,"Hair"),addparticipant(condition,name,9,"Jaw"))
}
parstatsallL <- function(name, condition){
  cbind(addparticipantLR(condition,name,1,"LFEye"),addparticipantLR(condition, name, 3, "LFNose"), addparticipantLR(condition, name,5,"LFMouth"), addparticipantLR(condition,name,7,"LFHair"),addparticipantLR(condition,name,9,"LFJaw"))
}
parstatsallR <- function(name, condition){
  cbind(addparticipantLR(condition,name,2,"RFEye"),addparticipantLR(condition, name, 4, "RFNose"), addparticipantLR(condition, name,6,"RFMouth"), addparticipantLR(condition,name,8,"RFHair"),addparticipantLR(condition,name,10,"RFJaw"))
}
allparstatsall <- function(condition){
  pars<-asdparticipants()
  dataframe <- parstatsall(pars[1],condition)
  for (i in 2: length(pars)){
    dataframe <- rbind(parstatsall(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsallL<- function(condition){
  pars<-asdparticipants()
  #browser()
  dataframe <- parstatsallL(pars[1],condition)
  for (i in 2: length(pars)){
    dataframe <- rbind(parstatsallL(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsallR <- function(condition){
  pars<-asdparticipants()
  dataframe <- parstatsallR(pars[1],condition)
  for (i in 2: length(pars)){
    dataframe <- rbind(parstatsallR(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsalltyp <- function(condition){
  pars<-typparticipants()
  dataframe <- parstatsall(pars[1],condition)
  for (i in 2: length(pars)){
    
    dataframe <- rbind(parstatsall(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsalltypL <- function(condition){
  pars<-typparticipants()
  dataframe <- parstatsallL(pars[1],condition)
  for (i in 2: length(pars)){
    
    dataframe <- rbind(parstatsallL(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsalltypR <- function(condition){
  pars<-typparticipants()
  dataframe <- parstatsallR(pars[1],condition)
  for (i in 2: length(pars)){
    
    dataframe <- rbind(parstatsallR(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}

#_____Time to fixation functions_____#
TimetoFirstFix <- function(dataframe, name, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2){
  
  newdataframe <- dataframe[,grep(name,dataframe[1,])]
  newdataframe <- newdataframe[-(1:23),]
  times <- vector("integer")
  
  if (ncol(newdataframe) == 0){
    sdataframe <- data.frame(NA,NA,NA)
    colnames(sdataframe) <- c("Xeye", "Yeye","Time")
    
  }
  if (ncol(newdataframe) != 0){
    for (i in seq(1,ncol(newdataframe),3)){
      #browser()
      if (i != ncol(newdataframe)){
        ddataframe <- newdataframe[,c(i,i+1,i+2)]
      }
      if(name == "Marg.xlsm"){
        #browser()
      }
      if (i == 1){
        sdataframe <- getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
        colnames(sdataframe) <- c("Xeye", "Yeye","Time")
      }else{
        sdataframe <- rbind(sdataframe,getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2))
      }
      
    }
  }
  #browser()
  return(sdataframe)
}
TimetoFirstFixLR <- function(dataframe, name, xbound1,xbound2,ybound1,ybound2){
  
  newdataframe <- dataframe[,grep(name,dataframe[1,])]
  newdataframe <- newdataframe[-(1:23),]
  times <- vector("integer")
  
  if (ncol(newdataframe) == 0){
    sdataframe <- data.frame(NA,NA,NA)
    colnames(sdataframe) <- c("Xeye", "Yeye","Time")
    
  }
  if (ncol(newdataframe) != 0){
    for (i in seq(1,ncol(newdataframe),3)){
      ddataframe <- newdataframe[,c(i,i+1,i+2)]
      
      if (i == 1){
        sdataframe <- getfirsttime(ddataframe,xbound1,xbound2,ybound1,ybound2,xbound1,xbound2,ybound1,ybound2 )
        colnames(sdataframe) <- c("Xeye", "Yeye","Time")
      }else{
        sdataframe <- rbind(sdataframe,getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2,xbound1,xbound2,ybound1,ybound2))
      }
      
    }
  }
  #browser()
  return(sdataframe)
}
Timestats <- function(dataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2,AOI){
  pars <- asdparticipants()
  TimeToFixMean <- vector("integer") 
  TimeToFixStds <- vector("integer")
  
  for (i in 1: length(pars)) {
    
    rawdata <- TimetoFirstFix(dataframe, pars[i], xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
    TimeToFixMean[i]<- mean(as.numeric(unlist(rawdata[,3])),na.rm = TRUE)
    TimeToFixStds[i] <- sd(as.numeric(unlist(rawdata[,3])),na.rm=TRUE)
    #browser()
  }
  TimeToFixMean <- rev(TimeToFixMean)
  TimeToFixStds <- rev(TimeToFixStds)
  newdataframe <- data.frame(TimeToFixMean,TimeToFixStds)
  colnames(newdataframe) <- c(paste(colnames(newdataframe)[1], AOI, sep =""), paste(colnames(newdataframe)[2], AOI, sep =""))
  
  rownames(newdataframe)<-rev(pars)
  return(newdataframe)
}

Timestatstyp <- function(dataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2,AOI){
  pars <- typparticipants()
  TimeToFixMean <- vector("integer") 
  TimeToFixStds <- vector("integer")
  
  for (i in 1: length(pars)) {
    #browser()
    rawdata <- TimetoFirstFix(dataframe, pars[i], xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
    TimeToFixMean[i]<- mean(as.numeric(unlist(rawdata[,3])),na.rm = TRUE)
    TimeToFixStds[i] <- sd(as.numeric(unlist(rawdata[,3])),na.rm=TRUE)
    #browser()
  }
  TimeToFixMean <- rev(TimeToFixMean)
  TimeToFixStds <- rev(TimeToFixStds)
  newdataframe <- data.frame(TimeToFixMean,TimeToFixStds)
  colnames(newdataframe) <- c(paste(colnames(newdataframe)[1], AOI, sep =""), paste(colnames(newdataframe)[2], AOI, sep =""))
  
  rownames(newdataframe)<-rev(pars)
  return(newdataframe)
}
EveryAOItimeStat <- function(dataframe){
  cbind(Timestats(dataframe, 0.22565,0.35723,  0.42789, 0.5004,0.6411,	0.77268,	0.42789,	0.5004, "Eye"), 
        Timestats(dataframe, 0.2631,	0.32421,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"Nose"),
        Timestats(dataframe, 0.24832,	0.33506,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "Mouth"),
        Timestats(dataframe, 0.17833,	0.39912,	0.20202,	0.42603, 0.59429,	0.81508,	0.20202,	0.42603, "Hair"),
        Timestats(dataframe,  0.19509,	0.3799,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "Jaw"))
}
EveryAOItimeStatL <- function(dataframe){
  cbind(Timestats(dataframe, 0.22565,0.35723,  0.42789, 0.5004, 0.22565,0.35723,  0.42789, 0.5004, "LFEye"), 
        Timestats(dataframe, 0.2631,	0.32421,	0.51248,	0.58591, 0.2631,	0.32421,	0.51248,	0.58591,"LFNose"),
        Timestats(dataframe, 0.24832,	0.33506,	0.59707,	0.64912,0.24832,	0.33506,	0.59707,	0.64912, "LFMouth"),
        Timestats(dataframe, 0.17833,	0.39912,	0.20202,	0.42603,0.17833,	0.39912,	0.20202,	0.42603,"LFHair"),
        Timestats(dataframe, 0.19509,	0.3799,	0.65377,	0.74021,0.19509,	0.3799,	0.65377,	0.74021, "LFJaw"))
}
EveryAOItimeStatR <- function(dataframe){
  cbind(Timestats(dataframe, 0.6411,	0.77268,	0.42789,	0.5004,0.6411,	0.77268,	0.42789,	0.5004, "RFEye"), 
        Timestats(dataframe, 0.67856,	0.73967,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"RFNose"),
        Timestats(dataframe,  0.6687	,0.75544,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "RFMouth"),
        Timestats(dataframe, 0.59429,	0.81508,	0.20202,	0.42603,0.59429,	0.81508,	0.20202,	0.42603,"RFHair"),
        Timestats(dataframe, 0.6209, 0.80571,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "RFJaw"))
}

EveryAOItimeStattyp <- function(dataframe){
  cbind(Timestatstyp(dataframe, 0.22565,0.35723,  0.42789, 0.5004,0.6411,	0.77268,	0.42789,	0.5004, "Eye"), 
        Timestatstyp(dataframe, 0.2631,	0.32421,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"Nose"),
        Timestatstyp(dataframe, 0.24832,	0.33506,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "Mouth"),
        Timestatstyp(dataframe, 0.17833,	0.39912,	0.20202,	0.42603, 0.59429,	0.81508,	0.20202,	0.42603, "Hair"),
        Timestatstyp(dataframe,  0.19509,	0.3799,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "Jaw"))
}
EveryAOItimeStatLtyp <- function(dataframe){
  cbind(Timestatstyp(dataframe, 0.22565,0.35723,  0.42789, 0.5004, 0.22565,0.35723,  0.42789, 0.5004, "LFEye"), 
        Timestatstyp(dataframe, 0.2631,	0.32421,	0.51248,	0.58591, 0.2631,	0.32421,	0.51248,	0.58591,"LFNose"),
        Timestatstyp(dataframe, 0.24832,	0.33506,	0.59707,	0.64912,0.24832,	0.33506,	0.59707,	0.64912, "LFMouth"),
        Timestatstyp(dataframe, 0.17833,	0.39912,	0.20202,	0.42603,0.17833,	0.39912,	0.20202,	0.42603,"LFHair"),
        Timestatstyp(dataframe, 0.19509,	0.3799,	0.65377,	0.74021,0.19509,	0.3799,	0.65377,	0.74021, "LFJaw"))
}
EveryAOItimeStatRtyp <- function(dataframe){
  cbind(Timestatstyp(dataframe, 0.6411,	0.77268,	0.42789,	0.5004,0.6411,	0.77268,	0.42789,	0.5004, "RFEye"), 
        Timestatstyp(dataframe, 0.67856,	0.73967,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"RFNose"),
        Timestatstyp(dataframe,  0.6687	,0.75544,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "RFMouth"),
        Timestatstyp(dataframe, 0.59429,	0.81508,	0.20202,	0.42603,0.59429,	0.81508,	0.20202,	0.42603,"RFHair"),
        Timestatstyp(dataframe, 0.6209, 0.80571,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "RFJaw"))
}
getfirsttime <- function(dataframe,xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2){
  #browser()
  newdataframel <- dplyr::filter(dataframe, as.numeric(unlist(dataframe[3])) != -1)
  newdataframer <- dplyr::filter(dataframe, as.numeric(unlist(dataframe[3])) != -1)
  
  newdataframel <- filter(newdataframel, between(as.numeric(unlist(newdataframel[1])),xbound1,xbound2) & between(as.numeric(unlist(newdataframel[2])),ybound1,ybound2))
  newdataframer <- filter(newdataframer, between(as.numeric(unlist(newdataframer[1])),xboundr1,xboundr2) & between(as.numeric(unlist(newdataframer[2])), yboundr1, yboundr2))
  newdataframe <- rbind(newdataframel,newdataframer)
  colnames(newdataframe) <- c("Xeye", "Yeye","Time")
  return(newdataframe[1,])
}


#____Save data to Excel files____#
savedataI <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_Incorrect","FS_Incorrect","SF_Incorrect","I_Incorrect")
  # writetoExcel(list(incorrectstatsFF, incorrectstatsFS, incorrectstatsSF, incorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/ASD Stats/asdStats_Incorrect.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataC <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_Correct","FS_Correct","SF_Correct","I_Correct")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/ASD Stats/asdStats_Correct.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataA <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_All","FS_All","SF_All","I_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/ASD Stats/asdStats_All.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataItyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_Incorrect","FStyp_Incorrect","SFtyp_Incorrect","Ityp_Incorrect")
  # writetoExcel(list(incorrectstatsFF, incorrectstatsFS, incorrectstatsSF, incorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/Typ Stats/TypStats_Incorrect.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataCtyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_Correct","FStyp_Correct","SFtyp_Correct","Ityp_Correct")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/Typ Stats/TypStats_Correct.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataAtyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_All","FStyp_All","SFtyp_All","Ityp_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/Typ Stats/TypStats_All.xls",conditions)
  writetoCSV(datalist, conditions)
}


#remove all NA columns
FF1[sapply(FF1, function(x) all(is.na(x)))] <- NULL
FF2[sapply(FF2, function(x) all(is.na(x)))] <- NULL
FS1[sapply(FS1, function(x) all(is.na(x)))] <- NULL
FS2[sapply(FS2, function(x) all(is.na(x)))] <- NULL
SF1[sapply(SF1, function(x) all(is.na(x)))] <- NULL
SF2[sapply(SF2, function(x) all(is.na(x)))] <- NULL
I1[sapply(I1, function(x) all(is.na(x)))] <- NULL
I2[sapply(I2, function(x) all(is.na(x)))] <- NULL

#____Data loading_______#

#Incorrect. ASD

Front1 <- removeI(FF1)
Front2<- removeI(FF2)
SFront1 <- removeI(SF1)
SFront2 <- removeI(SF2)
FrontS1 <- removeI(FS1)
FrontS2 <- removeI(FS2)
Inverted1 <- removeI(I1)
Inverted2 <- removeI(I2)


#Merge and get just incorrect stats
incorrectstatsFF<- incorrectstats(Front1,Front2)
incorrectstatsFS<- incorrectstats(SFront1,SFront2)
incorrectstatsSF<- incorrectstats(FrontS1, FrontS2)
incorrectstatsI <- incorrectstats(Inverted1,Inverted2)

noincorrectFF <- incorrectcounter(incorrectstatsFF)
noincorrectFS <- incorrectcounter(incorrectstatsFS)
noincorrectSF <- incorrectcounter(incorrectstatsSF)
noincorrectI <- incorrectcounter(incorrectstatsI)

#LR stats
dFF<-allparstatsall(incorrectstatsFF)
dFS<-allparstatsall(incorrectstatsFS)
dSF<-allparstatsall(incorrectstatsSF)
dI<-allparstatsall(incorrectstatsI)

#L and R stats

dFF<-cbind(cbind(allparstatsallL(incorrectstatsFF), allparstatsallR(incorrectstatsFF)),dFF)
dFS<-cbind(cbind(allparstatsallL(incorrectstatsFS), allparstatsallR(incorrectstatsFS)),dFS)
dSF<-cbind(cbind(allparstatsallL(incorrectstatsSF), allparstatsallR(incorrectstatsSF)),dSF)
dI<-cbind(cbind(allparstatsallL(incorrectstatsI), allparstatsallR(incorrectstatsI)),dI)


dFF<-cbind(cbind(EveryAOItimeStatL(incorrectstatsFF),EveryAOItimeStatR(incorrectstatsFF)),dFF)
dFS<-cbind(cbind(EveryAOItimeStatL(incorrectstatsFS),EveryAOItimeStatR(incorrectstatsFS)),dFS)
dSF<-cbind(cbind(EveryAOItimeStatL(incorrectstatsSF),EveryAOItimeStatR(incorrectstatsSF)),dSF)
dI<-cbind(cbind(EveryAOItimeStatL(incorrectstatsI),EveryAOItimeStatR(incorrectstatsI)),dI)

dFFIASD<- cbind(dFF,EveryAOItimeStat(incorrectstatsFF))
dFSIASD<- cbind(dFS,EveryAOItimeStat(incorrectstatsFS))
dSFIASD<- cbind(dSF,EveryAOItimeStat(incorrectstatsSF))
dIIASD <- cbind(dI,EveryAOItimeStat(incorrectstatsI))

meanFF <- colMeans(dFFIASD,na.rm=TRUE)
meanFS <- colMeans(dFSIASD,na.rm=TRUE)
meanSF <- colMeans(dSFIASD,na.rm=TRUE)
meanI <- colMeans(dIIASD,na.rm=TRUE)
stdFF <- sapply(dFFIASD,sd, na.rm = TRUE)
stdFS <- sapply(dFSIASD, sd, na.rm = TRUE)
stdSF <- sapply(dSFIASD, sd, na.rm = TRUE)
stdI <- sapply(dIIASD, sd, na.rm = TRUE)

dFFIASD <- rbind(dFFIASD, meanFF)
row.names(dFFIASD)[length(rownames(dFFIASD))] <- "Averages"
dFFIASD <- rbind(dFFIASD, stdFF)
row.names(dFFIASD)[length(rownames(dFFIASD))] <- "Standard Deviations"

dFSIASD <- rbind(dFSIASD, meanFS)
row.names(dFSIASD)[length(rownames(dFSIASD))] <- "Averages"
dFSIASD <- rbind(dFSIASD, stdFS)
row.names(dFSIASD)[length(rownames(dFSIASD))] <- "Standard Deviations"

dSFIASD <- rbind(dSFIASD, meanSF)
row.names(dSFIASD)[length(rownames(dSFIASD))] <- "Averages"
dSFIASD <- rbind(dSFIASD, stdSF)
row.names(dSFIASD)[length(rownames(dSFIASD))] <- "Standard Deviations"

dIIASD <- rbind(dIIASD, meanI)
row.names(dIIASD)[length(rownames(dIIASD))] <- "Averages"
dIIASD <- rbind(dIIASD, stdI)
row.names(dIIASD)[length(rownames(dIIASD))] <- "Standard Deviations"


savedataI(dFFIASD,dFSIASD,dSFIASD,dIIASD)

#_________________________________________________#
#Correct. ASD
Front1 <- removeC(FF1)
Front2<- removeC(FF2)
SFront1 <- removeC(SF1)
SFront2 <- removeC(SF2)
FrontS1 <- removeC(FS1)
FrontS2 <- removeC(FS2)
Inverted1 <- removeC(I1)
Inverted2 <- removeC(I2)


#Merge and get just incorrect stats
correctstatsFF<- correctstats(Front1,Front2)
correctstatsFS<- correctstats(SFront1,SFront2)
correctstatsSF<- correctstats(FrontS1, FrontS2)
correctstatsI <- correctstats(Inverted1,Inverted2)

dFFC<-allparstatsall(correctstatsFF)
dFSC<-allparstatsall(correctstatsFS)
dSFC<-allparstatsall(correctstatsSF)
dIC<-allparstatsall(correctstatsI)

#L and R stats
dFFC<-cbind(cbind(allparstatsallL(correctstatsFF), allparstatsallR(correctstatsFF)),dFFC)
dFSC<-cbind(cbind(allparstatsallL(correctstatsFS), allparstatsallR(correctstatsFS)),dFSC)
dSFC<-cbind(cbind(allparstatsallL(correctstatsSF), allparstatsallR(correctstatsSF)),dSFC)
dIC<-cbind(cbind(allparstatsallL(correctstatsI), allparstatsallR(correctstatsI)),dIC)

dFFC<-cbind(cbind(EveryAOItimeStatL(correctstatsFF),EveryAOItimeStatR(correctstatsFF)),dFFC)
dFSC<-cbind(cbind(EveryAOItimeStatL(correctstatsFS),EveryAOItimeStatR(correctstatsFS)),dFSC)
dSFC<-cbind(cbind(EveryAOItimeStatL(correctstatsSF),EveryAOItimeStatR(correctstatsSF)),dSFC)
dIC<-cbind(cbind(EveryAOItimeStatL(correctstatsI),EveryAOItimeStatR(correctstatsI)),dIC)

dFFCASD<- cbind(dFFC,EveryAOItimeStat(correctstatsFF))
dFSCASD<- cbind(dFSC,EveryAOItimeStat(correctstatsFS))
dSFCASD<- cbind(dSFC,EveryAOItimeStat(correctstatsSF))
dICASD<- cbind(dIC,EveryAOItimeStat(correctstatsI))

meanFF <- colMeans(dFFCASD,na.rm=TRUE)
meanFS <- colMeans(dFSCASD,na.rm=TRUE)
meanSF <- colMeans(dSFCASD,na.rm=TRUE)
meanI <- colMeans(dICASD,na.rm=TRUE)
stdFF <- sapply(dFFCASD,sd, na.rm = TRUE)
stdFS <- sapply(dFSCASD, sd, na.rm = TRUE)
stdSF <- sapply(dSFCASD, sd, na.rm = TRUE)
stdI <- sapply(dICASD, sd, na.rm = TRUE)

dFFCASD <- rbind(dFFCASD, meanFF)
row.names(dFFCASD)[length(rownames(dFFCASD))] <- "Averages"
dFFCASD <- rbind(dFFCASD, stdFF)
row.names(dFFCASD)[length(rownames(dFFCASD))] <- "Standard Deviations"

dFSCASD <- rbind(dFSCASD, meanFS)
row.names(dFSCASD)[length(rownames(dFSCASD))] <- "Averages"
dFSCASD <- rbind(dFSCASD, stdFS)
row.names(dFSCASD)[length(rownames(dFSCASD))] <- "Standard Deviations"

dSFCASD <- rbind(dSFCASD, meanSF)
row.names(dSFCASD)[length(rownames(dSFCASD))] <- "Averages"
dSFCASD <- rbind(dSFCASD, stdSF)
row.names(dSFCASD)[length(rownames(dSFCASD))] <- "Standard Deviations"

dICASD <- rbind(dICASD, meanI)
row.names(dICASD)[length(rownames(dICASD))] <- "Averages"
dICASD <- rbind(dICASD, stdI)
row.names(dICASD)[length(rownames(dICASD))] <- "Standard Deviations"


savedataC(dFFCASD,dFSCASD,dSFCASD,dICASD)

#__________________________________________________#
#All. ASD

statsFF<- allstats(FF1,FF2)
statsFS<- allstats(SF1,SF2)
statsSF<- allstats(FS1,FS2)
statsI <- allstats(I1,I2)

dFF<-allparstatsall(statsFF)
dFS<-allparstatsall(statsFS)
dSF<-allparstatsall(statsSF)
dI<-allparstatsall(statsI)

#L and R stats

dFF<-cbind(cbind(allparstatsallL(statsFF), allparstatsallR(statsFF)),dFF)
dFS<-cbind(cbind(allparstatsallL(statsFS), allparstatsallR(statsFS)),dFS)
dSF<-cbind(cbind(allparstatsallL(statsSF), allparstatsallR(statsSF)),dSF)
dI<-cbind(cbind(allparstatsallL(statsI), allparstatsallR(statsI)),dI)

dFF<-cbind(cbind(EveryAOItimeStatL(statsFF),EveryAOItimeStatR(statsFF)),dFF)
dFS<-cbind(cbind(EveryAOItimeStatL(statsFS),EveryAOItimeStatR(statsFS)),dFS)
dSF<-cbind(cbind(EveryAOItimeStatL(statsSF),EveryAOItimeStatR(statsSF)),dSF)
dI<-cbind(cbind(EveryAOItimeStatL(statsI),EveryAOItimeStatR(statsI)),dI)

dFFASD<- cbind(dFF,EveryAOItimeStat(statsFF))
dFSASD<- cbind(dFS,EveryAOItimeStat(statsFS))
dSFASD<- cbind(dSF,EveryAOItimeStat(statsSF))
dIASD <- cbind(dI,EveryAOItimeStat(statsI))

meanFF <- colMeans(dFFASD,na.rm=TRUE)
meanFS <- colMeans(dFSASD,na.rm=TRUE)
meanSF <- colMeans(dSFASD,na.rm=TRUE)
meanI <- colMeans(dIASD,na.rm=TRUE)
stdFF <- sapply(dFFASD,sd, na.rm = TRUE)
stdFS <- sapply(dFSASD, sd, na.rm = TRUE)
stdSF <- sapply(dSFASD, sd, na.rm = TRUE)
stdI <- sapply(dIASD, sd, na.rm = TRUE)

dFFASD <- rbind(dFFASD, meanFF)
row.names(dFFASD)[length(rownames(dFFASD))] <- "Averages"
dFFASD <- rbind(dFFASD, stdFF)
row.names(dFFASD)[length(rownames(dFFASD))] <- "Standard Deviations"

dFSASD <- rbind(dFSASD, meanFS)
row.names(dFSASD)[length(rownames(dFSASD))] <- "Averages"
dFSASD <- rbind(dFSASD, stdFS)
row.names(dFSASD)[length(rownames(dFSASD))] <- "Standard Deviations"

dSFASD <- rbind(dSFASD, meanSF)
row.names(dSFASD)[length(rownames(dSFASD))] <- "Averages"
dSFASD <- rbind(dSFASD, stdSF)
row.names(dSFASD)[length(rownames(dSFASD))] <- "Standard Deviations"

dIASD <- rbind(dIASD, meanI)
row.names(dIASD)[length(rownames(dIASD))] <- "Averages"
dIASD <- rbind(dIASD, stdI)
row.names(dIASD)[length(rownames(dIASD))] <- "Standard Deviations"

names <- rownames(dFFASD)
dFFASD <- cbind.fill(dFFASD, noincorrectFF, fill = 0)
rownames(dFFASD) <- names

names <- rownames(dFSASD)
dFSASD <- cbind.fill(dFSASD, noincorrectFS, fill = 0)
rownames(dFSASD) <- names

names <- rownames(dSFASD)
dSFASD <- cbind.fill(dSFASD, noincorrectSF, fill = 0)
rownames(dSFASD) <- names

names <- rownames(dIASD)
dIASD <- cbind.fill(dIASD, noincorrectI, fill = 0)
rownames(dIASD) <- names
savedataA(dFFASD,dFSASD,dSFASD,dIASD)


#____________________________________________________#
#Incorrect. TYP
if (length(typparticipants) != 0){
  Front1 <- removeI(FF1)
  Front2<- removeI(FF2)
  SFront1 <- removeI(SF1)
  SFront2 <- removeI(SF2)
  FrontS1 <- removeI(FS1)
  FrontS2 <- removeI(FS2)
  Inverted1 <- removeI(I1)
  Inverted2 <- removeI(I2)
  
  
  #Merge and get just incorrect stats
  incorrectstatsFF<- incorrectstatstyp(Front1,Front2)
  incorrectstatsFS<- incorrectstatstyp(SFront1,SFront2)
  incorrectstatsSF<- incorrectstatstyp(FrontS1, FrontS2)
  incorrectstatsI <- incorrectstatstyp(Inverted1,Inverted2)
  
  noincorrectFF <- incorrectcounter(incorrectstatsFF)
  noincorrectFS <- incorrectcounter(incorrectstatsFS)
  noincorrectSF <- incorrectcounter(incorrectstatsSF)
  noincorrectI <- incorrectcounter(incorrectstatsI)
  
  dFF<-allparstatsalltyp(incorrectstatsFF)
  dFS<-allparstatsalltyp(incorrectstatsFS)
  dSF<-allparstatsalltyp(incorrectstatsSF)
  dI<-allparstatsalltyp(incorrectstatsI)
  
  #L and R stats
  dFF<-cbind(cbind(allparstatsalltypL(incorrectstatsFF), allparstatsalltypR(incorrectstatsFF)),dFF)
  dFS<-cbind(cbind(allparstatsalltypL(incorrectstatsFS), allparstatsalltypR(incorrectstatsFS)),dFS)
  dSF<-cbind(cbind(allparstatsalltypL(incorrectstatsSF), allparstatsalltypR(incorrectstatsSF)),dSF)
  dI<-cbind(cbind(allparstatsalltypL(incorrectstatsI), allparstatsalltypR(incorrectstatsI)),dI)

  dFF<-cbind(cbind(EveryAOItimeStatLtyp(incorrectstatsFF),EveryAOItimeStatRtyp(incorrectstatsFF)),dFF)
  dFS<-cbind(cbind(EveryAOItimeStatLtyp(incorrectstatsFS),EveryAOItimeStatRtyp(incorrectstatsFS)),dFS)
  dSF<-cbind(cbind(EveryAOItimeStatLtyp(incorrectstatsSF),EveryAOItimeStatRtyp(incorrectstatsSF)),dSF)
  dI<-cbind(cbind(EveryAOItimeStatLtyp(incorrectstatsI),EveryAOItimeStatRtyp(incorrectstatsI)),dI)
  
  dFFITyp<- cbind(dFF,EveryAOItimeStattyp(incorrectstatsFF))
  dFSITyp<- cbind(dFS,EveryAOItimeStattyp(incorrectstatsFS))
  dSFITyp<- cbind(dSF,EveryAOItimeStattyp(incorrectstatsSF))
  dIITyp <- cbind(dI,EveryAOItimeStattyp(incorrectstatsI))
  
  meanFF <- colMeans(dFFITyp,na.rm=TRUE)
  meanFS <- colMeans(dFSITyp,na.rm=TRUE)
  meanSF <- colMeans(dSFITyp,na.rm=TRUE)
  meanI <- colMeans(dIITyp,na.rm=TRUE)
  stdFF <- sapply(dFFITyp,sd, na.rm = TRUE)
  stdFS <- sapply(dFSITyp, sd, na.rm = TRUE)
  stdSF <- sapply(dSFITyp, sd, na.rm = TRUE)
  stdI <- sapply(dIITyp, sd, na.rm = TRUE)
  
  dFFITyp <- rbind(dFFITyp, meanFF)
  row.names(dFFITyp)[length(rownames(dFFITyp))] <- "Averages"
  dFFITyp <- rbind(dFFITyp, stdFF)
  row.names(dFFITyp)[length(rownames(dFFITyp))] <- "Standard Deviations"
  
  dFSITyp <- rbind(dFSITyp, meanFS)
  row.names(dFSITyp)[length(rownames(dFSITyp))] <- "Averages"
  dFSITyp <- rbind(dFSITyp, stdFS)
  row.names(dFSITyp)[length(rownames(dFSITyp))] <- "Standard Deviations"
  
  dSFITyp <- rbind(dSFITyp, meanSF)
  row.names(dSFITyp)[length(rownames(dSFITyp))] <- "Averages"
  dSFITyp <- rbind(dSFITyp, stdSF)
  row.names(dSFITyp)[length(rownames(dSFITyp))] <- "Standard Deviations"
  
  dIITyp <- rbind(dIITyp, meanI)
  row.names(dIITyp)[length(rownames(dIITyp))] <- "Averages"
  dIITyp <- rbind(dIITyp, stdI)
  row.names(dIITyp)[length(rownames(dIITyp))] <- "Standard Deviations"
  
  savedataItyp(dFFITyp,dFSITyp,dSFITyp,dIITyp)
  
  
  #____________________________________________________#
  #Correct. TYP
  Front1 <- removeC(FF1)
  Front2<- removeC(FF2)
  SFront1 <- removeC(SF1)
  SFront2 <- removeC(SF2)
  FrontS1 <- removeC(FS1)
  FrontS2 <- removeC(FS2)
  Inverted1 <- removeC(I1)
  Inverted2 <- removeC(I2)
  
  
  #Merge and get just incorrect stats
  correctstatsFF<- correctstatstyp(Front1,Front2)
  correctstatsFS<- correctstatstyp(SFront1,SFront2)
  correctstatsSF<- correctstatstyp(FrontS1, FrontS2)
  correctstatsI <- correctstatstyp(Inverted1,Inverted2)
  
  
  dFFC<-allparstatsalltyp(correctstatsFF)
  dFSC<-allparstatsalltyp(correctstatsFS)
  dSFC<-allparstatsalltyp(correctstatsSF)
  dIC<-allparstatsalltyp(correctstatsI)
  
  dFFC<-cbind(cbind(allparstatsalltypL(correctstatsFF), allparstatsalltypR(correctstatsFF)),dFFC)
  dFSC<-cbind(cbind(allparstatsalltypL(correctstatsFS), allparstatsalltypR(correctstatsFS)),dFSC)
  dSFC<-cbind(cbind(allparstatsalltypL(correctstatsSF), allparstatsalltypR(correctstatsSF)),dSFC)
  dIC<-cbind(cbind(allparstatsalltypL(correctstatsI), allparstatsalltypR(correctstatsI)),dIC)
  
  dFFC<-cbind(cbind(EveryAOItimeStatLtyp(correctstatsFF),EveryAOItimeStatRtyp(correctstatsFF)),dFFC)
  dFSC<-cbind(cbind(EveryAOItimeStatLtyp(correctstatsFS),EveryAOItimeStatRtyp(correctstatsFS)),dFSC)
  dSFC<-cbind(cbind(EveryAOItimeStatLtyp(correctstatsSF),EveryAOItimeStatRtyp(correctstatsSF)),dSFC)
  dIC<-cbind(cbind(EveryAOItimeStatLtyp(correctstatsI),EveryAOItimeStatRtyp(correctstatsI)),dIC)
  
  dFFCTyp<- cbind(dFFC,EveryAOItimeStattyp(correctstatsFF))
  dFSCTyp<- cbind(dFSC,EveryAOItimeStattyp(correctstatsFS))
  dSFCTyp<- cbind(dSFC,EveryAOItimeStattyp(correctstatsSF))
  dICTyp<- cbind(dIC,EveryAOItimeStattyp(correctstatsI))
  
  meanFF <- colMeans(dFFCTyp,na.rm=TRUE)
  meanFS <- colMeans(dFSCTyp,na.rm=TRUE)
  meanSF <- colMeans(dSFCTyp,na.rm=TRUE)
  meanI <- colMeans(dICTyp,na.rm=TRUE)
  stdFF <- sapply(dFFCTyp,sd, na.rm = TRUE)
  stdFS <- sapply(dFSCTyp, sd, na.rm = TRUE)
  stdSF <- sapply(dSFCTyp, sd, na.rm = TRUE)
  stdI <- sapply(dICTyp, sd, na.rm = TRUE)
  
  dFFCTyp <- rbind(dFFCTyp, meanFF)
  row.names(dFFCTyp)[length(rownames(dFFCTyp))] <- "Averages"
  dFFCTyp <- rbind(dFFCTyp, stdFF)
  row.names(dFFCTyp)[length(rownames(dFFCTyp))] <- "Standard Deviations"
  
  dFSCTyp <- rbind(dFSCTyp, meanFS)
  row.names(dFSCTyp)[length(rownames(dFSCTyp))] <- "Averages"
  dFSCTyp <- rbind(dFSCTyp, stdFS)
  row.names(dFSCTyp)[length(rownames(dFSCTyp))] <- "Standard Deviations"
  
  dSFCTyp <- rbind(dSFCTyp, meanSF)
  row.names(dSFCTyp)[length(rownames(dSFCTyp))] <- "Averages"
  dSFCTyp <- rbind(dSFCTyp, stdSF)
  row.names(dSFCTyp)[length(rownames(dSFCTyp))] <- "Standard Deviations"
  
  dICTyp <- rbind(dICTyp, meanI)
  row.names(dICTyp)[length(rownames(dICTyp))] <- "Averages"
  dICTyp <- rbind(dICTyp, stdI)
  row.names(dICTyp)[length(rownames(dICTyp))] <- "Standard Deviations"
  
  savedataCtyp(dFFCTyp,dFSCTyp,dSFCTyp,dICTyp)
  
  #____________________________________________________#
  #All. TYP
  
  statsFF<- allstatstyp(FF1,FF2)
  statsFS<- allstatstyp(SF1,SF2)
  statsSF<- allstatstyp(FS1,FS2)
  statsI <- allstatstyp(I1,I2)
  
  dFF<-allparstatsalltyp(statsFF)
  dFS<-allparstatsalltyp(statsFS)
  dSF<-allparstatsalltyp(statsSF)
  dI<-allparstatsalltyp(statsI)
  
  dFF<-cbind(cbind(allparstatsalltypL(statsFF), allparstatsalltypR(statsFF)),dFF)
  dFS<-cbind(cbind(allparstatsalltypL(statsFS), allparstatsalltypR(statsFS)),dFS)
  dSF<-cbind(cbind(allparstatsalltypL(statsSF), allparstatsalltypR(statsSF)),dSF)
  dI<-cbind(cbind(allparstatsalltypL(statsI), allparstatsalltypR(statsI)),dI)

  dFF<-cbind(cbind(EveryAOItimeStatLtyp(statsFF),EveryAOItimeStatRtyp(statsFF)),dFF)
  dFS<-cbind(cbind(EveryAOItimeStatLtyp(statsFS),EveryAOItimeStatRtyp(statsFS)),dFS)
  dSF<-cbind(cbind(EveryAOItimeStatLtyp(statsSF),EveryAOItimeStatRtyp(statsSF)),dSF)
  dI<-cbind(cbind(EveryAOItimeStatLtyp(statsI),EveryAOItimeStatRtyp(statsI)),dI)

  dFFTyp<- cbind(dFF,EveryAOItimeStattyp(statsFF))
  dFSTyp<- cbind(dFS,EveryAOItimeStattyp(statsFS))
  dSFTyp<- cbind(dSF,EveryAOItimeStattyp(statsSF))
  dITyp <- cbind(dI,EveryAOItimeStattyp(statsI))
  
  meanFF <- colMeans(dFFTyp,na.rm=TRUE)
  meanFS <- colMeans(dFSTyp,na.rm=TRUE)
  meanSF <- colMeans(dSFTyp,na.rm=TRUE)
  meanI <- colMeans(dITyp,na.rm=TRUE)
  stdFF <- sapply(dFFTyp,sd, na.rm = TRUE)
  stdFS <- sapply(dFSTyp, sd, na.rm = TRUE)
  stdSF <- sapply(dSFTyp, sd, na.rm = TRUE)
  stdI <- sapply(dITyp, sd, na.rm = TRUE)
  
  dFFTyp <- rbind(dFFTyp, meanFF)
  row.names(dFFTyp)[length(rownames(dFFTyp))] <- "Averages"
  dFFTyp <- rbind(dFFTyp, stdFF)
  row.names(dFFTyp)[length(rownames(dFFTyp))] <- "Standard Deviations"
  
  dFSTyp <- rbind(dFSTyp, meanFS)
  row.names(dFSTyp)[length(rownames(dFSTyp))] <- "Averages"
  dFSTyp <- rbind(dFSTyp, stdFS)
  row.names(dFSTyp)[length(rownames(dFSTyp))] <- "Standard Deviations"
  
  dSFTyp <- rbind(dSFTyp, meanSF)
  row.names(dSFTyp)[length(rownames(dSFTyp))] <- "Averages"
  dSFTyp <- rbind(dSFTyp, stdSF)
  row.names(dSFTyp)[length(rownames(dSFTyp))] <- "Standard Deviations"
  
  dITyp <- rbind(dITyp, meanI)
  row.names(dITyp)[length(rownames(dITyp))] <- "Averages"
  dITyp <- rbind(dITyp, stdI)
  row.names(dITyp)[length(rownames(dITyp))] <- "Standard Deviations"
  
  names <- rownames(dFFTyp)
  dFFTyp <- cbind.fill(dFFTyp, noincorrectFF, fill = 0)
  rownames(dFFTyp) <- names
  
  names <- rownames(dFSTyp)
  dFSTyp <- cbind.fill(dFSTyp, noincorrectFS, fill = 0)
  rownames(dFSTyp) <- names
  
  names <- rownames(dSFTyp)
  dSFTyp <- cbind.fill(dSFTyp, noincorrectSF, fill = 0)
  rownames(dSFTyp) <- names
  
  names <- rownames(dITyp)
  dITyp <- cbind.fill(dITyp, noincorrectI, fill = 0)
  rownames(dITyp) <- names
  
  savedataAtyp(dFFTyp,dFSTyp,dSFTyp,dITyp)
  
}

save(dFFTyp,dFSTyp,dSFTyp,dITyp, dFFITyp,dFSITyp, dSFITyp, dIITyp, dFFCTyp,dFSCTyp,dSFCTyp,dICTyp,dFFASD,dFSASD,dSFASD,dIASD,dFFCASD,dFSCASD,dSFCASD,dICASD,dFFIASD,dFSIASD,dSFIASD,dIIASD, file = paste(getwd(),"/Master RDA files_RG/Heatmapdata.rda",sep=""))

df <- rbind(dFFTyp, dFFASD)
df2 <- rbind(dFFITyp, dFFIASD)
df3 <- rbind(dFFCTyp, dFFCASD)
dfNoNas<-replaceNAWithMeans(df)
list <- unlist(lapply(rownames(dfNoNas), function(x) if(grepl("A0",x)) "ASD" else if(grepl("Averages",x)||grepl("Standard Deviations",x)) "" else "TYP"))
dfNoNas$Condition<-list

#writetoExcelone(dfNoNas, "/Users/Rgao/Desktop/McGill/Condition CSV files_RG/aggregate_data_filtered_all2.xlsx", "aggregate_data", "table3")
writetoExcelone(dfNoNas, paste(getwd(),"/Condition CSV files_RG/aggregate_data_filtered_all2.xlsx",sep=""), "aggregate_data", "table3")
writetoCSVone(df, "aggregate_data_all")
writetoCSVone(df2, "aggregate_data_incorrect")
writetoCSVone(df3, "aggregate_data_correct")

df <- rbind(dFFITyp, dFFIASD)
dfNoNas <- replaceNAWithMeans(df)
list <- unlist(lapply(rownames(dfNoNas), function(x) if(grepl("A0",x)) "ASD" else if(grepl("Averages",x)||grepl("Standard Deviations",x)) "" else "TYP"))
dfNoNas$Condition<-list
#writetoExcelone(dfNoNas, "/Users/Rgao/Desktop/McGill/Condition CSV files_RG/aggregate_data_filtered_all2.xlsx", "aggregate_data_incorrect", "table32")
writetoExcelone(dfNoNas, paste(getwd(),"/Condition CSV files_RG/Gaze Data/aggregate_data_filtered_all2.xlsx",sep=""), "aggregate_data_incorrect", "table32")
#save(dFFTyp,dFSTyp,dSFTyp,dITyp,dFFCTyp,dFSCTyp,dSFCTyp,dICTyp,dFFITyp,dFSITyp,dSFITyp,dITyp,dFFASD,dFSASD,dSFASD,dIASD,dFFCASD,dFSCASD,dSFCASD,dICASD,dFFIASD,dFSIASD,dSFIASD,dIIASD, file = "StatData.rda")

