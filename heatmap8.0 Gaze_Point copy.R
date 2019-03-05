require(tidyverse)
require(svDialogs)
require(openxlsx)
require(WriteXLS)
require(readxl)
require(pracma)
require(tidyr)
require(dplyr)
require(DT)
require(gridExtra)
require(shiny)
require(rasterImage)
require(ggplot2)
require(ggmap)
require(jpeg)
require(shinyWidgets)
require(readxl)
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
    mean <- as.data.frame(t(colMeans(dataframes[[j]][,(2:ncol(dataframes[[j]]))],na.rm=TRUE)))
    mean["Row.names"] <- NA
    std <- sapply(dataframes[[j]],sd, na.rm = TRUE)
    dataframes[[j]] <- rbind(dataframes[[j]], mean)
    
    row.names(dataframes[[j]])[length(rownames(table))] <- "Averages"
    dataframes[[j]]<- rbind(dataframes[[j]], std)
    row.names(dataframes[[j]])[length(rownames(dataframes[[j]]))] <- "Standard Deviations"
    dataframes[[j]][is.na(dataframes[[j]])] <- "NA"
    dataframes[[j]]$Row.names[(nrow(dataframes[[j]]))-1] <- "Averages"
    dataframes[[j]]$Row.names[(nrow(dataframes[[j]]))] <- "Standard Deviations"
    writeDataTable(wb, conditions[[j]], as.data.frame(dataframes[[j]]), rowNames = FALSE) 
    
  }
  saveWorkbook(wb, file, overwrite = TRUE)
  
}
writetoCSV <- function(dataframes, conditions){
  file = paste(getwd(), "/Condition CSV files_RG", sep = "")
  for (j in 1: length(conditions)){
    write.csv(dataframes[[j]], file = paste(file, paste(conditions[[j]], ".csv", sep = ""), sep ="/"), row.names= TRUE)
  }
}
savedataI <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_Incorrect","FS_Incorrect","SF_Incorrect","I_Incorrect")
  # writetoExcel(list(incorrectstatsFF, incorrectstatsFS, incorrectstatsSF, incorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),"/ASD Stats/asdStats_Incorrect.xls",sep=""),conditions)
  writetoCSV(datalist, conditions)
}
savedataC <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_Correct","FS_Correct","SF_Correct","I_Correct")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),"/ASD Stats/asdStats_Correct.xls",sep=""),conditions)
  writetoCSV(datalist, conditions)
}
savedataA <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_All","FS_All","SF_All","I_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),"/ASD Stats/asdStats_All.xls",sep=""),conditions)
  writetoCSV(datalist, conditions)
}
savedataItyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_Incorrect","FStyp_Incorrect","SFtyp_Incorrect","Ityp_Incorrect")
  # writetoExcel(list(incorrectstatsFF, incorrectstatsFS, incorrectstatsSF, incorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),"/TYP Stats/TypStats_Incorrect.xls",sep=""),conditions)
  writetoCSV(datalist, conditions)
}
savedataCtyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_Correct","FStyp_Correct","SFtyp_Correct","Ityp_Correct")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),"/TYP Stats/TypStats_Correct.xls",sep=""),conditions)
  writetoCSV(datalist, conditions)
}
savedataAtyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_All","FStyp_All","SFtyp_All","Ityp_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),"/TYP Stats/TypStats_All.xls",sep=""),conditions)
  writetoCSV(datalist, conditions)
}

asdparticipants <- function(file){
  pars <-list.files(file, pattern = "\\.xlsm", full.names = FALSE)
  parsdf <- data.frame(V1 = pars)
  pars <- filter(parsdf, substr(V1, 1,1) != "~")
  pars <- filter(pars, V1 != "Master.xlsm")
  samplesize <-length(pars$V1)
  asdpar <- filter(pars, substr(V1,1,2) == "A0")
  return(asdpar$V1)
}
getBehavourial <- function(eyetrackingdata,eyetrackingdata2){
  rowStart <- 19
  rowEnd <- 21
  
  behave1 <- eyetrackingdata[(rowStart:rowEnd),]
  behave2 <- eyetrackingdata2[(rowStart:rowEnd),]
  
  colnames(behave1) <- as.character(unlist(eyetrackingdata[2,]))
  colnames(behave2) <- as.character(unlist(eyetrackingdata2[2,]))
  
  behave1[sapply(behave1,function(x) all(is.na(x)))] <- NULL
  behave2[sapply(behave2,function(x) all(is.na(x)))] <- NULL
  
  behave <- rbind(behave1,behave2)

  seq1 <- seq(2,ncol(behave),by = 3)
  behave <- behave[,seq1]
  
  totalspeedavgs <- behave[1,]
  correctspeedavgs <- behave[2,]
  incorrectspeedavgs <- behave[3,]
  totalspeedavgs2 <- behave[4,]
  correctspeedavgs2 <- behave[5,]
  incorrectspeedavgs2 <- behave[6,]
  
  behave <- tibble(
    TotalSpeedAvgs = (as.numeric(stack(totalspeedavgs)$values) + as.numeric(stack(totalspeedavgs2)$values))/2,
    CorrectSpeedAvgs = (as.numeric(stack(correctspeedavgs)$values)+as.numeric(stack(correctspeedavgs2)$values))/2,
    IncorrectSpeedAvgs = (as.numeric(stack(incorrectspeedavgs)$values)+as.numeric(stack(incorrectspeedavgs2)$values))/2,
    #Participant = stack(totalspeedavgs)$ind
  )
  row.names(behave) <- stack(totalspeedavgs)$ind
  
  return(data.frame(behave))
}
#file <- paste(getwd(),"/ExcelDocs/", sep = "")
file <- file.choose()
load(paste(getwd(),"/Master RDA files_RG/Heatmapdata.rda",sep=""))
user.input<- dlg_message("Is Master File Updated? If so reload data?", "yesno")$res
if (user.input == "yes"){
  eyetrackingdata <- read_excel(file, sheet = 2, col_names = FALSE) 
  eyetrackingdata2 <- read_excel(file, sheet = 3,  col_names = FALSE)
  eyePar <- eyetrackingdata[-(1:24),]
  colnames(eyePar) <- as.character(unlist(eyetrackingdata[2,]))
  eyePar2 <- eyetrackingdata2[-(1:24),]
  colnames(eyePar2) <- as.character(unlist(eyetrackingdata2[2,]))
  eyeCorrectNot <- eyetrackingdata[-(4:24),]
  eyeCorrectNot <- eyeCorrectNot[-(1:2),]
  colnames(eyeCorrectNot) <- as.character(unlist(eyetrackingdata[2,]))
  eyeCorrectNot2  <- eyetrackingdata2[-(4:24),]
  eyeCorrectNot2 <- eyeCorrectNot2[-(1:2),]
  colnames(eyeCorrectNot2) <- as.character(unlist(eyetrackingdata2[2,]))
  
  #FS
  eyetrackingdataFS <- read_excel(file, sheet = 5,  col_names = FALSE) 
  eyetrackingdataFS2 <- read_excel(file, sheet = 6, col_names = FALSE)
  eyeParFS <- eyetrackingdataFS[-(1:24),]
  colnames(eyeParFS) <- as.character(unlist(eyetrackingdataFS[2,]))
  eyeParFS2 <- eyetrackingdataFS2[-(1:24),]
  colnames(eyeParFS2) <- as.character(unlist(eyetrackingdataFS2[2,]))
  eyeCorrectNotFS <- eyetrackingdataFS[-(4:24),]
  eyeCorrectNotFS <- eyeCorrectNotFS[-(1:2),]
  colnames(eyeCorrectNotFS) <- as.character(unlist(eyetrackingdataFS[2,]))
  eyeCorrectNotFS2  <- eyetrackingdataFS2[-(4:24),]
  eyeCorrectNotFS2 <- eyeCorrectNotFS2[-(1:2),]
  colnames(eyeCorrectNotFS2) <- as.character(unlist(eyetrackingdataFS2[2,]))
  
  #SF
  eyetrackingdataSF <- read_excel(file, sheet = 7,col_names = FALSE) 
  eyetrackingdataSF2 <- read_excel(file, sheet = 8,col_names = FALSE)
  eyeParSF <- eyetrackingdataSF[-(1:24),]
  colnames(eyeParSF) <- as.character(unlist(eyetrackingdataSF[2,]))
  eyeParSF2 <- eyetrackingdataSF2[-(1:24),]
  colnames(eyeParSF2) <- as.character(unlist(eyetrackingdataSF2[2,]))
  eyeCorrectNotSF <- eyetrackingdataSF[-(4:24),]
  eyeCorrectNotSF <- eyeCorrectNotSF[-(1:2),]
  colnames(eyeCorrectNotSF) <- as.character(unlist(eyetrackingdataSF[2,]))
  eyeCorrectNotSF2  <- eyetrackingdataSF2[-(4:24),]
  eyeCorrectNotSF2 <- eyeCorrectNotSF2[-(1:2),]
  colnames(eyeCorrectNotSF2) <- as.character(unlist(eyetrackingdataSF2[2,]))
  
  #I
  eyetrackingdataI <- read_excel(file, sheet = 10,col_names = FALSE) 
  eyetrackingdataI2 <- read_excel(file, sheet = 11,col_names = FALSE)
  eyeParI <- eyetrackingdataI[-(1:24),]
  colnames(eyeParI) <- as.character(unlist(eyetrackingdataI[2,]))
  eyeParI2 <- eyetrackingdataI2[-(1:24),]
  colnames(eyeParI2) <- as.character(unlist(eyetrackingdataI2[2,]))
  eyeCorrectNotI <- eyetrackingdataI[-(4:24),]
  eyeCorrectNotI <- eyeCorrectNotI[-(1:2),]
  colnames(eyeCorrectNotI) <- as.character(unlist(eyetrackingdataI[2,]))
  eyeCorrectNotI2  <- eyetrackingdataI2[-(4:24),]
  eyeCorrectNotI2 <- eyeCorrectNotI2[-(1:2),]
  colnames(eyeCorrectNotI2) <- as.character(unlist(eyetrackingdataI2[2,]))
  
  behaveFF <- getBehavourial(eyetrackingdata,eyetrackingdata2)
  behaveFS <- getBehavourial(eyetrackingdataFS,eyetrackingdataFS2)
  behaveSF <- getBehavourial(eyetrackingdataSF, eyetrackingdataSF2)
  behaveI <- getBehavourial(eyetrackingdataI, eyetrackingdataI2)
  
  behave <- list(behaveFF,behaveFS,behaveSF, behaveI)
  
  eyetrackingdatal <- list(eyetrackingdata, eyetrackingdataFS, eyetrackingdataSF, eyetrackingdataI)
  eyetrackingdatal2 <- list(eyetrackingdata2, eyetrackingdataFS2, eyetrackingdataSF2, eyetrackingdataI2)
  eyeCorrectNotl <- list(eyeCorrectNot, eyeCorrectNotFS, eyeCorrectNotSF, eyeCorrectNotI)
  eyeCorrectNotl2 <- list(eyeCorrectNot2, eyeCorrectNotFS2, eyeCorrectNotSF2, eyeCorrectNotI2)
  eyeParl <- list(eyePar, eyeParFS, eyeParSF, eyeParI)
  eyeParl2 <- list(eyePar2, eyeParFS2, eyeParSF2, eyeParI2)
  condition <- c("FF","FS","SF","I")
  
  for (i in 1: 4){
    #browser()
    data <- rbind(eyeParl[[i]],eyeParl2[[i]])
    data[sapply(data, function(x) all(is.na(x)))] <- NULL
    
    seq1 <- seq(1,ncol(data)-2, by = 3)
    seq2 <- seq(2,ncol(data), by = 3)
    seq3 <- seq(3,ncol(data), by = 3)
    
    dataXeye <- stack(data[,seq1])
    dataYeye <- stack(data[,seq2])
    dataTime <- stack(data[,seq3])
    
    alldata <- tibble(
      Xeye = as.numeric(dataXeye$values),
      Yeye = as.numeric(dataYeye$values),
      Time = as.numeric(dataTime$values),
      Participant = dataXeye$ind
    )
    alldata <- filter(alldata, Time != -1)
    alldata <- filter(alldata, Xeye > 0 || Yeye > 0)
    
    #________________Incorrect___________________________
    
    incorrectdata1 <- eyeCorrectNotl[[i]]
    incorrectdata1 <- incorrectdata1[,grepl("Incorrect",incorrectdata1[1,])]
    incorrectdata2 <- eyeCorrectNotl2[[i]]
    incorrectdata2 <- incorrectdata2[,grepl("Incorrect",incorrectdata2[1,])]
    
    incorrectdata1 <- incorrectdata1[-(1:1),]
    incorrectdata2 <- incorrectdata2[-(1:1),]
    
    #incorrectdata1[sapply(data, function(x) all(is.na(x)))] <- NULL
    #incorrectdata2[sapply(data, function(x) all(is.na(x)))] <- NULL
    
    seqI1 <- seq(1,ncol(incorrectdata1)-2, 3)
    seqI2 <- seq(2,ncol(incorrectdata1), 3)
    seqI3 <- seq(3,ncol(incorrectdata1), 3)
    #browser()
    
    dataXeyeI <- stack(incorrectdata1[,seqI1])
    dataYeyeI <- stack(incorrectdata1[,seqI2])
    dataTime <- stack(incorrectdata1[,seqI3])
    
    alldataI <- tibble(
      Xeye = as.numeric(dataXeyeI$values),
      Yeye = as.numeric(dataYeyeI$values),
      Time = as.numeric(dataTime$values),
      Participant = dataXeyeI$ind
    )
    
    seqI1 <- seq(1,ncol(incorrectdata2)-2, 3)
    seqI2 <- seq(2,ncol(incorrectdata2), 3)
    seqI3 <- seq(3,ncol(incorrectdata2), 3)
    
    dataXeyeI <- stack(incorrectdata2[,seqI1])
    dataYeyeI <- stack(incorrectdata2[,seqI2])
    dataTime <- stack(incorrectdata2[,seqI3])
    
    alldataI2 <- tibble(
      Xeye = as.numeric(dataXeyeI$values),
      Yeye = as.numeric(dataYeyeI$values),
      Time = as.numeric(dataTime$values),
      Participant = dataXeyeI$ind
    )
    
    alldataI<-rbind(alldataI,alldataI2)
    alldataI<-filter(alldataI, Time != -1)
    alldataI<-filter(alldataI, Xeye > 0)
    alldataI<-filter(alldataI, Yeye > 0)
    
    #_________________Correct_______________________
    cdata1 <- eyeCorrectNotl[[i]]
    cdata1 <- cdata1[,!grepl("Incorrect",cdata1[1,])]
    cdata2 <- eyeCorrectNotl2[[i]]
    cdata2 <- cdata2[,grepl("Incorrect",cdata2[1,])]
    
    cdata1 <- cdata1[-(1:1),]
    cdata2 <- cdata2[-(1:1),]
    
    cdata1[sapply(cdata1, function(x) all(is.na(x)))] <- NULL
    cdata2[sapply(cdata2, function(x) all(is.na(x)))] <- NULL
    
    #browser()
    seqI1 <- seq(1,ncol(cdata1), 3)
    seqI2 <- seq(2,ncol(cdata1), 3)
    seqI3 <- seq(3,ncol(cdata1), 3)
    
    dataXeyeC <- stack(cdata1[,seqI1])
    dataYeyeC <- stack(cdata1[,seqI2])
    dataTime <- stack(cdata1[,seqI3])
    
    alldataC <- tibble(
      Xeye = as.numeric(dataXeyeC$values),
      Yeye = as.numeric(dataYeyeC$values),
      Time = as.numeric(dataTime$values),
      Participant = dataXeyeC$ind
    )
    
    seqI1 <- seq(1,ncol(cdata2), 3)
    seqI2 <- seq(2,ncol(cdata2), 3)
    seqI3 <- seq(3,ncol(cdata2), 3)
    
    dataXeyeC <- stack(cdata2[,seqI1])
    dataYeyeC <- stack(cdata2[,seqI2])
    dataTime <- stack(cdata2[,seqI3])
    
    alldataC2 <- tibble(
      Xeye = as.numeric(dataXeyeC$values),
      Yeye = as.numeric(dataYeyeC$values),
      Time = as.numeric(dataTime$values),
      Participant = dataXeyeC$ind
    )
    
    alldataC<-filter(alldataC, Time != -1)
    alldataC<-filter(alldataC, Xeye > 0)
    alldataC<-filter(alldataC, Yeye > 0)
    
    if (condition[[i]] == "FF"){
      datatrialFF <- alldata
      datatrialFFC <- alldataC
      datatrialFFI <- alldataI
    }
    if (condition[[i]] == "FS"){
      datatrialFS <- alldata
      datatrialFSC <- alldataC
      datatrialFSI <- alldataI
    }
    if (condition[[i]]== "SF"){
      datatrialSF <- alldata
      datatrialSFC <- alldataC
      datatrialSFI <- alldataI
    }
    if (condition[[i]] == "I"){
      datatrialI <- alldata
      datatrialIC <- alldataC
      datatrialII <- alldataI
    }
  }
  save(behaveFF,behaveFS,behaveSF,behaveI,datatrialFF,datatrialFFI,datatrialFFC,datatrialFS,datatrialFSI,datatrialFSC,datatrialSF,datatrialSFI,datatrialSFC,datatrialI,datatrialII,datatrialIC,names, file = "Master RDA files_RG/DataforHeatmap2.rda")
  
}else{load("Master RDA files_RG/DataforHeatmap2.rda")}
colMax <- function(data) sapply(data,max, na.rm = TRUE)

#namelist <- asdparticipants(paste(getwd(),"/ExcelDocs/", sep = ""))

maxtimeFF <- max(datatrialFF[3])
maxtimeFS <- max(datatrialFS[3])
maxtimeSF <- max(datatrialSF[3])
maxtimeI <- max(datatrialI[3])

maxtimeFFC <- max(datatrialFFC[3])
maxtimeFSC <- max(datatrialFSC[3])
maxtimeSFC <- max(datatrialSFC[3])
maxtimeIC <- max(datatrialIC[3])
maxtimeFFI <- max(datatrialFFI[3])
maxtimeFSI <- max(datatrialFSI[3])
maxtimeSFI <- max(datatrialSFI[3])
maxtimeII <- max(datatrialII[3])
#Add on behaverioul
#ASD
#FF
dFFASD <- merge(dFFASD, behaveFF, by = 'row.names', all=FALSE)
row.names(dFFASD) <- dFFASD$Row.names
dFFCASD <- merge(dFFCASD, behaveFF, by = 'row.names', all=FALSE)
row.names(dFFCASD) <- dFFCASD$Row.names
dFFIASD <- merge(dFFIASD, behaveFF, by = 'row.names', all=FALSE)
row.names(dFFASD) <- dFFASD$Row.names
#FS
dFSASD <- merge(dFSASD, behaveFS, by = 'row.names', all=FALSE)
row.names(dFFASD) <- dFFASD$Row.names
dFSCASD <- merge(dFSCASD, behaveFS, by = 'row.names', all=FALSE)
row.names(dFFCASD) <- dFFCASD$Row.names
dFSIASD <- merge(dFSIASD, behaveFS, by = 'row.names', all=FALSE)
row.names(dFFASD) <- dFFASD$Row.names
#SF
dSFASD <- merge(dSFASD, behaveSF, by = 'row.names', all=FALSE)
row.names(dSFASD) <- dSFASD$Row.names
dSFCASD <- merge(dSFCASD, behaveSF, by = 'row.names', all=FALSE)
row.names(dSFCASD) <- dSFCASD$Row.names
dSFIASD <- merge(dSFIASD, behaveSF, by = 'row.names', all=FALSE)
row.names(dSFASD) <- dSFASD$Row.names
#I
dIASD <- merge(dIASD, behaveI, by = 'row.names', all=FALSE)
row.names(dIASD) <- dIASD$Row.names
dICASD <- merge(dICASD, behaveI, by = 'row.names', all=FALSE)
row.names(dICASD) <- dICASD$Row.names
dIIASD <- merge(dIIASD, behaveI, by = 'row.names', all=FALSE)
row.names(dIASD) <- dIASD$Row.names

# writetoCSVone(dFFASD, "/Gaze Data/ASD/FF_ALL_ASD")
# writetoCSVone(dFFCASD, "/Gaze Data/ASD/FF_CORRECT_ASD")
# writetoCSVone(dFFIASD, "/Gaze Data/ASD/FF_INCORRECT_ASD")
# 
# writetoCSVone(dFSASD, "/Gaze Data/ASD/FS_ALL_ASD")
# writetoCSVone(dFSCASD, "/Gaze Data/ASD/FS_CORRECT_ASD")
# writetoCSVone(dFSIASD, "/Gaze Data/ASD/FS_INCORRECT_ASD")
# 
# writetoCSVone(dSFASD, "/Gaze Data/ASD/SF_ALL_ASD")
# writetoCSVone(dSFCASD, "/Gaze Data/ASD/SF_CORRECT_ASD")
# writetoCSVone(dSFIASD, "/Gaze Data/ASD/SF_INCORRECT_ASD")
# 
# writetoCSVone(dIASD, "/Gaze Data/ASD/I_ALL_ASD")
# writetoCSVone(dICASD, "/Gaze Data/ASD/I_CORRECT_ASD")
# writetoCSVone(dIIASD, "/Gaze Data/ASD/I_INCORRECT_ASD")


#TYP
#FF
dFFTyp <- merge(dFFTyp, behaveFF, by = 'row.names', all=FALSE)
row.names(dFFTyp) <- dFFTyp$Row.names
dFFCTyp <- merge(dFFCTyp, behaveFF, by = 'row.names', all=FALSE)
row.names(dFFCTyp) <- dFFCTyp$Row.names
dFFITyp <- merge(dFFITyp, behaveFF, by = 'row.names', all=FALSE)
row.names(dFFTyp) <- dFFTyp$Row.names
#FS
dFSTyp <- merge(dFSTyp, behaveFS, by = 'row.names', all=FALSE)
row.names(dFFTyp) <- dFFTyp$Row.names
dFSCTyp <- merge(dFSCTyp, behaveFS, by = 'row.names', all=FALSE)
row.names(dFFCTyp) <- dFFCTyp$Row.names
dFSITyp <- merge(dFSITyp, behaveFS, by = 'row.names', all=FALSE)
row.names(dFFTyp) <- dFFTyp$Row.names
#SF
dSFTyp <- merge(dSFTyp, behaveSF, by = 'row.names', all=FALSE)
row.names(dSFTyp) <- dSFTyp$Row.names
dSFCTyp <- merge(dSFCTyp, behaveSF, by = 'row.names', all=FALSE)
row.names(dSFCTyp) <- dSFCTyp$Row.names
dSFITyp <- merge(dSFITyp, behaveSF, by = 'row.names', all=FALSE)
row.names(dSFTyp) <- dSFTyp$Row.names
#I
dITyp <- merge(dITyp, behaveI, by = 'row.names', all=FALSE)
row.names(dITyp) <- dITyp$Row.names
dICTyp <- merge(dICTyp, behaveI, by = 'row.names', all=FALSE)
row.names(dICTyp) <- dICTyp$Row.names
dIITyp <- merge(dIITyp, behaveI, by = 'row.names', all=FALSE)
row.names(dITyp) <- dITyp$Row.names

# writetoCSVone(dFFTyp, "/Gaze Data/Typ/FF_ALL_TYP")
# writetoCSVone(dFFCTyp, "/Gaze Data/Typ/FF_CORRECT_TYP")
# writetoCSVone(dFFITyp, "/Gaze Data/Typ/FF_INCORRECT_TYP")

# writetoCSVone(dFSTyp, "/Gaze Data/Typ/FS_ALL_TYP")
# writetoCSVone(dFSCTyp, "/Gaze Data/Typ/FS_CORRECT_TYP")
# writetoCSVone(dFSITyp, "/Gaze Data/Typ/FS_INCORRECT_TYP")
# 
# writetoCSVone(dSFTyp, "/Gaze Data/Typ/SF_ALL_TYP")
# writetoCSVone(dSFCTyp, "/Gaze Data/Typ/SF_CORRECT_TYP")
# writetoCSVone(dSFITyp, "/Gaze Data/Typ/SF_INCORRECT_TYP")
# 
# writetoCSVone(dITyp, "/Gaze Data/Typ/I_ALL_TYP")
# writetoCSVone(dICTyp, "/Gaze Data/Typ/I_CORRECT_TYP")
# writetoCSVone(dIITyp, "/Gaze Data/Typ/I_INCORRECT_TYP")
savedataA(dFFASD,dFSASD,dSFASD,dIASD)
savedataC(dFFCASD,dFSCASD,dSFCASD,dICASD)
savedataI(dFFIASD,dFSIASD,dSFIASD,dIIASD)

savedataAtyp(dFFTyp,dFSTyp,dSFTyp,dITyp)
savedataCtyp(dFFCTyp,dFSCTyp,dSFCTyp,dICTyp)
savedataItyp(dFFITyp,dFSITyp,dSFITyp,dIITyp)
participants <- unique(datatrialFF$Participant)


ui <- fluidPage(
  titlePanel("Eye Tracking Data"),
  
  
  
  #tabPanel("Front Front Statistics", dataTableOutput('table2')),
  #tabPanel("Front Side Statistics", dataTableOutput('table2')),
  #tabPanel("Side Front Statistics", dataTableOutput('table3')),
  #tabPanel('Inverted Statistics', dataTableOutput('table4')))),
  hr(),
  
  fluidRow(
    tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                          Shiny.onInputChange("innerWidth", window.innerWidth);
                          });
                          $(window).resize(function(e) {
                          Shiny.onInputChange("innerWidth", window.innerWidth);
                          });
                          ')),
    column(8,"Heatmaps", plotOutput(outputId = "plot2")),
    column(3, sliderInput("time", "Time:",  min = 0, max = maxtimeFF,value = c(min,max))),
    column(3, sliderInput("transparency", "Opacity", value = 1, min = 0, max = 1)),
    column(3,selectInput("condition", "Condition",
                         c("FF" = "FF",
                           "FS" = "FS",
                           "SF" = "SF",
                           "I" = "I"))),
    column(3,selectInput("correct", "Correct/Incorrect",
                         c("Correct" = "Correct",
                           "Incorrect" = "Incorrect",
                           "All" = "All"))),
    column(3,selectInput("TypASD", "Typ/ASD", c("Typ" = "Typ","ASD"="ASD"))),
    column(3,dropdownButton("participants", label = "Participants", circle = FALSE,
                            checkboxGroupInput('meses', 'Participant Removal:',
                                               unique(as.character(participants)),#row.names(OmarSessiones),
                                               selected = unique(as.character(participants)))))#row.names(OmarSessiones))
    
    ),
  fluidRow(
    column(3,checkboxInput("points", "Show points", value = FALSE, width = NULL)),
    DT::dataTableOutput('table2')
    
  )
  
  #dataTableOutput('table1'),
  
  
  
  
  # plotOutput(outputId = "trial1", width = "110%", height = "500px"),
  # plotOutput(outputId = "trial2", width = "110%", height = "500px"),
  # dataTableOutput('table2'),
  #  dataTableOutput('table3'),
  #  dataTableOutput('table4'),
  #  dataTableOutput('table5'),
  #  verbatimTextOutput("stats")
  
    )
#build web document that app lives in. Generates html webpage. that R components live in


server <- function(input, output, session){
  #define function for server. Contains the instructions that app usesto build plot.
  #handles the inputs and outputs (ie. drop down filters).
  #output is the graph.
  
  #output$hist<- renderPlot({
  # title <- "100 random normal values"
  #hist(rnorm(input$num), main = title)
  #})
  #output$stats <- renderPrint({
  # summary(rnorm(input$num))
  #})
  
  output$table2<- DT::renderDataTable({
    #browser()
    if (input$TypASD == "ASD"){
      #browser()
      if(input$condition == "FF"){
        if(input$correct == "Correct"){
          table <- dFFCASD
        }else if(input$correct == "Incorrect"){
          table <- dFFIASD
        }else{
          table <- dFFASD
        }
      }
      if(input$condition == "FS"){
        if(input$correct == "Correct"){
          table<- dFSCASD
        }else if(input$correct == "Incorrect"){
          table<- dFSIASD
        }else{
          table<- dFSASD
        }
      }
      if(input$condition == "SF"){
        if(input$correct == "Correct"){
          table<- dSFCASD 
        }else if(input$correct == "Incorrect"){
          table<- dSFIASD
        }else{
          table<- dSFASD
        }
      }
      if(input$condition == "I"){
        if(input$correct == "Correct"){
          
          table<- dICASD 
        }else if(input$correct == "Incorrect"){
          table<- dIIASD
        }else{
          table<- dIASD
        }
      }
    }
    if (input$TypASD == "Typ"){
      if(input$condition == "FF"){
        if(input$correct == "Correct"){
          table <- dFFCTyp
        }else if(input$correct == "Incorrect"){
          table <- dFFITyp
        }else{
          table <- dFFTyp
        }
      }
      if(input$condition == "FS"){
        if(input$correct == "Correct"){
          table<- dFSCTyp
        }else if(input$correct == "Incorrect"){
          table<- dFSITyp
        }else{
          table<- dFSTyp
        }
      }
      if(input$condition == "SF"){
        if(input$correct == "Correct"){
          table<- dSFCTyp 
        }else if(input$correct == "Incorrect"){
          table<- dSFITyp
        }else{
          table<- dSFTyp
        }
      }
      if(input$condition == "I"){
        if(input$correct == "Correct"){
          table<- dICTyp 
        }else if(input$correct == "Incorrect"){
          table<- dIITyp
        }else{
          table<- dITyp
        }
      }
    }
    
    #filter(table, rownames
    
    #filter(table, rownames(table) == input$participants)
    table <- filter(table, rownames(table) %in% input$meses)
    if (input$TypASD == "Typ"){
      list <- lapply(input$meses, function(x) if(!grepl("A0", x))x else NA)
      list <- list[!is.na(list)]
      #rownames(table) <- rev(list)
      
    }
    if (input$TypASD == "ASD"){
      list <- lapply(input$meses, function(x) if(grepl("A0", x))x else NA)
      list <- list[!is.na(list)]
      #rownames(table) <- rev(list)
      #browser()
    }
    mean <- as.data.frame(t(colMeans(table[,(2:ncol(table))],na.rm=TRUE)))
    mean["Row.names"] <- NA
    std <- sapply(table,sd, na.rm = TRUE)
    table <- rbind(table, mean)
    
    row.names(table)[length(rownames(table))] <- "Averages"
    table <- rbind(table, std)
    row.names(table)[length(rownames(table))] <- "Standard Deviations"
    table[is.na(table)] <- "NA"
    table$Row.names[(nrow(table))-1] <- "Averages"
    table$Row.names[(nrow(table))] <- "Standard Deviations"
    table 
    # mean <- colMeans(table)
    # std <- sapply(table,sd, na.rm = TRUE)
    # table <- rbind(table, mean)
    # 
    # row.names(table)[length(rownames(table))] <- "Averages"
    # table <- rbind(table, std)
    # row.names(table)[length(rownames(table))] <- "Standard Deviations"
    # table[is.na(table)] <- "NA"
    # table
    
  }, rownames = TRUE)
  # output$table3 <- renderDataTable(dffs)
  # output$table4 <- renderDataTable(dfsf)
  # output$table5 <- renderDataTable(dfi)
  observe({
    if (input$condition =="FF"){
      if(input$correct == "Incorrect"){
        maxtime = maxtimeFFI
      }else if (input$correct == "Correct"){
        maxtime = maxtimeFFC
      }else{maxtime = maxtimeFF}
      
      updateSliderInput(session,"time",
                        min = 0, max = maxtime, step = .1, value = c(min,max))
    }
    if (input$condition == "FS"){
      if(input$correct == "Incorrect"){
        maxtime = maxtimeFSI
      }else if (input$correct == "Correct"){
        maxtime = maxtimeFSC
      }else{maxtime = maxtimeFS}
      updateSliderInput(session,"time",
                        min = 0, max = maxtime, step = .1, value = c(min,max))
    }
    if (input$condition == "SF"){
      if(input$correct == "Incorrect"){
        maxtime = maxtimeSFI
      }else if (input$correct == "Correct"){
        maxtime = maxtimeSFC
      }else{maxtime = maxtimeSF}
      updateSliderInput(session, "time",
                        min = 0, max = maxtime, step = .1, value = c(min,max))
    }
    if (input$condition == "I"){
      if(input$correct == "Incorrect"){
        maxtime = maxtimeII
      }else if (input$correct == "Correct"){
        maxtime = maxtimeIC
      }else{maxtime = maxtimeI}
      updateSliderInput(session, "time",
                        min = 0, max = maxtime, step = .1, value = c(min,max))
    }
    if(input$TypASD == "ASD"){
      
      list <- unique(as.character(participants))
      list <- unique(as.character(lapply(list, function(x) if(startsWith(x,"A0")) x else NA)))
      list <- list[!is.na(list)]
      updateCheckboxGroupInput(session, "meses", 'Participant Removal:', participants, selected = list)
    }
    if (input$TypASD == "Typ"){
      list <- unique(as.character(participants))
      list <- unique(as.character(lapply(list, function(x) if(!startsWith(x,"A0c")) x else NA)))
      list <- list[!is.na(list)]
      updateCheckboxGroupInput(session, "meses", 'Participant Removal:', participants, selected = list)
    }
    
  })
  
  
  
  output$plot2<-renderPlot({
    #browser()
    if (input$condition == "FF"){
      if(input$correct == "Incorrect"){
        output = datatrialFFI
      }else if (input$correct == "Correct"){
        output = datatrialFFC
      }else{output = datatrialFF}
      img <- paste(getwd(), "/McgillStuff/Pics/S_105_FF_66.jpg", sep ="")
      img <- readJPEG(img)
      
      #output <- filter(output, input$participants)
    }
    if (input$condition == "FS"){
      if(input$correct == "Incorrect"){
        output = datatrialFSI
      }else if(input$correct == "Correct"){
        output = datatrialFSC
      }else{
        output = datatrialFS
      }
      img <-  paste(getwd(),"/McgillStuff/Pics/S_219_FS_1212.jpg",sep ="")
      img <- readJPEG(img)
      
    }
    if (input$condition == "SF"){
      if(input$correct == "Incorrect"){
        output = datatrialSFI
      }else if (input$correct == "Correct"){
        output = datatrialSFC
      }else {output = datatrialSF}
      img <-  paste(getwd(),"/McgillStuff/Pics/S_219_SF_1212.jpg",sep ="")
      img <- readJPEG(img)
    }
    if (input$condition == "I"){
      if (input$correct == "Incorrect"){
        output = datatrialII
      }else if (input$correct == "Correct"){
        output = datatrialIC
      }else {output = datatrialI}
      img <-  paste(getwd(),"/McgillStuff/Pics/D_105110_UDFF_84.jpg",sep ="")
      img <- readJPEG(img)
    }
    if (input$points){
      sizeinput = .1
    }else{
      sizeinput = -1
    }
    
    
    #browser()
    output <- filter(output, Time <= input$time[2])
    output <- filter(output, Time >= input$time[1])
    #browser()
    output <- filter(output, Participant %in% input$meses)
    if (input$TypASD == "ASD"){
      output <- subset(output, startsWith(as.character(Participant), "A0"))
    }
    if (input$TypASD == "Typ"){
      output <- subset(output, !startsWith(as.character(Participant), "A0"))
    }
    #browser()
    p<-ggplot(output, type = "n",aes(x = Xeye,y = Yeye))  +
      annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
      stat_density2d(geom = "polygon", aes(fill= ..level..), alpha = input$transparency) + 
      geom_point(size=sizeinput) +
      scale_fill_gradient(low = "green", high = "red")+#colors=rev(rainbow(100,start = 0, end=0.75))) + 
      scale_x_continuous(limits=c(0,1),expand=c(0,0))+
      scale_y_continuous(trans='reverse', limits = c(1,0), expand =c(0,0))+
      coord_fixed(ratio = 8/12, expand = TRUE)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                      axis.title.x=element_blank(),
                                                      axis.title.y=element_blank())
    # panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
    # panel.grid.minor=element_blank(),plot.background=element_blank())
    
    p
    
  }) 
  # tell server how to assemble inputs to outputs
  # save  objects to disply to output$, a list like object.
  #render creates the type of output you wish to make
}
shinyApp(ui = ui, server = server)


