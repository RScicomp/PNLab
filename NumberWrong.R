library(PNLabStats)
library(readxl)
library(openxlsx)
library(foreach)
library(doParallel)
rm(list = ls())
file <-file.choose()

system.time({
MFF <-master_sheets(file,2)
MFF2 <-master_sheets(file,3)
MFS <-master_sheets(file,5)
MFS2 <-master_sheets(file,6)
MSF <-master_sheets(file,7)
MSF2 <-master_sheets(file,8)
MI <-master_sheets(file,10)
MI2 <-master_sheets(file,11)})


parlist <-getUniqueParlist(MFF,1)
parlist <-na.omit(parlist)

registerDoParallel(8)

system.time(x<-foreach (i = (1: length(parlist)), .combine = rbind) %dopar% {
  #FF
  participant1 <- MFF[,grepl(parlist[i],names(MFF))]
  participant2 <- MFF2[,grepl(parlist[i],names(MFF2))]
  
  zeroCountLFEye<-0
  zeroCountRFEye<-0
  zeroCountNoseLF<-0
  zeroCountNoseRF<-0
  zeroMouthLF<-0
  zeroMouthRF<-0
  zeroHairLF<-0
  zeroHairRF<-0
  zeroJawLF<-0
  zeroJawRF<-0
  zeroTotal<-0
  
  for (j in seq(2,ncol(participant1),3)){
    if (participant1[3,j] == 0){
      zeroCountLFEye <- zeroCountLFEye+1
    }
    if(participant2[3,j] == 0){
      zeroCountLFEye<- zeroCountLFEye+1
    }
    if (participant1[4,j] == 0){
      zeroCountRFEye <- zeroCountRFEye+1
    }
    if(participant2[4,j] == 0){
      zeroCountRFEye<- zeroCountRFEye+1
    }
    if (participant1[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if(participant2[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if (participant1[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if(participant2[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if (participant1[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if(participant2[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if (participant1[8,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if(participant2[8,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if (participant1[9,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if(participant2[9,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if (participant1[10,j] == 0){
      zeroJawLF<-   zeroJawLF+1
    }
    if(participant2[10,j] == 0){
      zeroJawLF<- zeroJawLF+1
    }
    if (participant1[11,j] == 0){
      zeroJawRF<-   zeroJawRF+1
    }
    if(participant2[11,j] == 0){
      zeroJawRF<- zeroJawRF+1
    }
    if(participant2[12,j] == 0){
      zeroTotal<- zeroTotal+1
    }

  }
  list(zeroCountLFEye,zeroCountRFEye,
    zeroCountNoseLF,
    zeroCountNoseRF,
    zeroMouthLF,
    zeroMouthRF,
    zeroHairLF,
    zeroHairRF,
    zeroJawLF,
    zeroJawRF,
    zeroTotal)

})

system.time(FFNeverGaze<-foreach (i = (1: length(parlist)), .combine = rbind) %dopar% {
  #FF
  participant1 <- MFF[,grepl(parlist[i],names(MFF))]
  participant2 <- MFF2[,grepl(parlist[i],names(MFF2))]
  
  zeroCountLFEye<-0
  zeroCountRFEye<-0
  zeroCountNoseLF<-0
  zeroCountNoseRF<-0
  zeroMouthLF<-0
  zeroMouthRF<-0
  zeroHairLF<-0
  zeroHairRF<-0
  zeroJawLF<-0
  zeroJawRF<-0
  zeroTotal<-0
  zeroAll <-0
  
  for (j in seq(2,ncol(participant1),3)){
    if (participant1[3,j] == 0){
      zeroCountLFEye <- zeroCountLFEye+1
    }
    if(participant2[3,j] == 0){
      zeroCountLFEye<- zeroCountLFEye+1
    }
    if (participant1[4,j] == 0){
      zeroCountRFEye <- zeroCountRFEye+1
    }
    if(participant2[4,j] == 0){
      zeroCountRFEye<- zeroCountRFEye+1
    }
    if (participant1[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if(participant2[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if (participant1[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if(participant2[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if (participant1[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if(participant2[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if (participant1[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if(participant2[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if (participant1[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if(participant2[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if (participant1[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if(participant2[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if (participant1[11,j] == 0){
      zeroJawLF<-   zeroJawLF+1
    }
    if(participant2[11,j] == 0){
      zeroJawLF<- zeroJawLF+1
    }
    if (participant1[12,j] == 0){
      zeroJawRF<-   zeroJawRF+1
    }
    if(participant2[12,j] == 0){
      zeroJawRF<- zeroJawRF+1
    }
    if(participant1[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant2[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant1[3,j] == 0 && participant1[4,j]==0 && participant1[5,j]==0 && participant1[6,j] == 0 &&
       participant1[7,j] ==0 && participant1[8,j] == 0 && participant1[9,j]==0 && participant1[10,j]==0 &&
       participant1[11,j]==0 && participant1[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    if(participant2[3,j] == 0 && participant2[4,j]==0 && participant2[5,j]==0 && participant2[6,j] == 0 &&
       participant2[7,j] ==0 && participant2[8,j] == 0 && participant2[9,j]==0 && participant2[10,j]==0 &&
       participant2[11,j]==0 && participant2[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    
  }
  list(zeroCountLFEye,zeroCountRFEye,
       zeroCountNoseLF,
       zeroCountNoseRF,
       zeroMouthLF,
       zeroMouthRF,
       zeroHairLF,
       zeroHairRF,
       zeroJawLF,
       zeroJawRF,
       zeroTotal,zeroAll)
  
})
FFNeverGaze<-as.data.frame(FFNeverGaze)
colnames(FFNeverGaze) <- c("LFeye","RFeye","LFNose","RFNose","LFMouth","RFMouth","LFHair","RFHair","LFJaw","RFJaw","Total","All")
rownames(FFNeverGaze) <- parlist
system.time(FSNeverGaze<-foreach (i = (1: length(parlist)), .combine = rbind) %dopar% {
  #FF
  participant1 <- MFS[,grepl(parlist[i],names(MFS))]
  participant2 <- MFS2[,grepl(parlist[i],names(MFS2))]
  
  zeroCountLFEye<-0
  zeroCountRFEye<-0
  zeroCountNoseLF<-0
  zeroCountNoseRF<-0
  zeroMouthLF<-0
  zeroMouthRF<-0
  zeroHairLF<-0
  zeroHairRF<-0
  zeroJawLF<-0
  zeroJawRF<-0
  zeroTotal<-0
  zeroAll <-0
  
  for (j in seq(2,ncol(participant1),3)){
    if (participant1[3,j] == 0){
      zeroCountLFEye <- zeroCountLFEye+1
    }
    if(participant2[3,j] == 0){
      zeroCountLFEye<- zeroCountLFEye+1
    }
    if (participant1[4,j] == 0){
      zeroCountRFEye <- zeroCountRFEye+1
    }
    if(participant2[4,j] == 0){
      zeroCountRFEye<- zeroCountRFEye+1
    }
    if (participant1[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if(participant2[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if (participant1[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if(participant2[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if (participant1[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if(participant2[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if (participant1[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if(participant2[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if (participant1[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if(participant2[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if (participant1[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if(participant2[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if (participant1[11,j] == 0){
      zeroJawLF<-   zeroJawLF+1
    }
    if(participant2[11,j] == 0){
      zeroJawLF<- zeroJawLF+1
    }
    if (participant1[12,j] == 0){
      zeroJawRF<-   zeroJawRF+1
    }
    if(participant2[12,j] == 0){
      zeroJawRF<- zeroJawRF+1
    }
    if(participant1[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant2[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant1[3,j] == 0 && participant1[4,j]==0 && participant1[5,j]==0 && participant1[6,j] == 0 &&
       participant1[7,j] ==0 && participant1[8,j] == 0 && participant1[9,j]==0 && participant1[10,j]==0 &&
       participant1[11,j]==0 && participant1[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    if(participant2[3,j] == 0 && participant2[4,j]==0 && participant2[5,j]==0 && participant2[6,j] == 0 &&
       participant2[7,j] ==0 && participant2[8,j] == 0 && participant2[9,j]==0 && participant2[10,j]==0 &&
       participant2[11,j]==0 && participant2[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    
  }
  list(zeroCountLFEye,zeroCountRFEye,
       zeroCountNoseLF,
       zeroCountNoseRF,
       zeroMouthLF,
       zeroMouthRF,
       zeroHairLF,
       zeroHairRF,
       zeroJawLF,
       zeroJawRF,
       zeroTotal,zeroAll)
  
})
FSNeverGaze<-as.data.frame(FSNeverGaze)
colnames(FSNeverGaze) <- c("LFeye","RFeye","LFNose","RFNose","LFMouth","RFMouth","LFHair","RFHair","LFJaw","RFJaw","Total","All")
rownames(FSNeverGaze) <- parlist
system.time(SFNeverGaze<-foreach (i = (1: length(parlist)), .combine = rbind) %dopar% {
  #FF
  participant1 <- MSF[,grepl(parlist[i],names(MSF))]
  participant2 <- MSF2[,grepl(parlist[i],names(MSF2))]
  
  zeroCountLFEye<-0
  zeroCountRFEye<-0
  zeroCountNoseLF<-0
  zeroCountNoseRF<-0
  zeroMouthLF<-0
  zeroMouthRF<-0
  zeroHairLF<-0
  zeroHairRF<-0
  zeroJawLF<-0
  zeroJawRF<-0
  zeroTotal<-0
  zeroAll <-0
  
  for (j in seq(2,ncol(participant1),3)){
    if (participant1[3,j] == 0){
      zeroCountLFEye <- zeroCountLFEye+1
    }
    if(participant2[3,j] == 0){
      zeroCountLFEye<- zeroCountLFEye+1
    }
    if (participant1[4,j] == 0){
      zeroCountRFEye <- zeroCountRFEye+1
    }
    if(participant2[4,j] == 0){
      zeroCountRFEye<- zeroCountRFEye+1
    }
    if (participant1[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if(participant2[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if (participant1[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if(participant2[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if (participant1[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if(participant2[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if (participant1[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if(participant2[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if (participant1[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if(participant2[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if (participant1[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if(participant2[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if (participant1[11,j] == 0){
      zeroJawLF<-   zeroJawLF+1
    }
    if(participant2[11,j] == 0){
      zeroJawLF<- zeroJawLF+1
    }
    if (participant1[12,j] == 0){
      zeroJawRF<-   zeroJawRF+1
    }
    if(participant2[12,j] == 0){
      zeroJawRF<- zeroJawRF+1
    }
    if(participant1[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant2[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant1[3,j] == 0 && participant1[4,j]==0 && participant1[5,j]==0 && participant1[6,j] == 0 &&
       participant1[7,j] ==0 && participant1[8,j] == 0 && participant1[9,j]==0 && participant1[10,j]==0 &&
       participant1[11,j]==0 && participant1[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    if(participant2[3,j] == 0 && participant2[4,j]==0 && participant2[5,j]==0 && participant2[6,j] == 0 &&
       participant2[7,j] ==0 && participant2[8,j] == 0 && participant2[9,j]==0 && participant2[10,j]==0 &&
       participant2[11,j]==0 && participant2[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    
  }
  list(zeroCountLFEye,zeroCountRFEye,
       zeroCountNoseLF,
       zeroCountNoseRF,
       zeroMouthLF,
       zeroMouthRF,
       zeroHairLF,
       zeroHairRF,
       zeroJawLF,
       zeroJawRF,
       zeroTotal,zeroAll)
  
})
SFNeverGaze<-as.data.frame(SFNeverGaze)
colnames(SFNeverGaze) <- c("LFeye","RFeye","LFNose","RFNose","LFMouth","RFMouth","LFHair","RFHair","LFJaw","RFJaw","Total","All")
rownames(SFNeverGaze) <- parlist
system.time(INeverGaze<-foreach (i = (1: length(parlist)), .combine = rbind) %dopar% {
  #FF
  participant1 <- MI[,grepl(parlist[i],names(MI))]
  participant2 <- MI2[,grepl(parlist[i],names(MI2))]
  
  zeroCountLFEye<-0
  zeroCountRFEye<-0
  zeroCountNoseLF<-0
  zeroCountNoseRF<-0
  zeroMouthLF<-0
  zeroMouthRF<-0
  zeroHairLF<-0
  zeroHairRF<-0
  zeroJawLF<-0
  zeroJawRF<-0
  zeroTotal<-0
  zeroAll <-0
  
  for (j in seq(2,ncol(participant1),3)){
    if (participant1[3,j] == 0){
      zeroCountLFEye <- zeroCountLFEye+1
    }
    if(participant2[3,j] == 0){
      zeroCountLFEye<- zeroCountLFEye+1
    }
    if (participant1[4,j] == 0){
      zeroCountRFEye <- zeroCountRFEye+1
    }
    if(participant2[4,j] == 0){
      zeroCountRFEye<- zeroCountRFEye+1
    }
    if (participant1[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if(participant2[5,j] == 0){
      zeroCountNoseLF<- zeroCountNoseLF+1
    }
    if (participant1[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if(participant2[6,j] == 0){
      zeroCountNoseRF<- zeroCountNoseRF+1
    }
    if (participant1[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if(participant2[7,j] == 0){
      zeroMouthLF<- zeroMouthLF+1
    }
    if (participant1[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if(participant2[8,j] == 0){
      zeroMouthRF<- zeroMouthRF+1
    }
    if (participant1[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if(participant2[9,j] == 0){
      zeroHairLF<-  zeroHairLF+1
    }
    if (participant1[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if(participant2[10,j] == 0){
      zeroHairRF<-  zeroHairRF+1
    }
    if (participant1[11,j] == 0){
      zeroJawLF<-   zeroJawLF+1
    }
    if(participant2[11,j] == 0){
      zeroJawLF<- zeroJawLF+1
    }
    if (participant1[12,j] == 0){
      zeroJawRF<-   zeroJawRF+1
    }
    if(participant2[12,j] == 0){
      zeroJawRF<- zeroJawRF+1
    }
    if(participant1[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant2[13,j] == 0){
      zeroTotal<- zeroTotal+1
    }
    if(participant1[3,j] == 0 && participant1[4,j]==0 && participant1[5,j]==0 && participant1[6,j] == 0 &&
       participant1[7,j] ==0 && participant1[8,j] == 0 && participant1[9,j]==0 && participant1[10,j]==0 &&
       participant1[11,j]==0 && participant1[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    if(participant2[3,j] == 0 && participant2[4,j]==0 && participant2[5,j]==0 && participant2[6,j] == 0 &&
       participant2[7,j] ==0 && participant2[8,j] == 0 && participant2[9,j]==0 && participant2[10,j]==0 &&
       participant2[11,j]==0 && participant2[12,j]==0){
      zeroAll <- zeroAll + 1
    }
    
  }
  list(zeroCountLFEye,zeroCountRFEye,
       zeroCountNoseLF,
       zeroCountNoseRF,
       zeroMouthLF,
       zeroMouthRF,
       zeroHairLF,
       zeroHairRF,
       zeroJawLF,
       zeroJawRF,
       zeroTotal,zeroAll)
  
})
INeverGaze<-as.data.frame(INeverGaze)
colnames(INeverGaze) <- c("LFeye","RFeye","LFNose","RFNose","LFMouth","RFMouth","LFHair","RFHair","LFJaw","RFJaw","Total","All")
rownames(INeverGaze) <- parlist
dataframes<-list(FFNeverGaze,FSNeverGaze,SFNeverGaze,INeverGaze)
conditions <-list("FF","FS","SF","I")


wb <- createWorkbook()

for (i in 1: length(conditions)){
  addWorksheet(wb, conditions[i])
}
#browser()
for (j in 1: length(dataframes)){
  #browser()
  writeDataTable(wb, conditions[[j]], as.data.frame(dataframes[[j]]), rowNames = TRUE) 
  
}
saveWorkbook(wb, paste(getwd(),"/NeverGazed.xls",sep=""),overwrite = TRUE)


writeToExcel(dataframes,paste(getwd(),"/NeverGazed.xls",sep=""), conditions)


