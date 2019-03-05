#install.packages("readxl")
#install.packages("WriteXLS")
#install.packages("openxlsx")
rm(list = ls())

require(openxlsx)
require(WriteXLS)
require(readxl)
require(pracma)
require(tidyr)
require(dplyr)
require(utils)
# participants <- function(file){
#   pars <- list.files(file,pattern = "\\.xlsm", full.names = FALSE)
#   parsdf <- data.frame(V1 = pars)
#   parsdf <- filter(parsdf, substr(pars,1,1) != "~")
#   parsdf <- filter(parsdf, V1 != "Master.xlsm")
#   parsdf <- filter(parsdf, V1 != "_Template.xlsm")
#   parsdf <- filter(parsdf, V1 != "Master_RG.xlsm")
#   return (parsdf)
# }

#Select the Fixation Excel File
file <- file.choose()

#Make CSV files of every sheet in Master file
FFG <- read_excel(file, sheet = 2)
FFG1 <- FFG[(1:15),]
FFG2 <- FFG[(19:33),]
FSG <- read_excel(file, sheet = 3)
FSG1 <- FSG[(1:15),]
FSG2 <- FSG[(19:33),]
SFG <- read_excel(file,sheet = 4)
SFG1 <- SFG[(1:15),]
SFG2 <- SFG[(19:33),]
IG <- read_excel(file, sheet = 5)
IG1 <- IG[(1:15),]
IG2 <- IG[(19:33),]


# FF1T<- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep = "/"), sheet = 2, skip =24)
# FF2T<- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep = "/"), sheet = 3, skip =24)
# FS1T<- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep ="/"), sheet = 5, skip =24)
# FS2T <- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep ="/"), sheet = 6, skip =24)
# SF1T <- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep ="/"), sheet = 7,skip =24)
# SF2T <- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep ="/"), sheet = 8,skip =24)
# I1T <- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep ="/"), sheet = 10, skip =24)
# I2T <- read_excel(paste(file, "ExcelDocs/Master.xlsm", sep ="/"), sheet = 11, skip =24) 


#file <- paste(file, "Condition CSV files_RG", sep = "/")

# df <- read.csv(file = paste(file, "FF .csv", sep = "/"))
# #rownames(df) <- df$X
# #df$X <- NULL
# 
# dffs <- read.csv(file = paste(file, "FS .csv", sep = "/"))
# #rownames(dffs) <- dffs$X
# #dffs$X <- NULL
# 
# dfsf <- read.csv(file = paste(file, "SF .csv", sep = "/"))
# #rownames(dfsf) <- dfsf$X
# #dfsf$X <- NULL
# 
# dfi <- read.csv(file = paste(file, "I .csv", sep = "/"))
# #rownames(dfi) <- dfi$X
# #dfi$X <- NULL


saveFile <- paste(getwd(),"Master RDA files_RG/FixationPoints.rda",sep="/")
#save(FF1,FF2,FS1,FS2,SF1,SF2,I1,I2,FF1T,FF2T,FS1T,FS2T,SF1T,SF2T,I1T,I2T,file = paste(file, "Master RDA files_RG/Masters.rda", sep = "/"))
save(FFG1,FFG2,FSG1,FSG2,SFG1,SFG2,IG1,IG2,file= saveFile)

source("CleanData.R")

