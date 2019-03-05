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

file <- file.choose()
#Make CSV files of every sheet in Master file
FFp <- read_excel(file, sheet = 1)
FF1 <- read_excel(file, sheet = 2)
FF2 <- read_excel(file, sheet = 3)
FSp <- read_excel(file, sheet = 4)
FS1 <- read_excel(file, sheet = 5)
FS2 <- read_excel(file, sheet = 6)
SF1 <- read_excel(file, sheet = 7)
SF2 <- read_excel(file, sheet = 8)
Ip <- read_excel(file, sheet = 9)
I1 <- read_excel(file, sheet = 10)
I2 <- read_excel(file, sheet = 11) 

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


file = getwd()
#save(FF1,FF2,FS1,FS2,SF1,SF2,I1,I2,FF1T,FF2T,FS1T,FS2T,SF1T,SF2T,I1T,I2T,file = paste(file, "Master RDA files_RG/Masters.rda", sep = "/"))
save(FF1,FF2,FS1,FS2,SF1,SF2,I1,I2,file = paste(file, "Master RDA files_RG/Masters.rda", sep = "/"))

