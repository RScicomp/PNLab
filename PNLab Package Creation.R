

getUniqueParlist<- function(data,rowno){
  return(unique(unlist(data[rowno,])))
}

writetoExcel<- function(dataframes, file, conditions){
  require(openxlsx)

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

master_sheets <- function(file,sheetnumber){
  require(readxl)
  master_data <-read_excel(file,sheet = sheetnumber)
  colnames(master_data) <- master_data[1,]
  master_data <-master_data[(1:17),]
  return(master_data)
}
