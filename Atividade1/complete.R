source('Atividade1/toStr.R')
complete <- function(directory = "Atividade1/specdata/", id = 1:332){
  casesVec <- c()
  idVec <- c()
  for (i in id) {
    dir <- paste0(directory, toStr(i))
    dfAux <- read.csv(dir)
    casesVec <- append(casesVec, sum(complete.cases(dfAux)))
    idVec <- append(idVec, i)
  }
  return(data.frame("id" = idVec, "nobs" = casesVec))
}