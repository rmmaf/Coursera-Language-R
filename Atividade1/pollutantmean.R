source('Atividade1/toStr.R')
pollutantmean <- function(directory = "Atividade1/specdata/", pollutant, id = 1:332){
  vec <- c()
  for (i in id) {
    dir <- paste0(directory, toStr(i))
    dfAux <- read.csv(dir)
    dfAux <- na.omit(dfAux[, pollutant])
    vec <- append(vec, as.vector(dfAux))
  }
  
  return(mean(vec))
}
