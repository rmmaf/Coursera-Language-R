toStr <- function(x){
  if (x < 10){
    return(paste0("00", x, ".csv"))
  } else if (x < 100){
    return(paste0("0", x, ".csv"))
  } else {
    return(paste0(x, ".csv"))
  }
}
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
