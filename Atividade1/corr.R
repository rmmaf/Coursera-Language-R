source('Atividade1/toStr.R')
source('Atividade1/complete.R')
corr <- function(directory = "Atividade1/specdata/", threshold = 0){
  comp <- complete()
  comp <- subset(comp, nobs > threshold)
  id <- as.vector(comp$id)
  corVec <- c()
  if(is.null(id)){
    return(0)
  } else {
    for (i in id) {
      dir <- paste0(directory, toStr(i))
      dfAux <- read.csv(dir)
      dfAux <- na.omit(dfAux)
      dfAux$Date <- NULL
      dfAux$ID <- NULL
      corVec <- append(corVec, cor(dfAux)[1,2])
    }
    
    return(corVec)
  }
}