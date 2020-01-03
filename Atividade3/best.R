best <- function(state, outcome) {
  ## Read outcome data
  outcomeTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(any(outcomeTable$State == state)){
    outcomeTable <- split(outcomeTable, outcomeTable$State)[[state]]
  } else {
    stop("invalid state")
  }
  if(outcome == "heart attack"){
    outcomeTable[, 11] <- as.numeric(outcomeTable[, 11])
    outcomeTable <- outcomeTable[order(outcomeTable[, 11], outcomeTable[, 2]),]
    return(outcomeTable[1, 2])
  } else if(outcome == "heart failure"){
    outcomeTable[, 17] <- as.numeric(outcomeTable[, 17])
    outcomeTable <- outcomeTable[order(outcomeTable[, 17], outcomeTable[, 2]),]
    return(outcomeTable[1, 2])
  } else if(outcome == "pneumonia"){
    outcomeTable[, 23] <- as.numeric(outcomeTable[, 23])
    outcomeTable <- outcomeTable[order(outcomeTable[, 23], outcomeTable[, 2]),]
    return(outcomeTable[1, 2])
  } else {
    stop("invalid outcome")
  }
}
