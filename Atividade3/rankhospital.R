rankhospital <- function(state, outcome, num = "best") {
      if(num == "best"){
            num <- 1
      }
      outcomeTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      outcomeTable[, 11] <- as.numeric(outcomeTable[, 11])
      outcomeTable[, 17] <- as.numeric(outcomeTable[, 17])
      outcomeTable[, 23] <- as.numeric(outcomeTable[, 23])
      outcomeTable <- na.omit(outcomeTable)
      
      if(any(outcomeTable$State == state)){
            outcomeTable <- split(outcomeTable, outcomeTable$State)[[state]]
            if(num == "worst"){
                  num <- nrow(outcomeTable)
            } else if(num > nrow(outcomeTable)){
                  return(NA)
            }
      } else {
            stop("invalid state")
      }
      if(outcome == "heart attack"){
            outcomeTable <- outcomeTable[order(outcomeTable[, 11], outcomeTable[, 2]),]
            return(outcomeTable[num, 2])
      } else if(outcome == "heart failure"){
            outcomeTable <- outcomeTable[order(outcomeTable[, 17], outcomeTable[, 2]),]
            return(outcomeTable[num, 2])
      } else if(outcome == "pneumonia"){
            outcomeTable <- outcomeTable[order(outcomeTable[, 23], outcomeTable[, 2]),]
            return(outcomeTable[num, 2])
      } else {
            stop("invalid outcome")
      }
}
