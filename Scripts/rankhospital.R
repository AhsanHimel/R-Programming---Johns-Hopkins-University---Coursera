rankhospital<- function(state, outcome, num = "best"){
  ## reading data
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  ## validity check
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  ## taking required subset
  outcome2 <- subset(outcome1, State == state)
  cnum <- switch(outcome,
                 "heart attack" = 11,
                 "heart failure" = 17,
                 "pneumonia" = 23)
  outcome2[ ,cnum] <- as.numeric(outcome2[ ,cnum])
  outcome3 <- outcome2[order(outcome2[ ,cnum],outcome2[,2]), ]
  outcome3 <- outcome3[(!is.na(outcome3[ ,cnum])),]
  if(num == "best"){
    num <- 1
  }            
  else if (num == "worst"){
    num <- nrow(outcome3)
  }      
  return(outcome3[num,2])
}