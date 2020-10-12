best <- function(state, outcome){
  ## to read the data file
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Validity checking
  if(!any(state==df$State)){
    stop(print("invalid state"))
  } else if((outcome %in% c("heart attack","heart failure","pneumonia")) == F){
    stop(print("invalid outcome"))
  }
  ## Subset and replace the data frame
  df1 <- subset(df,State==state)
  ## selecting column number according to outcome
  cnum <- switch(outcome,
                 "heart attack" = 11,
                 "heart failure" = 17,
                 "pneumonia" = 23)
  ## finding the minimum value in selected column
  min <- min(as.numeric(df1[,cnum]),na.rm=T)
  ## finding the row
  nmin <- which(as.numeric(df1[,cnum]) == min)
  ## finding the best hospital with minimum rate and sorting
  besthospital <- df1[nmin,2]
  return(besthospital[1])
}