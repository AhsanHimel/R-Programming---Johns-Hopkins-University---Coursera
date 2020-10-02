complete <- function(directory, id = 1:332){
  output <- data.frame(id=numeric(0), 
                       nobs=numeric(0))
  for(i in id){
    filedir <- paste0(directory,"/", sprintf("%03d",i),".csv")
    df <- read.csv(filedir)
    output <- rbind(output, 
                    data.frame(id=i,
                               nobs=length(which(complete.cases(df)))))
  }
  return(output)
}

complete("specdata",1:10)