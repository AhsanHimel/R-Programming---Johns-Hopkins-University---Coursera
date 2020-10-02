pollutantmean <- function(directory, pollutant, id=1:332){
  vals <- c()
  for (filename in id){
    filedir <- paste0(directory,"/", sprintf("%03d",filename),".csv")
    data <- read.csv(filedir)[pollutant]
    vals <- c(vals, data[!is.na(data)])
  }
  return(mean(vals))
}

pollutantmean("specdata","sulfate")
pollutantmean("specdata","nitrate")