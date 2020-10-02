corr <- function(directory, threshold=0){
  comp <- complete(directory)
  complete_dfs <- comp[comp$nobs > threshold, ]
  corrs <- numeric(0)
  for(i in complete_dfs$id){
    filedir <- paste0(directory,"/", sprintf("%03d",i),".csv")
    file <- read.csv(filedir)
    complete_rows <- complete.cases(file)
    obs <- file[complete_rows,]
    corrs <- c(corrs,
              cor(obs$sulfate, obs$nitrate))
  }
  corrs
}


corr("specdata",1000)