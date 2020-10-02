library(dplyr)

df <- read.csv("hw1_data.csv",header=T)

names(df)

df[1:2,]
dim(df)
tail(df,2)
df[47,]

is.na(df$Ozone) %>% which() %>% length()

df$Ozone %>% mean(.,na.rm=T)

df[which(df$Ozone>31 & df$Temp>90),"Solar.R"] %>% mean()
df[which(df$Month == 6),"Temp"] %>% mean()
df[which(df$Month == 5),] %>% summary()

