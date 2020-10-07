#### Question 1 ####

library(datasets)
data(iris)
str(iris)

summary(iris[iris$Species=="virginica",])
# ans: 7

#### Question 2 ####

apply(iris[, 1:4], 2, mean)

#### Question 3 ####

data(mtcars)
str(mtcars)

with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)

#### Question 4 ####

library(dplyr)

(mean(mtcars$hp[mtcars$cyl==4]) - mean(mtcars$hp[mtcars$cyl==8])) %>% abs()
# ans. 127

#### Question 5 ####

debug(ls)
ls()
# type help at the Browse[]> to see what can be done here
# Ans. Execution of 'ls' will suspend at the beginning of the function and you will be in the browser. 
