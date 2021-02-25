## Lab 4 

rm(list=ls())
setwd("C:/Users/Binod/Desktop/CUNY_ISPH-2020/BIOS620_Spring_2021/BIOS620_Spring_2021/Class 4/Lab4/")
## mtcars (Motor Trend Car Road Tests)
data("mtcars")

## Data information
?mtcars
dim(mtcars)
head(mtcars)


## 
summary(mtcars$mpg)
hist(mtcars$mpg, main = "Histogram mpg")

## am	Transmission: (0 = automatic, 1 = manual)
table(mtcars$am)

fit1 <- lm( mpg ~ 1 + am, data = mtcars)
summary(fit1)

## switch am coding: (0=manual, 1=automatic)
mtcars$am.new <- ifelse(mtcars$am == 0, 1, 0)
fit2 <- lm( mpg ~ 1 + factor(am.new), data = mtcars)
summary(fit2)

## check coefficients between fit1 and fit2
fit1
fit2


### covariates: am and hp
fit3 <- lm( mpg ~ 1 + factor(am) + hp, data = mtcars)
summary(fit3)

## covariates: am, hp and cyl
table(mtcars$cyl)
barplot(table(mtcars$cyl))

fit4 <- lm( mpg ~ 1 + factor(am) + hp + factor(cyl), data = mtcars)
summary(fit4)



#read SPSS data file
rm(list=ls())
setwd("C:/Users/Binod/Desktop/CUNY_ISPH-2020/BIOS620_Spring_2021/BIOS620_Spring_2021/Class 4/Lab4/")
library(foreign)

## Read SPSS data
nhanes <- read.spss("NHANES_Lab4.sav", to.data.frame = TRUE)

