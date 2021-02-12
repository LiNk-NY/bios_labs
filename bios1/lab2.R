### -- Lab 2: Simple linear regression
rm(list=ls())
data()

## working dir
setwd( "C:/Users/Binod/Desktop/CUNY_ISPH-2020/BIOS620_Spring_2021 (1)/BIOS620_Spring_2021/Class 2/Lab2" )

## mtcars (Motor Trend Car Road Tests)
data("mtcars")

## Data information
?mtcars
dim(mtcars)
head(mtcars)



write.table(mtcars, file = "mtcars.txt",  sep = "\t")

mydata <- read.table("mtcars.txt")

## -- Check plots

hist(mydata$mpg, main ="Histogram of MPG")

boxplot( cbind( Disp =mydata$disp, HP=mydata$hp) )

pairs(mtcars, main = "mtcars data", gap = .5)


##-- Simple linear regression between MPG and Wt
plot(mydata$mpg, mydata$wt)

myfit <- lm( mpg ~ wt, data = mydata)
summary(myfit)
myfit

##-- plot fitted values
yhat<- predict(myfit) 
plot( mydata$mpg, yhat)
abline(a=0,b=1)


## Read SAS data file

setwd("C:/Program Files/SASUniversityEdition/myfolders")
library("sas7bdat")
mysas =  read.sas7bdat(file = "French_males.sas7bdat")
dim(mysas)
head(mysas)


# rm(list=ls())
# install.packages("haven")
# library("haven")
# haven::read_sas("French_males.sas7bdat")
