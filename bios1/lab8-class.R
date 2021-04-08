rm(list = ls())
setwd("D:\\CUNY_ISPH-2020\\BIOS620_Spring_2021\\Class 8\\Lecture")
library(foreign); 
help.rct <- read.spss("data_HELP.sav")

with( help.rct, table(treat, linkstatus) )

fit <- glm( linkstatus ~ 1 + treat, family=binomial, data=help.rct); 
summary(fit)

exp(fit$coefficient[2])



###
### Titanic data
###

rm(list=ls())
setwd("D:\\CUNY_ISPH-2020\\BIOS620_Spring_2021\\Class 8\\Lecture")
load("titanic_train.rda")

table( titanic_train$Sex)
table( titanic_train$Sex, titanic_train$Survived )

fit <- glm( Survived ~ 1 + Sex, family=binomial, data=titanic_train); 
summary(fit)

exp(fit$coefficient[1])
exp(fit$coefficient[2])


