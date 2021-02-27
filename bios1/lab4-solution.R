### Lab-4

# BIOS 620/820 Lab 4
rm(list=ls())
setwd("D:/CUNY_ISPH-2020/BIOS620_Spring_2021/Class 4/Lab4/")
library(foreign)

## Read SPSS data
nhanes <- read.spss("NHANES_Lab4.sav", to.data.frame = TRUE)

# Using the NHANES data (SPSS format) to fit a multivariate model
# with BPXSY1 as response variable, Education as study variable,
# Exactage and Male as confounders.


##- describe each variable numerically and visually
summary( nhanes$BPXSY1 ) 
summary( nhanes$Exactage ) 
table(nhanes$Male)
table(nhanes$Education)

hist( nhanes$BPXSY1 ) 
boxplot( nhanes[, c("BPXSY1", "Exactage")] ) 

# describe pairwise relationship between the study variable and
# outcome variable numerically and visually (no hypothesis
#                                            testing)

pairs(nhanes[, c("BPXSY1", "Exactage", "Education", "Male") ])
cor ( nhanes[, c("BPXSY1", "Exactage", "Male") ], use = "pairwise.complete.obs")



# for the Education variable (which is a categorical variable), fit
# the model in two ways and compare the results:
#   I creating two dummay variables using if/else condition
# I using the factor() function
# I Define/change reference category for categorical variable in R

## Fitting with categorical variable "Male"
fit1 <- lm(BPXSY1 ~ 1 + factor(Male), data = nhanes)
summary(fit1)


## Education variable
## Using factor funtion
fit2 <- lm(BPXSY1 ~ 1 + factor(Male) + factor(Education), data = nhanes)
summary(fit2)


## Education variable
## Defining two dummy variables
levels(nhanes$Education)
levels(nhanes$Education) <- c("HS0", "HS1", "College")


## Defining two dummy variable, one way
nhanes$HS1     <- ifelse(nhanes$Education == "HS1",    1, 0)
nhanes$College <- ifelse(nhanes$Education == "College", 1, 0)
fit3 <- lm(BPXSY1 ~ 1 + factor(Male) + HS1 + College, data = nhanes)
summary(fit3)

## Defining two dummy variable, another way
nhanes$HS0     <- ifelse(nhanes$Education == "HS0",    1, 0)
nhanes$HS1     <- ifelse(nhanes$Education == "HS1",    1, 0)
fit4 <- lm(BPXSY1 ~ 1 + factor(Male) + HS0 + HS1, data = nhanes)
summary(fit4)

## Male, Exactage, Education variables
fit5 <- lm(BPXSY1 ~ 1 + factor(Male) + Exactage + factor(Education), data = nhanes)
summary(fit5)


