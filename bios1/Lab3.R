
## BIOS 620/PUBH 802 Lab 3

# Multivariate regression for confounding models

#################
# Generate data #
#################

# SL: sugar level
# SBP: sybomlic blood pressue

set.seed(1)
AGE <- rnorm(100, mean=35, sd=5)
SEX <- rbinom(100, 1, 0.5)
SL <-  rnorm(100, mean=50 + 0.01*AGE - 0.01*SEX, sd=2)
SBP <- rnorm(100, mean=109.47 + 2.00*SL + 0.5*AGE + 0.5*SEX, sd=5)

#note: both AGE and SEX are confounders in this simulation


# Univariate analysis: between outcome and study variable
fit1 <- lm(SBP~1 + SL)
summary(fit1)

# Multivariate analysis: adjusting confounder AGE 
# (omitting SEX, so, essentially, we are treating SEX as an "unobserved
#  confunder in this analysis)

fit2 <- lm(SBP~1 + SL + AGE)
summary(fit2)

# Calling a stargazer library to produce nice summary table
library(stargazer)
stargazer(fit1, fit2, title="Regression Results",
          type="text",
          style="default", header=FALSE)

###################
# Lab exercise    #
###################

# 1. set different seeds to see how the results changes, especially
#    the coefficient for SL (sugar level)
###------ Chang:  seed

myseed = 10
set.seed( myseed)
AGE <- rnorm(100, mean=35, sd=5)
SEX <- rbinom(100, 1, 0.5)
SL <-  rnorm(100, mean=50 + 0.01*AGE - 0.01*SEX, sd=2)
SBP <- rnorm(100, mean=109.47 + 2.00*SL + 0.5*AGE + 0.5*SEX, sd=5)

fit3 <- lm(SBP~1 + SL + AGE)
summary(fit3)

stargazer(fit1, fit2, fit3, title="Regression Results",
          type="text",
          style="default", header=FALSE)



# 2. choose different magnitude of confounding from the unobserved 
#    confounder SEX, re-fit the model and observe the change of the
#    impact of SL to SBP (the main study association)
rm(list=ls())
beta2 <- 0.8
sdv = 5
myseed = 10
set.seed( myseed)

AGE <- rnorm(100, mean=35, sd=5)
SEX <- rbinom(100, 1, 0.5)
SL <-  rnorm(100, mean=50 + 0.01*AGE - 0.01*SEX, sd=2)
SBP <- rnorm(100, mean=109.47 + 2.00 *SL + beta2*AGE + 0.5*SEX, sd= sdv)

fit1 <- lm(SBP~1 + SL + AGE)

#### -- change beta2
beta2 = 0.2
SBP <- rnorm(100, mean=109.47 + 2.00 *SL + beta2*AGE + 0.5*SEX, sd= sdv)

fit2 <- lm(SBP~1 + SL + AGE)


stargazer(fit1, fit2, title="Regression Results",
          type="text",
          style="default", header=FALSE)


# 3. try other changes you might consider, e.g., magnitude of error (for
#    SL and/or SBP)
###------ Chang: beta1, beta2 and sdv
rm(list=ls())
beta1 <- 1.5
beta2 <- 1
sdv = 10
myseed = 1
set.seed( myseed)
AGE <- rnorm(1000, mean=35, sd=5)
SEX <- rbinom(1000, 1, 0.5)
SL <-  rnorm(1000, mean=50 + 0.01*AGE - 0.01*SEX, sd=2)
SBP <- rnorm(1000, mean=109.47 + beta1 *SL + beta2*AGE + 0.5*SEX, sd= sdv)

fit1 <- lm(SBP~1 + SL + AGE)
summary(fit1)


# Finally, write a `for` loop to gather results from 1000 seeds and
# quantify the bias (PRB) of estimating coefficient of SL due to
# omiting confounders.

rm(list=ls())
beta1.true <- 1.5

beta1.c <- c()

for( myseed in 1:1000){
  set.seed( myseed)
  AGE <- rnorm(1000, mean=35, sd=5)
  SEX <- rbinom(1000, 1, 0.5)
  SL <-  rnorm(1000, mean=50 + 0.01*AGE - 0.01*SEX, sd=2)
  SBP <- rnorm(1000, mean=109.47 + beta1.true *SL + 0.5*AGE + 0.5*SEX, sd= 5)
  
  myfit <- lm(SBP~1 + SL + AGE + SEX)
  summary(myfit)
  beta1.c <- c( beta1.c, coefficients(myfit)["SL"] )
  
  beta1.c
}
print(mean((beta1.c - beta1.true) / beta1.true) * 100) 




