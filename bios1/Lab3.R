
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

# 2. choose different magnitude of confounding from the unobserved 
#    confounder SEX, re-fit the model and observe the change of the
#    impact of SL to SBP (the main study association)

# 3. try other changes you might consider, e.g., magnitude of error (for
#    SL and/or SBP)

# Finally, write a `for` loop to gather results from 1000 seeds and
# quantify the bias (PRB) of estimating coefficient of SL due to
# omiting confounders.

