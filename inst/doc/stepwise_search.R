## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)
library(MASS)

#Creating data to work with
set.seed(1)
simdata <- simulate_inverse_gaussian(N = 100000, link = "1/mu^2", 
                                     weights = c(1, 2, 3), unrelated = 3)

#Y looks like an inverse gaussian distribution. 
ggplot(simdata, aes(x=Y)) +
  geom_histogram(bins = 30)

#Setting the simplest model and the most complex model.
scopeArg <- list(
  lower = Y ~ 1,
  upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3
)

#Run search
startingModel <- glm(Y ~ 1, data = simdata, family = inverse.gaussian(link = "1/mu^2"))
glmSearch <- stepAIC(startingModel, scopeArg)

summary(glmSearch)

rm(simdata, scopeArg, glmSearch, startingModel)



## -----------------------------------------------------------------------------
#Creating data to work with
set.seed(2)
simdata <- simulate_inverse_gaussian(N = 100000, link = "1/mu^2", 
                                     weights = c(1, 2, 3), unrelated = 20)

#Y looks like an inverse gaussian distribution. 
ggplot(simdata, aes(x=Y)) +
  geom_histogram(bins = 30)

#Setting the simplest model and the most complex model.
scopeArg <- list(
  lower = Y ~ 1,
  upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3 + Unrelated3 + 
    Unrelated4 + Unrelated5 + Unrelated6 + Unrelated7 + Unrelated8 + Unrelated9 + 
    Unrelated10 + Unrelated11 + Unrelated12 + Unrelated13 + Unrelated14 + Unrelated15 + 
    Unrelated16 + Unrelated17 + Unrelated18 + Unrelated19 + Unrelated20
)

#Run search
startingModel <- glm(Y ~ 1, data = simdata, family = inverse.gaussian(link = "1/mu^2"))
glmSearch <- stepAIC(startingModel, scopeArg)

summary(glmSearch)

rm(simdata, scopeArg, glmSearch, startingModel)

## -----------------------------------------------------------------------------
#Creating data to work with
set.seed(3)
simdata <- simulate_inverse_gaussian(N = 1000, link = "1/mu^2", 
                                     weights = c(1, 2, 3), unrelated = 3)

#Y looks like an inverse gaussian distribution. 
ggplot(simdata, aes(x=Y)) +
  geom_histogram(bins = 30)

#Setting the simplest model and the most complex model.
scopeArg <- list(
  lower = Y ~ 1,
  upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3
)

#Run search
startingModel <- glm(Y ~ 1, data = simdata, family = inverse.gaussian(link = "1/mu^2"))
glmSearch <- stepAIC(startingModel, scopeArg)

summary(glmSearch)

rm(simdata, scopeArg, glmSearch, startingModel)

## -----------------------------------------------------------------------------
#Creating data to work with
set.seed(4)
simdata <- simulate_inverse_gaussian(N = 1000, link = "1/mu^2", 
                                     weights = c(1, 2, 3), unrelated = 20)

#Y looks like an inverse gaussian distribution. 
ggplot(simdata, aes(x=Y)) +
  geom_histogram(bins = 30)

#Setting the simplest model and the most complex model.
scopeArg <- list(
  lower = Y ~ 1,
  upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3 + Unrelated3 + 
    Unrelated4 + Unrelated5 + Unrelated6 + Unrelated7 + Unrelated8 + Unrelated9 + 
    Unrelated10 + Unrelated11 + Unrelated12 + Unrelated13 + Unrelated14 + Unrelated15 + 
    Unrelated16 + Unrelated17 + Unrelated18 + Unrelated19 + Unrelated20
)

#Run search
startingModel <- glm(Y ~ 1, data = simdata, family = inverse.gaussian(link = "1/mu^2"))
glmSearch <- stepAIC(startingModel, scopeArg)

summary(glmSearch)

rm(simdata, scopeArg, glmSearch, startingModel)

