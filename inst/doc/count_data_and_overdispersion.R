## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = T,
                      results = "hide")

## ----PoissonHistogram, echo=TRUE-----------------------------------------
library(stats)
library(MASS)
library(dplyr, warn.conflicts= FALSE)
library(ggplot2)
library(GlmSimulatoR)

set.seed(1)

#lambda = 1
poisson <- rpois(n = 10000, lambda = 1)
poissonDF <- as.data.frame(x=poisson)

ggplot(poissonDF, aes(x=poisson)) + 
  geom_histogram(bins = 100)

#lambda = 5
poisson <- rpois(n = 10000, lambda = 5)
poissonDF <- as.data.frame(x=poisson)

ggplot(poissonDF, aes(x=poisson)) + 
  geom_histogram(bins = 100)

#lambda = 10
poisson <- rpois(n = 10000, lambda = 10)
poissonDF <- as.data.frame(x=poisson)

ggplot(poissonDF, aes(x=poisson)) + 
  geom_histogram(bins = 100)


## ---- echo=FALSE---------------------------------------------------------
rm(poisson, poissonDF)


## ----PoissonGlm, echo=TRUE, results='markup'-----------------------------

set.seed(1)
simdata <- simulate_poisson(N = 10000, weights = c(.5, 1))

#Response looks similar to above histograms
ggplot(simdata, aes(x=Y)) + 
  geom_histogram(bins = 100)

glmPoisson <- glm(Y ~ X1 + X2, data = simdata, family = poisson(link = "log"))
summary(glmPoisson)


## ---- echo=FALSE---------------------------------------------------------
rm(simdata, glmPoisson)

## ----PoissonMuSigma, echo=TRUE, results='markup'-------------------------

set.seed(1)

#lambda = 1
poisson <- rpois(n = 10000, lambda = 1)
mean(poisson)
var(poisson)

#lambda = 5
poisson <- rpois(n = 10000, lambda = 5)
mean(poisson)
var(poisson)

#lambda = 10
poisson <- rpois(n = 10000, lambda = 10)
mean(poisson)
var(poisson)

## ---- echo=FALSE---------------------------------------------------------
rm(poisson)

## ----PoissonNegativeBinomial1, echo=TRUE---------------------------------
set.seed(1)

poisson <- rpois(n = 10000, lambda = 1)
poissonDF <- as.data.frame(x=poisson)

ggplot(poissonDF, aes(x=poisson)) + 
  geom_histogram(bins = 100)

negBin <- rnegbin(n = 10000, mu = 1, theta = 1000)
negBinDF <- as.data.frame(x=negBin)

ggplot(negBinDF, aes(x=negBin)) + 
  geom_histogram(bins = 100)


## ---- echo=FALSE---------------------------------------------------------
rm(poisson, poissonDF, negBin, negBinDF)

## ----PoissonNegativeBinomial2, echo=TRUE---------------------------------
set.seed(1)

poisson <- rpois(n = 10000, lambda = 1)
poissonDF <- as.data.frame(x=poisson)

ggplot(poissonDF, aes(x=poisson)) + 
  geom_histogram(bins = 100)

negBin <- rnegbin(n = 10000, mu = 1, theta = 1)
negBinDF <- as.data.frame(x=negBin)

ggplot(negBinDF, aes(x=negBin)) + 
  geom_histogram(bins = 100)

## ---- echo=FALSE---------------------------------------------------------
rm(poisson, poissonDF, negBin, negBinDF)

## ----NegativeBinomialGlm, echo=TRUE, results='markup'--------------------

set.seed(1)
simdata <- simulate_negative_binomial(N = 10000, weights = c(.5, 1), ancillary = 5) #ancillary is theta.

#Response looks like a negative binomial distribution.
ggplot(simdata, aes(x=Y)) + 
  geom_histogram(bins = 200)

glmPoisson <- glm(Y ~ X1 + X2, data = simdata, family = poisson(link = "log"))
glmNB <- glm.nb(Y ~ X1 + X2, data = simdata, link = "log")


summary(glmPoisson)
summary(glmNB)


## ---- echo=FALSE---------------------------------------------------------
rm(simdata, glmPoisson, glmNB)

