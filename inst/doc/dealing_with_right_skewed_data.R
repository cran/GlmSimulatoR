## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)
library(dplyr)
library(stats)

set.seed(1)
#Very right skewed. Skewness 2
Gamma <- rgamma(1000, shape = 1, scale = 1)
temp <- tibble(gamma = Gamma)
ggplot(temp, aes(x=gamma)) + 
  geom_histogram(bins = 30)

#Very right skewed and spread out more. Skewness 2
Gamma <- rgamma(1000, shape = 1, scale = 5)
temp <- tibble(gamma = Gamma)
ggplot(temp, aes(x=gamma)) + 
  geom_histogram(bins = 30)

#Hump moves slightly towards the middle. Skewness 1.414214
Gamma <- rgamma(1000, shape = 2, scale = 1)
temp <- tibble(gamma = Gamma)
ggplot(temp, aes(x=gamma)) + 
  geom_histogram(bins = 30)

#Hump moves slightly more towards the middle. Skewness 1.154701
Gamma <- rgamma(1000, shape = 3, scale = 1)
temp <- tibble(gamma = Gamma)
ggplot(temp, aes(x=gamma)) + 
  geom_histogram(bins = 30)

#Hump moves slightly more towards the middle. Skewness 0.8944272
Gamma <- rgamma(1000, shape = 5, scale = 1)
temp <- tibble(gamma = Gamma)
ggplot(temp, aes(x=gamma)) + 
  geom_histogram(bins = 30)

#Nearly gaussian. Very slightly right skewed. Skewness .2
Gamma <- rgamma(1000, shape = 100, scale = 1)
temp <- tibble(gamma = Gamma)
ggplot(temp, aes(x=gamma)) + 
  geom_histogram(bins = 30)


## ---- echo=FALSE--------------------------------------------------------------
rm(Gamma, temp)

## -----------------------------------------------------------------------------
#Make data
set.seed(1)
simdata <- simulate_gamma(N = 10000, link = "inverse", 
                          weights = c(1, 2, 3), ancillary = .05)
#Confirm Y ~ gamma
ggplot(simdata, aes(x = Y)) + 
  geom_histogram(bins = 30)

glm <- glm(Y ~  X1 + X2 + X3, data = simdata, family = Gamma("inverse"))

#Mean Squared Error
mean((simdata$Y - predict(glm, newdata = simdata, type = "response"))^2)

## -----------------------------------------------------------------------------
library(statmod)

set.seed(1)
Invgauss <- rinvgauss(1000, mean = 1, shape = .2)
temp <- tibble(Invgauss = Invgauss)
ggplot(temp, aes(x=Invgauss)) + 
  geom_histogram(bins = 30)

Invgauss <- rinvgauss(1000, mean = 1, shape = 1)
temp <- tibble(Invgauss = Invgauss)
ggplot(temp, aes(x=Invgauss)) + 
  geom_histogram(bins = 30)

Invgauss <- rinvgauss(1000, mean = 1, shape = 3)
temp <- tibble(Invgauss = Invgauss)
ggplot(temp, aes(x=Invgauss)) + 
  geom_histogram(bins = 30)

Invgauss <- rinvgauss(1000, mean = 10, shape = .2)
temp <- tibble(Invgauss = Invgauss)
ggplot(temp, aes(x=Invgauss)) + 
  geom_histogram(bins = 30)

Invgauss <- rinvgauss(1000, mean = 10, shape = 10)
temp <- tibble(Invgauss = Invgauss)
ggplot(temp, aes(x=Invgauss)) + 
  geom_histogram(bins = 30)

Invgauss <- rinvgauss(1000, mean = 10, shape = 100)
temp <- tibble(Invgauss = Invgauss)
ggplot(temp, aes(x=Invgauss)) + 
  geom_histogram(bins = 30)


## ---- echo=FALSE--------------------------------------------------------------
rm(Invgauss, temp)

## -----------------------------------------------------------------------------
#Make data
set.seed(1)
simdata <- simulate_inverse_gaussian(N = 10000, link = "inverse", 
                          weights = c(1, 2, 3), ancillary = 10)
#Confirm Y is right skewed
ggplot(simdata, aes(x = Y)) + 
  geom_histogram(bins = 30)

glm <- glm(Y ~  X1 + X2 + X3, data = simdata, family = inverse.gaussian(link = "inverse"))

#Mean Squared Error
mean((simdata$Y - predict(glm, newdata = simdata, type = "response"))^2)

