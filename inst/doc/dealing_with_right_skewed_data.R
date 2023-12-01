## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(stats)

set.seed(1)
# Very right skewed. Skewness 2
gamma_rv <- rgamma(1000, shape = 1, scale = 1)
temp <- tibble(gamma = gamma_rv)
ggplot(temp, aes(x = gamma)) +
  geom_histogram(bins = 30)

# Hump moves slightly more towards the middle. Skewness 1.154701
gamma_rv <- rgamma(1000, shape = 3, scale = 1)
temp <- tibble(gamma = gamma_rv)
ggplot(temp, aes(x = gamma)) +
  geom_histogram(bins = 30)

# Nearly gaussian. Very slightly right skewed. Skewness .2
gamma_rv <- rgamma(1000, shape = 100, scale = 1)
temp <- tibble(gamma = gamma_rv)
ggplot(temp, aes(x = gamma)) +
  geom_histogram(bins = 30)

## ----echo=FALSE---------------------------------------------------------------
rm(gamma_rv, temp)

## -----------------------------------------------------------------------------
# Make data
set.seed(1)
simdata <- simulate_gamma(
  N = 10000, link = "inverse",
  weights = c(1, 2, 3), ancillary = .05
)
# Confirm Y ~ gamma
ggplot(simdata, aes(x = Y)) +
  geom_histogram(bins = 30)

glm <- glm(Y ~ X1 + X2 + X3, data = simdata, family = Gamma("inverse"))

# Mean Squared Error
mean((simdata$Y - predict(glm, newdata = simdata, type = "response"))^2)

