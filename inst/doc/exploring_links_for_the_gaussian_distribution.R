## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)
library(stats)

set.seed(1)
simdata <- simulate_gaussian(
  N = 1000, weights = c(1, 3), link = "inverse",
  unrelated = 1, ancillary = .005
)

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = Y)) +
  geom_histogram(bins = 30)

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = X1, y = Y)) +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = X2, y = Y)) +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = Unrelated1, y = Y)) +
  geom_point()

## -----------------------------------------------------------------------------
cor(x = simdata$X1, y = simdata$Y)
cor(x = simdata$X2, y = simdata$Y)
cor(x = simdata$Unrelated1, y = simdata$Y)

## -----------------------------------------------------------------------------
glm_inverse_x2 <- glm(Y ~ X2,
  data = simdata,
  family = gaussian(link = "inverse")
)
glm_inverse_x1_x2 <- glm(Y ~ X1 + X2,
  data = simdata,
  family = gaussian(link = "inverse")
)
glm_inverse_x1x2u1 <- glm(Y ~ X1 + X2 + Unrelated1,
  data = simdata,
  family = gaussian(link = "inverse")
)

summary(glm_inverse_x2)$aic
summary(glm_inverse_x1_x2)$aic # correct model
summary(glm_inverse_x1x2u1)$aic

## -----------------------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)
library(stats)

set.seed(1)
simdata <- simulate_gaussian(
  N = 1000, weights = c(.3, .8), link = "log",
  unrelated = 1, ancillary = 1
)

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = Y)) +
  geom_histogram(bins = 30)

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = X1, y = Y)) +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = X2, y = Y)) +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(simdata, aes(x = Unrelated1, y = Y)) +
  geom_point()

## -----------------------------------------------------------------------------
cor(x = simdata$X1, y = simdata$Y)
cor(x = simdata$X2, y = simdata$Y)
cor(x = simdata$Unrelated1, y = simdata$Y)

## -----------------------------------------------------------------------------
glm_identity <- glm(Y ~ X1 + X2,
  data = simdata,
  family = gaussian(link = "identity")
)
glm_inverse <- glm(Y ~ X1 + X2,
  data = simdata,
  family = gaussian(link = "inverse")
)
glm_log <- glm(Y ~ X1 + X2,
  data = simdata,
  family = gaussian(link = "log")
)

summary(glm_identity)$aic
summary(glm_inverse)$aic
summary(glm_log)$aic # correct model.

