## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example1------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)

set.seed(1)
simdata <- simulate_gaussian(N = 100, weights = 1, xrange = 10, ancillary = 1) #GlmSimulatoR function
ggplot(simdata, aes(x = X1, y = Y)) + 
  geom_point()

rm(simdata)

## ----setup---------------------------------------------------------------
set.seed(1)
simdata <- simulate_gaussian(N = 200, weights = c(1, 2, 3))

glmModel <- glm(Y ~ X1 + X2 + X3, data = simdata, family = gaussian(link = "identity"))
summary(glmModel)$coefficients

rm(glmModel)

## ------------------------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)

set.seed(1)
simdata <- simulate_gaussian(link = "identity")

linearModel <- lm(Y ~ X1 + X2 + X3, data = simdata)
glmModel <- glm(Y ~ X1 + X2 + X3, data = simdata, family = gaussian(link = "identity"))

summary(linearModel)$coefficients
summary(glmModel)$coefficients

rm(simdata, linearModel, glmModel)

