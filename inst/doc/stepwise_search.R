## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GlmSimulatoR)
library(MASS)

# Creating data to work with
set.seed(1)
simdata <- simulate_inverse_gaussian(
  N = 100000, link = "1/mu^2",
  weights = c(1, 2, 3), unrelated = 3
)

# Setting the simplest model and the most complex model.
scope_arg <- list(
  lower = Y ~ 1,
  upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3
)

# Run search
starting_model <- glm(Y ~ 1,
  data = simdata,
  family = inverse.gaussian(link = "1/mu^2")
)
glm_search <- stepAIC(starting_model, scope_arg, trace = 0)

summary(glm_search)

rm(simdata, scope_arg, glm_search, starting_model)

## -----------------------------------------------------------------------------
# Creating data to work with
set.seed(4)
simdata <- simulate_inverse_gaussian(
  N = 1000, link = "1/mu^2",
  weights = c(1, 2, 3), unrelated = 20
)
# Setting the simplest model and the most complex model.
scope_arg <- list(
  lower = Y ~ 1,
  upper = Y ~ X1 + X2 + X3 + Unrelated1 + Unrelated2 + Unrelated3 +
    Unrelated4 + Unrelated5 + Unrelated6 + Unrelated7 + Unrelated8 +
    Unrelated9 + Unrelated10 + Unrelated11 + Unrelated12 + Unrelated13 +
    Unrelated14 + Unrelated15 + Unrelated16 + Unrelated17 + Unrelated18 +
    Unrelated19 + Unrelated20
)

# Run search
starting_model <- glm(Y ~ 1,
  data = simdata, family =
    inverse.gaussian(link = "1/mu^2")
)
glm_search <- stepAIC(starting_model, scope_arg, trace = 0)

summary(glm_search)

rm(simdata, scope_arg, glm_search, starting_model)

