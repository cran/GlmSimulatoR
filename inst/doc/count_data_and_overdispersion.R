## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  results = "hide"
)

## ----PoissonHistogram, echo=TRUE----------------------------------------------
library(stats)
library(MASS)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(GlmSimulatoR)

set.seed(1)

# lambda 1
poisson <- rpois(n = 10000, lambda = 1)
poisson_df <- as_tibble(poisson)

ggplot(poisson_df, aes(value)) +
  geom_histogram(bins = 100)

# lambda 5
poisson <- rpois(n = 10000, lambda = 5)
poisson_df <- as_tibble(poisson)

ggplot(poisson_df, aes(value)) +
  geom_histogram(bins = 100)

# lambda 10
poisson <- rpois(n = 10000, lambda = 10)
poisson_df <- as_tibble(poisson)

ggplot(poisson_df, aes(value)) +
  geom_histogram(bins = 100)

## ----echo=FALSE---------------------------------------------------------------
rm(poisson, poisson_df)

## ----PoissonGlm, echo=TRUE, results='markup'----------------------------------
set.seed(1)
simdata <- simulate_poisson(N = 10000, weights = c(.5, 1), link = "log")

# Response looks similar to above histograms
ggplot(simdata, aes(Y)) +
  geom_histogram(bins = 100)

glm_poisson <- glm(Y ~ X1 + X2, data = simdata, family = poisson(link = "log"))
summary(glm_poisson)

## ----echo=FALSE---------------------------------------------------------------
rm(simdata, glm_poisson)

## ----PoissonNegativeBinomial1, echo=TRUE--------------------------------------
set.seed(1)

poisson <- rpois(n = 10000, lambda = 1)
poisson_df <- as_tibble(poisson)

ggplot(poisson_df, aes(value)) +
  geom_histogram(bins = 100)

neg_bin_rv <- rnegbin(n = 10000, mu = 1, theta = 1000)
neg_bin_df <- as_tibble(neg_bin_rv)

ggplot(neg_bin_df, aes(value)) +
  geom_histogram(bins = 100)

## ----echo=FALSE---------------------------------------------------------------
rm(poisson, poisson_df, neg_bin_rv, neg_bin_df)

## ----PoissonNegativeBinomial2, echo=TRUE--------------------------------------
set.seed(1)

poisson <- rpois(n = 10000, lambda = 1)
poisson_df <- as_tibble(poisson)

ggplot(poisson_df, aes(value)) +
  geom_histogram(bins = 100)

neg_bin_rv <- rnegbin(n = 10000, mu = 1, theta = 1)
neg_bin_df <- as_tibble(neg_bin_rv)

ggplot(neg_bin_df, aes(value)) +
  geom_histogram(bins = 100)

## ----echo=FALSE---------------------------------------------------------------
rm(poisson, poisson_df, neg_bin_rv, neg_bin_df)

## ----NegativeBinomialGlm, echo=TRUE, results='markup'-------------------------
set.seed(1)
simdata <- simulate_negative_binomial(
  N = 10000, weights = c(.5, 1),
  ancillary = 5, link = "log"
) # ancillary is theta.

# Response looks like a negative binomial distribution.
ggplot(simdata, aes(Y)) +
  geom_histogram(bins = 200)

glm_poisson <- glm(Y ~ X1 + X2, data = simdata, family = poisson(link = "log"))
glm_nb <- glm.nb(Y ~ X1 + X2, data = simdata, link = "log")

summary(glm_poisson)
summary(glm_nb)

## ----echo=FALSE---------------------------------------------------------------
rm(simdata, glm_poisson, glm_nb)

