## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----model, echo=TRUE---------------------------------------------------------
library(GlmSimulatoR)
library(ggplot2)
library(cplm, quietly = TRUE)
set.seed(1)

simdata <- simulate_tweedie(weight = .2, ancillary = 1.15, link = "log")

ggplot(simdata, aes(x = Y)) + 
  geom_histogram(bins = 30)

## ----model2, echo=TRUE--------------------------------------------------------
glmModel <- cpglm(Y ~ X1, data =simdata, link = "log")
summary(glmModel)


