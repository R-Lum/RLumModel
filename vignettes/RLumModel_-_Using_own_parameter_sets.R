## ---- echo=FALSE, message = FALSE----------------------------------------
library(RLumModel)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.pos = 'H', fig.align = 'center')

## ------------------------------------------------------------------------
own_parameters <- list(
  N = c(2e15, 2e15, 2.4e16, 1e17),
  E = c(0, 0, 0, 0),
  s = c(0, 0, 0, 0),
  A = c(2e-8, 2e-9, 4e-9, 1e-8),
  B = c(0, 0, 5e-11, 4e-8),
  K = 0,
  model = "customized",
  R = 1.7e15)

## ------------------------------------------------------------------------
own_state_parameters <- c(0, 0, 0, 9.4e15)

## ----set sequence Pagonis 2009-------------------------------------------
sequence <- list(RF = c(20, 0.1, 0.1))

