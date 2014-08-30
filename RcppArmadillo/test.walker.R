library(rbenchmark)
library(testthat)
library(Rcpp)
library(RcppArmadillo)
sourceCpp('test.walker.cpp')

## Definition of Sampling Frame
n.elem <- 1e3
frame2 <- rnorm(n.elem)
probs2 <- runif(n.elem)

## Definition of sampling regime
## Use replacement throughout
.replace <- TRUE
## Samplesize
n.samples1 <- 1e4

## With probabilities
r.prob <- sample(frame2, n.samples1, prob = probs2, replace = .replace)

cpp.prob <- csample_num(frame2, n.samples1, prob = probs2, replace = .replace)


