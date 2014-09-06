library(rbenchmark)
library(testthat)
library(Rcpp)
library(RcppArmadillo)
sourceCpp('test.walker.cpp', rebuild=T, verbose=T)

.seed = 39
set.seed(.seed)
## Definition of Sampling Frame
n.elem <- 1e3
frame2 <- rnorm(n.elem)
probs2 <- runif(n.elem)

## Definition of sampling regime
## Use replacement throughout
.replace <- TRUE
## Samplesize
n.samples1 <- n.elem
#n.samples1 <- 1e4

## With probabilities
set.seed(.seed)
r.prob <- sample(frame2, n.samples1, prob = probs2, replace = .replace)
#print(h(cbind(n.samples1, probs2)))

set.seed(.seed)
cpp.prob <- csample_num(frame2, n.samples1, prob = probs2, replace = .replace)
#print(h(cbind(n.samples1, probs2)))

## compare results
.test <- cbind(r.prob, cpp.prob)
print(head(.test))
print(identical(r.prob, cpp.prob))
print(all.equal(r.prob, cpp.prob))
gc()



## not walker
if(F) {
probs3 <- c(probs2[1:100], rep(0, n.elem-100))
set.seed(.seed)
r.prob <- sample(frame2, n.samples1, prob = probs3, replace = .replace)
set.seed(.seed)
cpp.prob <- csample_num(frame2, n.samples1, prob = probs3, replace = .replace)
.test <- cbind(r.prob, cpp.prob)
print(head(.test))
}
