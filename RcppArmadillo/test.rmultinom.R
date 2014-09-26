.seed=39
set.seed(.seed)
.n <- 5
.size <- 20
prob <- 1:10/1.0
r.multinom <- rmultinom(.n, .size, prob=prob)

library(Rcpp)
library(RcppArmadillo)
sourceCpp('test.rmultinom.cpp')
set.seed(.seed)
c.multinom <- rmultinomC(.n, .size, prob)

print(identical(r.multinom, c.multinom))
