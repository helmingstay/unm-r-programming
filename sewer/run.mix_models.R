## deprecated

## xian - shared intercept, different slopes
## best model??
## use maximum likelihood (REML=F) so results are comparable w/anova
.dat <- sewtemp.weather
temp.mix.models <- list(
    rand_both=lmer(SewTempC ~ MeanTempC + (1|Interceptor:Manhole), data=.dat, REML=F),
    rand_both_1=lmer(SewTempC ~ MeanTempC + (1|Interceptor/Manhole), data=.dat, REML=F),
    rand_interceptor=lmer(SewTempC ~ MeanTempC + (1|Interceptor), data=.dat, REML=F),
    rand_manhole=lmer(SewTempC ~ MeanTempC + (1|Manhole), data=.dat, REML=F),
    fixed_b_by_interceptor.rand_manhole=lmer(SewTempC ~ MeanTempC+Interceptor  + (1|Manhole), data=.dat, REML=F),
    fixed_m_by_interceptor.rand_manhole=lmer(SewTempC ~ MeanTempC:Interceptor  + (1|Manhole), data=.dat, REML=F),
    fixed_mb_by_interceptor.rand_manhole=lmer(SewTempC ~ MeanTempC*Interceptor  + (1|Manhole), data=.dat, REML=F)
)



## same for mix models
## show BIC of each model
.mix.bic <- llply(temp.mix.models, function(x) BIC(x))
.mix.bic
## best 2
.mix.best <- .best.n(temp.mix.models, .mix.bic)
## compare with anova
anova(.mix.best[[2]], .mix.best[[1]])

## xian - I'm *not* sure the BIC numbers above are directly comparable 
## between linear models and mixed models 
## I *think* they are??
## In any case, the simple mb.by.interceptor model is good
## the best mixed model might satisfy model assumptions a little better... 
## 
## note that the model also fails badly in the lower tail - 
## e.g. nonlinear at low temps
##
## pseudo-r-sq of both are approx equivalent to each other
## and to r-sq of best linear model
.pseudo.r.sq1(.mix.best[[1]])
.pseudo.r.sq2(.mix.best[[1]])
##
## show summary of best linear and mixed model;
summary(.mix.best[[1]])
