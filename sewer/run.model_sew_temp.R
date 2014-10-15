## build all possible models in named list
## y = mx + b
temp.lin.models <- list(
    null=lm(SewTempC ~ MeanTempC, data=sewer.weather),
    ## including min & max temp - signif but doesn't help much
    #all.temp=lm(SewTempC ~ MeanTempC + Max, data=sewer.weather),
    b.by.interceptor=lm(SewTempC ~ MeanTempC + Interceptor, data=sewer.weather),
    b.by.manhole=lm(SewTempC ~ MeanTempC + Manhole, data=sewer.weather),
    m.by.interceptor=lm(SewTempC ~ MeanTempC : Interceptor, data=sewer.weather),
    m.by.manhole=lm(SewTempC ~ MeanTempC : Manhole, data=sewer.weather),
    mb.by.interceptor=lm(SewTempC ~ MeanTempC * Interceptor, data=sewer.weather),
    mb.by.manhole=lm(SewTempC ~ MeanTempC * Manhole, data=sewer.weather)
)
## xian - shared intercept, different slopes
## best model??
## use maximum likelihood (REML=F) so results are comparable w/anova
temp.mix.models <- list(
    rand_both=lmer(SewTempC ~ MeanTempC + (1|Interceptor:Manhole), data=sewer.weather, REML=F),
    rand_both_1=lmer(SewTempC ~ MeanTempC + (1|Interceptor/Manhole), data=sewer.weather, REML=F),
    rand_interceptor=lmer(SewTempC ~ MeanTempC + (1|Interceptor), data=sewer.weather, REML=F),
    rand_manhole=lmer(SewTempC ~ MeanTempC + (1|Manhole), data=sewer.weather, REML=F),
    fixed_b_by_interceptor.rand_manhole=lmer(SewTempC ~ MeanTempC+Interceptor  + (1|Manhole), data=sewer.weather, REML=F),
    fixed_m_by_interceptor.rand_manhole=lmer(SewTempC ~ MeanTempC:Interceptor  + (1|Manhole), data=sewer.weather, REML=F),
    fixed_mb_by_interceptor.rand_manhole=lmer(SewTempC ~ MeanTempC*Interceptor  + (1|Manhole), data=sewer.weather, REML=F)
)

## compare linear models
## 
## convenience function
## function returns the list elements with the n best scores
.best.n <- function(.list, .scores, n=2) {
    ## order list 
   .list <- .list[ order(unlist(.scores)) ]
    ## only return the first n elements
    ret <- .list[ 1:n ]
    ret
} 
## show BIC of each model
## smaller is better
.lin.bic <- ldply(temp.lin.models, function(x) BIC(x))
## order by increasing BIC
.lin.bic <- .lin.bic[order(.lin.bic$V1),]
