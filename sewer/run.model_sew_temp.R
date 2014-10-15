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

## compare linear models
## 
## get BIC of each model
## smaller is better
.lin.bic <- ldply(temp.lin.models, function(x) BIC(x))
## order by increasing BIC
.lin.bic <- .lin.bic[order(.lin.bic$V1),]
## pull out best 2
.lin.best <- temp.lin.models[.lin.bic$.id[1:2]]

## compare with anova
anova(.lin.best[[2]], .lin.best[[1]])
summary(.lin.best[[1]])

## adj.r.sq as percent
## chose b.by.intercept - parsimonious
.best.rsq <- sprintf('%0.3f', ss(temp.lin.models$mb.by.interceptor)$adj.r.sq)
