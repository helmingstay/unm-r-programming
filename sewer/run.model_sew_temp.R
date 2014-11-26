## build all possible models in named list
## y = mx + b
.dat <- sewtemp.weather
temp.lin.models <- list(
    null=lm(SewTempC ~ MeanTempC, data=.dat),
    ## including min & max temp - signif but doesn't help much
    #all.temp=lm(SewTempC ~ MeanTempC + Max, data=.dat),
    b.by.interceptor=lm(SewTempC ~ MeanTempC + Interceptor, data=.dat),
    b.by.manhole=lm(SewTempC ~ MeanTempC + Manhole, data=.dat),
    m.by.interceptor=lm(SewTempC ~ MeanTempC : Interceptor, data=.dat),
    m.by.manhole=lm(SewTempC ~ MeanTempC : Manhole, data=.dat),
    mb.by.interceptor=lm(SewTempC ~ MeanTempC * Interceptor, data=.dat),
    mb.by.manhole=lm(SewTempC ~ MeanTempC * Manhole, data=.dat)
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

## get adjusted r squared for each model
# .lin.arsed <- ldply(temp.lin.models, function(x) return(summary(x)$adj.r.squared))
# .lin.arsed[,2] <- round(.lin.arsed[,2], 2)
# .lin.arsed
## not very informative = all either 0.77 or 0.78....

## compare with anova
anova(.lin.best[[2]], .lin.best[[1]])
summary(.lin.best[[1]])

## adj.r.sq as percent
## chose b.by.intercept - parsimonious
.best.rsq <- sprintf('%0.3f', summary(temp.lin.models$mb.by.interceptor)$adj.r.sq)

