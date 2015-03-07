## build all possible models in named list
## y = mx + b

.ndays.mean <- 2:60
ndays.list <- lapply(.ndays.mean, function(.ndays) {
    ## store non-dotted vars for return
    ret <- within(list(), {
        ## rolling mean of preceding vals (e.g. align right)
        weather.roll <- rollmean(weather.xts, .ndays, align='right')
        weather.df <- cbind(Date=index(weather.roll), as.data.frame(weather.roll))
        ## left join should equal inner - we have plenty of weather
        dat <- join(sewtemp, weather.df, type='inner')
        ## test the same simple model for each ndays
        mod <- lm(SewTempC ~ MeanTempC + Interceptor, dat)
    })
    return(ret)
})
rsq.ndays <- data.frame(
    ndays=.ndays.mean, 
    ## pull out rsq for each model
    adj.r.sq=sapply(ndays.list, function(.l) summary(.l$mod)$adj.r.sq)
)

## 43?
## pull out the best rsq, and model + data
best.index <- which.max(rsq.ndays$adj.r.sq)
best.ndays <- rsq.ndays$ndays[best.index]
.best.rsq <- rsq.ndays$adj.r.sq[best.index]
.lin.best <- ndays.list[[best.index]]$mod
best.dat <- ndays.list[[best.index]]$dat
best.weather <- ndays.list[[best.index]]$weather.df
best.weather.roll <- ndays.list[[best.index]]$weather.roll

if(F) {
    ## model selection stuff
    ## most all of these are within spitting distance
    ## chose a simple model for presentation
    .dat <- best.dat
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
    #.lin.best <- temp.lin.models[.lin.bic$.id[1:2]]


    ## get adjusted r squared for each model
    # .lin.arsed <- ldply(temp.lin.models, function(x) return(summary(x)$adj.r.squared))
    # .lin.arsed[,2] <- round(.lin.arsed[,2], 2)
    # .lin.arsed
    ## not very informative = all either 0.77 or 0.78....

    ## compare with anova
    #anova(.lin.best[[2]], .lin.best[[1]])
    #summary(.lin.best[[1]])
}
