### make another data frame to include weather data
## joining sewer data to the temp.weekdf from Sewer_results_summary.Rnw
#block.sewtemp.airtemp.week <- join(block.sewtemp.week, airtemp.week.df, by=c('week', 'year'), type='left')
## remove missing rows
#block.sewtemp.airtemp.week <- na.omit(block.sewtemp.airtemp.week) # n=95

if(F) {
## deprecated now
## build all possible models in named list
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
