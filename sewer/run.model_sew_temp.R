## rolling mean of preceding N days of air temp
## model sewtemp / blockage on this
## select optimal N by AIC / rsq

.ndays.mean <- 2:60
ndays.list <- lapply(.ndays.mean, function(.ndays) {
    ## store non-dotted vars for return
    ret <- within(list(), suppressMessages({
        ndays <- .ndays
        ## rolling mean of preceding vals (e.g. align right)
        weather.roll <- rollmean(weather.xts, .ndays, align='right')
        weather.df <- cbind(Date=index(weather.roll), as.data.frame(weather.roll))
        ## left join should equal inner - we have plenty of weather
        dat.sewtemp <- join(sewtemp, weather.df, type='inner')
        ## test the same simple model for each ndays
        mod.sewtemp <- lm(SewTempC ~ MeanTempC + Interceptor, dat.sewtemp)
        ## same for blocks
        .tmp.greasevec <- as.vector(sewer.xts$is.grease)
        block.day.df <- data.frame(
            Date = index(sewer.xts),
            grease = .tmp.greasevec == 1,
            not.grease = .tmp.greasevec == 0
        )
        block.day.df <- melt(block.day.df, id.vars='Date')
        dat.block <- join(block.day.df, weather.df, type='inner')
        mod.block <- glm(value ~ MeanTempC * variable, dat.block, family = 'binomial')
        ## now for weekly blocks
        dat.block.week <- join(sewer.block.week.melt, weather.df, type='inner')
        ## try lm, AICs can be compared w/above
        ## much less good
        mod.block.week <- glm.nb(value ~ MeanTempC * variable, dat.block.week)
        mod.block.week.lm <- lm(value ~ MeanTempC, dat.block.week)
        ## just grease / nongrease separately
        mod.block.week.grease <- glm.nb(value ~ MeanTempC, subset(dat.block.week, variable=='grease'))
        mod.block.week.not <- glm.nb(value ~ MeanTempC, subset(dat.block.week, variable!='grease'))

        dat.block.week.grease.ar <- droplevels(subset(dat.block.week, variable=='grease'))
        ## lag blocks by 1 week and join
        .tmp.lag1 <- with(dat.block.week.grease.ar, 
            data.frame(
                Date=Date+7,
                lag1 = value
        ))
        ## lag blocks by 2 week and join
        .tmp.lag2 <- with(dat.block.week.grease.ar, 
            data.frame(
                Date=Date+14,
                lag2 = value
        ))
        dat.block.week.grease.ar <- join(dat.block.week.grease.ar, .tmp.lag1) 
        dat.block.week.grease.ar <- join(dat.block.week.grease.ar, .tmp.lag2) 
        mod.block.week.grease.ar <- glm.nb(value ~ MeanTempC + lag1, dat.block.week.grease.ar)
    }))
    return(ret)
})


## goodness of fit summaries for weekly blockages
fit.block.week <- ldply(ndays.list, function(.l) with(.l, 
    data.frame(ndays, 
        aic=AIC(mod.block.week), 
        prop.dev=mk.prop.dev(mod.block.week, .as.string=F),
        aic.grease=AIC(mod.block.week.grease), 
        prop.dev.grease=mk.prop.dev(mod.block.week.grease, .as.string=F),
        aic.not=AIC(mod.block.week.not), 
        prop.dev.not=mk.prop.dev(mod.block.week.not, .as.string=F),
        aic.ar=AIC(mod.block.week.grease.ar), 
        prop.dev.ar=mk.prop.dev(mod.block.week.grease.ar, .as.string=F)
        #aic.lm=AIC(mod.block.week.lm), 
        #rsq=summary(mod.block.week.lm)$adj.r.sq
    )
))

## goodness of fit summaries for sewtemp
rsq.ndays <- ldply(ndays.list, function(.l) with(.l, 
    data.frame(ndays, 
        ## pull out rsq for each model
        adj.r.sq=summary(mod.sewtemp)$adj.r.sq,
        aic=AIC(mod.sewtemp)
    )
))

## 43?
## pull out the best rsq, and model + data
best.index <- which.max(rsq.ndays$adj.r.sq)
best.ndays <- rsq.ndays$ndays[best.index]
.best.rsq <- rsq.ndays$adj.r.sq[best.index]
.lin.best <- ndays.list[[best.index]]$mod.sewtemp
best.dat <- ndays.list[[best.index]]$dat.sewtemp
best.weather <- ndays.list[[best.index]]$weather.df
best.weather.roll <- ndays.list[[best.index]]$weather.roll
