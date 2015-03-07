source('mk.ts.helpers.R')

## xts class not meant for repeated measures per timestep
## but sewtemp has multiple samples per day, 
## thus we must first aggreg sewtemp by day
sewtemp.day <- mk.period.apply(
    sewtemp.xts, k=1, mk.na.omit.mean
)

## possible lags and aggregation periods (days)
## loop through all combinations
## join into data.frame
## build models, pull out statistics
obs.period <- seq(from=1, to=20, by=1)
obs.lag <- 0
#obs.lag <- seq(from=-11, to=11, by=1)

## for each observation window length
## re-aggregate and rerun 
uberlist <- lapply(obs.period, function(this.per) {
    cat('\n.')
    ## count blocks per period
    ## final count gets more "continuous" with longer period
    ## sewer.xts is a T/F vector T if grease, F if not
    block.per <- mk.period.apply( 
        sewer.xts, k=this.per, mk.count.blocks, .per=this.per
    )

    ## only lag temps, not blocks
    ret.lag <- lapply(obs.lag, function(this.lag){
        ## lag daily, then aggregate
        ## otherwise total lag = this.lag * this.per days
        weather.lag <- lag(weather.xts, k=this.lag)
        sewtemp.lag <- lag(sewtemp.day, k=this.lag)
        cat('+')
        ## store everything in return list
        outret <- within(list(),{
            lag <- this.lag
            per <- this.per
            ## final lagged then agged predictor temps
            weather.per <- mk.period.apply(
                weather.lag, k=this.per, mean
            )
            sewtemp.per <- mk.period.apply(
                sewtemp.lag$SewTempC, k=this.per, function(x) mean(x, na.rm=T)
            )

            ## convenience list for lapply join to blockage data
            join.list <- list(
                weath=list(dat = weather.per, xvar='MeanTempC'),
                sewtemp=list(dat= sewtemp.per, xvar='SewTempC')
            )

            ## join sewtemp and airtemp, make model and plot for each
            modlist <- lapply(join.list, function(.join) {
                .xts = cbind(block.per, .join$dat, join='left')
                #if (this.lag >3 && this.per>5) browser()
                .df <- mk.df.from.xts(.xts, id.vars=c('Date', 'doy', .join$xvar))
                .keep.levels <- c('grease')
                .df <- droplevels(subset(.df, variable %in% .keep.levels))
                .df$per <- per
                ret <- mk.modlist(.df, .xvar=.join$xvar)
                ret$fin.xts <- .xts
                return(ret)
            })
            ## clean up names
            names(modlist) <- sapply(join.list, function(x) x$xvar)
        }) ## end within
    return(outret)
})
names(ret.lag) <- paste0('l',obs.lag)
return(ret.lag)
})
names(uberlist) <- paste0('d',obs.period)

## pull out statistics into df
uber.df <- ldply(uberlist, function(.llag) {
    ldply(.llag, function(.lper) {
        ldply( .lper$modlist, function(.lin) {
            ret <- with(.lin, data.frame(
                per=.lper$per, lag=.lper$lag, 
                rsq, 
                #dev=mod$deviance,
                #null.dev = mod$null.deviance,
                nobs, nweeks, var=var(dat$value)
            ))
            #ret$pseudo.rsq <- with(ret, 1-(dev/null.dev))
            ret
        })
    })
})
