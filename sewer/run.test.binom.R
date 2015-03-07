## like run.lag_period.R,
## but don't aggregate, just lag
## and test a binomial model of ">0 blocks today"
## model results indicate no signal here
source('mk.ts.helpers.R')

obs.period <- 1
obs.lag <- seq(from=-11, to=11, by=1)

## convenience list for organization, 
## assign into blank list
days.l <- within(list(),{
    ## this.per was looped, just one day here
    this.per <- obs.period
    ## count blocks per period
    ## final count gets more "continuous" with longer period
    ## sewer.xts is a T/F vector T if grease, F if not
    ## 
    ## again, just blocks per day here
    block.per <- mk.period.apply( 
        sewer.xts, k=this.per, mk.count.blocks, .per=this.per
    )
    ## binomial response - there are blocks today
    block.per$grease <- block.per$grease > 0
    ## try moving average of temp
    #.mean.win <- 7
    #.weather.ma <- rollmean(weather.xts, .mean.win, align='right')
    ret.lag <- lapply(obs.lag, function(this.lag){
        ## lag daily, then aggregate
        ## only lag temps, not blocks
        weather.lag <- lag(.weather.ma, k=this.lag)
        ## response variables to model
        .keep.levels <- c('grease')
        ## pack merged ts, model into list
        outret <- within(list(),{
            ## store in list
            lag <- this.lag
                #weath=list(dat = weather.per, xvar='MeanTempC'),
            ## join sewtemp and airtemp, make model and plot for each
            fin.xts = cbind(block.per, weather.lag, join='left')
            .df <- mk.df.from.xts(fin.xts, id.vars=c('Date', 'doy', 'MeanTempC'))
            .df <- droplevels(subset(.df, variable %in% .keep.levels))
            modlist <- mk.modlist(.df, .xvar='MeanTempC', modfun=function(formula, data) glm(formula, data=data, family='binomial'))
        })
        return(outret)
    })
    names(ret.lag) <- paste0('l',obs.lag)
    return(ret.lag)
})

