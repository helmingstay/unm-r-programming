require(xts)
require(reshape2)
require(ggplot2)

## convenience function, like apply.weekly
## but construct arbitrary daily endpoints
mk.period.apply <- function(.xts, k, fun=mean){
    .ends <- endpoints(.xts, k=k, on='days')
    ret <- period.apply(.xts, .ends, fun)
    return(ret)
}


## convenience function
## extract xts coredata and index as Date, melt
mk.df.from.xts <- function(.xts, id.vars) {
    ret <- cbind(Date=index(.xts), as.data.frame(.xts))
    ret <- melt(ret, id.vars=id.vars)
}

## plot and model the final dataframe, 
## store everything in list
mk.modlist <- function(.df, 
    .xvar, .yvar='value', .facetvar = 'variable'
) {
    ret <- within(list(),{
        plot <- ggplot(.df, aes_string(x=.xvar, y=.yvar)) +
            facet_wrap(formula(paste0('~',.facetvar))) + 
            geom_point() + theme_bw() +
            geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2)
        form <- formula(sprintf('%s~%s * %s', .yvar, .xvar, .facetvar))
        mod <- glm(form, data = .df, family='poisson')
        dev <- mk.prop.dev(mod)
        nobs <- length(na.omit(residuals(mod)))
        nweeks <- length(unique(na.omit(.df)$Date))
    })
    return(ret)
}

obs.period <- seq(from=2, to=13, by=1)
obs.lag <- seq(from=-6, to=10, by=2)
## re-aggregate and rerun for each observation window length
#uberlist <- lapply(obs.period, function(.per) {
uberlist <- lapply(obs.lag, function(this.lag) {
    cat('\n.')
    ret.per <- lapply(obs.period, function(this.per){
        cat('+')
    #.per <- 5
    lag.weath <- lag(weather.xts, k=this.lag)
    lag.sewtemp <- lag(sewtemp.xts, k=this.lag)
    outret <- within(list(),{
        ## store in list
        lag <- this.lag
        per <- this.per
        weather.per <- mk.period.apply(
            lag.weath, k=this.per, mean
        )
        sewtemp.per <- mk.period.apply(
            lag.sewtemp, k=this.per, mk.na.omit.mean
        )
        block.per <- mk.period.apply(
            sewer.xts, k=this.per, function(x) {
                x <- na.omit(x)
                ## in sampling period, no reported blocks
                if (length(x) == 0) return(c(0,0,0))
                ## T/F index removes NAs
                #browser()
                c(all=length(x), grease=length(x[x]), not.grease=length(x[!x]))
        })

        ## join to blockage data
        join.list <- list(
            list(dat = weather.per, xvar='MeanTempC'),
            list(dat= sewtemp.per, xvar='SewTempC')
        )

        ## join sewtemp and airtemp, make model and plot for each
        modlist <- lapply(join.list, function(.join) {
            .xts = cbind(block.per, .join$dat, join='left')
            .df <- mk.df.from.xts(.xts, id.vars=c('Date',.join$xvar))
            .df <- droplevels(subset(.df, variable %in% c('grease','not.grease')))
            ret <- mk.modlist(.df, .xvar=.join$xvar)
            return(ret)
        })
        names(modlist) <- sapply(join.list, function(x) x$xvar)
    }) ## end within
    return(outret)
})
names(ret.per) <- paste0('d',obs.period)
return(ret.per)
})
#names(uberlist) <- paste0('d',obs.period)
names(uberlist) <- paste0('l',obs.lag)

uber.df <- ldply(uberlist, function(.llag) {
    ldply(.llag, function(.lper) {
        ldply( .lper$modlist, function(.lin) {
            with(.lin, data.frame(
                per=.lper$per, lag=.lper$lag, dev=as.numeric(dev), nobs, nweeks)
            )
        })
    })
})

## nice plot of predictive power by lag and observation period
uber.plot <- ggplot(uber.df, aes(x=lag, y=per, fill=dev)) + 
    facet_wrap( ~.id) + geom_tile() + 
    scale_fill_gradient2(low='blue', mid='grey', high='red', midpoint=0.1);
#plot(p)
