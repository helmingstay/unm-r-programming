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
    ret <- cbind(
        Date=index(.xts), doy=.indexyday(.xts), 
        as.data.frame(.xts)
    )
    ret <- melt(ret, id.vars=id.vars)
}

## turn year-of-day into -1:1
mk.year.cos <- function(yday) cos( (2*pi*yday)/365 )

## plot and model the final dataframe, 
## store everything in list
mk.modlist <- function(.df, 
    .xvar, .yvar='value', .facetvar = 'variable'
) {
    ret <- within(list(),{
        dat <- na.omit(.df)
        form <- formula(sprintf('(%s) ~ %s', .yvar, .xvar, .facetvar))
        ## poisson regression, offset by aggregate period
        #form <- formula(sprintf('%s~ offset(log(per)) + %s * %s', .yvar, .xvar, .facetvar))
        plot <- ggplot(dat, aes_string(x=.xvar, y=sprintf('(%s)',.yvar))) +
            #facet_wrap(formula(paste0('~',.facetvar))) + 
            geom_point() + theme_bw() +
            #geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) 
            geom_smooth(method = "lm", colour='blue', size=1.2)
        mod <- lm(form, data = dat)
        #mod <- glm(form, data = dat, family='poisson')
        rsq <- summary(mod)$adj.r.sq
        dev <- mk.prop.dev(mod)
        nobs <- length(na.omit(residuals(mod)))
        nweeks <- length(unique(dat$Date))
    })
    return(ret)
}


## sewtemp has multiple samples per day, 
## must first aggreg by day
## xts not intended for repeated measures per timestep
sewtemp.day <- mk.period.apply(
    sewtemp.xts, k=1, mk.na.omit.mean
)

obs.period <- seq(from=1, to=20, by=1)
obs.lag <- 0
#obs.lag <- seq(from=-11, to=33, by=3)
## re-aggregate and rerun for each observation window length
#uberlist <- lapply(obs.period, function(.per) {

uberlist <- lapply(obs.period, function(this.per) {
    cat('\n.')
    ## count blocks per period
    ## final count gets more "continuous" with longer period
    ## sewer.xts is a T/F vector T if grease, F if not
    block.per <- mk.period.apply(
        sewer.xts, k=this.per, function(x) {
            ## NAs result from join with weather 
            x <- na.omit(x)
            ## in sampling period, weather but no block reports
            if (length(x) == 0) return(c(0,0,0))
            ## T/F index removes NAs
            #browser()
            ret <- c(all=length(x), grease=length(x[x]), not.grease=length(x[!x]))
            ## normalize to events/day?
            ret <- ret/this.per
    })

    ret.lag <- lapply(obs.lag, function(this.lag){

        ## lag daily, then aggregate
        ## only lag temps, not blocks
        weather.lag <- lag(weather.xts, k=this.lag)
        sewtemp.lag <- lag(sewtemp.day, k=this.lag)

        cat('+')
    #.per <- 5
    outret <- within(list(),{
        ## store in list
        lag <- this.lag
        per <- this.per
        ## final lagged and agged temps
        weather.per <- mk.period.apply(
            weather.lag, k=this.per, mean
        )
        sewtemp.per <- mk.period.apply(
            sewtemp.lag$SewTempC, k=this.per, function(x) mean(x, na.rm=T)
        )

        ## for lapply join to blockage data
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
            ret <- mk.modlist(.df, .xvar='mk.year.cos(doy)') #.xvar=.join$xvar)
            ret$fin.xts <- .xts
            return(ret)
        })
        names(modlist) <- sapply(join.list, function(x) x$xvar)
    }) ## end within
    return(outret)
})
names(ret.lag) <- paste0('l',obs.lag)
return(ret.lag)
})
names(uberlist) <- paste0('d',obs.period)

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

#.df <- subset(uber.df, .id=='MeanTempC')
.df <- uber.df
pp <- ggplot(.df, aes(x=factor(lag), y=per, fill=rsq)) + 
    facet_wrap(~.id) + 
    geom_tile() + 
    scale_fill_gradient2(low='blue', mid='grey', high='red', midpoint=median(.df$rsq)) 


.df <- subset(uber.df, .id=='MeanTempC' & lag==1)
#.df <- uber.df
pp1 <- ggplot(.df, aes(x=per, y=rsq)) + 
    geom_point() + 
    geom_smooth(type='lm')

## nice plot of predictive power by lag and observation period
uber.plot <- ggplot(uber.df, aes(x=lag, y=per, fill=null.dev)) +
    facet_wrap( ~.id) + geom_tile() + 
    scale_fill_gradient2(low='blue', mid='grey', high='red', midpoint=0.1);
#pdf('uber.pdf', width=7, height=5); plot(uber.plot); dev.off()
#plot(p)
