## timeseries/xts handling functions 
## used by run.lag_period.R
require(xts)
require(reshape2)
require(ggplot2)

## convenience function, like apply.weekly
## but construct arbitrary daily endpoints
mk.period.apply <- function(.xts, k, fun=mean, ...){
    .ends <- endpoints(.xts, k=k, on='days')
    ret <- period.apply(.xts, .ends, fun, ...)
    return(ret)
}

## used by mk.period.apply, count blocks per period
## sewer.xts is a T/F vector T if grease, F if not
mk.count.blocks <- function(x, do.norm=T, .per) {
    ## NAs result from join with weather 
    x <- na.omit(x)
    ## in sampling period, weather but no block reports
    if (length(x) == 0) return(c(0,0,0))
    ## T/F index removes NAs
    #browser()
    ret <- c(all=length(x), grease=length(x[x]), not.grease=length(x[!x]))
    ## normalize to events/day?
    if (do.norm) {
        ## k is the named arg to mk.period.apply
        ret <- ret/.per
    }
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
mk.modlist <- function(.df, modfun=lm, 
    .formformat = '%s ~ %s', ## .yvar, .xvar, .facetvar
    ## poisson regression, offset by aggregate period:
    #'%s~ offset(log(per)) + %s * %s'
    .xvar, .yvar='value', .facetvar = 'variable',
    .smooth.meth='lm', ... # extra vars passed to geom_smooth
) {
    ret <- within(list(),{
        dat <- na.omit(.df)
        form <- formula(sprintf(.formformat, .yvar, .xvar, .facetvar))
        plot <- ggplot(dat, aes_string(x=.xvar, y=sprintf('(%s)',.yvar))) +
            geom_point() + theme_bw() +
            #geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) 
            geom_smooth(method = .smooth.meth,...)
        mod <- modfun(form, data = dat)
        #mod <- glm(form, data = dat, family='poisson')
        rsq <- summary(mod)$adj.r.sq
        dev <- mk.prop.dev(mod)
        nobs <- length(na.omit(residuals(mod)))
        nweeks <- length(unique(dat$Date))
    })
    return(ret)
}
