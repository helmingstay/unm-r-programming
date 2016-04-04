## Miscellaneous convenience functions

## output helpers
## print xtables as html
my.xtable <- function(x,...) print(xtable(x, ...), type='html')
my.startable <- function(x,...) stargazer(x, type='html', ...)
## prep model into lsmeans data.frame for my.xtable
## Get 95%CI as text
my.lsm.df <- function(mod, .spec) {
    ret <- summary(lsmeans(mod, .spec), type='response')
    ret$CI <- sprintf('%1.2f-%1.2f', ret$asymp.LCL, ret$asymp.UCL)
    ret <- subset(ret, select=c(-df,-asymp.LCL, -asymp.UCL))
    ret
}

## 
fahrenheit.to.celsius <- function(x) (x-32)*(5/9)

## convenience function, turn data.frame into xts using date col
mk.xts <- function(.df, .datcol) {
    xts(.df[,.datcol], .df$Date)
}

## convenience fun, cbind.xts weather to data
## filling w/zeros, but subset to range of data
mk.cbind.weather <- function(.weather, .dat){
    .ind <- index(.dat)
    .range <- sprintf("%s::%s", min(.ind), max(.ind))
    .weather = .weather[.range,]
    ret <- cbind.xts(.weather, .dat, fill=0)
    ret
}



## convenience fun,
## weekly mean temp, sum of other variables
mk.weekly.summary <- function(x, .tempcol = 'MeanTempC') {
   ret = colSums(x)
    ret[.tempcol] <- mean(x[,.tempcol])
    ret
}

## convenience function,
## return melted data.frame of xts, including date col
mk.df.melt <- function(.xts, .idvars =c('Date', 'MeanTempC')) {
    ret <- as.data.frame(.xts)
    ret$Date <- index(.xts)
    ret <- melt(ret, id.vars=.idvars)
    return(ret)
}

## used?
## function for period.apply
## number of observations w/ grease (is.grease TRUE), not.grease, and total
.greasefun <- function(x){
    grease <- length(x[x])
    not.grease <- length(x[!x])
    ret <- c(grease=grease, not.grease=not.grease, total = grease+not.grease)
    return(ret)
}

mk.prop.dev <- function(x, .as.string=T) {
    ## takes a glm, returns prop reduction diviance 
    ## see zheng 2000
    D <- 1 - x$deviance/x$null.deviance
    if (.as.string) {
        D <- sprintf('%2.3f', D)
    }
    return(D)
}

## alternate versions, From Bolker's lmm page:
## http://glmm.wikidot.com/faq
.pseudo.r.sq1 <- function(m) {
    1-var(residuals(m))/(var(model.response(model.frame(m))))
}
.pseudo.r.sq2 <- function(m) {
   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
   summary(lmfit)$r.squared
}


## bind model predictions into df, add CI
mk.mod.ci <- function(.df, .mod) {
    ## actually used...
    # calculate predicted values and confidence intervals
    .df <- cbind(.df, predict(.mod, type='link', se.fit=T))
    .df <- within(.df, {
      phat <- exp(fit)
      LL <- exp(fit - (1.96 * se.fit))
      UL <- exp(fit + (1.96 * se.fit))
    })
    return(.df)
}

## no native ggplot2 method for plotting negbin models
#' make nice plot of model with CI and model predictions
#' x is string
mk.mod.ci.plot <- function(.df, .x, .xlab, .ylab,
    ## defaults follow
    point.y='N', point.shape=21, 
    ribbon.ymin='LL', ribbon.ymax='UL', ribbon.alpha=0.25, 
    line.y='phat', line.col='blue', 
    .theme=theme_classic()
){
    p <- ggplot(.df, aes_string(x=.x))
    p <- p + geom_point(aes_string(y=point.y), shape=point.shape)
    p <- p + geom_ribbon(aes_string(ymin=ribbon.ymin, ymax=ribbon.ymax), alpha=ribbon.alpha)# confidence bounds
    p <- p + geom_line(aes_string(y=line.y), colour=line.col) + # fitted points
      xlab(.xlab) + ylab(.ylab)
    p <- p + .theme
         #scale_y_sqrt() + #??ytrans
    p <- p + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    return(p)
}
