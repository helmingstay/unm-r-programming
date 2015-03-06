## depends on run.lag_period.R
cc <- uberlist$d15$l0
dd <- na.omit(cc$modlist$MeanTempC$fin.xts)
dd <- cbind(dd, daycos=mk.year.cos(.indexyday(dd)))
#ee <- as.zooreg(na.omit(dd))
ee <- ts(dd,  deltat=cc$per/365)

require(forecast)
ff <- auto.arima(dd[,'grease'], xreg=dd[,'MeanTempC'])
#ff1 <- arima(dd[,'grease'], xreg=dd[,'MeanTempC'], order=c(1,0,2))
ff1 <- arima(dd[,'grease'], xreg=dd[,'MeanTempC'], order=c(1,0,1))
plot(xts(residuals(ff1), index(dd)), type='h')
plot(as.vector(dd[,'MeanTempC']), fitted(ff1) )



require(stats)
#gg <- stl(ee[,'grease'], 'periodic')
#plot(gg)

hh = tsSmooth(StructTS(dd[,'grease'], 'level'))
plot(as.vector(dd[,'MeanTempC']), hh)

jj = decompose(ee[,'grease'])
plot(jj$seasonal)
plot(jj$random)


#kk = arima(dd[,'grease'], seasonal=list(order=c(2,0,2), period=365/cc$per))

## fit arima for different lags
ll <- lapply(-7:7, function(.lag) {
    ret <- list(lag=.lag,
        mod=auto.arima( dd[,'grease'], 
            xreg=lag(dd[,'MeanTempC'],.lag)#, c(2,0,0)
        )
    )
    return(ret)
})


if(F) {
    ## print out summary
    .tmp <- lapply(ll, function(.l) {
        cat(paste0('\n##########   ', .l$lag));print(ss(.l$mod))
    })
}

## bind successive lags into cols
## including a seasonal term - cosine of the day of year
.getcols <- c('grease','MeanTempC', 'daycos')
mm <- llply( -4:4, function(.l){
    ret <- data.frame(
        lag(dd[,.getcols], .l)
    )
    colnames(ret) <- paste0(.getcols, .l)
    ret
})
mm <- na.omit(do.call(cbind, mm))
nn <- lm( grease0 ~ 1, mm)
oo <- step( nn, scope = formula(grease0 ~  MeanTempC0 + MeanTempC1 + MeanTempC2 + MeanTempC3 + MeanTempC4 + grease1 + grease2 + grease3  + daycos0 +  daycos1 +  daycos2 +  daycos3 +  daycos4 +  `daycos-1` +  `daycos-2` +  `daycos-3` 
))
