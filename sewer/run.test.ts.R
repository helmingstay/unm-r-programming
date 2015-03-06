## depends on run.lag_period.R
cc <- uberlist$d10$l0
dd <- cc$modlist$MeanTempC$fin.xts
#ee <- as.zooreg(na.omit(dd))
ee <- ts(na.omit(dd),  deltat=cc$per/365)

require(forecast)
ff <- auto.arima(dd[,'grease'], xreg=dd[,'MeanTempC'])
#ff1 <- arima(dd[,'grease'], xreg=dd[,'MeanTempC'], order=c(1,0,2))
ff1 <- arima(dd[,'grease'], xreg=dd[,'MeanTempC'], order=c(1,0,1))
plot(xts(residuals(ff1), index(dd)), type='h')
plot(as.vector(dd[,'MeanTempC']), fitted(ff1) )



require(stats)
gg <- stl(ee[,'grease'], 'periodic')
plot(gg)

hh = tsSmooth(StructTS(dd[,'grease'], 'level'))
plot(as.vector(dd[,'MeanTempC']), hh)
