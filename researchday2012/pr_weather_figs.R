require(plyr)
require(foreach)
## need to load puerto_rico_weather.RData
## turn into a list of xts
pr.weather.xts = dlply(pr_weather, 'V6', function(x) { 
    #if(nrow(x)<2000) {return(NULL)};  
    ## numeric columns -- xts is a matrix, can't mix col classes
    numids = laply(x, is.numeric); 
    ## turn into xts: data, timebase
    return( xts(x[,numids], as.Date(x$AST)))
}, .progress='text')


## 
myvar = 'Mean.Wind.SpeedMPH'
myquant=0.995

aa=llply(pr.weather.xts, function(aa) {
        ## if empty do nothing
        if (nrow(aa)<10) return(); 
        ##  return rows that are high for this var
        ret =  aa[aa[,myvar]>quantile(aa[,myvar], myquant, na.rm=T),myvar]
        return(ret)
})

## easy way to combine xts
pr.max.weather = foreach(bb=iter(aa), .combine=cbind) %do% {bb}
pr.max.nsites = xts(aaply(pr.max.weather, 1, function(x) { sum(!is.na(x))}), index(pr.max.weather))

plot(xyplot(pr.max.weather, type=c('p','g', 'h'), screens=1, col=1:6,
    main=sprintf('Days with %s > %s percentile', myvar, myquant)
))

if(F){
## for several stations, lowest pressure is at highest max wind
llply(pr.weather.xts, function(x) { if (length(x)==0) return(); plot(xyplot(Mean.Wind.SpeedMPH ~ Min.Sea.Level.PressureIn, as.data.frame(x), type=c('p','g'))); readline()})
}
