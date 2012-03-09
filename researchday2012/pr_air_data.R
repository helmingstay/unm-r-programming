## download daily weather data from pr airports, 2002-2011
## into single dataframe with lat/lon,
## create summary dataframe w lan/lon &number of days, 
## save both as RData file

## download world airport data from http://openflights.svn.sourceforge.net/viewvc/openflights/openflights/data/airports.dat 
## read in file, get pr airports
pr_air = droplevels(subset(read.table('airports.dat', sep=','), V4 == 'Puerto Rico'))

## for each airport, 
pr_weather = ddply( pr_air, 'V6', function(x) {
  ## for each year
  ldply(2002:2011, function(myyear) {
  myair = x$V6
  ## download data, read into dataframe
  myurl = sprintf('http://www.wunderground.com/history/airport/%s/%s/1/1/CustomHistory.html?dayend=1&monthend=1&yearend=2011&format=0', myair, myyear)
  ret = read.table(url(myurl), sep=',', header=T)
  ## return if no data
  if(!(nrow(ret)>1)) {return()}
  ret$airport = myair
  ret$lat = x$V7
  ret$lon = x$V8
  ret$year = myyear
  ret
  })
}, .progress='text')  ## this can take a while...

## only keep used levels
pr_weather = droplevels(pr_weather)

## for each airport, get latlon and number of days of data
pr_weather_summary = ddply(pr_weather, 'airport', function(x){
  y=x[1,]
  data.frame(airport=y$airport, lat=y$lat, lon=y$lon, ndays=nrow(x))
})

## save data file
save(pr_weather, pr_weather_summary, file='puerto_rico_weather.RData')