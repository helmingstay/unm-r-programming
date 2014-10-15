
## next, load in weather data,
## used by sewtempandblock.R
## METAR data, scraped with weather-scape.R
weather <- read.csv('abq-temps-2005-2014.csv')
## Turn factor into date    
weather$day <- as.Date(weather$MST)
## Convert Fahrenheit into Celsius
weather[,2:4] <- fahrenheit.to.celsius(weather[,2:4])
## Inspect
str(weather)
## Melt data
weather.melt <- melt( weather, id.vars=c('day', 'MST'))
## Inspect
#str(weather.melt)


## combine weather and sewer data in one dataset
intersect(colnames(weather), colnames(sewer))
sewer.weather <- join(sewer, weather, type='left')
#head(sewer.weather, 3)

# xts(data, timebase), timebase is a vector that can be turned into a POSIXct
timebase <- as.POSIXct( weather$day) + 7*60*60 # correct time zone (add 7 hours in seconds)
# remove non-numeric columns (MST and the date)
weather.xts <- xts(subset(weather, select=-c(MST, day)), timebase) 
# only include weather data within range of sewer blockage data
## bad code!!
#weather.xts <- weather.xts["2009-01-06/2013-01-31"]
## Add day year, week, and year from the POSIXct date
sewer$doy <- as.numeric(strftime(sewer$date, format='%j'))
sewer$week <- sewer$doy %/% 7
sewer$year <- as.numeric(strftime(sewer$date, format='%Y'))
## Number of problems for each week
sewer.fail.week = ddply( sewer, c('year', 'week'),
    function(x) { 
      data.frame(N=nrow(x))  } 
    ## show progress
    #.progress='text'
)
## Get weekly mean temp
temp.week <- weather.xts$Mean.TemperatureF
temp.week <- apply.weekly(temp.week, FUN=mean)
## week and year from xts .index functions
## see ?.index for details
temp.week$week <- .indexyday(temp.week) %/% 7
temp.week$year <- .indexyear(temp.week) + 1900
## Turn into data.frame and join with sewer data
temp.week.df <- data.frame(temp.week)
## Full or "outer" join -- keep weather data for weeks without problems
# left join excludes weather data for weeks with no blockage data
sewer.join <- join(sewer.fail.week, temp.week.df, type='full')
## Weeks with no problems 
sewer.join$N[is.na(sewer.join$N)] <- 0
## Why are there remaining NAs in Mean.TemperatureF?
sewer.join <- na.omit(sewer.join)
