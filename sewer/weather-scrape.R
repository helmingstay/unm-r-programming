## Grab daily weather temps for ABQ METAR station
## For many years, 
## combine into single data frame
## write out results to csv file
## should only have to do this once
.years <- 2005:2014
## for each year, construct url that returns csv 
## of weather data
.urls <- sprintf('http://www.wunderground.com/history/airport/KABQ/%d/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2013&req_city=NA&req_state=NA&req_statename=NA&format=1', .years)

## subset data, just keep temp cols
.weather.cols <- c('MST', 'Max.TemperatureF', 'Mean.TemperatureF', 'Min.TemperatureF')
abq.temps <- ldply(.urls, function(.url) {
    print(.url)
    ## read data from web
    ret <- read.table(.url, header=T, sep=',')
    ## grab date/temp cols
    ret <- subset(ret, select=.weather.cols)
    ret
})
## write out file
write.table(abq.temps, file='abq-temps-2005-2014.csv', sep=',', row.names=F)
