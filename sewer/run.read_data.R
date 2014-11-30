## In this file,
## sewer temp grabsamples, air temp, and sewer blockages
## do temps first

########################################
### Sewage grabsample data
########################################
## Define column classes to read data 
## there are text comments in line with data
## force measurement cols to read as numeric
# Interceptor,Manhole,Date,Time,Temp,ph,Tot. Sulfide,Dis. Sulfide,Tot. Iron,Ferrous Fe,,
.colClasses <- c(Interceptor='factor', Manhole='factor', Date='character', Time='character', Temp='numeric' ,ph='NULL', Tot.Sulfide='NULL', Dis.Sulfide='NULL', Tot.Iron='NULL', Ferrous.Fe='NULL')
## read grab-data
## path relative to current dir
## 
sewtemp <- read.table("data/allgrabdata_datefix.csv", sep=',', header=T, comment.char='#', colClasses=.colClasses)
## from char to date
sewtemp$Date <- as.Date(as.POSIXct(sewtemp$Date, format='%d-%m-%y')) 
##
# some Temperatures have been entered as Celsius; most are Fahrenheit
## above freezing
.F.rows <- which(sewtemp$Temp > 32)
sewtemp$Temp[.F.rows] <- fahrenheit.to.celsius(sewtemp$Temp[.F.rows])
sewtemp <- unique(sewtemp) # remove duplicate entries
## rename sewer temp col
#sewtemp$ph[sewtemp$ph > 14] <- NA # remove erroneous entries
sewtemp <- rename(sewtemp, c(Temp='SewTempC'))
## remove rows with no sewer temperature readings
sewtempn <- na.omit(subset(sewtemp, select=c(Date, SewTempC, Interceptor, Manhole)))
sewtemp.xts <- xts(sewtempn, sewtempn$Date) #
#### Get weekly mean sewer temperature
### at the moment this averages across all interceptors, manholes and days
### is there a better way??
sewtemp.week <- apply.weekly(sewtemp.xts$SewTempC, FUN=mean)
sewtemp.day <- apply.daily(sewtemp.xts$SewTempC, 
    function(x) c(mean=mean(as.numeric(x)), nobs=length(x))
)

#plot(sewtemp.week) # appears to work ok
## week and year from xts .index functions
## see ?.index for details
sewtemp.week$week <- .indexyday(sewtemp.week) %/% 7
sewtemp.week$year <- .indexyear(sewtemp.week) + 1900
## Turn into data.frame and join with sewer data
sewtemp.week.df <- data.frame(sewtemp.week)

precip <- read.csv('data/abq-tempsandrain-2005-2014.csv')
precip <- subset(precip, select=c(MST, Precipitationmm))
precip$no.precip <- precip$Precipitationmm == "0.00"

########################################
### Air temperature / weather data
########################################
weather <- read.csv('data/abq-temps-2005-2014.csv')
## shorten colnames for convenience
colnames(weather) <- gsub('.Temperature', 'Temp', colnames(weather))
# Turn factor into date    
weather$Date <- as.Date(as.POSIXct(weather$MST, format='%Y-%m-%d'))
# Convert Fahrenheit into Celsius
## find cols containing temp
.wcols <- grep('TempF', colnames(weather))
weather[,.wcols] <- fahrenheit.to.celsius(weather[,.wcols])
## update colnames to reflect C
colnames(weather) <- gsub('TempF', 'TempC', colnames(weather))
## Melt weather - used where??
weather.melt <- melt( weather, id.vars=c('MST', 'Date'))
# only grab measurement / non-date columns
#.tmp <- subset(weather, select=c(MaxTempC, MeanTempC, MinTempC))
weather.xts <- xts(subset(weather, select=MeanTempC), weather$Date)
## Get weekly mean temp
airtemp.week <- apply.weekly(weather.xts$MeanTempC, FUN=mean)
## week and year from xts .index functions
## see ?.index for details
#airtemp.week$week <- .indexyday(airtemp.week) %/% 7
#airtemp.week$year <- .indexyear(airtemp.week) + 1900
## Turn into data.frame and join with sewer data
#airtemp.week.df <- data.frame(airtemp.week)
## Full or "outer" join -- keep weather data for weeks without problems


########################################
### Sewer blockage data
########################################
## load sewer data (old data, superceded??)
## key:
## 10-40 near miss
## 10-42 any spill
## 10-48 property damage 
sewer <- read.csv('data/new-ABQ-sewer.csv')
sewer$is.grease <- grepl('GR', sewer$CAUSE)
## Convert reporting date column into time-based object
sewer$Date <- as.Date(as.POSIXct( sewer$REPORTDATE, format='%m/%d/%Y %H:%M'))
sewer.xts <- xts( subset(sewer, select=is.grease), sewer$Date)

#sewer.day <- apply.daily(sewer.xts, FUN=.greasefun)
#sewer.week <- apply.weekly(sewer.xts, FUN=.greasefun)
## Add day year, week, and year from the POSIXct date
#sewer$doy <- as.numeric(strftime(sewer$Date, format='%j'))
#sewer$week <- sewer$doy %/% 7
#sewer$year <- as.numeric(strftime(sewer$Date, format='%Y'))

## Number of problems for each week
if(F){
sewer.block.week = ddply( sewer, c('year', 'week'),
    function(x) { 
      data.frame(N=nrow(x))  }
    ## show progress
    #.progress='text'
)

## as above, by cause
.tmp <- sewer
sewer.block.cause.week = ddply( sewer, c('year', 'week', 'is.grease'),
    function(x) { 
      data.frame(N=nrow(x))  }
    ## show progress
    #.progress='text'
)
}


########################################
### Combining data / post-processing
########################################
## join to sewer temperatures
#intersect(colnames(weather), colnames(sewtemp)) # both contain 'Date
sewtemp.weather <- join(sewtemp, weather)
## explicitly remove NAs, e.g. only keep weeks we have sew temp grab samples for
sewtemp.weather <- na.omit(sewtemp.weather)

## join failures with air temp.
## left join excludes weather data for weeks with no blockage data
block.airtemp.week <- join(sewer.block.week, airtemp.week.df, type='full')
## Weeks with no problems 
block.airtemp.week$N[is.na(block.airtemp.week$N)] <- 0
## Why are there remaining NAs in Mean.TemperatureF?
block.airtemp.week <- na.omit(block.airtemp.week)

## sewer blockages per week, by cause
.tmp1 <- subset(sewer, select=c(Date, is.grease))
.tmp1$is.grease <- factor(.tmp1$is.grease, levels=c(T,F), labels=c('grease', 'not.grease'))
.tmp2 <- dcast(Date ~ is.grease, data=.tmp1)
.tmp22 <- mk.xts(.tmp2, .datcol = c('grease','not.grease'))
.tmp3 <- mk.cbind.weather(weather.xts, .tmp22)
.tmp4 <- apply.weekly(.tmp3, mk.weekly.summary)
.tmp5 <- mk.df.melt(.tmp4)
.tmp5$variable <- relevel(.tmp5$variable, "not.grease") 
block.cause.airtemp.week <- .tmp5

#block.cause.airtemp.week <- join(sewer.block.cause.week, airtemp.week.df, type='full')
## Weeks with no problems 
#block.cause.airtemp.week$N[is.na(block.cause.airtemp.week$N)] <- 0
#block.cause.airtemp.week <- na.omit(block.cause.airtemp.week)

## Full or "outer" join -- keep weather data for weeks without problems
block.sewtemp.week <- join(sewer.block.week, sewtemp.week.df, type='full')
## Weeks with no problems 
block.sewtemp.week$N[is.na(block.sewtemp.week$N)] <- 0
## Remove all rows missing either temperature or blockage data
block.sewtemp.week <- na.omit(block.sewtemp.week) # n=153
### make another data frame to include weather data
## joining sewer data to the temp.weekdf from Sewer_results_summary.Rnw
block.sewtemp.airtemp.week <- join(block.sewtemp.week, airtemp.week.df, by=c('week', 'year'), type='left')
## remove missing rows
block.sewtemp.airtemp.week <- na.omit(block.sewtemp.airtemp.week) # n=95
