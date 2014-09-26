#### Using sewer temperature data to predict number of blockages:


#### Load packages
require(weathermetrics) # to fix units of Temp
require(plyr)           # for summarising blockages by week
require(reshape2)           
require(xts)            # for manipulating temperature timeseries
require(AER)            # for testing overdispersion
require(ggplot2)        # for plotting
require(MASS)           # for fitting negative binomial models
require(pscl)           # for Vuong's closeness test
require(lmtest)         # for likelihood ratio test

#### first load weather data (and old sewer data??)
source('run.weather.R')

### sewer blockage data
sewblock <- read.csv('http://unm-r-programming.googlecode.com/files/new-ABQ-sewer.csv')

## Convert reporting date column into time-based object
sewblock$Date <- as.POSIXct( sewblock$REPORTDATE, format='%m/%d/%Y %H:%M')
sewblock$day <- as.Date(sewblock$Date)

## Add day, week, and year from the POSIXct date
sewblock$doy <- as.numeric(strftime(sewblock$Date, format='%j'))
sewblock$week <- sewblock$doy %/% 7
sewblock$year <- as.numeric(strftime(sewblock$Date, format='%Y'))
#head(sewblock, 3)

## Number of problems for each week

sewer.fail.week = ddply( sewblock, c('year', 'week'),
                         function(x) { 
                           data.frame(N=nrow(x))  } 
                         ## show progress
                         #.progress='text'
)
#head(sewer.fail.week)


### sewer temperature data
.colClasses <- c(Interceptor='factor', Manhole='factor', Date='character'
                 , Time='character', Temp='numeric' ,ph='NULL', Tot.Sulfide='NULL', 
                 Dis.Sulfide='NULL', Tot.Iron='NULL', Ferrous.Fe='NULL')

sewtemp <- read.table("allgrabdata_datefix.csv", sep=',', header=T, comment.char='#', colClasses=.colClasses)
## xian - posixct gives a full date spec, 
## can't use it *just* for time
## we're not really using this though
## do this *before* date col 
sewtemp$DateTime <- with(sewtemp, 
                         as.POSIXct( paste(Date, Time),
                                     format='%d-%m-%y %H:%M'
                         ))
sewtemp$Date <- as.POSIXct(sewtemp$Date, format='%d-%m-%y') # fix dates
# some Temperatures have been entered as Celsius; most are Fahrenheit
.F.rows <- which(sewtemp$Temp > 32)
sewtemp$Temp[.F.rows] <- fahrenheit.to.celsius(sewtemp$Temp[.F.rows])
sewtemp <- unique(sewtemp) # remove duplicate entries
#sewtemp$ph[sewtemp$ph > 14] <- NA # remove erroneous entries
# str(sewtemp) # inspect

### xts(data, timebase), timebase is a vector that can be turned into a POSIXct
## remove rows with no sewer temperature readings
sewtempn <- na.omit(subset(sewtemp, select=c(Date, Temp, Interceptor, Manhole)))
timebase <- as.POSIXct( sewtempn$Date) + 7*60*60 # correct time zone (add 7 hours in seconds)
sewtemp.xts <- xts(sewtempn, timebase) # 

#### Get weekly mean sewer temperature
### at the moment this averages across all interceptors, manholes and days
### is there a better way??
sewtemp.week <- sewtemp.xts$Temp
sewtemp.week <- apply.weekly(sewtemp.week, FUN=mean)
#plot(sewtemp.week) # appears to work ok
## week and year from xts .index functions
## see ?.index for details
sewtemp.week$week <- .indexyday(sewtemp.week) %/% 7
sewtemp.week$year <- .indexyear(sewtemp.week) + 1900

## Turn into data.frame and join with sewer data
sewtemp.week.df <- data.frame(sewtemp.week)
#head(temp.week.df, 3)
## Full or "outer" join -- keep weather data for weeks without problems
sewtemp.join <- join(sewer.fail.week, sewtemp.week.df, type='full')
#head(sewtemp.join, 3)

## Weeks with no problems 
sewtemp.join$N[is.na(sewtemp.join$N)] <- 0
## Remove all rows missing either temperature or blockage data
sewtemp.join <- na.omit(sewtemp.join) # n=153

### make another data frame to include weather data
## joining sewer data to the temp.weekdf from Sewer_results_summary.Rnw
sewtemp.w.join <- join(sewtemp.join, temp.week.df, by=c('week', 'year'), type='left')
## remove missing rows
sewtemp.w.join <- na.omit(sewtemp.w.join) # n=95

#plot(sewtemp.join$Temp, sewtemp.join$N) # apparent negative trend?


#### Fit models
hist(sewtemp.join$N) # histogram of data
# overdispersed count data with a lot of zeros, 
# but no additional zero-generating process - 
# therefore negative binomial GLM is best


#### Predict number of blockages using sewer temperature data

## negative binomial model
nb.block <- glm.nb(N ~ Temp, data=sewtemp.join)
summary(nb.block) # highly significant; AIC = 638.37

## check assumptions by comparing to Poisson model
# fit Poisson model
pois.block <- glm(N ~ Temp, data=sewtemp.join, family='poisson')
pois.block$aic # AIC=811.46 - are these comparable?
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(nb.block, pois.block) # negbin is a very significant improvement

## get plots out of .R files!!
## plotting - no native ggplot2 method for plotting negbin models
# calculate predicted values and confidence intervals
sewtemp.join <- cbind(sewtemp.join, predict(nb.block, type='link', se.fit=T))
sewtemp.join <- within(sewtemp.join, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(sewtemp.join, aes(x=Temp))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly sewage temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)
pblock <- p


#### Compare the use of weather versus sewer temperature for predicting blockages
hist(sewtemp.w.join$N) # also seems zero-inflated, but less so

### fit models

## sewer temp
nb.st <- glm.nb(N ~ Temp, data=sewtemp.w.join)
summary(nb.st) # highly sig; AIC = 440.26

## check assumptions by comparing to Poisson model
# fit Poisson model
pois.st <- glm(N ~ Temp, data=sewtemp.w.join, family='poisson')
pois.st$aic # AIC = 450.93
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(nb.st, pois.st) # negbin is a very significant improvement; 

## weather temperature
nb.wt <- glm.nb(N ~ Mean.TemperatureF, data=sewtemp.w.join)
summary(nb.wt) # highly sig; AIC = 443.73

## check assumptions by comparing to Poisson model
# fit Poisson model
pois.wt <- glm(N ~ Mean.TemperatureF, data=sewtemp.w.join, family='poisson')
pois.wt$aic # AIC = 456.10
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(nb.wt, pois.wt) # negbin is a very significant improvement; 


## compare models using sewer and air temperature directly
# Vuong's closeness test for non-nested models - is there a better test?
vuong(nb.st, nb.wt) # using sewer temp is not a significant improvement (p > 0.1)


# negbin is a big improvement over standard Poisson
# using actual sewer temperature is at most a modest improvement over air temperature


### plot - no native ggplot2 method for plotting negbin models

## sewer temp

# calculate predicted values and confidence intervals
sewtemp.w.joins <- cbind(sewtemp.w.join, predict(nb.st, type='link', se.fit=T))
sewtemp.w.joins <- within(sewtemp.w.joins, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(sewtemp.w.joins, aes(x=Temp))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly sewer temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)

## weather temp

# calculate predicted values and confidence intervals
sewtemp.w.joinw <- cbind(sewtemp.w.join, predict(nb.wt, type='link', se.fit=T))
sewtemp.w.joinw <- within(sewtemp.w.joinw, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(sewtemp.w.joinw, aes(x=Mean.TemperatureF))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly air temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)
