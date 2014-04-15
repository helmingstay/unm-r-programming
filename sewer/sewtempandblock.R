#### Using sewer temperature data to predict number of blockages:

#### Load packages
require(plyr)    # for summarising blockages by week
require(xts)     # for manipulating temperature timeseries
require(AER)     # for testing overdispersion
require(ggplot2) # for plotting
require(pscl)    # for fitting zero-inflated models
#require(MuMIn)   # for finding QAIC of models
#### read in data

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
library(weathermetrics) # fix units of Temp
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

#plot(sewtemp.join$Temp, sewtemp.join$N) # apparent negative trend?


#### Fit models
## note - negative binomial GLMs would be better than either quasipoisson or ZIP
## will sort this out next

#### Predict number of blockages using sewer temperature data

### Count data, assume y is ~poisson distributed
hist(sewtemp.join$N) # but also appears zero-inflated..

###  Try Poisson GLM
sewtemp.glm.pois <- glm(N ~ Temp, data=sewtemp.join, family=poisson)

## Test for overdispersion
quasipois.disp <- dispersiontest(sewtemp.glm.pois) 
quasipois.disp
## v overdispersed (dispersion parameter > 3, p << 0.001)

### So use quasipoisson GLM to account for overdispersion
sewtemp.glm.qpois <- glm(N ~ Temp, data=sewtemp.join, family=quasipoisson)
summary(sewtemp.glm.qpois) # highly sig
with(sewtemp.glm.qpois, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.086

## No AIC for quasi-likelihood models - try QAIC?
## QAIC() in {MuMIn}
## why is this NA?
#QAIC(sewtemp.glm.qpois, chat= unlist(quasipois.disp$estimate))
# QAIC takes as input a loglikelihood and a dispersion parameter
# so function should be used on poisson, not quasipoisson, model
# as it brings its own quasi-ness to the party
# should be possible to calculate manually

### plot

p <- ggplot(sewtemp.join)
p <- p + geom_point(aes(x=Temp, y=N), shape=21)
p <- p + stat_smooth(method='glm', family='poisson', aes(x=Temp, y=N)) + 
  xlab('Mean weekly temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)



#### Compare the use of weather versus sewer temperature for predicting blockages
### make new data frame
## joining sewer data to the temp.weekdf from Sewer_results_summary.Rnw
sewtemp.w.join <- join(sewtemp.join, temp.week.df, by=c('week', 'year'), type='left')
## remove missing rows
sewtemp.w.join <- na.omit(sewtemp.w.join) # n=95
hist(sewtemp.w.join$N) # also seems zero-inflated, but less so

### fit models
## Poisson / Quasi-Poisson GLMs: st=sewer temperature; wt=weather temperature

## sewer temperature
# try Poisson
b.st.pm <- glm(N ~ Temp, data=sewtemp.w.join, family='poisson')
AIC(b.st.pm) # 450.9
with(b.st.pm, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.16
## other pseudoR^2s are available in pR2() from {pscl} - investigate?
dispersiontest(b.st.pm) # overdispersed p=0.01; dispersion = 1.5
# so use quasi-Poisson
b.st.qpm <- glm(N ~ Temp, data=sewtemp.w.join, family='quasipoisson') 
summary(b.st.qpm) # highly significant

## weather temperature
# try Poisson:
b.wt.pm <- glm(N ~ TempMeanF, data=sewtemp.w.join, family='poisson')
AIC(b.wt.pm) # 456.1 - less than sewer temperature, but not disastrously so
with(b.wt.pm, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.13 # similar
dispersiontest(b.wt.pm) # overdispersed p=0.01; dispersion = 1.6
# so use quasi-Poisson
b.wt.qpm <- glm(N ~ TempMeanF, data=sewtemp.w.join, family='quasipoisson')
summary(b.wt.qpm) # highly significant

## Zero-inflated models

# sewer temperature
z.st <- zeroinfl(N ~ Temp, data=sewtemp.w.join, dist='poisson')
summary(z.st) # zero-inflation model not significant
AIC(z.st) # 439.7 - better than standard Poisson
# null.deviance not available - other pseudoR^2 method needed

# weather temperature
z.wt <- zeroinfl(N ~ TempMeanF, data=sewtemp.w.join, dist='poisson')
summary(z.wt) # zero inflation model is significant
AIC(z.wt) # 445.5 - still better than both Poisson models