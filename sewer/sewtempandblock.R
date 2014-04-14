### sewer blockage df
sewblock <- read.csv('http://unm-r-programming.googlecode.com/files/new-ABQ-sewer.csv')
## Convert reporting date column into time-based object
sewblock$Date <- as.POSIXct( sewblock$REPORTDATE, format='%m/%d/%Y %H:%M')
sewblock$day <- as.Date(sewblock$Date)

## Add day year, week, and year from the POSIXct date
sewblock$doy <- as.numeric(strftime(sewblock$Date, format='%j'))
sewblock$week <- sewblock$doy %/% 7
sewblock$year <- as.numeric(strftime(sewblock$Date, format='%Y'))
#head(sewblock, 3)

## Number of problems for each week
require(plyr)
sewer.fail.week = ddply( sewblock, c('year', 'week'),
                         function(x) { 
                           data.frame(N=nrow(x))  } 
                         ## show progress
                         #.progress='text'
)
#head(sewer.fail.week)

### sewer temp df

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
str(sewtemp) # inspect

### combine temp and blockage data in one dataset
intersect(colnames(sewtemp), colnames(sewblock))
blocktemp <- join(sewblock, sewtemp, type='full')
blocktemp <- subset(blocktemp, select=c(ABCWUA_10CODE, CAUSE, Date, day, doy, week, 
                                        year, Interceptor, Manhole, Temp))
blocktempn<- na.omit(blocktemp)
names(blocktemp)
head(blocktemp, 3)
names(blocktemp)
summary(blocktemp)

# xts(data, timebase), timebase is a vector that can be turned into a POSIXct
sewtempn <- na.omit(subset(sewtemp, select=c(Date, Temp, Interceptor, Manhole)))
timebase <- as.POSIXct( sewtempn$Date) + 7*60*60 # correct time zone (add 7 hours in seconds)
require(xts)
sewtemp.xts <- xts(sewtempn, timebase) # 

## Get weekly mean temp
sewtemp.week <- sewtemp.xts$Temp
sewtemp.week <- apply.weekly(sewtemp.week, FUN=mean)
#plot(temp.week)
## week and year from xts .index functions
## see ?.index for details
sewtemp.week$week <- .indexyday(sewtemp.week) %/% 7
sewtemp.week$year <- .indexyear(sewtemp.week) + 1900

## Turn into data.frame and join with sewer data
sewtemp.week.df <- data.frame(sewtemp.week)
#head(temp.week.df, 3)
## Full or "outer" join -- keep weather data for weeks without problems
sewtemp.join <- join(sewer.fail.week, sewtemp.week.df, type='full')
#head(sewer.join, 3)

## Weeks with no problems 
sewtemp.join$N[is.na(sewtemp.join$N)] <- 0
## Why are there remaining NAs in TempMeanF?
sewtemp.join <- na.omit(sewtemp.join)

plot(sewtemp.join$Temp, sewtemp.join$N)

##Temp
## Count data, assume y is poisson distributed
sewtemp.glm.pois <- glm(N ~ Temp, data=sewtemp.join, family=poisson)

## Test for overdispersion
require(AER)
dispersiontest(sewtemp.glm.pois) # v overdispersed, so use quasiopoisson
sewtemp.glm.qpois <- glm(N ~ Temp, data=sewtemp.join, family=quasipoisson)

summary(sewtemp.glm.qpois) # highly sig
with(sewtemp.glm.pois, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.086

## plot
require(ggplot2)
p <- ggplot(sewtemp.join)
p <- p + geom_point(aes(x=Temp, y=N), shape=21)
p <- p + stat_smooth(method='glm', family='poisson', aes(x=Temp, y=N)) + 
  xlab('Mean weekly temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)

sewtemp.w.join <- join(sewtemp.join, temp.week.df, by=c('week', 'year'), type='left')
sewtemp.w.join <- na.omit(sewtemp.w.join)
b.st.qpm <- glm(N ~ Temp, data=sewtemp.w.join, family='poisson')
AIC(b.st.qpm) # 450
with(b.st.qpm, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.16

b.wt.qpm <- glm(N ~ TempMeanF, data=sewtemp.w.join, family='poisson')
AIC(b.wt.qpm) # 456
with(b.wt.qpm, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.13

### Zero-inflated models
require(pscl)
z.st <- zeroinfl(N ~ Temp, data=sewtemp.w.join, dist='poisson')
summary(z.st)
AIC(z.st)
with(z.st, (null.deviance-deviance)/null.deviance) # pseudoR^2 = 0.13
z.temp.glm.pois <- glm(N ~ Temp, data=sewtemp.w.join, family=poisson)
AIC(z.temp.glm.pois)

z.wt <- zeroinfl(N ~ TempMeanF, data=sewtemp.w.join, dist='poisson')
summary(z.wt)