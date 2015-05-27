# read data
allfog <- read.csv("~/Documents/fog/allfog.csv")
str(allfog)

# create date and day of year (doy) columns
allfog$date <- as.Date(allfog$Convert.Date, format="%m/%d/%y")
allfog$doy  <- as.numeric(strftime(allfog$date, format="%j"))
allfog$fog  <- as.numeric(allfog$RESULT)

# plot trends for all data
with(allfog, plot(date, RESULT))
with(allfog, plot(doy , RESULT))


## plot each interceptor individually

# multifigure plot
opar <- par() # save old par settings
par(mfrow=c(2,3)) # two rows , three cols

with(allfog, plot(doy , RESULT, main="All")) # first plot contains all data

# remove interceptors with few data points
by(allfog, allfog$SAMPLE_POINT_ID, FUN=function(df) {
  if(nrow(df) >= 52) { # exclude points with less than a 1 week/year measurements
    plot(df$doy, df$RESULT,
         main=paste(df$SAMPLE_POINT_ID[1]))
  }
})
par(opar) # restore old par settings

summary(lm(as.numeric(RESULT) ~ doy, data=allfog)) # bad fit; not sig
# try a quadratic - still a bad fit
summary(lm(as.numeric(RESULT) ~ poly(doy, 2), data=allfog)) # model just sig, params not
# try a third order
summary(lm(as.numeric(RESULT) ~ poly(doy, 3), data=allfog)) # bad fit; not sig

# TODO - plot the quadratic - this doesn't work
# with(allfog, plot(doy, RESULT))
# lines(predict(lm(as.numeric(RESULT) ~ I(doy^2) + doy, data=allfog)))
# abline(lm(as.numeric(RESULT) ~ doy, data=allfog))


#### join with weather data

## read in weather data
weather <- read.csv('/home/josh/unm-r-programming/sewer/data/abq-temps-2005-2015.csv')
## shorten colnames for convenience
colnames(weather) <- gsub('.Temperature', 'Temp', colnames(weather))
# Turn factor into date    
weather$date <- as.Date(as.POSIXct(weather$MST, format='%Y-%m-%d'))
# Convert Fahrenheit into Celsius
## find cols containing temp
.wcols <- grep('TempF', colnames(weather))
weather[,.wcols] <- fahrenheit.to.celsius(weather[,.wcols])
## update colnames to reflect C
colnames(weather) <- gsub('TempF', 'TempC', colnames(weather))

## database join
#intersect(colnames(weather), colnames(allfog)) # both contain 'date'
require(plyr)
fogtemp <- join(allfog, weather, type='inner')

# linear model
lm.temp.fog <- lm(fog ~ MeanTempC, data=fogtemp)
summary(lm.temp.fog) # not sig at all, R^2 ~0

#plot
with(fogtemp, plot(MeanTempC, fog))
abline(lm.temp.fog)

##### differences between interceptors
# identify interceptors with enough data
.ints <- levels(
  allfog$SAMPLE_POINT_ID)[as.numeric(by(allfog, allfog$SAMPLE_POINT_ID, nrow)) >= 52]
fogints <- subset(fogtemp, SAMPLE_POINT_ID %in% .ints)

# plot
require(lattice)
bwplot(fog ~ SAMPLE_POINT_ID, data=fogints)

## test
lm.ints.fog <- lm(fog ~ SAMPLE_POINT_ID, data=fogints)
summary(lm.ints.fog)
# Edith and  Tijeri have signifcantly higher fog levels

####### model with interceptor and temp
lm.ints.temp <- lm(fog ~ SAMPLE_POINT_ID + MeanTempC, data=fogints)
summary(lm.ints.temp) # no effect of temp, just same 2 interceptors