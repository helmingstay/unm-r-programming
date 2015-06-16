# read data
allfog <- read.csv("~/Documents/fog/allfog.csv")
str(allfog)

# create date and day of year (doy) columns
allfog$date <- as.Date(allfog$Convert.Date, format="%m/%d/%y")
allfog$doy  <- as.numeric(strftime(allfog$date, format="%j"))

# which results are non-numeric?
levels(allfog$RESULT)[grep('<', levels(allfog$RESULT))] # many.....
allfog$inequality <- grepl('<', allfog$RESULT) # which elements have inequality?

# initialise new vector for cleaned data
allfog$fog <- NA
# directly copy numeric strings
allfog$fog[!allfog$inequality] <- as.numeric(
  as.character(allfog$RESULT[!allfog$inequality]))
# remove inequality sign and use half detection limit, per Bruce Thomson
allfog$fog[allfog$inequality] <- as.numeric(
  as.character( gsub('<', '', allfog$RESULT[allfog$inequality]) ) ) / 2 
# log transform because of extreme values
allfog$lfog <- log(allfog$fog)


# plot trends for all data
with(allfog, plot(date, lfog))
with(allfog, plot(doy , lfog))


## plot each interceptor individually

# multifigure plot
opar <- par() # save old par settings
par(mfrow=c(2,3)) # two rows , three cols

with(allfog, plot(doy , lfog, main="All")) # first plot contains all data

# remove interceptors with few data points
by(allfog, allfog$SAMPLE_POINT_ID, FUN=function(df) {
  if(nrow(df) >= 52) { # exclude points with less than a 1 week/year measurements
    plot(df$doy, df$lfog,
         main=paste(df$SAMPLE_POINT_ID[1]))
  }
})
par(opar) # restore old par settings


# models using log-transformed data
summary(lm(lfog ~ doy, data=allfog)) # bad fit; but sig decrease during the year
# try a quadratic - still a bad fit
summary(lm(lfog ~ poly(doy, 2), data=allfog)) # bad fit; 2nd order not sig
# try a third order
summary(lm(lfog ~ poly(doy, 3), data=allfog)) # bad fit; 2nd & 3rd orders not sig

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
require(weathermetrics)
weather[,.wcols] <- fahrenheit.to.celsius(weather[,.wcols])
## update colnames to reflect C
colnames(weather) <- gsub('TempF', 'TempC', colnames(weather))

## database join
#intersect(colnames(weather), colnames(allfog)) # both contain 'date'
require(plyr)
fogtemp <- join(allfog, weather, type='inner')

# linear model
lm.temp.fog <- lm(fog ~ MeanTempC, data=fogtemp); summary(lm.temp.fog) # not sig at all, R^2 ~0
# try with log transformed values because of outliers / extreme values
lm.temp.lfog <- lm(log(fog) ~ MeanTempC, data=fogtemp); summary(lm.temp.lfog) # still terrible

#plot
with(fogtemp, plot(MeanTempC, fog)); abline(lm.temp.fog)
# plot of log values
with(fogtemp, plot(MeanTempC, log(fog))); abline(lm.temp.lfog) # clearly shows no relationship!

##### differences between interceptors
# identify interceptors with enough data
.ints <- levels(
  allfog$SAMPLE_POINT_ID)[as.numeric(by(allfog, allfog$SAMPLE_POINT_ID, nrow)) >= 52]
fogints <- subset(fogtemp, SAMPLE_POINT_ID %in% .ints)

# plot
require(lattice)
bwplot(lfog ~ SAMPLE_POINT_ID, data=fogints) # using log-transformed points

## test
lm.ints.fog <- lm(lfog ~ SAMPLE_POINT_ID, data=fogints)
summary(lm.ints.fog)
# sig differences between most interceptors using log-transformed values
# R^2 = 0.12

####### model with interceptor and temp
lm.ints.temp <- lm(lfog ~ SAMPLE_POINT_ID + MeanTempC, data=fogints)
summary(lm.ints.temp) # no effect of temp, just same interceptors
