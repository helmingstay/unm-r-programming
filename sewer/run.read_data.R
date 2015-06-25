require(reshape2)
## In this file,
## sewer temp grabsamples, air temp, and sewer blockages
## do temps first

## convenience function - turn range into xts time index
## e.g. '2001-01-01::2002-01-01'
mk.xts.range <- function(.range) {
    sprintf('%s::%s', .range[1], .range[2])
}

## convenience function, take 2 xts objects
## the skeleton should be sampled daily
## dat is NA filled to missing timepoints
mk.xts.fill <- function(dat, skel, ret.col){
    .range <- range(index(dat))
    ## subset skel to limits of dat
    skel <- skel[mk.xts.range(.range),]
    ## fill dat w/NAs
    ret <- suppressWarnings(cbind(dat, skel))
    ret <- ret[,ret.col]
    ## only return the specified columns 
    return(ret)
}



########################################
### Air temperature / weather data
########################################
weather <- read.csv('data/abq-temps-2005-2015.csv')
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
#weather.melt <- melt( weather, id.vars=c('MST', 'Date'))
# only grab measurement / non-date columns
#.tmp <- subset(weather, select=c(MaxTempC, MeanTempC, MinTempC))
weather.xts <- xts(subset(weather, select=MeanTempC), weather$Date)


## Get weekly mean temp
## use best.weather data.frame instead? rollmean computed above in run.model...
airtemp.week <- apply.weekly(weather.xts$MeanTempC, FUN=mean)
airtemp.week.df <- data.frame(
    MeanTempC=airtemp.week, Date=index(airtemp.week)
)


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
sewtemp <- na.omit(subset(sewtemp, select=c(Date, SewTempC, Interceptor, Manhole)))

## store range of finished data for later
sewtemp.stats <- with(sewtemp, list(
    min=min(Date),
    max=max(Date),
    nobs=length(Date),
    ndays=length(unique(Date))
))

## process merges w/timeseries tools
sewtemp.xts <- xts(sewtemp, sewtemp$Date) #
## fill missing days w/NAs
sewtemp.xts <- mk.xts.fill(sewtemp.xts, weather.xts, 'SewTempC')

#### Get weekly mean sewer temperature
### at the moment this averages across all interceptors, manholes and days
mk.na.omit.mean <- function(x) {
    .tmp <- na.omit(x)
    ret <- c(SewTempC=mean(as.numeric(.tmp)), nobs=length(.tmp))
    return(ret)
}


### is there a better way??
## get mean / #obs of sewer temp
sewtemp.week <- apply.weekly(sewtemp.xts, mk.na.omit.mean)
sewtemp.day <- apply.daily(sewtemp.xts, mk.na.omit.mean)


#plot(sewtemp.week) # appears to work ok
## week and year from xts .index functions
## see ?.index for details
#sewtemp.week$week <- .indexyday(sewtemp.week) %/% 7
#sewtemp.week$year <- .indexyear(sewtemp.week) + 1900
## Turn into data.frame and join with sewer data
sewtemp.week.df <- as.data.frame(sewtemp.week)
sewtemp.week.df$Date <- index(sewtemp.week)
## only keep weeks with at least one obs
sewtemp.week.df <- subset(sewtemp.week.df, nobs>0) 


## not much to see here...
precip <- read.csv('data/abq-tempsandrain-2005-2014.csv')
precip <- subset(precip, select=c(MST, Precipitationmm))
precip$no.precip <- precip$Precipitationmm == "0.00"


########################################
### Sewer blockage data
########################################
## load sewer data (old data, superceded??)
## key:
## 10-40 near miss
## 10-42 any spill
## 10-48 property damage 
  
sewer <- read.csv('data/UNM_R_Analysis_Join_Update.csv.gz')
## Convert reporting date column into time-based object
sewer$Date <- as.Date(as.POSIXct(as.character(sewer$REPORTDATE), format='%m/%d/%y %H:%M'))

## Grease is involved, include codes:
## GREASE + alot more?  GRS??
sewer$is.grease <- grepl('GR', sewer$CAUSE)
## except for this one, as per Mark 
sewer$is.grease[grepl("SDGTGRVL", sewer$CAUSE)] <- FALSE

sewer.stats <- with(sewer, list(
    min=min(Date),
    max=max(Date),
    nobs=length(Date),
    ndays=length(unique(Date))
))
sewer.range <- range(sewer$Date)
## Pull bool vector of whether blockage is grease-caused
sewer.xts <- xts( subset(sewer, select=is.grease), sewer$Date)
## use merge to fill unsampled weeks w/NA
## only for sample duration of sewer blockages
sewer.xts <- mk.xts.fill(sewer.xts, weather.xts, 'is.grease')

## compute blockages per week using xts,
## then move into data.frame
block.all.xts <- apply.weekly(sewer.xts, function(x){
    ## all nas have length 0
    length(na.omit(x))
})
## as above, by cause
block.cause.xts <- apply.weekly( sewer.xts, function(x){
        x <- na.omit(x)
        if (length(x) == 0) return(c(0,0))
        ## T/F index removes NAs
        ## sums blocks caused by grease (T) 
        ## vs not grease (F)
        cbind(grease=nrow(x[x]), not.grease=nrow(x[!x]))
})

## save ts object for plotting
block.plot.xts <- cbind(block.all.xts, block.cause.xts[,1])
colnames(block.plot.xts) <- c('All Causes', 'Grease')

## combine into one object 
sewer.block.week <- data.frame(
    Date=index(block.all.xts),
    all=as.vector(block.all.xts),
    grease=block.cause.xts[,1],
    not.grease=block.cause.xts[,2]
)
## long-form, just keep 
sewer.block.week.melt <- melt(
    subset(sewer.block.week, select=-all), id.vars='Date'
)

## error-checking
#if(!identical( index(.tmp.all), index(.tmp.cause))){
#    stop('Indexes should be identical')
#}
