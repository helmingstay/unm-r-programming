#### Calculating weekly mean FOG reading
require(xts)

## extract only log(fog) and date
all.lfog <- subset(allfog, select=c('lfog', 'date') )

## convert to xts format
all.lfog$Date <- as.POSIXct(all.lfog$date, format='%Y-%m-%d')
all.lfog <- all.lfog[order(all.lfog$Date),]
allfog.xts <- xts( all.lfog$lfog, order.by=all.lfog$Date ) # only the lfog col


## aggregate FOG measurements by week
allfog.day <- apply.daily(allfog.xts, mean)
allfog.week <- apply.weekly(allfog.day, mean) # mean is default function; no NAs
nrow(allfog.week) # 232 observations + 1 dummy
length(unique(allfog.week)) / nrow(allfog.week) # all are unique


## convert back to dataframe for joining and analysis
allfog.week.df <- as.data.frame(allfog.week)
## convert index to Date format
allfog.week.df$Date <- as.Date( index(allfog.week) )
names(allfog.week.df)[1] <- 'meanlfog' # rename column

#### additional columns for compatibility with sewblock, greaseblock &c
## sporadic FOG sampling means columns do not align with other dataframes
allfog.week.df$year <- as.numeric( format(allfog.week.df$Date, '%Y') )
allfog.week.df$week <- as.numeric( format(allfog.week.df$Date, '%j') ) %/% 7 # integer division

## check that all is well so far
str(allfog.week.df)
summary(allfog.week.df)


#### Join with block.airtemp.week data frame (includes temp, blocks and blocks)
baw <- block.airtemp.week
baw$year <- as.numeric( format(baw$Date, '%Y') )
baw$week <- as.numeric( format(baw$Date, '%j') ) %/% 7

## check intersect
intersect(colnames(baw), colnames(allfog.week.df))

## join by chosen columns
baw.join <- join(baw, allfog.week.df, by=c('year', 'week'), type='inner')

## glms - blocks ~ temp and fog, subsetted by cause
glm.btf.g <- glm(value ~ MeanTempC + meanlfog, 
                   data=subset(baw.join, variable=='grease'), 
                   family='poisson')
summary(glm.btf.g) # positive association with blockages; interaction not sig

glm.btf.n <- glm(value ~ MeanTempC + meanlfog, 
                 data=subset(baw.join, variable=='not.grease'), 
                 family='poisson')
summary(glm.btf.n) # positive association with blockages; interaction not sig
