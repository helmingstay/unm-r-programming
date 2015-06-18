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
# are all year-week combos unique?
allfog.week.df$year.week <- sprintf('%s.%s', allfog.week.df$year, allfog.week.df$week)
length(unique (allfog.week.df$year.week)) / nrow(allfog.week.df) # not quite: 1 duplicate!
# which are the duplicates?
allfog.week.df[duplicated(allfog.week.df$year.week),] # only 2 weeks, can be excluded
allfog.week.df <- allfog.week.df[!duplicated(allfog.week.df$year.week),]# excluding duplicates

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

## try including cause in the model as well
glm.btf.c <- glm(value ~ MeanTempC + meanlfog + variable, data=baw.join) # all 3 are v sig
summary(glm.btf.c)

##### Plotting these results

### first, we need to predict the fitted values from model
badger <- predict(glm.btf.c, se.fit=T) # model predictions
baw.join$phat <- badger$fit # fitted value
baw.join$ul <- badger$fit + badger$se.fit
baw.join$ll <- badger$fit - badger$se.fit

### plot of blocks against temperature
plot.temp <- ggplot(baw.join, aes(x=MeanTempC, y=value)) +
  theme_bw() + 
  xlab('Mean Weekly Air Temperature (C)') +
  ylab('Blocks Per Week') + 
  facet_grid(.~variable) + 
  geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) +
  geom_point() 
plot.fog <- ggplot(baw.join, aes(x=MeanTempC, y=meanlfog)) +
  theme_bw() + 
  xlab('Mean Weekly FOG Concentration') +
  ylab('Blocks Per Week') + 
  facet_grid(.~variable) + 
  geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) +
  geom_point() 
require(gridExtra)
grid.arrange(plot.temp, plot.fog, nrow=2)

#### contour plot
source(mk.gridder.R) # B Junker's function

## for real data - note function is very slow!
gridded.data <- gridder(baw.join$MeanTempC, baw.join$meanlfog, baw.join$value)
names(gridded.data) <- c('temp', 'fog', 'block')

p <- ggplot(gridded.data) + theme_classic()
p <- p + stat_contour(aes(x=temp, y=fog, z=block, color=..level..))
p.grid <- p + xlab('Mean Weekly Air Temperature') + ylab('Mean Weekly log(FOG)')
print(p.grid)
