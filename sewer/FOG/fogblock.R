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
allfog.week.df[duplicated(allfog.week.df$year.week),] # only 2 weeks, can be excluded for now
allfog.week.df <- allfog.week.df[!duplicated(allfog.week.df$year.week),]# excluding duplicates



#### Join with block.airtemp.week data frame (includes temp, blocks and FOG)
baw <- block.airtemp.week
baw$year <- as.numeric( format(baw$Date, '%Y') )
baw$week <- as.numeric( format(baw$Date, '%j') ) %/% 7

## check intersect
intersect(colnames(baw), colnames(allfog.week.df))


## join by chosen columns
baw.join <- join(baw, allfog.week.df, by=c('year', 'week'), type='inner')


##### Predictive models #####

## glms - blocks ~ temp and fog, subsetted by cause
glm.btf.g <- glm(value ~ MeanTempC + meanlfog, 
                   data=subset(baw.join, variable=='grease'), 
                   family='poisson')
summary(glm.btf.g) # positive association with blockages; interaction not sig
mk.prop.dev(glm.btf.g) # 16.4


glm.btf.n <- glm(value ~ MeanTempC + meanlfog, 
                 data=subset(baw.join, variable=='not.grease'), 
                 family='poisson')
summary(glm.btf.n) # positive association with blockages; interaction not sig
mk.prop.dev(glm.btf.n) # 0.09



#### try including cause in the model as well

glm.btf.c <- glm(value ~ MeanTempC + meanlfog + variable, data=baw.join, family='poisson') 
summary(glm.btf.c) # all 3 are v sig
mk.prop.dev(glm.btf.c) # 18.9


##### Plotting these results

### first, we need to predict the fitted values from model
### TODO
### I couldn't get this to work
### ggplot joins all fitted values with an unsmoothed line
### possibly a problem of having 2 predictors in the model?
### instead, plots below plot separate glm for each facet (?)

# badger <- predict(glm.btf.c, se.fit=T) # model predictions
# baw.join$phat <- badger$fit # fitted value
# baw.join$ul <- badger$fit + badger$se.fit
# baw.join$ll <- badger$fit - badger$se.fit

### plot of blocks against temperature
plot.temp <- ggplot(baw.join, aes(x=MeanTempC, y=value)) +
  theme_bw() + 
  xlab('Mean Weekly Air Temperature (C)') +
  ylab('Blocks Per Week') + 
  facet_grid(.~variable) + 
  geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) +
  geom_point() 

### plot of FOG
plot.fog <- ggplot(baw.join, aes(x=MeanTempC, y=meanlfog)) +
  theme_bw() + 
  xlab('Mean Weekly FOG Concentration') +
  ylab('Blocks Per Week') + 
  facet_grid(.~variable) + 
  geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) +
  geom_point() 

## plot together
require(gridExtra)
grid.arrange(plot.temp, plot.fog, nrow=2)
# very difficult to see the effect of FOG in this plot
# possibly because only visible when effect of temperature controlled


## might be interesting to plot the residuals from the temp-only model instead
# fit model
glm.bt.c <- glm(value ~ MeanTempC + variable, data=baw.join, family='poisson')
summary(glm.bt.c) # sig of course
mk.prop.dev(glm.bt.c) # 17.1

# plot
with(glm.bt.c, plot(data$meanlfog, residuals))
# ...maybe? positive correlation definitely visible



## these visualisations aren't very good
# 3d scatter plots look awful (I tried...)
# try a contour plot

#### contour plot
source(mk.gridder.R) # defines B Junker's function

## function is for real data 
## note function is very slow!
gridded.data <- gridder(baw.join$MeanTempC, baw.join$meanlfog, baw.join$value)
names(gridded.data) <- c('temp', 'fog', 'block')

## plot output from gridder()
p <- ggplot(gridded.data) + theme_classic()
p <- p + stat_contour(aes(x=temp, y=fog, z=block, color=..level..))
p.grid <- p + xlab('Mean Weekly Air Temperature') + ylab('Mean Weekly log(FOG)')
print(p.grid)





####### Model selection#########
require(MuMIn) # MUlti Model INference

### for full model
glm.full <- glm(value ~ MeanTempC + meanlfog + variable, data=baw.join, family='poisson')

# rank all models
glm.dredge <- dredge(glm.full, rank='BIC', extra=function(x)
  D = 1 - x$deviance/x$null.deviance) # what is the best IC here?
glm.dredge # check out the table
# top model is full model
# deltaBIC ~13 for model w/o fog !


# model averaging
glm.mav <- model.avg(glm.dredge)
summary(glm.mav) # variable importance of all vars == 1


###for subsets - no differences in model ranks etc
glm.g.full <- glm(value ~ MeanTempC + meanlfog, 
               data=subset(baw.join, variable=='grease'), family='poisson')
glm.g.dredge <- dredge(glm.g.full, extra=function(x)
  D = 1 - x$deviance/x$null.deviance)
glm.g.dredge # same story as for full model

glm.n.full <- glm(value ~ MeanTempC + meanlfog, 
                 data=subset(baw.join, variable!='grease'), family='poisson')
glm.n.dredge <- dredge(glm.n.full, extra=function(x)
  D = 1 - x$deviance/x$null.deviance)
glm.n.dredge # same story as full model
