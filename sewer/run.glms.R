.annotate.size = 12
#### Fit models
hist(block.sewtemp.week$value) # histogram of data

## both sewer temp and air temp,
## not separated by cause
block.bothtemp.list <- within(list(),{
  dat <- block.bothtemp
  ## clean up factors for facet plotting
  dat$variable <- factor(dat$variable, 
                         levels=c('SewTempC', 'MeanTempC'),
                         labels=c('Sewer Temp: mST/W (C)', 'Air Temp: mAT/W')
  )
  plot <- ggplot(dat, aes(x=value, y=all)) +
    theme_bw() + 
    xlab('Mean Weekly Sewer and Air Temperature (C)') +
    ylab('Total Blocks Per Week') + 
    facet_wrap(~variable, scales='free_x') + 
    geom_smooth(method = "glm", method.args = list(family = "poisson"), colour='blue', size=1.2) +
    geom_point()
  
  mods <- dlply(dat, 'variable', function(.df){
    ret <- glm.nb(all ~ value, data=.df)
    ret$data <- .df
    return(ret)
  })
  devs <- llply(mods, mk.prop.dev)
})


##!! xian 2014-11-24
## ive organized everything into lists, above, one per model
## prediction code plotting code wrapped intos functions 
## in mk.helpers.R
## 
## see more notes below on dup code
## block.airtemp.week is big (all obs)
## block.sewtemp.airtemp.week is much smaller, just sewtemp grab weeks

## data to use in model
## xian -- combine all results for given model into named list
## use temp list, assign name at end
## $mod, $dev, $p (for plot), $nobs
## was Overdispersed count data, use negative binomial
#.l$mod <- glm.nb(value ~ MeanTempC, data=.dat)
block.airtemp.list <- within(list(), {
  ## now just use poisson
  ## xian: 2015-11 sorry, my bad, NB is really needed
  ## pois has wildly overoptimistic standard errors
  dat <- block.airtemp.week
  ## clean up factors for facet plotting
  dat$variable <- factor(dat$variable, 
                         levels=c('not.grease', 'grease'),
                         labels=c('Not Grease', 'Grease')
  )
  mod <- glm.nb(value ~ MeanTempC * variable, 
                data=dat
  )
  mod$data <- dat
  dev <- mk.prop.dev(mod)
  .dat.sub <- subset(dat, variable=='Grease')
  mod.grease <- glm.nb(value ~ MeanTempC, data=.dat.sub)
  ## for compat w/glm
  mod.grease$data <- .dat.sub
  dev.grease <- mk.prop.dev(mod.grease)
  .dat.sub <- subset(dat, variable!='Grease')
  mod.nogrease <- glm.nb(value ~ MeanTempC, data=.dat.sub)
  mod.nogrease$data <- .dat.sub
  dev.nogrease <- mk.prop.dev(mod.nogrease)
  nobs <- length(mod$residuals)
  ## number of unique dates post-merge
  nweeks <- length(unique(dat$Date))
  # calculate predicted values and confidence intervals
  #.l$pred <- mk.mod.ci(.df=.dat, .mod=.l$mod)
  # plot
  plot <- ggplot(dat, aes(x=MeanTempC, y=value)) +
    theme_bw() + 
    xlab('Mean Weekly Air Temperature (C)') +
    ylab('Blocks Per Week') + 
    facet_grid(.~variable) + 
    geom_smooth(method = "glm", method.args = list(family = "poisson"), colour='blue', size=1.2) +
    geom_point() 
  ##
  ## thanksgiving residuals
  tday <- mod.grease$data; 
  ## day of year
  tday$yday <- as.POSIXlt(tday$Date)$yday; 
  tday$resid = mod.grease$residuals; 
  tday$Thanksgiving = 'Other'; 
  tday = within(tday, {
    Thanksgiving[yday>=329 & yday <= 341] <- 'Holiday'
  });
  tday.plot <- ggplot(tday, aes(y=resid, x=Thanksgiving)) +
    theme_bw() + 
    ylab('Model Residuals\n(Excess Grease Blocks/week)') +
    xlab('Thanksgiving Weeks') +
    #theme(legend.position=c(0.85, 0.2)) +
    geom_point(position=position_jitter(width=0.2), size=1.3, color="#777777")+
    geom_boxplot(fill=NA) 
})

#### Predict number of blockages using sewer temperature data
.dat <- block.sewtemp.week
## pack list as above
block.sewtemp.list <- within(list(), {
  dat <- block.sewtemp.week
  ## clean up factors for facet plotting
  dat$variable <- factor(dat$variable, 
                         levels=c('not.grease', 'grease'),
                         labels=c('Not Grease', 'Grease')
  )
  mod <- glm.nb(value ~ SewTempC * variable, data=dat)
  sub.grease <- glm.nb(value ~ SewTempC, data=subset(dat, variable=='Grease'))
  sub.nogrease <- glm.nb(value ~ SewTempC, data=subset(dat, variable!='Grease'))
  #summary(nb.block) # highly significant; AIC = 638.37
  nobs <- length(mod$residuals)
  nweeks <- length(unique(dat$Date))
  ## get proportion deviance
  dev <- mk.prop.dev(mod)
  dev.grease <- mk.prop.dev(sub.grease)
  dev.nogrease <- mk.prop.dev(sub.nogrease)
  #pred <- mk.mod.ci(.df=.l$.dat, .mod=.l$mod)
  # plot
  plot <- ggplot(dat, aes(x=SewTempC, y=value)) +
    theme_bw() + 
    xlab('Mean Weekly Sewer Temperature (C)') +
    ylab('Blocks Per Week') + 
    facet_grid(.~variable) + 
    geom_smooth(method = "glm", method.args = list(family = "poisson"), colour='blue', size=1.2) +
    geom_point() 
})
#mk.mod.ci.plot(.l$pred, .x="SewTempC", .xlab="Mean weekly sewage temperature (°C)", .ylab="Number of incidents per week", .theme=theme_bw())
## annotate plot, 
## nudge A/B: >1 goes up/right, <1 goes down/left
#nudge <-  data.frame(x=1.1, y=0.9)
#nudge <-  data.frame(x=1.01, y=0.97)
## pull list to manipulate, avoid confusion
#.l$plot <- .l$plot +
#annotate("text", 
#x=min(.dat$SewTempC)*nudge$x,
#y=max(.dat$value)*nudge$y, 
#label = 'A', size=.annotate.size
#)
#summary(sewer.nb) # very signiificant model
#block.sewtemp <- .l
## 

#block.sewtemp$mod$aic # AIC=811.46 - are these comparable?
#lrtest(block.sewtemp.nb$mod, block.sewtemp.pois$mod) # negbin is a very significant improvement


block.airtemp.pred <- within(list(), {
  ## fit model to before cutoff, predict after
  cutoff = as.Date('2012-04-12')
  ## only look at grease
  .tmpdat <- subset(block.airtemp.week, variable == 'grease')
  dat <- subset(.tmpdat, Date < cutoff)
  pred <- subset(.tmpdat, Date > cutoff)
  mod <- glm.nb(value ~ MeanTempC, data=dat)
  pred$pred <- predict(mod, newdata=pred, type='response')
  plot <- ggplot(pred, aes(x=pred, y=value)) +
    theme_bw() + 
    xlab('Blocks Per Week (Predicted)') +
    ylab('Blocks Per Week (Observed)') + 
    geom_abline(slope=1, intercept=0, linetype='dotted') +
    geom_smooth(method='lm') + 
    geom_point() 
  # linear model of fitted vs observed values
  pred.lm <- lm(value~pred, data=pred)
})


########################################
### FOG data
########################################
# sources another file which loads and cleans this data
#source('run.load_fog.R')
## source in writeup.Rmd

### pack FOG analysis in list as above
block.foglevel.list <- within(list(), {
  dat <- baw.join
  mod.full <- glm.nb(value ~ MeanTempC + meanlfog + variable, data=dat)
  mod.nofog <- glm.nb(value ~ MeanTempC + variable, data=dat)
  n.obs <- nobs(mod.full)
  aic.full <- AIC(mod.full)
  aic.nofog <- AIC(mod.nofog)
  dev.full <- mk.prop.dev(mod.full)
  dev.nofog <- mk.prop.dev(mod.nofog)  
  sub.grease <-  glm.nb(value ~ MeanTempC + meanlfog, 
                        data=subset(dat, variable=='grease'))
  sub.nogrease <-  glm.nb(value ~ MeanTempC + meanlfog, 
                          data=subset(dat, variable!='grease'))
  grey.palette <- colorRampPalette(c('white', 'black'), bias=1, space='Lab')
  plot <- levelplot(block ~ temp * fog, data=gridded.data, 
                    col.regions=grey.palette(100),
                    xlab='Mean Weekly Air Temperature (C)',
                    ylab='Mean Weekly FOG Level (units?)')
})
