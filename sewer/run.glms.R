#### Fit models
hist(block.sewtemp.week$N) # histogram of data
# overdispersed count data with a lot of zeros, 
# but no additional zero-generating process - 
# therefore negative binomial GLM is best


##!! xian 2014-11-24
## ive organized everything into lists, above, one per model
## prediction code plotting code wrapped intos functions 
## in mk.helpers.R
## 
## see more notes below on dup code
## block.airtemp.week is big (all obs)
## block.sewtemp.airtemp.week is much smaller, just sewtemp grab weeks

## data to use in model
.dat <- block.airtemp.week
## xian -- combine all results for given model into named list
## use temp list, assign name at end
## $mod, $dev, $p (for plot), $nobs
.l <- list()
## Overdispersed count data, use negative binomial
.l$dat <- .dat
.l$mod <- glm.nb(N ~ MeanTempC, data=.dat)
.l$dev <- mk.prop.dev(.l$mod)
.l$nobs <- length(.l$mod$residuals)
# calculate predicted values and confidence intervals
.l$pred <- mk.mod.ci(.df=.dat, .mod=.l$mod)
# plot
.l$plot <- mk.mod.ci.plot(.l$pred, .x="MeanTempC", .xlab="Mean weekly air temperature (°C)", .ylab="Number of incidents per week")
## annotate plot, 
## nudge A/B
nudge <-  data.frame(x=0.90, y=0.97)
.l$plot <- .l$plot +
    annotate("text", 
        x=min(.dat$MeanTempC)*nudge$x,
        y=max(.dat$N)*nudge$y, 
        label = 'B', size=16
    )
## assign list a real name
## number of sewer blockages by airtemp, negative binom model
block.airtemp.nb <- .l
#summary(sewer.nb) # very signiificant model
##

## check assumptions by comparing to Poisson model
## pack into list as above
.l <- list()
.l$dat <- .dat
.l$mod <- glm(N ~ MeanTempC, data=.dat, family='poisson')
.l$dev <- mk.prop.dev(.l$mod)
.l$nobs <- length(.l$mod$residuals)
.l$pred <- mk.mod.ci(.df=.dat, .mod=.l$mod)
# plot
.l$plot <- mk.mod.ci.plot(.l$pred, .x="MeanTempC", .xlab="Mean weekly air temperature (°C)", .ylab="Number of incidents per week")
block.airtemp.pois <- .l

lrtest.airtemp <- lrtest(block.airtemp.nb$mod, block.airtemp.pois$mod)

#### Predict number of blockages using sewer temperature data
## negative binomial model
.dat <- block.sewtemp.week
## pack list as above
.l <- list()
.l$dat <- .dat
.l$mod <- glm.nb(N ~ SewTempC, data=.dat)
#summary(nb.block) # highly significant; AIC = 638.37
.l$nobs <- length(.l$mod$residuals)
## get proportion deviance
.l$dev <- mk.prop.dev(.l$mod)
.l$pred <- mk.mod.ci(.df=.dat, .mod=.l$mod)
# plot
.l$plot <- mk.mod.ci.plot(.l$pred, .x="SewTempC", .xlab="Mean weekly sewage temperature (°C)", .ylab="Number of incidents per week")
## annotate plot, 
## nudge A/B: >1 goes up/right, <1 goes down/left
#nudge <-  data.frame(x=1.1, y=0.9)
nudge <-  data.frame(x=1.01, y=0.97)
## pull list to manipulate, avoid confusion
.l$plot <- .l$plot +
    annotate("text", 
        x=min(.dat$SewTempC)*nudge$x,
        y=max(.dat$N)*nudge$y, 
        label = 'A', size=16
    )
#summary(sewer.nb) # very signiificant model
block.sewtemp.nb <- .l
## 
## check assumptions by comparing to Poisson model
## use same .dat
.l <- list()
.l$dat <- .dat
.l$mod <- glm(N ~ SewTempC, data=.dat, family='poisson')
# can compare with likelihood ratio test as Poisson model is nested in negbin
.l$nobs <- length(.l$mod$residuals)
## get proportion deviance
.l$dev <- mk.prop.dev(.l$mod)
block.sewtemp.pois <- .l


lrtest.sewtemp <- lrtest(block.sewtemp.nb$mod, block.sewtemp.pois$mod)

block.sewtemp.pois$mod$aic # AIC=811.46 - are these comparable?
lrtest(block.sewtemp.nb$mod, block.sewtemp.pois$mod) # negbin is a very significant improvement


## !!xian 2014-11-24
## there was a *lot* of repeated code below
##
## Models of glm(N ~ MeanTempC, data=block.airtemp.week) make sense,
## but glm(N ~ MeanTempC, data=***block.sewtemp.week***) is *much* smaller, only makes sense for SewTempC.
## e.g. doesn't make sense to model a dataset that we subsetted for another variable


#### Compare the use of weather versus sewer temperature for predicting blockages
hist(block.sewtemp.airtemp.week$N) # also seems zero-inflated, but less so

##??xian this gives error for model w/different nobs
## compare models using sewer and air temperature directly
# Vuong's closeness test for non-nested models - is there a better test?
if(FALSE) vuong(block.sewtemp.nb$mod,block.airtemp.nb$mod) 
#vuong(nb.st, nb.wt) # using sewer temp is not a significant improvement (p > 0.1)


# negbin is a big improvement over standard Poisson
# using actual sewer temperature is at most a modest improvement over air temperature

