#### Fit models
hist(sewtemp.join$N) # histogram of data
# overdispersed count data with a lot of zeros, 
# but no additional zero-generating process - 
# therefore negative binomial GLM is best

## Overdispersed count data, use negative binomial
sewer.nb <- glm.nb(N ~ MeanTempC, data=sewer.join)
summary(sewer.nb) # very signiificant model
.dev.block.airtemp <- mk.prop.dev(sewer.nb)
##
## check assumptions by comparing to Poisson model
# fit Poisson model
sewer.ps <- glm(N ~ MeanTempC, data=sewer.join, family='poisson')

#### Predict number of blockages using sewer temperature data
## negative binomial model
nb.block <- glm.nb(N ~ SewTempC, data=sewtemp.join)
summary(nb.block) # highly significant; AIC = 638.37
## get proportion deviance
.dev.block.sewtemp <- mk.prop.dev(nb.block)

## check assumptions by comparing to Poisson model
# fit Poisson model
pois.block <- glm(N ~ SewTempC, data=sewtemp.join, family='poisson')
pois.block$aic # AIC=811.46 - are these comparable?
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(nb.block, pois.block) # negbin is a very significant improvement

## get plots out of .R files!!
## plotting - no native ggplot2 method for plotting negbin models
# calculate predicted values and confidence intervals
glm.pred.pois <- cbind(sewtemp.join, predict(nb.block, type='link', se.fit=T))
glm.pred.pois <- within(glm.pred.pois, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(glm.pred.pois, aes(x=SewTempC))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly sewage temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)
pblock <- p
.n.pblock <- length(nb.block$residuals)

#### Compare the use of weather versus sewer temperature for predicting blockages
hist(sewtemp.w.join$N) # also seems zero-inflated, but less so

### fit NB models

## sewer temp
nb.st <- glm.nb(N ~ SewTempC, data=sewtemp.w.join)
summary(nb.st) # highly sig; AIC = 440.26

## check assumptions by comparing to Poisson model
# fit Poisson model
pois.st <- glm(N ~ SewTempC, data=sewtemp.w.join, family='poisson')
pois.st$aic # AIC = 450.93
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(nb.st, pois.st) # negbin is a very significant improvement; 

## weather temperature
nb.wt <- glm.nb(N ~ MeanTempC, data=sewtemp.w.join)
summary(nb.wt) # highly sig; AIC = 443.73

## check assumptions by comparing to Poisson model
# fit Poisson model
pois.wt <- glm(N ~ MeanTempC, data=sewtemp.w.join, family='poisson')
pois.wt$aic # AIC = 456.10
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(nb.wt, pois.wt) # negbin is a very significant improvement; 


## compare models using sewer and air temperature directly
# Vuong's closeness test for non-nested models - is there a better test?
vuong(nb.st, nb.wt) # using sewer temp is not a significant improvement (p > 0.1)


# negbin is a big improvement over standard Poisson
# using actual sewer temperature is at most a modest improvement over air temperature


### plot - no native ggplot2 method for plotting negbin models

## sewer temp
# calculate predicted values and confidence intervals
glm.pred.sewtemp <- cbind(sewtemp.w.join, predict(nb.st, type='link', se.fit=T))
glm.pred.sewtemp <- within(glm.pred.sewtemp, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(glm.pred.sewtemp, aes(x=SewTempC))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly sewer temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)

## weather temp

# calculate predicted values and confidence intervals


glm.pred.airtemp <- cbind(sewtemp.w.join, predict(nb.wt, type='link', se.fit=T))
glm.pred.airtemp <- within(glm.pred.airtemp, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(glm.pred.airtemp, aes(x=MeanTempC))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly air temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
print(p)


## actually used...
# calculate predicted values and confidence intervals
sewblock <- cbind(sewer.join, predict(sewer.nb, type='link', se.fit=T))
sewblock <- within(sewblock, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})

# plot
p <- ggplot(sewblock, aes(x=MeanTempC))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly air temperature (°C)') + ylab('Number of incidents per week')
p <- p + theme_classic() + 
     #scale_y_sqrt() + #??ytrans
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
#print(p)
# use in multi-figure plot
pairblock <- p
pairblock <- pairblock + annotate("text", 
                            x=min(sewblock$MeanTempC),
                            y=max(sewblock$N), label = 'B')
.n.pairblock <- length(sewer.nb$residuals)
