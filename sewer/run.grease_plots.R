## Number of problems for each code and week
.grease <- subset(sewer, grepl('GR', CAUSE))
head(.grease)
sewer.fail.grease = ddply( .grease, c('year', 'week'),
    function(x) { 
      data.frame(N=nrow(x))  } 
    ## show progress
    #.progress='text'
)
##
sewer.join.grease <- join(sewer.fail.grease, temp.week.df, type='full')
##
## Weeks with no problems 
sewer.join.grease$N[is.na(sewer.join.grease$N)] <- 0
# number of grease incidents
sum(sewer.join.grease$N)
sewer.join.grease <- na.omit(sewer.join.grease)
##
grease.nb <- glm.nb(N ~ MeanTempC, data=sewer.join.grease)
summary(grease.nb)
##
# pseudo R^2
#pr2.grease <- with(grease.nb, (null.deviance-deviance)/null.deviance) 
#paste('Pseudo $R^2$ = ', signif(pr2.grease, 2))
##
## check assumptions by comparing to Poisson model
# fit Poisson model
grease.ps <- glm(N ~ MeanTempC, data=sewer.join.grease)
summary(grease.ps)
# can compare with likelihood ratio test as Poisson model is nested in negbin
lrtest(grease.nb, grease.ps) # negbin is a very significant improvement; 


# number of problems that weren't caused by grease
notgrease <- subset(sewer, !grepl('GR', sewer$CAUSE))
sewer.fail.notgrease = ddply(notgrease, c('year', 'week'),
    function(x) { 
      data.frame(N=nrow(x))  } 
    ## show progress
    #.progress='text'
)
##
sewer.join.notgrease <- join(sewer.fail.notgrease, temp.week.df, type='full')
##
## Weeks with no problems 
sewer.join.notgrease$N[is.na(sewer.join.notgrease$N)] <- 0
## Why are there remaining NAs in Mean.TemperatureF?
sewer.join.notgrease <- na.omit(sewer.join.notgrease)
##
# number of non-grease incidents
sum(sewer.join.notgrease$N)
# proportion that are greasey
sum(sewer.join.grease$N) / ( sum(sewer.join.grease$N) + sum(sewer.join.notgrease$N))
##
## model
notgrease.nb <- glm.nb(N ~ MeanTempC, data=sewer.join.notgrease)
pr2.notgrease<-with(notgrease.nb, (null.deviance-deviance)/null.deviance) 
summary(notgrease.nb) # only just significant
## Temperature does not significantly predict non-grease blockages!

# calculate predicted values and confidence intervals
greaseblock <- cbind(sewer.join.grease, predict(grease.nb, type='link', se.fit=T))
greaseblock <- within(greaseblock, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})
##
# plot
p <- ggplot(greaseblock, aes(x=MeanTempC))
p <- p + geom_point(aes(y=N), shape=21)
p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
p <- p + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly air temperature (°C)') + ylab('Number of grease-caused incidents per week')
p <- p + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p <- p + annotate("text", x=-5.5, y=max(greaseblock$N)+1, label = 'A')
p <- p + ylim(0, max(greaseblock$N)+1) # set both plots with equal y axes
#print(p) 
.p.grease <- p
.n.grease <- length(grease.nb$residuals)

# calculate predicted values and confidence intervals
notgreaseblock <- cbind(sewer.join.notgrease, predict(notgrease.nb, type='link', se.fit=T))
notgreaseblock <- within(notgreaseblock, {
  phat <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})
##
# make plot
r <- ggplot(notgreaseblock, aes(x=MeanTempC))
r <- r + geom_point(aes(y=N), shape=21)
r <- r + geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.25)# confidence bounds
r <- r + geom_line(aes(y=phat), colour='blue') + # fitted points
  xlab('Mean weekly air temperature (°C)') + ylab('Weekly number of incidents not caused by grease')
r <- r + theme_classic() + 
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
r <- r + ylim(0, max(greaseblock$N + 1)) # set both plots with equal y axes
r <- r + annotate("text", x=-5.5, y=max(greaseblock$N)+1, label = 'B')
#print(r)
.p.notgrease <- r
## number of observations in model
.n.notgrease <- length(notgrease.nb$residuals)

# check that prediction is different for same doy in different years
#bb <- ddply(curlpred, .(doy), summary) 
