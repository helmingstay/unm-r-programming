## pack list
## pull out is/isnt grease for inspection
##
.l <- list()
.l$data <- subset(block.cause.airtemp.week, is.grease)
##
# number of grease incidents
.l$sum <- sum(.l$data$N)
#sewer.join.grease <- na.omit(sewer.join.grease)
##
.l$mod <- glm.nb(N ~ MeanTempC, data=.l$data)
.l$dev <- mk.prop.dev(.l$mod)
#summary(grease.nb)
##
# pseudo R^2
#pr2.grease <- with(grease.nb, (null.deviance-deviance)/null.deviance) 
#paste('Pseudo $R^2$ = ', signif(pr2.grease, 2))
##
## check assumptions by comparing to Poisson model
# fit Poisson model
.l$mod.pois <- glm(N ~ MeanTempC, data=.l$data)
# can compare with likelihood ratio test as Poisson model is nested in negbin
 .l$lrtest <- lrtest(.l$mod, .l$mod.pois) # negbin is a very significant improvement; 
## save list as named obj
# calculate predicted values and confidence intervals
.l$pred <- mk.mod.ci(.df=.l$data, .mod=.l$mod)
.l$plot <- mk.mod.ci.plot(.l$pred, .x="MeanTempC", .xlab="Mean weekly air temperature (°C)", .ylab="Number of grease-caused incidents per week")
.l$plot <- .l$plot +
    annotate("text", x=-5.5, y=max(.l$data$N)+1, 
        label = 'A', size=16
    ) + 
    ylim(0, max(.l$data$N)+1) # set both plots with equal y axes
#print(p) 
.l$nobs <- length(.l$mod$residuals)
grease <- .l

## as above for not grease
.l <- list()
.l$data  <- subset(block.cause.airtemp.week, !is.grease)
## NAs in data??
##
## Weeks with no problems 
##
# number of non-grease incidents
.l$sum <- sum(.l$data$N)
## model
##!! nb doesn't converge
#.l$mod <- glm.nb(N ~ MeanTempC, data=.l$data)
.l$mod.pois <- .l$mod <- glm(N ~ MeanTempC, data=.l$data, family='poisson')
# fit Poisson model
#.l$mod.pois <- glm(N ~ MeanTempC, data=.l$data, family='poisson')
.l$dev <- mk.prop.dev(.l$mod)
# can compare with likelihood ratio test as Poisson model is nested in negbin
#.l$lrtest <- lrtest(.l$mod, .l$mod.pois) # negbin is still a very significant improvement;
# ie neither model is much good
#AIC(notgrease.nb); AIC(notgrease.ps) # negbin far superior by AIC
# calculate predicted values and confidence intervals
.l$pred <- mk.mod.ci(.df=.l$data, .mod=.l$mod)
.l$plot <- mk.mod.ci.plot(.l$pred, .x="MeanTempC", .xlab="Mean weekly air temperature (°C)", .ylab="Number of incidents per week not caused by grease")
.l$plot <- .l$plot +
    annotate("text", x=-5.5, y=max(grease$data$N)+1, 
        label = 'B', size=16
    ) + 
    ylim(0, max(grease$data$N)+1) # set both plots with equal y axes
#print(p) 
.l$nobs <- length(.l$mod$residuals)
not.grease <- .l

## Temperature does not significantly predict non-grease blockages!

# proportion that are greasey
grease.ratio <- grease$sum/(grease$sum + not.grease$sum)


# check that prediction is different for same doy in different years
#bb <- ddply(curlpred, .(doy), summary) 
