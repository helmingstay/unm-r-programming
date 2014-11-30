.annotate.size = 12
##
.l <- list()
.l$dat <- .dat <- block.cause.airtemp.week
.form <- formula(value ~ variable*MeanTempC) 
.l$mod <- glm.nb(.form, data=.dat)
.l$mod.pois <- glm(.form, data=.dat, family='poisson')
## no difference 
.l$lrtest <- lrtest(.l$mod, .l$mod.pois)
.l$pred <- mk.mod.ci(.df=.dat, .mod=.l$mod.pois)
.l$dev <- mk.prop.dev(.l$mod)
## model predictions
.pred.grease <- subset(.l$pred, variable == 'grease')
.pred.not.grease <- subset(.l$pred, variable == 'not.grease')
## plots
## grease
.l$plot.grease <- mk.mod.ci.plot(.pred.grease, 
    .theme=theme_bw(),
    .x="MeanTempC", .xlab="Mean weekly air temperature (°C)",
    .ylab="Number of grease-caused incidents per week"
) + annotate("text", x=-5.5, y=max(.dat$N), 
        label = 'A', size=.annotate.size
    ) + ylim(0, max(.dat$N)+0.5) 
## not grease
.l$plot.not.grease <- mk.mod.ci.plot(.pred.not.grease, 
    .theme=theme_bw(),
    .x="MeanTempC", .xlab="Mean weekly air temperature (°C)",
    .ylab="Number of incidents per week not caused by grease"
) + annotate("text", x=-5.5, y=max(.dat$N), 
        label = 'B', size=.annotate.size
    ) + ylim(0, max(.dat$N)+0.5)
# set both plots with equal y axes
grease.fullmod <- .l

## pack list
## pull out is/isnt grease for inspection
##
.l <- list()
.l$data <- subset(block.cause.airtemp.week, variable=='grease')
##
# number of grease incidents
.l$sum <- sum(.l$data$N)
#sewer.join.grease <- na.omit(sewer.join.grease)
.l$nobs <- nrow(.l$data)
grease <- .l

## as above for not grease
.l <- list()
.l$data <- subset(block.cause.airtemp.week, variable=='not.grease')
## NAs in data??
##
## Weeks with no problems 
##
# number of non-grease incidents
.l$sum <- sum(.l$data$N)
## model
.l$nobs <- nrow(.l$data)
not.grease <- .l

## Temperature does not significantly predict non-grease blockages!

# proportion that are greasey
grease.ratio <- grease$sum/(grease$sum + not.grease$sum)
