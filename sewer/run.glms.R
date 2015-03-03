.annotate.size = 12
#### Fit models
hist(block.sewtemp.week$value) # histogram of data
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
## xian -- combine all results for given model into named list
## use temp list, assign name at end
## $mod, $dev, $p (for plot), $nobs
## was Overdispersed count data, use negative binomial
#.l$mod <- glm.nb(value ~ MeanTempC, data=.dat)
block.airtemp.list <- within(list(), {
    ## now just use poisson
    dat <- block.airtemp.week
    mod <- glm(value ~ MeanTempC * variable, 
        data=dat, family='poisson'
    )
    dev <- mk.prop.dev(mod)
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
        geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) +
        geom_point() 
})

#mk.mod.ci.plot(.l$pred, .x="MeanTempC", .xlab="Mean weekly air temperature (°C)", .ylab="Number of incidents per week", .theme=theme_bw())
    ## annotate plot, 
    ## nudge A/B
    #.nudge <-  data.frame(x=0.90, y=0.97)
    #.l$plot <- .l$plot +
    #    annotate("text", 
    #        x=min(.dat$MeanTempC)*nudge$x,
    #        y=max(.dat$value)*nudge$y, 
    #        label = 'B', size=.annotate.size
    #    )
    ## assign list a real name
    ## number of sewer blockages by airtemp, negative binom model
    #block.airtemp <- .l
    #summary(sewer.nb) # very signiificant model
    ##

#### Predict number of blockages using sewer temperature data
.dat <- block.sewtemp.week
## pack list as above
block.sewtemp.list <- within(list(), {
    dat <- block.sewtemp.week
    mod <- glm(value ~ SewTempC + variable, data=dat, family='poisson')
    #summary(nb.block) # highly significant; AIC = 638.37
    nobs <- length(mod$residuals)
    nweeks <- length(unique(dat$Date))
    ## get proportion deviance
    dev <- mk.prop.dev(mod)
    #pred <- mk.mod.ci(.df=.l$.dat, .mod=.l$mod)
    # plot
    plot <- ggplot(dat, aes(x=SewTempC, y=value)) +
        theme_bw() + 
        xlab('Mean Weekly Sewer Temperature (C)') +
        ylab('Blocks Per Week') + 
        facet_grid(.~variable) + 
        geom_smooth(method = "glm", family="poisson", colour='blue', size=1.2) +
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


    ## !!xian 2014-11-24
    ## there was a *lot* of repeated code below
    ##
    ## Models of glm(N ~ MeanTempC, data=block.airtemp.week) make sense,
    ## but glm(N ~ MeanTempC, data=***block.sewtemp.week***) is *much* smaller, only makes sense for SewTempC.
    ## e.g. doesn't make sense to model a dataset that we subsetted for another variable
