mk.prop.dev <- function(x) {
    ## takes a glm, returns prop reduction diviance 
    ## see zheng 2000
    D <- 1 - x$deviance/x$null.deviance
    sprintf('%1.3f', D)
}

## alternate versions, From Bolker's lmm page:
## http://glmm.wikidot.com/faq
.pseudo.r.sq1 <- function(m) {
    1-var(residuals(m))/(var(model.response(model.frame(m))))
}
.pseudo.r.sq2 <- function(m) {
   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
   summary(lmfit)$r.squared
}


## bind model predictions into df, add CI
mk.mod.ci <- function(.df, .mod) {
    ## actually used...
    # calculate predicted values and confidence intervals
    .df <- cbind(.df, predict(.mod, type='link', se.fit=T))
    .df <- within(.df, {
      phat <- exp(fit)
      LL <- exp(fit - (1.96 * se.fit))
      UL <- exp(fit + (1.96 * se.fit))
    })
    return(.df)
}

## no native ggplot2 method for plotting negbin models
#' make nice plot of model with CI and model predictions
#' x is string
mk.mod.ci.plot <- function(.df, .x, .xlab, .ylab,
    ## defaults follow
    point.y='N', point.shape=21, 
    ribbon.ymin='LL', ribbon.ymax='UL', ribbon.alpha=0.25, 
    line.y='phat', line.col='blue', 
    .theme=theme_classic()
){
    p <- ggplot(.df, aes_string(x=.x))
    p <- p + geom_point(aes_string(y=point.y), shape=point.shape)
    p <- p + geom_ribbon(aes_string(ymin=ribbon.ymin, ymax=ribbon.ymax), alpha=ribbon.alpha)# confidence bounds
    p <- p + geom_line(aes_string(y=line.y), colour=line.col) + # fitted points
      xlab(.xlab) + ylab(.ylab)
    p <- p + .theme
         #scale_y_sqrt() + #??ytrans
    p <- p + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
    return(p)
}
