library(lattice)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(plyr)
library(xtable)
library(lsmeans)
#library(weathermetrics) ## F to C
#library(HH) # for fancy ancova plot
library(lme4) # for glmer - mixed effects (nested interceptors / manholes)
#library(AER)
#library(pscl) # for ZIP models
library(xts)
library(MASS)
#library(stargazer)
## helper functions
library(lubridate)
library(captioner)
library(knitr)
##
.lat.theme <- list(strip.background=list(col="lightgrey"))
##
opts_chunk$set(dev=c('png', 'tiff'),
    message=FALSE,
    #fig.width=6,
    dpi=c(150, 300),
    dev.args=list( 
        png = list(type="cairo-png", antialias='subpixel'),
        tiff = list(compression="lzw")
    )
)
