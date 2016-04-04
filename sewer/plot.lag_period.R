## deprecated
## source('run.lag_period.R')
#.df <- subset(uber.df, .id=='MeanTempC')
.df <- uber.df
pp <- ggplot(.df, aes(x=factor(lag), y=per, fill=rsq)) + 
    facet_wrap(~.id) + 
    geom_tile() + 
    scale_fill_gradient2(low='blue', mid='grey', high='red', midpoint=median(.df$rsq))

.df1 <- subset(uber.df, .id=='MeanTempC' & lag==0)
pp1 <- ggplot(.df1, aes(x=per, y=rsq)) + 
    geom_point() + 
    geom_smooth(type='lm')

## nice plot of predictive power by lag and observation period
uber.plot <- ggplot(uber.df, aes(x=lag, y=per, fill=var)) +
    facet_wrap( ~.id) + geom_tile() + 
    scale_fill_gradient2(low='blue', mid='grey', high='red', midpoint=0.1);
#pdf('uber.pdf', width=7, height=5); plot(uber.plot); dev.off()
#plot(p)


## binomial...
densityplot( ~ MeanTempC | value, days.l$ret.lag$l1$modlist$dat)
