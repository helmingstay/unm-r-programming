## get the total number of birds observed by name
pr.species.counts = ddply(pr.birds, 'Scientific.Name', function(x) { 
    data.frame(
     sum=sum(pmax(x$Observation.Count, x$Observation.Count..At.Least. , na.rm=T))
    )
})

## quantile to threshold at
myquant = 0.2
## names of birds that are observed with frequency less than threshold
pr.rarebirds = factor(subset(pr.species.counts, sum<quantile(sum, myquant))$Scientific.Name)
## name and date of rare birds
pr.rareobs = subset(pr.birds, Scientific.Name %in% pr.rarebirds)[,c('Scientific.Name','Date')]

## number of rare species at each date
pr.raredates = ddply(pr.rareobs, 'Date', function(x) { 
    return(data.frame(nspecies = nrow(x)))
})

## turn the above into a timeseries object
pr.rareobsx = with(pr.raredates, xts(data.frame(nspecies=nspecies), Date))

## number of rare birds by date + number of wind-event weather stations
pr.rare.weather = merge(pr.max.nsites, pr.rareobsx)

## plot timeseries of by year of rare wind events and rare birds
if(F){
lapply(2002:2011, function(x) {
    myticks = seq(from = as.Date(sprintf("%s-1-1",x)), by='month', length.out=12)
    plot(
        xyplot(pr.rare.weather[as.character(x),], 
            type=c('g','p','h'), screens=1, col=1:2, pch=1:2, cex=1:2,
            scales = list(x = list(at = myticks))
        )); 
    readline(x)
})
}

