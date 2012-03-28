## this is an RData file
load('eBird_Puerto_Rico_effort_2002_to_2011.RData')

## there's no waaaay I'm typing that over and over!
pr.birds = Puerto_Rico_effort_2002_2011

## with() is just like a local attach -- pull out all the date info
## and convert to a real R-style Date
pr.birds$Date = as.Date(with(pr.birds, {sprintf('%s-%s-%s', Year.Collected,  Month.Collected, Day.Collected)}))
pr.birds$season = factor((pr.birds$Julian.Day %/% 90)%%4, labels=c('Winter','Spring','Summer','Fall'))
myshoreorder = c('Anseriformes', 'Charadriiformes', 'Gruiformes', 'Ciconiiformes', 'Phoenicopteriformes', 'Podicipediformes', 'Procellariiformes', 'Podicipediformes')
pr.birds$is.shore = pr.birds$Order %in% myshoreorder

## number of unique species per day and location
pr.species = ddply(pr.birds, c('Decimal.Longitude', 'Decimal.Latitude', 'Date', 'season', 'is.shore'), 
    function(x) {
        ret = data.frame(nspecies=length(unique(factor(x$Scientific.Name))))
        return(ret)
    }, .progress='text')
