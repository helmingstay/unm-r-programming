## this is an RData file
load('eBird_Puerto_Rico_effort_2002_to_2011.RData')

## there's no waaaay I'm typing that over and over!
pr.birds = Puerto_Rico_effort_2002_2011

## with() is just like a local attach -- pull out all the date info
## and convert to a real R-style Date
pr.birds$Date = as.Date(with(pr.birds, {sprintf('%s-%s-%s', Year.Collected,  Month.Collected, Day.Collected)}))
pr.birds$season = factor((pr.birds$Julian.Day %/% 90)%%4, labels=c('Winter','Spring','Summer','Fall'))
## shorebirds 
myshoreorder = c('Anseriformes', 'Charadriiformes', 'Gruiformes', 'Ciconiiformes', 'Phoenicopteriformes', 'Podicipediformes', 'Procellariiformes', 'Podicipediformes')

myshorefamily = c('Alcedinidae', 'Anatidae', 'Charadriidae', 'Fregatidae', 'Haematopodidae', 'Laridae', 'Pelecanidae', 'Phaethontidae', 'Phalacrocoracidae', 'Phoenicopteridae', 'Podicipedidae', 'Procellariidae', 'Rallidae', 'Recurvirostridae', 'Scolopacidae', 'Stercorariidae', 'Sulidae', 'Threskiornithidae')

pr.birds$is.shore = factor(pr.birds$Family %in% myshorefamily, labels=c('Not Shorebird Family', 'Shorebird Family'))
## from nat: 
## Some baseline number of some of these families (Anatidae, Laridae, Podicipedidae, Rallidae) should be in shore regularly, along streams and/or in lakes.  But a large increase in them post hurricane would be interesting.  And other families (Fregatidae, Procellaridae, Stercorariidae, Sulidae) should really never be inland unless a hurricane blew them there.
 
## number of unique species per day and location
pr.species.season.shore = ddply(pr.birds, c('Decimal.Longitude', 'Decimal.Latitude', 'Date', 'season', 'is.shore'), 
    function(x) {
        ret = data.frame(nspecies=length(unique(factor(x$Scientific.Name))))
        return(ret)
    }, .progress='text')


pr.species.season = ddply(pr.birds, c('Decimal.Longitude', 'Decimal.Latitude', 'Date', 'season'), 
    function(x) {
        ret = data.frame(nspecies=length(unique(factor(x$Scientific.Name))))
        return(ret)
    }, .progress='text')
