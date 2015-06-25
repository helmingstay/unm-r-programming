########################################
### Combining data / post-processing
########################################
## join to sewer temperatures
#intersect(colnames(weather), colnames(sewtemp)) # both contain 'Date
##?? there are some odd edge effects 
## due to alignment of incomplete weeks leading to a weather-NA
## inner join fixes

.temps <- join(
    subset(sewtemp.week.df, select=-nobs), 
    best.weather, type='full'
)
.blocks <- subset(sewer.block.week, select=c(Date, all))
## keep all blocks, trim weather
block.bothtemp <- join(.blocks, .temps, type='left')
## melt air/sewtemp together, remove nas
## e.g. weekly temp for one but not the other
block.bothtemp <- na.omit(melt(block.bothtemp, id.vars=c('Date','all')))


## join failures with air temp.
## sewer.block.week already sampled for all weeks
## left join limits air temp to sewer block timerange 
block.airtemp.week <- join(
    sewer.block.week.melt, best.weather, type='inner'
)

block.counts <- dlply(block.airtemp.week, 'variable', function(x)
    sum(x$value)
)
grease.ratio <- with(block.counts, grease/(grease+not.grease))

## Inner join - weeks where we have both blockage data and temp measures
block.sewtemp.week <- join(sewer.block.week.melt, sewtemp.week.df, 
    type='inner'
)

