## Full Dataset
## 
mydf <- read.delim('eBird_PR_Data.csv',header=T)

## Only Puerto Rico (e.g. remove errors)
PR <- subset(mydf,Country=='Puerto Rico')
PR$Country <- factor(PR$Country)

## Inspect
str(PR)
summary(PR$Year.Collected)
colnames(PR)
## these columns record effort
summary(PR$Protocol.Type)
summary(PR$Protocol.Code)

#Subset to only include records that have effort associated with them
PR_effort <- subset(PR, Protocol.Code!="P20")
summary(PR_effort)

## We want to know # of samples per year.
## Turn year from numeric into factor and then use summary(factor)
## to get this
aa <- factor(PR_effort$Year.Collected)
bb <- summary(aa)

## we want pretty graphs... so use lattice
require(lattice)

## years as strings
levels(aa)
## turn years back to numbers
as.numeric(levels(aa))
## as.numeric of a factor will not do what you probably want
## Important Point.
as.numeric(tail(aa))
## if your factor has numeric levels, 
## you need to make the factor a character first, then make it numeric
as.numeric(as.character(tail(aa)))

## Number of samples per year
plot(as.numeric(levels(aa)),bb)
## plot as a line rather than points
plot(as.numeric(levels(aa)),bb, type='l')
## plot steps
plot(as.numeric(levels(aa)),bb, type='s')
plot(as.numeric(levels(aa)),bb, type='S')
## plot like a barplot
plot(as.numeric(levels(aa)),bb, type='h')

## using lattice rather than standard R graphics
## note that you need to surround these with plot() if you want them to make figures from source()
xyplot(bb~as.numeric(levels(aa)))
xyplot(bb~as.numeric(levels(aa)), type=c('s','p','g'))

## subset data into later years (when more data are available)
PR_effort_2002 <- subset(PR_effort, Year.Collected>=2002)

## drop unused levels
PR_effort_2002 <- droplevels(PR_effort_2002)

summary(PR_effort_2002$Order)

## find number of species in each family
## here we're using summary(factor()) to do the hard work of
## finding number of observations by level, and so it's length is,
## by definition, the number of observed levels.
require(plyr)
xx <- ddply(PR_effort_2002, 'Family', function(x) {
  length(summary(factor(x$Scientific.Name)))
})

## same, except by family and year
yy <- ddply(PR_effort_2002, c('Year.Collected', 'Family'), function(x) {
  length(summary(factor(x$Scientific.Name)))
})

## pretty picture -- number of species by family and year
xyplot(V1~Year.Collected|Family, data=yy)

## plot to file
## this is a lot of data, so we need a lot of pixels!
## set a high res to make R use those pixels wisely.
png('FIG-species_by_family_by_year.png', width=3600, height=2500, res=300)
## lattice often requires an explicit call to plot
plot(
  xyplot(V1~Year.Collected|Family, data=yy, type=c('p','s','l'), 
    cex=0.25)
)  
dev.off()
