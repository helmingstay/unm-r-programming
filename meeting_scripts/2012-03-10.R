mydf <- read.delim('eBird_PR_Data.csv',header=T)
## inspection
dim(mydf)
head(mydf)
## Xian says most important - shows internal structure of the data
str(mydf) 
colnames(mydf)

## further exploring our data
levels(mydf$Observation.Date)
summary(mydf$Effort.Measurement.2)
summary(mydf$Survey.Area.Percent.Covered)
summary(mydf$Effort.Measurement.3)
summary(mydf$Observation.Date)

## example of how factors & levels work
aa <- factor(c('a','a','a','b'), levels=letters)
summary(aa)
## example of how including a factor datapoint 
## that is not one of the levels affects things - makes it NA
ab <- factor(c('a','a','ab','b'), levels=letters)
summary(ab)

## there are countries listed that aren't Puerto Rico,
## so make a subset that just has Puerto Rico in it
PR <- subset(mydf,Country=='Puerto Rico')
## check to see if there are any Puerto Rico observations with country = United States
US <- subset(mydf, Country=="United States")
PR1 <- subset(US, State.Province=="Puerto Rico") #there are not

## Make the US the only country that is a level within the US subset
US$Country <- factor(US$Country)

## Make Puerto Rico the only level within PR "Country"
PR$Country <- factor(PR$Country)

## search for column names that have the word "Effort" in the name
grep('Effort', colnames(PR))

## look at summary of just the columns with effort in them
summary(PR[,grep('Effort', colnames(PR))])

## look at summary of just the columns with effort 
## and protocol in the names that are protocol type P48
summary(subset(PR[,grep('Effort|Protocol',colnames(PR))], Protocol.Code=='P48'))


