PR_data=read.delim("Puerto_Rico_eBird_AVN_Data.csv",header=T)
## dimensions row,col
dim(PR_data)
str(PR_data)
summary(PR_data)


## summary() of a factor shows the number of observations for each factor,
## which can be very helpful!
summary(PR_data$Protocol.Type)
summary(subset(PR_data,Protocol.Type=="eBird Casual Observation"))
summary(subset(PR_data,Protocol.Type==levels(Protocol.Type)[1]))
summary(factor(subset(PR_data,Protocol.Type==levels(Protocol.Type)[1])$Duration))
summary(factor(subset(PR_data,Protocol.Type==levels(Protocol.Type)[1])$Duration))
## definite increase over time!
## Matt says eBird is released in 2002, and becomes widely publicized in 2005-6
summary(factor(PR_data$Year))
## More during the winter
summary(factor(PR_data$Month))
## How many species are represented?
summary(factor(PR_data$Common.Name))
levels(factor(PR_data$Common.Name))
levels(factor(PR_data$Scientific.Name))
levels(factor(PR_data$Genus))
levels(factor(PR_data$Common.Name))

