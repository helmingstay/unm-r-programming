## define B Junker's function
source('mk.gridder.R') 

## interpolate over 2d grid?
## function is for real data
## note function is very slow!
gridded.data <- gridder(baw.join$MeanTempC, baw.join$meanlfog, baw.join$value)
names(gridded.data) <- c('temp', 'fog', 'block')


.grey.palette <- colorRampPalette(c('white', 'black'), bias=1, space='Lab')
## 2d guassian kernel model predictions
.plot.grid <- levelplot(
    block ~ temp * fog, 
    data=gridded.data, 
    col.regions=.grey.palette(100),
    xlab='Mean Weekly Air Temperature (C)',
    ylab='Mean Weekly FOG Level (units?)'
)
