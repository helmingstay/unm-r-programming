## make map of pr and airports

## from http://stackoverflow.com/questions/5385713/state-level-unemployment-in-r
library(ggplot2);

## many many libraries need to be installed to do this
## you probably don't want to run
if (F){
  require("rgdal") # requires sp, will use proj.4 if installed
  require("maptools")
  require(gpclib)
  ## need to give permission?
  gpclibPermit()
  ## get shapefile (3-digit zip) from:
  ## http://www2.census.gov/cgi-bin/shapefiles2009/state-files?state=72
  ## extract into folder "shapefiles" for dns arg below
  pr.shp = readOGR(dsn="shapefiles", layer="tl_2009_72_zcta3")
  pr.shp@data$id = rownames(pr.shp@data)
  pr.points = fortify.SpatialPolygonsDataFrame(pr.shp, region="id")
  pr.df = join(pr.points, pr.shp@data, by="id")
  save(pr.shp, file= 'puerto_rico_shp.RData')
}   else { 
  ## otherwise just load the data file
  load('puerto_rico_shp.RData') 
}
## plot
## uses dataframe from pr_air_data
## or just load the data file
load('puerto_rico_weather.RData')
## map -- outline from shapefile, airports as dots,
## number of days (2002:2011) of records as color
## map projection
pr_plot = ggplot() +
  geom_polygon( data=pr.df, aes(long,lat,group=group), 
                fill='transparent', colour='black') +
  geom_point( data=pr_weather_summary, aes(lon, lat, colour=ndays), size=5) +
  coord_map()

png('FIG-pr_geog.png', width=2500, height=1000, res=250)
  print(pr_plot)
dev.off()