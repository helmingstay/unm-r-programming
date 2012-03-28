## make map of pr and airports

## from http://stackoverflow.com/questions/5385713/state-level-unemployment-in-r
library(ggplot2);

## many many libraries need to be installed to do this
## you probably don't want to run
if (T){
  require("rgdal") # requires sp, will use proj.4 if installed
  require("maptools")
  require(gpclib)
  ## need to give permission?
  gpclibPermit()
  ## get shapefile (3-digit zip) from:
  ## http://www2.census.gov/cgi-bin/shapefiles2009/state-files?state=72
  ## extract into folder "shapefiles" for dns arg below
  pr.shp = readOGR(dsn="shapefiles", layer="s_01ja11")
  ## subset doesn't work for special Spatial object
  pr.shp = pr.shp[pr.shp$STATE=='PR',]
  pr.shp@data$id = rownames(pr.shp@data)
  pr.points = fortify.SpatialPolygonsDataFrame(pr.shp, region="id")
  pr.df = join(pr.points, pr.shp@data, by="id")
  save(pr.shp, pr.df, file= 'puerto_rico_shp.RData')
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
                fill='transparent', colour='black', width=2) +
  geom_point( data=pr_weather_summary, aes(lon, lat, colour=ndays), size=8, alpha=0.75) +
  labs(x='', y='', colour = 'Days of\nWeather\nRecords\n2002-2011') +
  ## see theme_get() for a full list of options
  opts(plot.margin=unit(c(-1,0,-1,-0.5), 'lines'), 
    plot.background=theme_rect(fill=NA, colour=NA), 
    axis.text.x=theme_text(colour='black'), 
    axis.text.y=theme_text(colour='black')
  ) + coord_map(project = 'lagrange')

png('FIG-pr_geog.png', width=3000, height=1000, res=250)
  print(pr_plot)
dev.off()
