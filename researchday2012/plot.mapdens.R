library(ggplot2);
library(hexbin);
## geography dataframe
if(F){
    load('puerto_rico_shp.RData') 
}

pr_plot = ggplot() +
#Year.Collected
    coord_map() + #project = 'lagrange')
  geom_hex( data=pr.species, aes(Decimal.Longitude, Decimal.Latitude), alpha=0.95, binwidth=c(0.1, 0.5)) +
  labs(x='', y='') + #stat_binhex() + geom_hex() +
  ## see theme_get() for a full list of options
  opts(plot.margin=unit(c(-1,0,-1,-0.5), 'lines'), 
    plot.background=theme_rect(fill=NA, colour=NA), 
    axis.text.x=theme_text(colour='black'), 
    axis.text.y=theme_text(colour='black')
  ) + 
  geom_polygon( data=pr.df, aes(long,lat,group=group), 
                fill='transparent', colour='black', width=2) 
print(pr_plot)
