library(ggplot2);
library(hexbin);
## geography dataframe
if(F){
    load('puerto_rico_shp.RData') 
}

pr_plot = ggplot(pr.species.season) +
#pr_plot = ggplot(pr.species.season.shore) +
#Year.Collected
  geom_hex( data=pr.species, aes(Decimal.Longitude, Decimal.Latitude), alpha=0.95, binwidth=c(0.12, 0.1)) +
  labs(x='', y='') + #stat_binhex() + geom_hex() +
  ## see theme_get() for a full list of options
  opts(plot.margin=unit(c(-1,0,-1,-0.5), 'lines'), 
    plot.background=theme_rect(fill=NA, colour=NA), 
    axis.text.x=theme_text(colour='black'), 
    axis.text.y=theme_text(colour='black')
  ) + 
  geom_polygon( data=pr.df, aes(long,lat,group=group), 
                fill='transparent', colour='black', width=2) +
  facet_grid(. ~ season )+
  #facet_grid(season ~ is.shore)
  #facet_wrap(~ season, nrow=2, ncol=2, scales='free') +
    coord_map() #project = 'lagrange')

png('FIG-nspec.season.png', width=5000, height=1500, res=250)
print(pr_plot)
dev.off()
