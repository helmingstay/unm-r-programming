if(T){
## no good, tickmarks too close
    #myticks = seq(from = as.Date("2002-1-1"), by='month', as.Date("2011-12-1"))
tmp = index(pr.rare.weather)
ntick = as.numeric(tail(tmp,1) - head(tmp,1))%/%60
png(file='FIG-rare.bird.wind.png', width=3500, height=700, res=250)
    plot(
        xyplot(pr.rare.weather, alpha=0.75,
            type=c('g','p','h'), screens=1, col=1:2, pch=c(1,4), cex=c(1,1.5),
            scales = list(x = list(tick.number = ntick, rot=60, format='%b %Y')))
            #scales = list(x = list(at = myticks)))
    )
dev.off()
}
