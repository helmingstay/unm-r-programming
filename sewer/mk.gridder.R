# from http://www.stat.cmu.edu/~brian/463/hw02/on-making-contour-plots.r (B Junker)

# I wrote a function "gridder()" that will estimate the z values on a
# regular grid, from any old irregular (x,y,z) triples.  Before we can
# make the contour plot, we have to pass the belly, totlngth and chest
# variable through the"gridder" function.

# here's gridder():

gridder <- function(x, y, z, grid.x, grid.y, h.x, h.y) {
  
  # error checking and filling in default values
  
  n <- length(x)
  stopifnot(length(y)==n, length(z)==n)
  
  if(missing(grid.x)) {
    grid.x <- seq(min(x),max(x),length=n)
  }
  
  if(missing(grid.y)) {
    grid.y <- seq(min(y),max(y),length=n)
  }
  
  if(missing(h.x)) {
    h.x <- 1.06*sd(x)/n^(1/5) # silverman's univariate rule of thumb
  }
  
  if(missing(h.y)) {
    h.y <- 1.06*sd(y)/n^(1/5) # silverman's univariate rule of thumb
  }
  
  # the guts of the function come next; they make a 2-dimensional
  # kernel-smoothed regression using independent gaussian kernels in
  # each coordinate, from which we can estimate values of z above the
  # regular grid:
  
  grid.out <- expand.grid(grid.x,grid.y)  # make a regular grid
  x.out <- grid.out[,1]                   # extract the x values
  y.out <- grid.out[,2]                   # extract the y values
  n.out <- length(x.out)
  z.out <- c(NA,n.out)                    # set up a vector for the z values
  
  for (i in 1:n.out) {                    # fill in the z's using a 2-dim
    xdiffs <- x - x.out[i]                # gaussian kernel regression est
    ydiffs <- y - y.out[i]
    weights <- dnorm(xdiffs,sd=h.x)*dnorm(ydiffs,sd=h.y)
    z.out[i] <- sum(z*weights)/sum(weights)
  }
  
  return(data.frame(x=x.out,y=y.out,z=z.out))
  
}