pattern_grid <- function(nx = 8, ny = nx) {
  # Make a regular grid
  the_ppp <- spatstat::rsyst(nx=nx, ny=ny)
  # Adapt coordinates so that they start at .5 and end at nx or ny - .5
  the_ppp$window$xrange <- the_ppp$window$xrange * nx
  the_ppp$window$yrange <- the_ppp$window$yrange * ny
  the_ppp$x <- (the_ppp$x - the_ppp$x[1]) * nx + .5
  the_ppp$y <- (the_ppp$y - the_ppp$y[1]) * ny + .5
  return(the_ppp)
}

pattern_matrix <- function(nx = 8, ny = nx) {
  # Make a matrix
  the_matrix <- matrix(nrow=ny, ncol=nx)
  return(the_matrix)
}

