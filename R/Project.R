

nnwhich.community_gridmodel <- function(X, type = "von Neumann") {
  neighbors <- function(point, type) {
    dx <- as.integer(round(abs(X$pattern$x[point] - X$pattern$x)))
    dy <- as.integer(round(abs(X$pattern$y[point] - X$pattern$y)))
    if(type == "von Neumann" | type == "4") the_neighbors <- which(((dx==1L | dx==X$pattern$window$xrange[2]-1L) & (dy==0L)) |
                                                                     (dy==1L | dy==X$pattern$window$yrange[2]-1L) & (dx==0L))
    if(type == "8") the_neighbors <- which((dx<=1L | dx==X$pattern$window$xrange[2]-1L) &
                                             (dy<=1L | dy==X$pattern$window$yrange[2]-1L) & !(dx==0L & dy==0L))
    if(type == "Moore" | type == "24") the_neighbors <- which((dx<=2L | dx>=X$pattern$window$xrange[2]-2L) &
                                                                (dy<=2L | dy>=X$pattern$window$yrange[2]-2L) & !(dx==0L & dy==0L))
    return(the_neighbors)
  }
  return(t(sapply(seq(X$pattern$n), neighbors, type=type)))
}
