#' CommSim
#'
#' Community simulator.
#'
#' This package alows simulating the evolution along time of communities of agents, whose location is decribed by a pattern (a matrix or a point pattern).
#' The communities are represented by R6 classes: \code{\link{community_model}}. A model contains a pattern, a timeline and an evolution method that is run along the timeline.
#' This is a classical approach in community ecoology modeling or agent based models.
#'
#' @name CommSim
#' @docType package
NULL


#' Neighbor points in a grid model
#'
#' @param X A \code{\link{community_model}}.
#' @param neighborhood Definition of the neighborhood. "von Neumann" or "4" means the upper, lower, left and right neighbors are considered; "8" means the 8 nearest neighbors; "Moore" or "24" means the 24 nearest neighbors.
#'
#' @return A matrix. Each line corresponds to a point of the grid (points are ordered): values are the neighbor points.
#' @export
#'
#' @examples
nnwhich.community_gridmodel <- function(X, neighborhood = "von Neumann") {
  neighbors <- function(point, neighborhood) {
    dx <- as.integer(round(abs(X$pattern$x[point] - X$pattern$x)))
    dy <- as.integer(round(abs(X$pattern$y[point] - X$pattern$y)))
    if(neighborhood == "von Neumann" | neighborhood == "4") the_neighbors <- which(((dx==1L | dx==X$pattern$window$xrange[2]-1L) & (dy==0L)) |
                                                                     (dy==1L | dy==X$pattern$window$yrange[2]-1L) & (dx==0L))
    if(neighborhood == "8") the_neighbors <- which((dx<=1L | dx==X$pattern$window$xrange[2]-1L) &
                                             (dy<=1L | dy==X$pattern$window$yrange[2]-1L) & !(dx==0L & dy==0L))
    if(neighborhood == "Moore" | neighborhood == "24") the_neighbors <- which((dx<=2L | dx>=X$pattern$window$xrange[2]-2L) &
                                                                (dy<=2L | dy>=X$pattern$window$yrange[2]-2L) & !(dx==0L & dy==0L))
    return(the_neighbors)
  }
  return(t(sapply(seq(X$pattern$n), neighbors, neighborhood=neighborhood)))
}


#' plot Community Models
#'
#' S3 method to force the use of the R6 method.
#'
#' This is a helper function to always use the R6 method rather than the S3 method: \code{plot(MyModel)} is translated to \code{MyModel$plot}.
#'
#' @param x The model to plot.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return NULL
#' @export
#'
#' @examples
plot.community_model <- function(x, ...) {
  x$plot(...)
}


#' autoplot Community Models
#'
#' S3 method to force the use of the R6 method.
#'
#' This is a helper function to always use the R6 method rather than the S3 method: \code{autoplot(MyModel)} is translated to \code{MyModel$autoplot}.
#'
#' @param object The model to plot.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return NULL
#' @export
#'
#' @examples
autoplot.community_model <- function(object, ...) {
  object$autoplot(...)
}
