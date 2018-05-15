#' Pattern
#'
#' The location of the agents of a model is described by a pattern.
#' Several patterns are available by default.
#' They may be point patterns (of class \code{\link{ppp}}). Regular point patterns are called grids.
#' They may also be matrices with no explicit coordinates by only notions of neighborhood. Matrix models are by far faster to run.
#' Patterns are to be used in the \code{pattern} field of a \code{\link{community_model}}.
#'
#' Available patterns:
#' \describe{
#'   \item{\code{pattern_grid}}{A rectangular, regular grid of points. Each point has marks. Point are on a regular grid of 1x1 units of distance.}
#'   \item{\code{pattern_matrix}}{A matrix. Each cell of the matrix has a numeric value.}
#' }
#'
#'
#' @param nx The number of X values (columns) of the grid or the matrix.
#' @param ny The number of y values (lines) of the grid or the matrix.
#' @name pattern
#' @return \code{evolve} methods should modifiy the model's pattern directly and return \code{NULL}.
NULL


#' @rdname pattern
#'
#' @export
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

#' @rdname pattern
#'
#' @export
pattern_matrix <- function(nx = 8, ny = nx) {
  # Make a matrix
  the_matrix <- matrix(nrow=ny, ncol=nx)
  return(the_matrix)
}
