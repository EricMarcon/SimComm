#' Patterns
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
#'   \item{\code{pattern_matrix_individuals}}{A matrix. Each cell of the matrix is an agent and has a numeric value equal to its type.}
#'   \item{\code{pattern_matrix_logical}}{A matrix. Each cell of the matrix is \code{FALSE} or \code{TRUE} (dead or alive, present or absent...).}
#'   \item{\code{pm_Conway_blinker}}{A logical matrix for \href{https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Examples_of_patterns}{Conway's game of life}. Three cells are alive. The pattern is designed to oscillate.}
#'   \item{\code{pm_Conway_glider}}{A logical matrix for \href{https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Examples_of_patterns}{Conway's game of life}. The pattern is designed to move.}
#' }
#'
#'
#' @param nx The number of X values (columns) of the grid or the matrix.
#' @param ny The number of y values (lines) of the grid or the matrix.
#' @param S The number of species.
#' @param Distribution The distribution of species frequencies. May be \code{"lnorm"} (log-normal), \code{"lseries"} (log-series), \code{"geom"} (geometric) or \code{"bstick"} (broken stick).
#' @param sd The simulated distribution standard deviation. For the log-normal distribution, this is the standard deviation on the log scale.
#' @param prob The proportion of ressources taken by successive species in the geometric model.
#' @param alpha Fisher's alpha in the log-series model.
#' @name patterns
#' @return \code{evolve} methods should modifiy the model's pattern directly and return \code{NULL}.
NULL


#' @rdname patterns
#'
#' @export
pattern_grid <- function(nx = 8, ny = nx, S = 300, Distribution = "lnorm",  sd = 1, prob = 0.1, alpha = 40) {
  # Make a regular grid
  the_ppp <- spatstat::rsyst(nx=nx, ny=ny)
  # Adapt coordinates so that they start at .5 and end at nx or ny - .5
  the_ppp$window$xrange <- the_ppp$window$xrange * nx
  the_ppp$window$yrange <- the_ppp$window$yrange * ny
  the_ppp$x <- (the_ppp$x - the_ppp$x[1]) * nx + .5
  the_ppp$y <- (the_ppp$y - the_ppp$y[1]) * ny + .5
  # Draw a random community
  the_community <-  entropart::rCommunity(1, size=100*nx*ny, S=S, Distribution=Distribution, sd=sd, prob=prob, alpha=alpha, CheckArguments=FALSE)
  # Names are sp#
  spNames<- paste0("sp", seq(length(the_community)))
  # Make marks: a dataframe with 2 columns: PointType and PointWeight, defining a wmppp from package dbmss.
  the_ppp$marks <- data.frame(PointType=sample(spNames, size=nx*ny, replace=TRUE, prob=the_community/sum(the_community)), PointWeight=1)
  # The class of pattern_grid obbjects is ppp, defined in spatstat. Make it a wmppp for package dbmss.
  class(the_ppp) <- c("wmppp", class(the_ppp))
  return(the_ppp)
}



#' @rdname patterns
#'
#' @export
pattern_matrix_individuals <- function(nx = 8, ny = nx, S = 300, Distribution = "lnorm",  sd = 1, prob = 0.1, alpha = 40) {
  # Draw a random community
  the_community <-  entropart::rCommunity(1, size=100*nx*ny, S=S, Distribution=Distribution, sd=sd, prob=prob, alpha=alpha, CheckArguments=FALSE)
  # Names are numbers
  spNames<- seq(length(the_community))
  # Make a matrix
  the_matrix <- matrix(sample(spNames, size=nx*ny, replace=TRUE, prob=the_community/sum(the_community)), nrow=ny, ncol=nx)
  # Class
  class(the_matrix) <- c("pattern_matrix_individuals", class(the_matrix))
  return(the_matrix)
}



#' @rdname patterns
#'
#' @export
pattern_matrix_logical <- function(nx = 8, ny = nx, prob = 0.5) {
  # Draw the presence/absence of individuals. Make a matrix
  the_matrix <- matrix(stats::rbinom(nx*ny, size=1, prob=prob) == 1, nrow=ny, ncol=nx)
  # Class
  class(the_matrix) <- c("pattern_matrix_logical", class(the_matrix))
  return(the_matrix)
}

#' @rdname patterns
#'
#' @export
pm_Conway_blinker <- function() {
  the_matrix <- matrix(FALSE, nrow=5, ncol=5)
  the_matrix[3, 2:4] <- TRUE
  # Class
  class(the_matrix) <- c("pattern_matrix_logical", class(the_matrix))
  return(the_matrix)
}


#' @rdname patterns
#'
#' @export
pm_Conway_glider <- function(nx = 50, ny = nx) {
  # At least 5x5
  nx <- max(nx, 5)
  ny <- max(ny, 5)
  the_matrix <- matrix(FALSE, nrow=ny, ncol=nx)
  the_matrix[2, 2:4] <- TRUE
  the_matrix[3, 4] <- the_matrix[4, 3] <- TRUE
  # Class
  class(the_matrix) <- c("pattern_matrix_logical", class(the_matrix))
  return(the_matrix)
}
