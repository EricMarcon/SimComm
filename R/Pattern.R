#' Pattern
#'
#' @description
#' The location of the agents of a model is described by a pattern.
#'
#' @details
#' Several patterns are available by default.
#' They may be point patterns (of class [spatstat.geom::ppp]).
#' Regular point patterns are called grids.
#' They may also be matrices with no explicit coordinates by only notions of neighborhood.
#' Matrix models are by far faster to run.
#' Point patterns are provided by [SpatDiv::rSpCommunity].
#' Patterns are to be used in the `pattern` field of a [community_model].
#'
#' Available patterns are:
#'
#'   - `pattern_grid` A rectangular, regular grid of points.
#'      Each point has marks.
#'      Point are on a regular grid of 1x1 units of distance.
#'   - `pattern_matrix_individuals` A matrix.
#'      Each cell of the matrix is an agent and has a numeric value equal to its type.
#'   - `pattern_matrix_logical` A matrix.
#'      Each cell of the matrix is `FALSE` or `TRUE` (dead or alive, present or absent...).
#'   - `pm_Conway_blinker` A logical matrix for [Conway's game of life](https://en.wikipedia.org/wiki/Conway\\%27s_Game_of_Life#Examples_of_patterns).
#'      Three cells are alive.
#'      The pattern is designed to oscillate.
#'   - `pm_Conway_glider` A logical matrix for [Conway's game of life](https://en.wikipedia.org/wiki/Conway\\%27s_Game_of_Life#Examples_of_patterns).
#'      The pattern is designed to move.
#'
#'
#' @param nx The number of X values (columns) of the grid or the matrix.
#' @param ny The number of Y values (lines) of the grid or the matrix.
#' @param S The number of species.
#' @param Distribution The distribution of species frequencies. May be "lnorm" (log-normal), "lseries" (log-series), "geom" (geometric) or "bstick" (broken stick).
#' @param sd The simulated distribution standard deviation. For the log-normal distribution, this is the standard deviation on the log scale.
#' @param prob The proportion of ressources taken by successive species in the geometric model.
#' @param alpha Fisher's alpha in the log-series model.
#' @name pattern
NULL



#' @rdname pattern
#'
#' @export
pattern_grid <- function(
    nx = 8,
    ny = nx,
    S = 300,
    Distribution = "lnorm",
    sd = 1,
    prob = 0.1,
    alpha = 40) {
  # Make a regular grid
  the_ppp <- spatstat.geom::rsyst(nx=nx, ny=ny)
  # Adapt coordinates so that they start at .5 and end at nx or ny - .5
  the_ppp$window$xrange <- the_ppp$window$xrange * nx
  the_ppp$window$yrange <- the_ppp$window$yrange * ny
  the_ppp$x <- (the_ppp$x - the_ppp$x[1]) * nx + .5
  the_ppp$y <- (the_ppp$y - the_ppp$y[1]) * ny + .5
  # Draw a random community
  the_community <-  entropart::rCommunity(
    1,
    size = 100 * nx * ny,
    S = S,
    Distribution = Distribution,
    sd = sd,
    prob = prob,
    alpha = alpha,
    CheckArguments=FALSE
  )
  # Names are sp#
  spNames<- paste0("sp", seq(length(the_community)))
  # Make marks: a dataframe with 2 columns: PointType and PointWeight, defining a wmppp from package dbmss.
  the_ppp$marks <- data.frame(
    PointType = sample(spNames, size = nx * ny, replace = TRUE, prob = the_community / sum(the_community)),
    PointWeight = 1
  )
  # The class of pattern_grid objects is ppp, defined in spatstat. Make it a wmppp for package dbmss.
  class(the_ppp) <- c("wmppp", class(the_ppp))
  return(the_ppp)
}


#' @rdname pattern
#'
#' @export
pattern_matrix_individuals <- function(
    nx = 8,
    ny = nx,
    S = 300,
    Distribution = "lnorm",
    sd = 1,
    prob = 0.1,
    alpha = 40) {
  # Draw a random community
  the_community <-  entropart::rCommunity(
    1,
    size = 100 * nx * ny,
    S = S,
    Distribution = Distribution,
    sd = sd,
    prob = prob,
    alpha = alpha,
    CheckArguments = FALSE
  )
  # Names are numbers
  spNames<- seq(length(the_community))
  # Make a matrix
  the_matrix <- matrix(
    sample(spNames, size = nx * ny, replace = TRUE, prob = the_community / sum(the_community)),
    nrow=ny,
    ncol=nx
  )
  # Class
  class(the_matrix) <- c("pattern_matrix_individuals", class(the_matrix))
  return(the_matrix)
}


#' @rdname pattern
#'
#' @export
pattern_matrix_logical <- function(
    nx = 8,
    ny = nx,
    prob = 0.5) {
  # Draw the presence/absence of individuals. Make a matrix
  the_matrix <- matrix(
    stats::rbinom(nx * ny, size = 1, prob = prob) == 1,
    nrow = ny,
    ncol = nx
  )
  # Class
  class(the_matrix) <- c("pattern_matrix_logical", class(the_matrix))
  return(the_matrix)
}

#' @rdname pattern
#'
#' @export
pm_Conway_blinker <- function() {
  the_matrix <- matrix(FALSE, nrow = 5, ncol = 5)
  the_matrix[3, 2:4] <- TRUE
  # Class
  class(the_matrix) <- c("pattern_matrix_logical", class(the_matrix))
  return(the_matrix)
}


#' @rdname pattern
#'
#' @export
pm_Conway_glider <- function(nx = 50, ny = nx) {
  # At least 5x5
  nx <- max(nx, 5)
  ny <- max(ny, 5)
  the_matrix <- matrix(FALSE, nrow = ny, ncol = nx)
  the_matrix[2, 2:4] <- TRUE
  the_matrix[3, 4] <- the_matrix[4, 3] <- TRUE
  # Class
  class(the_matrix) <- c("pattern_matrix_logical", class(the_matrix))
  return(the_matrix)
}
