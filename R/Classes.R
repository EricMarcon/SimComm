#' Community Model Class
#'
#' The model represents a community, i.e. a set of interacting objects called agents. Their location is described by a \code{pattern} that \code{evolve}s along a \code{timeline}.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return An \code{\link{R6Class}}.
#' @format \code{\link{R6Class}} object.
#' @field pattern The pattern which describes the location of agents.
#' @field timeline A \code{\link{timeline}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(pattern = pattern_matrix(), evolve = evolve_none, timeline = timeline_regular())}}{Initialization.}
#'   \item{\code{plot(...)}}{Default plot method: plots the pattern.}
#'   \item{\code{run(animate = FALSE, sleep = 0)}}{Run the model.}
#' }
#' @examples
#' myModel <- community_model$new
community_model <- R6Class("community_model",
  public = list(
    pattern = NULL,
    evolve = NULL,
    timeline = NULL,

    initialize = function(pattern = pattern_grid(), evolve = evolve_none, timeline = timeline_regular()) {
      self$pattern <- pattern
      self$evolve <- function(deltaT, neighborhood) NA
      formals(self$evolve) <- formals(evolve)
      body(self$evolve) <- body(evolve)
      self$timeline <- timeline
    },

    plot = function(...) plot(self$pattern, ...),

    run = function(animate = FALSE, sleep = 0) {
      if(animate) self$plot(main="Initial")

      previous_time <- 0
      for(time in self$timeline$sequence) {
        delta_t <- time-previous_time
        self$evolve(delta_t)
        if(animate) {
          self$plot(main=paste("Time:", time))
          if (sleep > 0) Sys.sleep(sleep)
        }
      }
    }
  )
)


#' Community Grid Model Class
#'
#' A \code{\link{community_model}} whose pattern is a regular, rectangular grid of points.
#' @field tess A tesselization of the window containing the points, used to plot the model.
#' @docType class
#' @inherit community_model
#' @inheritParams community_model
#' @export
community_gridmodel <- R6Class("community_gridmodel",
  inherit = community_model,
  public = list(
    tess = NULL,

    initialize = function(pattern = pattern_grid(), evolve = evolve_none, timeline = timeline_regular()) {
      super$initialize(pattern=pattern, evolve=evolve, timeline=timeline)
      self$tess <- spatstat::dirichlet(self$pattern)
    },


    plot = function(...) {
      marks(self$tess) <- marks(self$pattern)
      plot(self$tess, do.col=TRUE, ...)
    }
  )
)



#' Community Matrix Model Class
#'
#' A \code{\link{community_model}} whose pattern is a regular, rectangular grid of points.
#' @docType class
#' @inherit community_model
#' @inheritParams community_model
#' @export
community_matrixmodel <- R6Class("community_matrixmodel",
  inherit = community_model,
  public = list(
    plot = function(...) {
      image(x=1:ncol(self$pattern), y=1:nrow(self$pattern), t(self$pattern),
            col = rainbow(max(self$pattern)), xlab="", ylab="", axes = FALSE, asp=1, ...)
    }
  )
)
