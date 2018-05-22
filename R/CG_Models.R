#' Community Drift, grid version
#'
#' A \code{\link{community_gridmodel}} where each cell contains an individual. Marks are species.
#' At each generation, each individual is replaced by one of its neghbors.
#'
#' @docType class
#' @inherit community_gridmodel
#' @inheritParams community_gridmodel
#' @field neighborhood A character string defining what is the neighborhood of a cell:
#' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
#' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
#' "Moore 2" or "24" for two rings of neighbors.
#' @export
cg_drift <- R6Class("cg_drift",
  inherit = community_gridmodel,
  private = list(
    neighbors = function(point) {
      dx <- as.integer(round(abs(self$pattern$x[point] - self$pattern$x)))
      dy <- as.integer(round(abs(self$pattern$y[point] - self$pattern$y)))
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4")
        the_neighbors <- which(((dx==1L | dx==self$pattern$window$xrange[2]-1L) & (dy==0L)) |
                                 (dy==1L | dy==self$pattern$window$yrange[2]-1L) & (dx==0L))
      if(self$neighborhood == "Moore 1" | self$neighborhood == "8")
        the_neighbors <- which((dx<=1L | dx==self$pattern$window$xrange[2]-1L) &
                                 (dy<=1L | dy==self$pattern$window$yrange[2]-1L) & !(dx==0L & dy==0L))
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24")
        the_neighbors <- which((dx<=2L | dx>=self$pattern$window$xrange[2]-2L) &
                                 (dy<=2L | dy>=self$pattern$window$yrange[2]-2L) & !(dx==0L & dy==0L))
      return(the_neighbors)
    },

    evolve =  function(time, save) {
      # Find the neighbors of points
      the_neighbors <- sapply(seq(self$pattern$n), private$neighbors)
      # Replace each point by a neighbor
      self$pattern$marks$PointType <- apply(the_neighbors, 2,  sample, size=1)
      if(save) {
        # Save the new pattern
        self$run_patterns[[which(self$timeline == time)]] <- self$pattern
      }
    }
  ),
  public = list(
    neighborhood = NULL,
    # palette = "PuOr",

    initialize = function(pattern = pattern_matrix_individuals(), timeline = 0, neighborhood = "von Neumann 1") {
      super$initialize(pattern=pattern, timeline=timeline)
      if (neighborhood %in% c("von Neumann 1", "4", "Moore 1", "8", "Moore 2", "24")) {
        self$neighborhood <- neighborhood
      } else {
        self$neighborhood <- "von Neumann 1"
        warning("The neighborhood definition was not recognized: set to the default value.")
      }
      # Colors for plot()
      # self$cols <- grDevices::rainbow(max(pattern))
    }
  )
)
