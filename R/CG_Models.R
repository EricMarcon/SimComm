#' Community Drift, grid version
#'
#' A \code{\link{community_gridmodel}} where each cell contains an individual. Marks are species.
#' At each generation, each individual is replaced by one of its neghbors.
#'
#' @docType class
#' @inherit community_gridmodel
#' @inheritParams community_gridmodel
#' @export
cg_drift <- R6Class("cg_drift",
  inherit = community_gridmodel,
  private = list(
    evolve =  function(time, save) {
      # Find the neighbors of points
      the_neighbors <- sapply(seq(self$pattern$n), function(point) self$neighbors(point))
      # Choose a neighbor for each point
      chosen_neighbors <- apply(the_neighbors, 2,  sample, size=1)
      # Change the marks
      self$pattern$marks$PointType <- sapply(chosen_neighbors, function(point) self$pattern$marks$PointType[point])
      if(save) {
        # Save the new pattern
        self$run_patterns[[which(self$timeline == time)]] <- self$pattern
      }
    }
  )
)
