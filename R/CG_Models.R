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
      # Prepare the vector of new species
      the_species <- self$pattern$marks$PointType

      for(point in seq(self$pattern$n)) {
        # Find the neighbors of the point
        the_neighbors <- self$neighbors(point)
        # Choose one
        chosen_neighbor <- sample(the_neighbors, size=1)
        # Change the point's mark
        the_species[point] <- self$pattern$marks$PointType[chosen_neighbor]
      }
      # Change the pattern
      self$pattern$marks$PointType <- the_species

      if(save) {
        # Save the new pattern
        self$run_patterns[[which(self$timeline == time)]] <- self$pattern
      }
    }
  )
)
