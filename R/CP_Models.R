#' Community Drift
#'
#' A [community_spcmodel].
#' At each generation, each individual is replaced by one of its neighbors.
#'
#'
#' @param n_neighbors Number of neighbors.
#' @param pattern The pattern which describes the location of agents.
#' @param time The point of the timeline considered.
#' Its value should be in `timeline`.
#' @param type The type of individuals. Informational only.
#' @param timeline A numeric vector that contains the points of time of interest.
#' @export
cp_drift <- R6::R6Class("cp_drift",
  inherit = community_spcmodel ,
  private = list(
    neighbors = NULL,
    evolve =  function(time, save) {
      # Choose one neighbor for each point
      chosen_neighbors <- apply(private$neighbors, 1, sample, size=1)
      # Change the types
      self$pattern$marks$PointType <- self$pattern$marks$PointType[chosen_neighbors]

      if(save) {
        # Save the new pattern
        self$run_patterns[[which(self$timeline == time)]] <- self$pattern
      }
    }
  ),

  public = list (
    #' @field n_neighbors The number of nearest neighbors to take into account.
    n_neighbors = NULL,

    #' @description
    #' Create a new object.
    initialize = function(
        pattern = SpatDiv::rSpCommunity(n = 1, size = 100, CheckArguments = FALSE),
        timeline = 0,
        type = "Species",
        n_neighbors = 6) {
      super$initialize(pattern = pattern, timeline = timeline, type = type)
      self$n_neighbors <- n_neighbors
      # Store the neighbors
      private$neighbors <- self$neighbors_n(self$n_neighbors)
    }
  )
)
