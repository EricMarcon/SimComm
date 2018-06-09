#' Community Drift
#'
#' A \code{\link{community_spcmodel}}.
#' At each generation, each individual is replaced by one of its neghbors.
#'
#'
#' @docType class
#' @inherit community_spcmodel
#' @inheritParams community_spcmodel
#' @field n_neighbors The number of nearest neighbors to take into account.
#' @export
cp_drift <- R6Class("cp_drift",
  inherit = community_spcmodel ,
  private = list(
    evolve =  function(time, save) {
      # Find the neighbors
      the_neighbors <- self$neighbors_n(self$n_neighbors)
      # Choose one for each point
      chosen_neighbors <- apply(the_neighbors, 1, sample, size=1)
      # Change the types
      self$pattern$marks$PointType <- self$pattern$marks$PointType[chosen_neighbors]

      if(save) {
        # Save the new pattern
        self$run_patterns[[which(self$timeline == time)]] <- self$pattern
      }
    }
  ),
  public = list (
    n_neighbors = NULL,

    initialize = function(pattern = SpatDiv::rSpCommunity(n=1, size = 100, CheckArguments = FALSE), timeline = 0, type = "Species", n_neighbors = 6) {
      super$initialize(pattern=pattern, timeline=timeline, type=type)
      self$n_neighbors <- n_neighbors
    }
  )
)
