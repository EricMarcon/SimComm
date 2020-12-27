#' Community Drift
#'
#' A \code{\link{community_matrixmodel}} where each cell contains an individual. Marks are species.
#' At each generation, each individual is replaced by one of its neighbors.
#'
#' Edge effects are eliminated by a toroidal correction.
#'
#' @docType class
#' @inherit community_matrixmodel
#' @inheritParams community_matrixmodel
#' @export
cm_drift <- R6Class("cm_drift",
  inherit = community_matrixmodel,
  private = list(
    evolve =  function(time, save) {
      # Prepare the buffer
      self$prepare_buffer()

      # Draw a neighbor
      for(row in seq(nrow(self$pattern))) {
        for(col in seq(ncol(self$pattern))) {
          self$pattern[row, col] <- sample(self$neighbors(row, col), size=1)
        }
      }

      if(save) {
        # Save the new pattern
        self$run_patterns[, , which(self$timeline == time)] <- self$pattern
      }
    }
  )
)




#' Conway's game of life
#'
#' A \code{\link{community_matrixmodel}} where each cell contains or not an individual.
#' At each generation, an individual may survive or not and empty cells be filled by a new individuals.
#'
#' The survival and generation rules are fixed by the number of neighbors of each cell.
#' Edge effects are eliminated by a toroidal correction.
#'
#' @docType class
#' @inherit community_matrixmodel
#' @inheritParams cm_drift
#' @field to_survive The number of neighbors necessary for an individual to survive. Default is \code{2:3}.
#' @field to_generate The number of neighbors necessary for an empty cell to generate an individual. Default is 3.
#' @export
cm_Conway <- R6Class("cm_Conway",
  inherit = community_matrixmodel,
  private = list(
    evolve =  function(time, save) {
      # Prepare the buffer
      self$prepare_buffer()

      # Change cells
      for(row in seq(nrow(self$pattern))) {
        for(col in seq(ncol(self$pattern))) {
          # Count the neighbors
          n_neighbors <- sum(self$neighbors(row, col))
          # Apply the rule
          self$pattern[row, col] <- (self$pattern[row, col] & (n_neighbors %in% self$to_survive)) | (!self$pattern[row, col] & (n_neighbors %in% self$to_generate))
        }
      }

      if(save) {
        # Save the new pattern
        self$run_patterns[, , which(self$timeline == time)] <- self$pattern
      }
    }
  ),
  public = list(
    to_survive = c(2, 3),
    to_generate = 3,

    initialize = function(pattern = pattern_matrix_individuals(), timeline = 0, type = "Alive", neighborhood = "Moore 1") {
      super$initialize(pattern=pattern, timeline=timeline, type=type, neighborhood=neighborhood)
    }
  )
)
