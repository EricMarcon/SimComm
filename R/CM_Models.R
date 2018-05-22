#' Community Drift
#'
#' A \code{\link{community_matrixmodel}} where each cell contains an individual. Marks are species.
#' At each generation, each individual is replaced by one of its neghbors.
#'
#' Edge effects are eliminated by a toroidal correction.
#'
#' @docType class
#' @inherit community_matrixmodel
#' @inheritParams community_matrixmodel
#' @field neighborhood A character string defining what is the neighborhood of a cell:
#' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
#' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
#' "Moore 2" or "24" for two rings of neighbors.
#' @export
cm_drift <- R6Class("cm_drift",
  inherit = community_matrixmodel,
  private = list(
    evolve =  function(time, save) {
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4") {
        # Add the buffer zone
        buffered <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
        buffered <- rbind(buffered[nrow(buffered), ], buffered, buffered[1, ])
        # Draw a neighbor
        for(row in 2:(nrow(buffered)-1)) {
          for(col in 2:(ncol(buffered)-1)) {
            # Find the neighbors
            neighbors <- c(buffered[row-1, col], buffered[row+1, col], buffered[row, col-1], buffered[row, col+1])
            # Draw one
            self$pattern[row-1, col-1] <- sample(neighbors, size=1)
          }
        }
      }
      if(self$neighborhood == "Moore 1" | self$neighborhood == "8") {
        # Add the buffer zone
        buffered <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
        buffered <- rbind(buffered[nrow(buffered), ], buffered, buffered[1, ])
        # Draw a neighbor
        for(row in 2:(nrow(buffered)-1)) {
          for(col in 2:(ncol(buffered)-1)) {
            # Find the neighbors
            neighbors <- as.vector(buffered[(row-1):(row+1), (col-1):(col+1)])[-5]
            # Draw one
            self$pattern[row-1, col-1] <- sample(neighbors, size=1)
          }
        }
      }
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24") {
        # Add the buffer zone
        buffered <- cbind(self$pattern[, (ncol(self$pattern)-1):ncol(self$pattern)], self$pattern, self$pattern[, 1:2])
        buffered <- rbind(buffered[(nrow(buffered)-1):nrow(buffered), ], buffered, buffered[1:2, ])
        # Draw a neighbor
        for(row in 3:(nrow(buffered)-2)) {
          for(col in 3:(ncol(buffered)-2)) {
            # Find the neighbors
            neighbors <- as.vector(buffered[(row-2):(row+2), (col-2):(col+2)])[-13]
            # Draw one
            self$pattern[row-2, col-2] <- sample(neighbors, size=1)
          }
        }
      }
      if(save) {
        # Save the new pattern
        self$run_patterns[, , which(self$timeline == time)] <- self$pattern
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
#' @field to_survive The number of neighbors necessary for an individual to survive.
#' @field to_generate The number of neighbors necessary for an empty cell to generate an individual.
#' @export
cm_Conway <- R6Class("cm_Conway",
  inherit = community_matrixmodel,
  private = list(
    evolve =  function(time, save) {
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4") {
        # Add the buffer zone
        buffered <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
        buffered <- rbind(buffered[nrow(buffered), ], buffered, buffered[1, ])
        # Draw a neighbor
        for(row in 2:(nrow(buffered)-1)) {
          for(col in 2:(ncol(buffered)-1)) {
            # Count the neighbors
            n_neighbors <- sum(c(buffered[row-1, col], buffered[row+1, col], buffered[row, col-1], buffered[row, col+1]))
            # Apply the rule
            self$pattern[row-1, col-1] <- (self$pattern[row-1, col-1] & (n_neighbors %in% self$to_survive)) | (!self$pattern[row-1, col-1] & (n_neighbors %in% self$to_generate))
          }
        }
      }
      if(self$neighborhood == "Moore 1" | self$neighborhood == "8") {
        # Add the buffer zone
        buffered <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
        buffered <- rbind(buffered[nrow(buffered), ], buffered, buffered[1, ])
        # Draw a neighbor
        for(row in 2:(nrow(buffered)-1)) {
          for(col in 2:(ncol(buffered)-1)) {
            # Count the neighbors
            n_neighbors <- sum((buffered[(row-1):(row+1), (col-1):(col+1)])[-5])
            # Apply the rule
            self$pattern[row-1, col-1] <- (self$pattern[row-1, col-1] & (n_neighbors %in% self$to_survive)) | (!self$pattern[row-1, col-1] & (n_neighbors %in% self$to_generate))
          }
        }
      }
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24") {
        # Add the buffer zone
        buffered <- cbind(self$pattern[, (ncol(self$pattern)-1):ncol(self$pattern)], self$pattern, self$pattern[, 1:2])
        buffered <- rbind(buffered[(nrow(buffered)-1):nrow(buffered), ], buffered, buffered[1:2, ])
        # Draw a neighbor
        for(row in 3:(nrow(buffered)-2)) {
          for(col in 3:(ncol(buffered)-2)) {
            # Count the neighbors
            n_neighbors <- sum((buffered[(row-2):(row+2), (col-2):(col+2)])[-13])
            # Draw one
            self$pattern[row-2, col-2] <- (self$pattern[row-2, col-2] & (n_neighbors %in% self$to_survive)) | (!self$pattern[row-2, col-2] & (n_neighbors %in% self$to_generate))
          }
        }
      }
      if(save) {
        # Save the new pattern
        self$run_patterns[, , which(self$timeline == time)] <- self$pattern
      }
    }
  ),
  public = list(
    neighborhood = NULL,
    to_survive = c(2, 3),
    to_generate = 3,

    initialize = function(pattern = pattern_matrix_logical(), timeline = 0, neighborhood = "Moore 1") {
      super$initialize(pattern=pattern, timeline=timeline)
      if (neighborhood %in% c("von Neumann 1", "4", "Moore 1", "8", "Moore 2", "24")) {
        self$neighborhood <- neighborhood
      } else {
        self$neighborhood <- "Moore 1"
        warning("The neighborhood definition was not recognized: set to the default value.")
      }
      # Colors for plot()
      # self$cols <- grDevices::rainbow(max(pattern))
    }
  )
)
