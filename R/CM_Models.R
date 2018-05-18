#' Community Drift
#'
#' A \code{\link{community_model}} whose pattern is a regular, rectangular grid of points.
#' @docType class
#' @inherit community_matrixmodel
#' @inheritParams community_matrixmodel
#' @export
cm_drift <- R6Class("community_matrixmodel",
  inherit = community_matrixmodel,
  public = list(
    neighborhood = NULL,

    initialize = function(pattern = pattern_matrix_individuals(), timeline = 0, neighborhood = "von Neumann") {
      super$initialize(pattern=pattern, timeline=timeline)
      if (neighborhood %in% c("von Neumann", "4", "8", "Moore", "24")) {
        self$neighborhood <- neighborhood
      } else {
        self$neighborhood <- "von Neumann"
        warning("The neighborhood definition was not recognized: set to the default value.")
      }
      # Colors for plot()
      self$cols <- grDevices::rainbow(max(pattern))
    },

    evolve =  function(time, save) {
      if(self$neighborhood == "von Neumann" | self$neighborhood == "4") {
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
      if(self$neighborhood == "8") {
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
      if(self$neighborhood == "Moore" | self$neighborhood == "24") {
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
  )
)
