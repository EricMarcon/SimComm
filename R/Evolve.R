
evolve_none <- function(deltaT) {
  return(self$pattern)
}

evolve_grid_byneighbors <- function(deltaT, type="von Neumann") {
  the_neighborhood <- nnwhich.community_gridmodel(self, type=type)
  self$pattern$marks <- sapply(seq(self$pattern$n), function(point) self$pattern$marks[sample(the_neighborhood[point, ], size=1)])
  return(self$pattern)
}

evolve_matrix_byneighbors <- function(deltaT, type="von Neumann") {
  # Prepare the return matrix
  the_matrix <- self$pattern
  if(type == "von Neumann" | type == "4") {
    # Add the buffer zone
    buffered <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
    buffered <- rbind(buffered[nrow(buffered), ], buffered, buffered[1, ])
    # Draw a neighbor
    for(row in 2:(nrow(buffered)-1)) {
      for(col in 2:(ncol(buffered)-1)) {
        # Find the neighbors
        neighbors <- c(buffered[row-1, col], buffered[row+1, col], buffered[row, col-1], buffered[row, col+1])
        # Draw one
        the_matrix[row-1, col-1] <- sample(neighbors, size=1)
      }
    }
  }

  # Autres types !!!

  # Set the result
  self$pattern <- the_matrix
  return(the_matrix)
}
