#' Model Evolution
#'
#' At each time of the \code{timeline}, the \code{evolve} method of a \code{\link{community_model}} is run.
#' It is a function that modifies the \code{pattern} of the model.
#' Several \code{evolve_xxx} functions are available here: they are to be used as \code{evolve} methods.
#'
#' Available functions:
#' \describe{
#'   \item{\code{evolve_none}}{ does nothing.}
#'   \item{\code{evolve_grid_byneighbors}}{Changes the type of each point of the grid by the type of one of its neighbors.}
#' }
#'
#' @param deltaT The time passed since the last step of the evolution of the model.
#' @param neighborhood Definition of the neighborhhod. "von Neumann" or "4" means the upper, lower, left and right neighbors are considered; "8" means the 8 nearest neighbors; "Moore" or "24" means the 24 nearest neighbors.
#' @name evolve
#' @return \code{evolve} methods should modifiy the model's pattern directly and return \code{NULL}.
NULL


#' @rdname evolve
#'
#' @export
evolve_none <- function() {
  return(NULL)
}

#' @rdname evolve
#'
#' @export
evolve_grid_byneighbors <- function(deltaT, neighborhood="von Neumann") {
  the_neighborhood <- nnwhich.community_gridmodel(self, neighborhood=neighborhood)
  self$pattern$marks <- sapply(seq(self$pattern$n), function(point) self$pattern$marks[sample(the_neighborhood[point, ], size=1)])
  return(NULL)
}

#' @rdname evolve
#'
#' @export
evolve_matrix_byneighbors <- function(deltaT, neighborhood="von Neumann") {
  # Prepare the return matrix
  the_matrix <- self$pattern

  if(neighborhood == "von Neumann" | neighborhood == "4") {
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

  if(neighborhood == "8") {
    # Add the buffer zone
    buffered <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
    buffered <- rbind(buffered[nrow(buffered), ], buffered, buffered[1, ])
    # Draw a neighbor
    for(row in 2:(nrow(buffered)-1)) {
      for(col in 2:(ncol(buffered)-1)) {
        # Find the neighbors
        neighbors <- as.vector(buffered[(row-1):(row+1), (col-1):(col+1)])[-5]
        # Draw one
        the_matrix[row-1, col-1] <- sample(neighbors, size=1)
      }
    }
  }

  if(neighborhood == "Moore" | neighborhood == "24") {
    # Add the buffer zone
    buffered <- cbind(self$pattern[, (ncol(self$pattern)-1):ncol(self$pattern)], self$pattern, self$pattern[, 1:2])
    buffered <- rbind(buffered[(nrow(buffered)-1):nrow(buffered), ], buffered, buffered[1:2, ])
    # Draw a neighbor
    for(row in 3:(nrow(buffered)-2)) {
      for(col in 3:(ncol(buffered)-2)) {
        # Find the neighbors
        neighbors <- as.vector(buffered[(row-2):(row+2), (col-2):(col+2)])[-13]
        # Draw one
        the_matrix[row-2, col-2] <- sample(neighbors, size=1)
      }
    }
  }

  # Set the result
  self$pattern <- the_matrix
  return(NULL)
}
