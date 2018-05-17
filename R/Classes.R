#' Community Model Class
#'
#' The model represents a community, i.e. a set of interacting objects called agents. Their location is described by a \code{pattern} that \code{evolve}s along a \code{timeline}.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return An \code{\link{R6Class}}.
#' @format \code{\link{R6Class}} object.
#' @field pattern The pattern which describes the location of agents.
#' @field timeline A numeric vector.
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(pattern = pattern_matrix(), evolve = evolve_none, timeline = 0)}}{Initialization.}
#'   \item{\code{plot(...)}}{Default plot method: plots the pattern.}
#'   \item{\code{run(animate = FALSE, sleep = 0)}}{Run the model.}
#' }
#' @examples
#' myModel <- community_model$new
community_model <- R6Class("community_model",
  public = list(
    pattern = NULL,
    timeline = NULL,
    last_time = NULL,
    run_patterns = NULL,

    initialize = function(pattern = NULL, timeline = 0) {
      self$pattern <- pattern
      self$timeline <- sort(timeline)
    },

    evolve = function(time, save) {},

    plot = function(..., time=NULL) plot(self$pattern, ...),

    prepare_to_run = function(save, continue) {},

    run = function(animate = FALSE, sleep = 0, save = FALSE, more_time = NULL) {
      if(animate) self$plot(main="Initial")
      # Prepare the time line
      if(is.null(more_time)) {
        # Start from scratch
        self$last_time = self$timeline[1]
      } else {
        # Order
        more_time <- sort(more_time)
        # Eliminate overlap
        if(more_time[1] <= self$last_time) {
          warning(paste("The extended time line tried to start at", more_time[1], "but the last time run is", self$last_time, "- Overlapping time has been ignored."))
          more_time <- more_time[more_time > self$last_time]
        }
        # Extend the time line
        self$timeline <- c(self$timeline, more_time)
      }
      # Prepare the data structure to save evolutions
      self$prepare_to_run(save, more_time)
      # Run the remaining timeline
      for(time in self$timeline[self$timeline > self$last_time]) {
        self$evolve(time, save)
        self$last_time <- time
        if(animate) {
          self$plot(main=paste("Time:", time))
          if (sleep > 0) Sys.sleep(sleep)
        }
      }
    }
  )
)


#' Community Grid Model Class
#'
#' A \code{\link{community_model}} whose pattern is a regular, rectangular grid of points.
#' @field tess A tesselization of the window containing the points, used to plot the model.
#' @docType class
#' @inherit community_model
#' @inheritParams community_model
#' @export
community_gridmodel <- R6Class("community_gridmodel",
  inherit = community_model,
  public = list(
    tess = NULL,

    initialize = function(pattern = pattern_grid(), timeline = 0) {
      super$initialize(pattern=pattern, timeline=timeline)
      self$tess <- spatstat::dirichlet(self$pattern)
    },

    plot = function(...) {
      marks(self$tess) <- marks(self$pattern)
      plot(self$tess, do.col=TRUE, ...)
    }
  )
)



#' Community Matrix Model Class
#'
#' A \code{\link{community_model}} whose pattern is a regular, rectangular grid of points.
#' @docType class
#' @inherit community_model
#' @inheritParams community_model
#' @export
community_matrixmodel <- R6Class("community_matrixmodel",
  inherit = community_model,
  private = list(
    saved_pattern =  function(time) {
      # Find the saved pattern at the chosen time
      if(is.null(time)) {
        the_pattern <- t(self$pattern)
      } else {
        if(is.null(self$run_patterns)) {
          stop("Nothing to plot. Run the model with argument save=TRUE before plotting saved patterns.")
        }
        which_pattern <- which(self$timeline == time)
        if (length(which_pattern) == 0) {
          warning("The time to plot the pattern was not found in the time line.")
          invisible(NULL)
        } else {
          the_pattern <- t(self$run_patterns[, , which_pattern])
        }
      }
      return(the_pattern)
    }
  ),
  public = list(
    neighborhood = NULL,
    cols = NULL,
    palette = "PuOr",

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
    },

    prepare_to_run = function(save, more_time) {
      if(save){
        # Data is a matrix. Save runs in a 3D array.
        if(is.null(more_time) | is.null(self$run_patterns)) {
          # Start from scratch
          self$run_patterns <- array(data=0, dim=c(nrow(self$pattern), ncol(self$pattern), length(self$timeline)))
          # Save the initial pattern
          self$run_patterns[seq(self$pattern)] <- self$pattern
        } else {
          if(length(more_time) > 0) {
            # Check consistency
            if(dim(self$run_patterns)[3] == sum(self$timeline <= self$last_time)) {
              # Save the data
              previous_patterns <- self$run_patterns
              # Extended the pattern
              self$run_patterns <- array(data=0, dim=c(nrow(self$pattern), ncol(self$pattern), length(self$timeline)))
              # Restore the data
              self$run_patterns[seq(previous_patterns)] <- previous_patterns
            } else {
              stop("The time line can not be extended beacause the number of saved patterns does not match the time already run.")
            }
          }
        }
      }
    },

    plot = function(..., time = NULL) {
      graphics::image(x=1:ncol(self$pattern), y=1:nrow(self$pattern), z=private$saved_pattern(time),
            col=self$cols, xlab="", ylab="", axes=FALSE, asp=1, ...)
    },

    autoplot = function(..., time = NULL) {
      the_pattern <- private$saved_pattern(time)
      dimnames(the_pattern) <- list(seq(nrow(the_pattern)), seq(ncol(the_pattern)))
      the_df <- as.data.frame.table(the_pattern)
      names(the_df) <- c("x", "y", "Species")
      the_plot <- ggplot2::ggplot(data=the_df, ggplot2::aes_(x=~x, y=~y)) +
        ggplot2::geom_tile(ggplot2::aes_(fill=~Species), col="black") +
        ggplot2::scale_fill_distiller(palette=self$palette) +
        ggplot2::theme(panel.grid = ggplot2::element_line()) +
        ggplot2::coord_fixed()
      return(the_plot)
    }
  )
)
