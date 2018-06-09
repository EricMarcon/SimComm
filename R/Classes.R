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
#' @field type The type of individuals. Informational only.
#' @field timeline A numeric vector.
#' @field last_time The last time (in the time line) the model has been run.
#' @field run_patterns The past patterns of the model, obtained by \code{run} and saved.
#' @field neighborhood A character string defining what is the neighborhood of a cell:
#' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
#' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
#' "Moore 2" or "24" for two rings of neighbors.
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(pattern = NULL, timeline = 0, type = "Species")}}{Initialization.}
#'   \item{\code{plot(time=NULL, sleep=animation::ani.options("interval"), ...)}}{Default plot method: plots the pattern.}
#'   \item{\code{autoplot(time=NULL, ...)}}{Makes a \code{\link{ggplot}} of the pattern.}
#'   \item{\code{run(animate = FALSE, sleep = animation::ani.options("interval"), save = FALSE, more_time = NULL)}}{Run the model.}
#'   \item{\code{saved_pattern(time)}}{Returns the pattern at the chosen time.}
#'   \item{\code{along_time(FUN, ...)}}{Applies the function FUN to the saved patterns along time and returns a dataframe with columns \code{x} for time and \code{y} for the results of FUN. FUN must return a single value.}
#' }
#' @examples
#' myModel <- community_matrixmodel$new()
#' myModel$autoplot()
community_model <- R6Class("community_model",
  private = list(
    pattern_class = NULL,
    pattern_by_index = function(i) {
      # Base class method to be overriden. Assumes that run_patterns is a list.
      return(self$run_patterns[[i]])
    },

    prepare_to_save = function(save, more_time) {
      # Code to prepare private$run_patterns to store the succesive patterns. Assumes that run_patterns is a list, to be overridden.
      if(save){
        if(is.null(more_time) | is.null(self$run_patterns)) {
          # Start from scratch
          self$run_patterns <- vector("list", length(self$timeline))
          # Save the initial pattern
          self$run_patterns[[1]] <- self$pattern
        } else {
          if(length(more_time) > 0) {
            # Check consistency
            if(length(self$run_patterns) == sum(self$timeline <= self$last_time)) {
              # Extended the pattern
              self$run_patterns <- c(self$run_patterns, vector("list", sum(self$timeline > self$last_time)))
            } else {
              stop("The time line can not be extended beacause the number of saved patterns does not match the time already run.")
            }
          }
        }
      }
    },

    evolve = function(time, save) {
      # Code to modify self$pattern first.
      ## Here
      # Last step: save the pattern.
      if(save) {
        # Save the new pattern
        self$run_patterns[[which(self$timeline == time)]] <- self$pattern
      }
    }
  ),
  public = list(
    pattern = NULL,
    type = NULL,
    timeline = NULL,
    last_time = NULL,
    run_patterns = NULL,

    initialize = function(pattern = NULL, timeline = 0, type = "Species") {
      self$pattern <- pattern
      private$pattern_class <- class(pattern)
      self$timeline <- sort(timeline)
      self$type <- type
    },

    # plot method for the pattern. To be overridden.
    plot = function(time=NULL, sleep=animation::ani.options("interval"), ...) {
      # if sleep > 0, use the animation package for a fluid sequence of images
      if (sleep>0) grDevices::dev.hold()
      # Specific plot code to be overridden
      plot(self$saved_pattern(time), ...)
      # Animation
      if (sleep>0) animation::ani.pause(sleep)
    },

    # ggplot method for the pattern. To be overridden.
    autoplot = function(time=NULL, ...) {
      # get the pattern to plot
      the_pattern <- self$saved_pattern(time)
      # Prepare a dataframe for ggplot and plot. To be overridden
      return(ggplot2::ggplot())
    },

    run = function(animate = FALSE, sleep = animation::ani.options("interval"), save = FALSE, more_time = NULL) {
      if(animate) self$plot(main="Initial")
      # Prepare the time line
      if(is.null(self$last_time)) self$last_time = self$timeline[1]
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
        # save or not depends on previous choice
        save <- !is.null(self$run_patterns)
      }
      # Prepare the data structure to save evolutions
      private$prepare_to_save(save, more_time)
      # Run the remaining timeline
      for(time in self$timeline[self$timeline > self$last_time]) {
        private$evolve(time, save)
        self$last_time <- time
        if(animate) {
          self$plot(main=paste("Time:", time), sleep=sleep)
        }
      }
    },

    saved_pattern = function(time) {
      # Find the saved pattern at the chosen time
      if(is.null(time)) {
        if(is.null(self$pattern)) {
          return(NULL)
        } else {
          the_pattern <- self$pattern
        }
      } else {
        if(is.null(self$run_patterns)) {
          stop("No saved patterns. Run the model with argument save=TRUE before plotting or using saved patterns.")
        }
        which_pattern <- which(self$timeline == time)
        if (length(which_pattern) == 0) {
          warning("The time to plot or use the pattern was not found in the time line.")
          invisible(NULL)
        } else {
          the_pattern <- private$pattern_by_index(which_pattern)
        }
      }
      # Set the class (not saved in run_patterns)
      class(the_pattern) <- private$pattern_class
      return(the_pattern)
    },

    along_time = function(FUN, ...) {
      if(is.null(self$run_patterns)) {
        stop("No saved patterns. Run the model with argument save=TRUE before using saved patterns.")
      }
      pattern_FUN <- function(time, original_FUN, ...) {
        return(original_FUN(self$saved_pattern(time), ...))
      }
      the_vector  <- sapply(self$timeline, pattern_FUN, original_FUN=FUN, ...)
      if(!is.vector(the_vector)) stop("The function to apply along time must return a single value.")
      the_df <- data.frame(x=self$timeline, y=the_vector)
      return(the_df)
    }
  )
)


#' Community Point Pattern Class
#'
#' A \code{\link{community_model}} whose pattern is a \code{\link[dbmss]{wmppp}} object..
#' @docType class
#' @inherit community_model
#' @inheritParams community_model
#' @export
community_spcmodel <- R6Class("community_spcmodel",
  inherit = community_model,
  public = list(
    tess = NULL,

    initialize = function(pattern = SpatDiv::rSpCommunity(n=1, size = 100, CheckArguments = FALSE), timeline = 0, type = "Species") {
      super$initialize(pattern=pattern, timeline=timeline)
      self$type <- type
      self$tess <- spatstat::dirichlet(self$pattern)
    },

    # The n nearest nighbors
    neighbors_n = function(n) {
      spatstat::nnwhich(self$pattern, k=seq(n))
    },

    # Neighbors less than r apart
    neighbors_r = function(r, keep_distances=FALSE) {
      distances <- spatstat::pairdist(self$pattern)
      # Eliminate the diagonal
      diag(distances) <- NA
      # Points less than r apart
      the_neighbors <- apply(distances, 1, function(distance) which(distance <= r))
      if(keep_distances) {
        # Restore the distance to self to zero
        diag(distances) <- 0
        attr(the_neighbors, "distances") <- distances
      }
      return(the_neighbors)
    },

    # The type of neighbors less than r apart
    neighbor_types_r = function(r) {
      # The max value of the factors is needed
      nb_species <- max(as.integer(self$pattern$marks$PointType))
      # Run C++ routine to fill a 3D array. Rows are points, columns are r, the 3rd dimension has a z-value per species. Values are the number (weights) of neighbors of each point, up to ditance r, of species z.
      the_neighbors_types <- SpatDiv:::parallelCountNbd(r=r, nb_species,
                                     x=self$pattern$x, y=self$pattern$y,
                                     Type=self$pattern$marks$PointType, Weight=self$pattern$marks$PointWeight)
      # The matrix of neighbor communities is built from the vector returned.
      dim(the_neighbors_types) <- c(self$pattern$n, nb_species)
      return(the_neighbors_types)
    },

    plot = function(time = NULL, sleep=0, which.marks = "PointType", ...) {
     # if sleep > 0, use the animation package for a fluid sequence of images
     if (sleep>0) grDevices::dev.hold()
     # Plot
     if (which.marks == "PointType") {
       spatstat::marks(self$tess) <- self$saved_pattern(time)$marks$PointType
       plot(self$tess, do.col=TRUE, ...)
       graphics::points(myModel$pattern$x, myModel$pattern$y)
     } else {
       plot(self$saved_pattern(time), which.marks=which.marks, ...)
     }
     # Animation
     if (sleep>0) animation::ani.pause(sleep)
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
  neighborhood = NULL,

  initialize = function(pattern = pattern_grid(), timeline = 0, type = "Species", neighborhood = "von Neumann 1") {
    super$initialize(pattern=pattern, timeline=timeline)
    self$tess <- spatstat::dirichlet(self$pattern)
    self$type <- type
    if (neighborhood %in% c("von Neumann 1", "4", "Moore 1", "8", "Moore 2", "24")) {
     self$neighborhood <- neighborhood
    } else {
     self$neighborhood <- "von Neumann 1"
     warning("The neighborhood definition was not recognized: set to the default value.")
    }
    # Colors for plot()
    # self$cols <- grDevices::rainbow(max(pattern))
  },

  neighbors = function(point) {
    dx <- as.integer(round(abs(self$pattern$x[point] - self$pattern$x)))
    dy <- as.integer(round(abs(self$pattern$y[point] - self$pattern$y)))
    if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4")
      the_neighbors <- which(((dx==1L | dx==self$pattern$window$xrange[2]-1L) & (dy==0L)) |
                              (dy==1L | dy==self$pattern$window$yrange[2]-1L) & (dx==0L))
    if(self$neighborhood == "Moore 1" | self$neighborhood == "8")
      the_neighbors <- which((dx<=1L | dx==self$pattern$window$xrange[2]-1L) &
                              (dy<=1L | dy==self$pattern$window$yrange[2]-1L) & !(dx==0L & dy==0L))
    if(self$neighborhood == "Moore 2" | self$neighborhood == "24")
      the_neighbors <- which((dx<=2L | dx>=self$pattern$window$xrange[2]-2L) &
                              (dy<=2L | dy>=self$pattern$window$yrange[2]-2L) & !(dx==0L & dy==0L))
    return(the_neighbors)
  },

  plot = function(time = NULL, sleep=0, which.marks = "PointType", ...) {
    # if sleep > 0, use the animation package for a fluid sequence of images
    if (sleep>0) grDevices::dev.hold()
    # Plot
    if (which.marks == "PointType") {
      spatstat::marks(self$tess) <- self$saved_pattern(time)$marks$PointType
      plot(self$tess, do.col=TRUE, ...)
    } else {
      plot(self$saved_pattern(time), which.marks=which.marks, ...)
    }
    # Animation
    if (sleep>0) animation::ani.pause(sleep)
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
    # The pattern with a buffer
    buffered_pattern = NULL,

    pattern_by_index = function(i) {
      return(self$run_patterns[, , i])
    },

    prepare_to_save = function(save, more_time) {
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
    }
  ),
  public = list(
    #cols = NULL,
    neighborhood = NULL,

    initialize = function(pattern = pattern_matrix_individuals(), timeline = 0, type = "Species", neighborhood = "von Neumann 1") {
      super$initialize(pattern=pattern, timeline=timeline, type=type)
      if (neighborhood %in% c("von Neumann 1", "4", "Moore 1", "8", "Moore 2", "24")) {
        self$neighborhood <- neighborhood
      } else {
        self$neighborhood <- "von Neumann 1"
        warning("The neighborhood definition was not recognized: set to the default value.")
      }
      # Colors for plot()
      # self$cols <- grDevices::rainbow(max(pattern))
    },

    # Prepare buffered_pattern at each step of evolution
    prepare_buffer = function() {
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4" |
         self$neighborhood == "Moore 1" | self$neighborhood == "8") {
        # Add the buffer zone of width 1
        private$buffered_pattern <- cbind(self$pattern[, ncol(self$pattern)], self$pattern, self$pattern[, 1])
        private$buffered_pattern <- rbind(private$buffered_pattern[nrow(private$buffered_pattern), ], private$buffered_pattern, private$buffered_pattern[1, ])
      }
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24") {
        # Add the buffer zone of width 2
        private$buffered_pattern <- cbind(self$pattern[, (ncol(self$pattern)-1):ncol(self$pattern)], self$pattern, self$pattern[, 1:2])
        private$buffered_pattern <- rbind(private$buffered_pattern[(nrow(private$buffered_pattern)-1):nrow(private$buffered_pattern), ], private$buffered_pattern, private$buffered_pattern[1:2, ])
      }
    },

    # Return the vector of neighbors of a cell
    neighbors = function(row, col) {
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4") {
        the_neighbors <- c(private$buffered_pattern[row, col+1], private$buffered_pattern[row+2, col+1], private$buffered_pattern[row+1, col], private$buffered_pattern[row+1, col+2])
      }
      if(self$neighborhood == "Moore 1" | self$neighborhood == "8") {
        the_neighbors <- as.vector(private$buffered_pattern[row:(row+2), (col):(col+2)])[-5]
      }
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24") {
        the_neighbors <- as.vector(private$buffered_pattern[row:(row+4), col:(col+4)])[-13]
      }
      return(the_neighbors)
    },

    plot = function(time = NULL, sleep=0, ...) {
      if (sleep>0) grDevices::dev.hold()
      graphics::image(x=1:ncol(self$pattern), y=1:nrow(self$pattern), z=t(self$saved_pattern(time)), xlab="", ylab="", axes=FALSE, asp=1, ...)
      if (sleep>0) animation::ani.pause(sleep)
    },

    autoplot = function(time = NULL, ...) {
      the_pattern <- t(self$saved_pattern(time))
      if(is.null(the_pattern)) {
        invisible(NULL)
      } else {
        dimnames(the_pattern) <- list(seq(nrow(the_pattern)), seq(ncol(the_pattern)))
        the_df <- as.data.frame.table(the_pattern)
        names(the_df) <- c("x", "y", "z")
        the_plot <- ggplot2::ggplot(data=the_df, ggplot2::aes_(x=~x, y=~y)) +
          ggplot2::geom_tile(ggplot2::aes_(fill=~z), col="black") +
          ggplot2::theme(panel.grid = ggplot2::element_line()) +
          ggplot2::coord_fixed() +
          ggplot2::labs(fill=self$type)
        return(the_plot)
      }
    }
  )
)
