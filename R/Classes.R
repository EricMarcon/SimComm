#' Community Model Class
#'
#' @description
#' The model represents a community, i.e. a set of interacting objects called agents.
#' Their location is described by a `pattern` that `evolve`s along a `timeline`.
#'
#' @param animate if `TRUE`, the evolution of the model is shown in an animation.
#' @param FUN A function to apply to each pattern of the evolution of the model.
#' It must return a single, numeric value.
#' @param more_time A numeric vector that extends the timeline.
#' @param pattern The pattern which describes the location of agents.
#' @param save if `TRUE`, the evolution of the model is saved.
#' if `FALSE`, only the final step is saved.
#' @param sleep The time (in ms) to sleep between each step of the evolution of the model.
#' @param time The point of the timeline considered.
#' Its value should be in `timeline`.
#' @param timeline A numeric vector that contains the points of time of interest.
#' @param type The type of individuals. Informational only.
#' @param ... Extra arguments to be passed to methods.
#' @examples
#' myModel <- community_matrixmodel$new()
#' myModel$autoplot()
community_model <- R6::R6Class("community_model",
  private = list(
    pattern_class = NULL,
    pattern_by_index = function(i) {
      # Base class method to be overridden.
      # Assumes that run_patterns is a list.
      return(self$run_patterns[[i]])
    },

    prepare_to_save = function(save, more_time) {
      # Code to prepare private$run_patterns to store the successive patterns.
      # Assumes that run_patterns is a list, to be overridden.
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
              self$run_patterns <- c(
                self$run_patterns,
                vector("list", sum(self$timeline > self$last_time))
              )
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
    #' @field pattern The pattern which describes the location of agents.
    pattern = NULL,
    #' @field type The type of individuals. Informational only.
    type = NULL,
    #' @field timeline A numeric vector.
    timeline = NULL,
    #' @field last_time The last time (in the time line) the model has been run.
    last_time = NULL,
    #' @field run_patterns The past patterns of the model, obtained by `run` and saved.
    run_patterns = NULL,

    #' @description
    #' Create a new instance of this [R6][R6::R6Class] class.
    initialize = function(pattern = NULL, timeline = 0, type = "Species") {
      self$pattern <- pattern
      private$pattern_class <- class(pattern)
      self$timeline <- sort(timeline)
      self$type <- type
    },

    #' @description
    #' Default plot method: plots the pattern.
    #' To be overridden.
    plot = function(
        time = NULL,
        sleep = animation::ani.options("interval"),
        ...) {
      # if sleep > 0, use the animation package for a fluid sequence of images
      if (sleep > 0) grDevices::dev.hold()
      # Specific plot code to be overridden
      plot(self$saved_pattern(time), ...)
      # Animation
      if (sleep > 0) animation::ani.pause(sleep)
    },

    #' @description
    #' Make a [ggplot2::ggplot] of the pattern.
    #' To be overridden.
    autoplot = function(time = NULL, ...) {
      # get the pattern to plot
      the_pattern <- self$saved_pattern(time)
      # Prepare a dataframe for ggplot and plot. To be overridden
      return(ggplot2::ggplot())
    },

    #' @description
    #' Run the model.
    run = function(
        animate = FALSE,
        sleep = animation::ani.options("interval"),
        save = FALSE,
        more_time = NULL) {
      if(animate) self$plot(main = "Initial")
      # Prepare the time line
      if(is.null(self$last_time)) self$last_time <- self$timeline[1]
      if(is.null(more_time)) {
        # Start from scratch
        self$last_time = self$timeline[1]
      } else {
        # Order
        more_time <- sort(more_time)
        # Eliminate overlap
        if(more_time[1] <= self$last_time) {
          warning(
            paste(
              "The extended time line tried to start at",
              more_time[1],
              "but the last time run is",
              self$last_time,
              "- Overlapping time has been ignored."
            )
          )
          more_time <- more_time[more_time > self$last_time]
        }
        # Extend the time line
        self$timeline <- c(self$timeline, more_time)
        # save or not depends on previous choice
        save <- !is.null(self$run_patterns)
      }
      # Prepare the data structure to save evolution
      private$prepare_to_save(save, more_time)
      # Run the remaining timeline
      for(time in self$timeline[self$timeline > self$last_time]) {
        private$evolve(time, save)
        self$last_time <- time
        if(animate) {
          self$plot(main=paste("Time:", time), sleep = sleep)
        }
      }
    },

    #' @description
    #' Return the pattern at the chosen time.
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

    #' @description
    #' Apply the function `FUN` to the saved patterns along time and return
    #' a dataframe with columns `x` for time and `y` for the results of `FUN`.
    #' `FUN` must return a single value.
    along_time = function(FUN, ...) {
      if(is.null(self$run_patterns)) {
        stop("No saved patterns. Run the model with argument save=TRUE before using saved patterns.")
      }
      pattern_FUN <- function(time, original_FUN, ...) {
        return(original_FUN(self$saved_pattern(time), ...))
      }
      the_vector  <- sapply(self$timeline, pattern_FUN, original_FUN = FUN, ...)
      if(!is.vector(the_vector)) stop("The function to apply along time must return a single value.")
      the_df <- data.frame(x = self$timeline, y = the_vector)
      return(the_df)
    }
  )
)


#' Community Point Pattern Class
#'
#' A [community_model] whose pattern is a [dbmss::wmppp] object..
#' @docType class
#' @param keep_distances if `TRUE`, save the vector of distances to neighbors in the attribute `the_neighbors`.
#' @param n The number of neighbors considered as in the neighborhood.
#' @param pattern The pattern which describes the location of agents.
#' @param r The distance that defines the neighborhood.
#' @param sleep The time (in ms) to sleep between each step of the evolution of the model.
#' @param time The point of the timeline considered.
#' @param timeline A numeric vector that contains the points of time of interest.
#' @param type The type of individuals. Informational only.
#' @param which.marks The marks to plot, that must be a the name of a column of the mark dataframe.
#' @param ... Extra arguments to be passed to methods.
#' @export
community_spcmodel <- R6::R6Class("community_spcmodel",
  inherit = community_model,
  public = list(
    #' @field tess The Dirichlet tessellation of the spatial point pattern.
    tess = NULL,

    #' @description
    #' Create a new instance of this [R6][R6::R6Class] class.
    initialize = function(
        pattern = SpatDiv::rSpCommunity(n=1, size = 100, CheckArguments = FALSE),
        timeline = 0,
        type = "Species") {
      super$initialize(pattern=pattern, timeline=timeline)
      self$type <- type
      self$tess <- spatstat.geom::dirichlet(self$pattern)
    },

    #' @description
    #' The n nearest neighbors
    neighbors_n = function(n) {
      spatstat.geom::nnwhich(self$pattern, k=seq(n))
    },

    #' @description
    #' Neighbors less than r apart
    neighbors_r = function(r, keep_distances = FALSE) {
      distances <- spatstat.geom::pairdist(self$pattern)
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

    #' @description
    #' The type of neighbors less than r apart
    neighbor_types_r = function(r) {
      # The max value of the factors is needed
      nb_species <- max(as.integer(self$pattern$marks$PointType))
      # Run C++ routine to fill a 3D array.
      # Rows are points, columns are r, the 3rd dimension has a z-value per species.
      # Values are the number (weights) of neighbors of each point, up to ditance r, of species z.
      the_neighbors_types <- SpatDiv:::parallelCountNbd(
        r = r,
        NbSpecies = nb_species,
        x = self$pattern$x,
        y = self$pattern$y,
        Type = self$pattern$marks$PointType,
        Weight = self$pattern$marks$PointWeight
      )
      # The matrix of neighbor communities is built from the vector returned.
      dim(the_neighbors_types) <- c(self$pattern$n, nb_species)
      return(the_neighbors_types)
    },

    #' @description
    #' Plots the pattern.
    plot = function(time = NULL, sleep=  0, which.marks = "PointType", ...) {
     # if sleep > 0, use the animation package for a fluid sequence of images
     if (sleep>0) grDevices::dev.hold()
     # Plot
     if (which.marks == "PointType") {
       spatstat.geom::marks(self$tess) <- self$saved_pattern(time)$marks$PointType
       plot(self$tess, do.col = TRUE, ...)
       graphics::points(myModel$pattern$x, myModel$pattern$y)
     } else {
       plot(self$saved_pattern(time), which.marks = which.marks, ...)
     }
     # Animation
     if (sleep>0) animation::ani.pause(sleep)
    }
  )
)



#' Community Grid Model Class
#'
#' A [community_model] whose pattern is a regular, rectangular grid of points.
#' @docType class
#' @param animate if `TRUE`, the evolution of the model is shown in an animation.
#' @param FUN A function to apply to each pattern of the evolution of the model.
#' It must return a single, numeric value.
#' @param more_time A numeric vector that extends the timeline.
#' @param neighborhood A character string defining what is the neighborhood of a cell:
#' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
#' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
#' "Moore 2" or "24" for two rings of neighbors.
#' @param pattern The pattern which describes the location of agents.
#' @param point The focal point around which a neighborhood is defined.
#' @param save if `TRUE`, the evolution of the model is saved.
#' if `FALSE`, only the final step is saved.
#' @param sleep The time (in ms) to sleep between each step of the evolution of the model.
#' @param time The point of the timeline considered.
#' Its value should be in `timeline`.
#' @param timeline A numeric vector that contains the points of time of interest.
#' @param type The type of individuals. Informational only.
#' @param which.marks The marks to plot, that must be a the name of a column of the mark dataframe.
#' @param ... Extra arguments to be passed to methods.
#' @export
community_gridmodel <- R6::R6Class("community_gridmodel",
  inherit = community_model,
  public = list(
    #' @field tess A tessellation of the window containing the points, used to plot the model.
    tess = NULL,
    #' @field neighborhood A character string defining what is the neighborhood of a cell:
    #' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
    #' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
    #' "Moore 2" or "24" for two rings of neighbors.
    neighborhood = NULL,

    #' @description
    #' Create a new instance of this [R6][R6::R6Class] class.
    initialize = function(
        pattern = pattern_grid(),
        timeline = 0,
        type = "Species",
        neighborhood = "von Neumann 1") {
      super$initialize(pattern = pattern, timeline = timeline)
      self$tess <- spatstat.geom::dirichlet(self$pattern)
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

    #' @description
    #' Return a vector that contains the indices of neighbors in the point pattern.
    neighbors = function(point) {
      dx <- as.integer(round(abs(self$pattern$x[point] - self$pattern$x)))
      dy <- as.integer(round(abs(self$pattern$y[point] - self$pattern$y)))
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4")
        the_neighbors <- which(
          ((dx == 1L | dx == self$pattern$window$xrange[2] - 1L) & (dy == 0L)) |
          (dy == 1L | dy == self$pattern$window$yrange[2] - 1L) &
          (dx == 0L)
        )
      if(self$neighborhood == "Moore 1" | self$neighborhood == "8")
        the_neighbors <- which(
          (dx <= 1L | dx == self$pattern$window$xrange[2] - 1L) &
          (dy <= 1L | dy == self$pattern$window$yrange[2] - 1L) &
          !(dx == 0L & dy == 0L)
        )
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24")
        the_neighbors <- which(
          (dx <= 2L | dx >= self$pattern$window$xrange[2] - 2L) &
          (dy <= 2L | dy >= self$pattern$window$yrange[2] - 2L) &
          !(dx == 0L & dy == 0L)
        )
      return(the_neighbors)
    },

    #' @description
    #' Plots the pattern.
    plot = function(time = NULL, sleep=0, which.marks = "PointType", ...) {
      # if sleep > 0, use the animation package for a fluid sequence of images
      if (sleep>0) grDevices::dev.hold()
      # Plot
      if (which.marks == "PointType") {
        spatstat.geom::marks(self$tess) <- self$saved_pattern(time)$marks$PointType
        plot(self$tess, do.col = TRUE, ...)
      } else {
        plot(self$saved_pattern(time), which.marks = which.marks, ...)
      }
      # Animation
      if (sleep>0) animation::ani.pause(sleep)
    }
  )
)



#' Community Matrix Model Class
#'
#' A [community_model] whose pattern is a regular, rectangular grid of points.
#' @docType class
#' @param animate if `TRUE`, the evolution of the model is shown in an animation.
#' @param col The column of the focal cell.
#' @param FUN A function to apply to each pattern of the evolution of the model.
#' It must return a single, numeric value.
#' @param more_time A numeric vector that extends the timeline.
#' @param neighborhood A character string defining what is the neighborhood of a cell:
#' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
#' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
#' "Moore 2" or "24" for two rings of neighbors.
#' @param pattern The pattern which describes the location of agents.
#' @param row The row of the focal cell.
#' @param save if `TRUE`, the evolution of the model is saved.
#' if `FALSE`, only the final step is saved.
#' @param sleep The time (in ms) to sleep between each step of the evolution of the model.
#' @param time The point of the timeline considered.
#' Its value should be in `timeline`.
#' @param timeline A numeric vector that contains the points of time of interest.
#' @param type The type of individuals. Informational only.
#' @param ... Extra arguments to be passed to methods.
#' @param pattern The pattern which describes the location of agents.
#' @export
community_matrixmodel <- R6::R6Class("community_matrixmodel",
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
              self$run_patterns <- array(
                data = 0,
                dim = c(
                  nrow(self$pattern),
                  ncol(self$pattern),
                  length(self$timeline)
                )
              )
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
    #' @field neighborhood A character string defining what is the neighborhood of a cell:
    #' "von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
    #' "Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
    #' "Moore 2" or "24" for two rings of neighbors.
    neighborhood = NULL,

    #' @description
    #' Create a new instance of this [R6][R6::R6Class] class.
    initialize = function(
        pattern = pattern_matrix_individuals(),
        timeline = 0,
        type = "Species",
        neighborhood = "von Neumann 1") {
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

    #' @description
    #' Prepare buffered_pattern at each step of evolution
    prepare_buffer = function() {
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4" |
         self$neighborhood == "Moore 1" | self$neighborhood == "8") {
        # Add the buffer zone of width 1
        private$buffered_pattern <- cbind(
          self$pattern[, ncol(self$pattern)],
          self$pattern, self$pattern[, 1]
        )
        private$buffered_pattern <- rbind(
          private$buffered_pattern[nrow(private$buffered_pattern), ],
          private$buffered_pattern, private$buffered_pattern[1, ]
        )
      }
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24") {
        # Add the buffer zone of width 2
        private$buffered_pattern <- cbind(
          self$pattern[, (ncol(self$pattern) - 1):ncol(self$pattern)],
          self$pattern, self$pattern[, 1:2]
        )
        private$buffered_pattern <- rbind(
          private$buffered_pattern[
            (nrow(private$buffered_pattern) - 1):nrow(private$buffered_pattern),

          ],
          private$buffered_pattern, private$buffered_pattern[1:2, ]
        )
      }
    },

    #' @description
    #' Return the vector of neighbors of a cell, defined by it row and column
    neighbors = function(row, col) {
      if(self$neighborhood == "von Neumann 1" | self$neighborhood == "4") {
        the_neighbors <- c(
          private$buffered_pattern[row,     col + 1],
          private$buffered_pattern[row + 2, col + 1],
          private$buffered_pattern[row + 1, col],
          private$buffered_pattern[row + 1, col + 2])
      }
      if(self$neighborhood == "Moore 1" | self$neighborhood == "8") {
        the_neighbors <- as.vector(
          private$buffered_pattern[row:(row + 2), (col):(col + 2)]
        )[-5]
      }
      if(self$neighborhood == "Moore 2" | self$neighborhood == "24") {
        the_neighbors <- as.vector(
          private$buffered_pattern[row:(row + 4), col:(col + 4)]
        )[-13]
      }
      return(the_neighbors)
    },

    #' @description
    #' Plots the pattern.
    plot = function(time = NULL, sleep=0, ...) {
      if (sleep>0) grDevices::dev.hold()
      graphics::image(
        x = 1:ncol(self$pattern),
        y = 1:nrow(self$pattern),
        z = t(self$saved_pattern(time)),
        xlab = "",
        ylab = "",
        axes = FALSE,
        asp = 1,
      ...)
      if (sleep > 0) animation::ani.pause(sleep)
    },

    #' @description
    #' Produces a ggplot of the pattern.
    autoplot = function(time = NULL, ...) {
      the_pattern <- t(self$saved_pattern(time))
      if(is.null(the_pattern)) {
        invisible(NULL)
      } else {
        dimnames(the_pattern) <- list(seq(nrow(the_pattern)), seq(ncol(the_pattern)))
        the_df <- as.data.frame.table(the_pattern)
        names(the_df) <- c("x", "y", "z")
        the_plot <- ggplot2::ggplot(
            data = the_df,
            ggplot2::aes(x = .data$x, y = .data$y)
          ) +
          ggplot2::geom_tile(ggplot2::aes(fill = .data$z), col = "black") +
          ggplot2::theme(panel.grid = ggplot2::element_line()) +
          ggplot2::coord_fixed() +
          ggplot2::labs(fill = self$type)
        return(the_plot)
      }
    }
  )
)
