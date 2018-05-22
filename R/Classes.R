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
#' @field last_time The last time (in the time line) the model has been run.
#' @field run_patterns The past patterns of the model, obtained by \code{run} and saved.
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(pattern = NULL, timeline = 0)}}{Initialization.}
#'   \item{\code{plot(..., time=NULL, sleep=animation::ani.options("interval"))}}{Default plot method: plots the pattern.}
#'   \item{\code{autoplot(..., time=NULL)}}{Makes a \code{\link{ggplot}} of the pattern.}
#'   \item{\code{run(animate = FALSE, sleep = 0, save = FALSE, more_time = NULL)}}{Run the model.}
#'   \item{\code{saved_pattern(time)}}{Returns the pattern at the chosen time.}
#'   \item{\code{along_time(FUN, ...)}}{Applies the function FUN to the saved patterns along time and returns a dataframe with columns \code{x} for time and \code{y} for the results of FUN. FUN must return a single value.}
#' }
#' @examples
#' myModel <- community_model$new
community_model <- R6Class("community_model",
  private = list(
    pattern_class = NULL,
    pattern_by_index = function(i) {
      # Base class method to be overriden. Assumes that run_patterns is a list.
      return(self$run_patterns[[i]])
    },

    prepare_to_run = function(save, more_time) {
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
        self$run_patterns <- self$pattern
      }
    }
  ),
  public = list(
    pattern = NULL,
    timeline = NULL,
    last_time = NULL,
    run_patterns = NULL,

    initialize = function(pattern = NULL, timeline = 0) {
      self$pattern <- pattern
      private$pattern_class <- class(pattern)
      self$timeline <- sort(timeline)
    },

    # plot method for the pattern. To be overridden.
    plot = function(..., time=NULL, sleep=0) {
      # if sleep > 0, use the animation package for a fluid sequence of images
      if (sleep>0) grDevices::dev.hold()
      # Specific plot code to be overridden
      plot(self$saved_pattern(time), ...)
      # Animation
      if (sleep>0) ani.pause(sleep)
    },

    # ggplot method for the pattern. To be overridden.
    autoplot = function(..., time=NULL) {
      # get the pattern to plot
      the_pattern <- self$saved_pattern(time)
      # Prepare a dataframe for ggplot and plot. To be overridden
      return(ggplot2::ggplot())
    },

    run = function(animate = FALSE, sleep = animation::ani.options("interval"), save = FALSE, more_time = NULL) {
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
        # save or not depends on previous choice
        save <- !is.null(self$run_patterns)
      }
      # Prepare the data structure to save evolutions
      private$prepare_to_run(save, more_time)
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
        the_pattern <- t(self$pattern)
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

    plot = function(..., which="PointType") {
      # if sleep > 0, use the animation package for a fluid sequence of images
      if (sleep>0) grDevices::dev.hold()
      # Plot
      if (which == "PointType") {
        spatstat::marks(self$tess) <- self$pattern$marks$PointType
        plot(self$tess, do.col=TRUE, ...)
      } else {
        plot(self$pattern, ..., which=which)
      }
      # Animation
      if (sleep>0) ani.pause(sleep)
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
    pattern_by_index = function(i) {
      return(t(self$run_patterns[, , i]))
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
    }
  ),
  public = list(
    #cols = NULL,

    plot = function(..., time = NULL, sleep=0) {
      if (sleep>0) grDevices::dev.hold()
      graphics::image(x=1:ncol(self$pattern), y=1:nrow(self$pattern), z=self$saved_pattern(time), xlab="", ylab="", axes=FALSE, asp=1, ...)
      if (sleep>0) ani.pause(sleep)
    },

    autoplot = function(...) {
      the_pattern <- self$saved_pattern(time)
      dimnames(the_pattern) <- list(seq(nrow(the_pattern)), seq(ncol(the_pattern)))
      the_df <- as.data.frame.table(the_pattern)
      names(the_df) <- c("x", "y", "Species")
      the_plot <- ggplot2::ggplot(data=the_df, ggplot2::aes_(x=~x, y=~y)) +
        ggplot2::geom_tile(ggplot2::aes_(fill=~Species), col="black") +
        ggplot2::theme(panel.grid = ggplot2::element_line()) +
        ggplot2::coord_fixed()
      return(the_plot)
    }
  )
)
