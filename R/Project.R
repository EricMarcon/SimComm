#' SimComm
#'
#' Community simulator.
#'
#' This package alows simulating the evolution along time of communities of agents, whose location is decribed by a pattern (a matrix or a point pattern).
#' The communities are represented by R6 classes: \code{\link{community_model}}.
#' A model contains a pattern, a timeline and an evolution method that is run along the timeline.
#' This is a classical approach in community ecology modeling or agent based models.
#'
#' @name SimComm
"_PACKAGE"


# Functions to reexport
#' @export
ggplot2::autoplot


#' plot Community Models
#'
#' S3 method to force the use of the R6 method.
#'
#' This is a helper function to always use the R6 method rather than the S3 method: \code{plot(MyModel)} is translated to \code{MyModel$plot}.
#'
#' @param x The model to plot.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return NULL
#' @importFrom graphics plot
#' @method plot community_model
#' @export
#'
#' @examples
#' myModel <- cm_drift$new(pattern_matrix_individuals(S=10))
#' plot(myModel)
plot.community_model <- function(x, ...) {
  x$plot(...)
}


#' autoplot Community Models
#'
#' S3 method to force the use of the R6 method.
#'
#' This is a helper function to always use the R6 method rather than the S3 method: \code{autoplot(MyModel)} is translated to \code{MyModel$autoplot}.
#'
#' @param object The model to plot.
#' @param ... Extra arguments passed to the plot method.
#'
#' @return NULL
#' @importFrom ggplot2 autoplot
#' @method autoplot community_model
#' @export
#'
#' @examples
#' myModel <- cm_drift$new(pattern_matrix_individuals(S=10))
#' autoplot(myModel)
autoplot.community_model <- function(object, ...) {
  object$autoplot(...)
}
