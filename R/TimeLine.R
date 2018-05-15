#' timeline_regular
#'
#' The moments when a \code{\link{community_model}} evolves.
#'
#' @param from Time of the fist event
#' @param to Time of the last event
#' @param by Intervals between events
#' @param unit Unit of time
#'
#' @return a list of two items. \code{sequence} is a vector of numeric, positive values. \code{unit} is a string that describes the unit of time.
#' @export
#'
#' @examples
#' timeline_regular
timeline_regular <- function(from = 1, to = 10, by = 1, unit = "Unit") {
  the_timeline <- list(sequence=seq(from, to, by), unit=unit)
  return(the_timeline)
}

