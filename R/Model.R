#' Model
#'
#' @param Pattern
#' @param Evolution
#' @param TimeLine
#'
#' @return
#' @export
#'
#' @examples
Model <- function(Pattern = NULL, Evolution = NULL, TimeLine = NULL) {

  # Default pattern
  if(is.null(Pattern)) {
    Pattern <- spatstat::tess(xgrid=0:4, ygrid=0:4)
    spatstat::tilenames(Pattern) <- seq(Pattern$n)
    spatstat::marks(Pattern) <- data.frame(Type=rep("Unique", Pattern$n), Size=1)
  }

  # Default evolution: identity
  if(is.null(Evolution)) {
    Evolution <- function(x, deltaT) {
      return(x)
    }
  }

  # Default time line
  if(is.null(TimeLine)) {
    TimeLine <- TimeLine()
  }

  theModel <- list(Pattern=Pattern, Evolution=Evolution, TimeLine=TimeLine)

  class(theModel) <-c("CommModel", class(theModel))
  return(theModel)
}



#' TimeLine
#'
#' @param Sequence
#' @param Unit
#'
#' @return
#' @export
#'
#' @examples
TimeLine <- function(Sequence = 1, Unit = "Unit") {
  theTimeLine <- list(Sequence, Unit)
  return(theTimeLine)
}



#' RunModel
#'
#' @param Model
#' @param Initialize
#' @param Animate
#'
#' @return
#' @export
#'
#' @examples
RunModel <- function(Model, Initialize = NULL, Animate = TRUE) {
  if(!is.null(Initialize)) {
    do.call(Initialize, Pattern)
  }

  if(Animate) plot(Model$Pattern, main="Initial")

  PreviousTime <- 0
  for(Time in Model$TimeLine) {
    deltaT <- Time-PreviousTime
    do.call(Model$Evolution, list(Model$Pattern, deltaT))
    if(Animate) plot(Model$Pattern, main=Time)
  }
}
