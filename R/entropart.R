#' entropart Methods
#'
#' The entropart package provides generic methods to measure diversity.
#' S3 methods to aplly them to simulated communities are available here.
#' \code{\link{AbdVector}} returns an abundance vector and \code{\link{ProbaVector}} returns a probability vector.
#' \code{\link{Tsallis}} returns Tsallis's entropy of a community, \code{\link{Diversity}} its diversity.
#' \code{\link{Richness}}, \code{\link{Shannon}}  and \code{\link{Simpson}} return particular indices of diversity.
#'
#'
#' @param x An object of class \code{\link{pattern_matrix_individuals}}.
#' @param NorP An object of class "wmppp" (\code{\link{pattern_matrix_individuals}}).
#' @param q A number: the order of entropy. Some corrections allow only a positive number. Default is 1 for Shannon entropy.
#' @param Correction A string containing one of the possible corrections: \code{"None"} (no correction), and \code{"Best"} are always valid. See the generic function help for other posiibilities.
#' @param Alpha The risk level, 5\% by default, used to optimize the jackknife order.
#' @param JackOver If \code{TRUE}, retain the jackknife order immediately superior to the optimal one, usually resulting in the overestimation of the number of species. Default is \code{FALSE}.
#' @param ... Further arguments. Unsused.
#' @param CheckArguments If \code{TRUE} (default), the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
#' @name entropart


#' @rdname entropart
#' @importFrom entropart as.AbdVector
#' @method as.AbdVector pattern_matrix_individuals
#' @export
#'
#' @examples
#' # A community matrix drift model
#' myModel <- cm_drift$new(pattern_matrix_individuals(S=10))
#' myModel$autoplot()
#' as.AbdVector(myModel$pattern)
as.AbdVector.pattern_matrix_individuals <-
function (x, ...)
{
  the_vector <- tapply(x, x, length)
  class(the_vector) <- c("AbdVector", "SpeciesDistribution", class(the_vector))
  return(the_vector)
}


#' @rdname entropart
#' @importFrom entropart as.ProbaVector
#' @method as.ProbaVector pattern_matrix_individuals
#' @export
#'
#' @examples
#' as.ProbaVector(myModel$pattern)
as.ProbaVector.pattern_matrix_individuals <-
function (x, ...)
{
  the_vector <- tapply(x, x, length)
  the_vector <- the_vector / sum(the_vector)
  class(the_vector) <- c("ProbaVector", "SpeciesDistribution", class(the_vector))
  return(the_vector)
}


#' @rdname entropart
#' @importFrom entropart Tsallis
#' @method Tsallis pattern_matrix_individuals
#' @export
#'
#' @examples
#' Tsallis(myModel$pattern)
Tsallis.pattern_matrix_individuals <-
function(NorP, q = 1, Correction = "Best", ..., CheckArguments = TRUE)
{
  return(entropart::bcTsallis(Ns=as.AbdVector.pattern_matrix_individuals(NorP), q=q, Correction=Correction, CheckArguments=CheckArguments))
}


#' @rdname entropart
#' @importFrom entropart Diversity
#' @method Diversity pattern_matrix_individuals
#' @export
#'
#' @examples
#' Diversity(myModel$pattern)
Diversity.pattern_matrix_individuals <-
function(NorP, q = 1, Correction = "Best", ..., CheckArguments = TRUE)
{
  return (entropart::bcDiversity(Ns=as.AbdVector.pattern_matrix_individuals(NorP), q=q, Correction=Correction, CheckArguments=CheckArguments))
}


#' @rdname entropart
#' @importFrom entropart Richness
#' @method Richness pattern_matrix_individuals
#' @export
#'
#' @examples
#' Richness(myModel$pattern)
Richness.pattern_matrix_individuals <-
function(NorP, Correction = "Chao1", Alpha = 0.05, JackOver = FALSE,  ..., CheckArguments = TRUE)
{
  return (entropart::bcRichness(Ns=as.AbdVector.pattern_matrix_individuals(NorP), Correction=Correction, Alpha=Alpha, JackOver=JackOver, CheckArguments=CheckArguments))
}



#' @rdname entropart
#' @importFrom entropart Shannon
#' @method Shannon pattern_matrix_individuals
#' @export
#'
#' @examples
#' Shannon(myModel$pattern)
Shannon.pattern_matrix_individuals <-
function(NorP, Correction = "Best", ..., CheckArguments = TRUE)
{
  return(entropart::bcShannon(Ns=as.AbdVector.pattern_matrix_individuals(NorP), Correction=Correction, CheckArguments=CheckArguments))
}


#' @rdname entropart
#' @importFrom entropart Simpson
#' @method Simpson pattern_matrix_individuals
#' @export
#'
#' @examples
#' Simpson(myModel$pattern)
Simpson.pattern_matrix_individuals <-
function(NorP, Correction = "Lande", ..., CheckArguments = TRUE)
{
  return(entropart::bcSimpson(Ns=as.AbdVector.pattern_matrix_individuals(NorP), Correction=Correction, CheckArguments=CheckArguments))
}
