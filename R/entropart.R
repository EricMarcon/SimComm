##### entropart Methods to measure the diversity of simulated communities ####


#' Abundance vector of a matrix of individuals
#'
#' Gets an \code{\link{AbdVector}} object from a \code{\link{pattern_matrix_individuals}} object to measure diversity with the entropart package.
#'
#' @param x An object of class \code{\link{pattern_matrix_individuals}}.
#' @param ... Further arguments. Unsused.
#'
#' @return An \code{\link{AbdVector}} object, which is a named integer vector.
#'
#' @importFrom entropart as.AbdVector
#' @method as.AbdVector pattern_matrix_individuals
#' @export
#'
#' @examples
as.AbdVector.pattern_matrix_individuals <-
function (x, ...)
{
  the_vector <- tapply(x, x, length)
  class(the_vector) <- c("AbdVector", "SpeciesDistribution", class(the_vector))
  return(the_vector)
}


#' Probabilty vector of a matrix of individuals
#'
#' Gets an \code{\link{ProbaVector}} object from a \code{\link{pattern_matrix_individuals}} object to measure diversity with the entropart package.
#'
#' @param x An object of class \code{\link{pattern_matrix_individuals}}.
#' @param ... Further arguments. Unsused.
#'
#' @return A \code{\link{ProbaVector}} object, which is a named integer vector.
#'
#' @importFrom entropart as.ProbaVector
#' @method as.ProbaVector pattern_matrix_individuals
#' @export
#'
#' @examples
as.ProbaVector.pattern_matrix_individuals <-
function (x, ...)
{
  the_vector <- tapply(x, x, length)
  the_vector <- the_vector / sum(the_vector)
  class(the_vector) <- c("ProbaVector", "SpeciesDistribution", class(the_vector))
  return(the_vector)
}


#' Tsallis (HCDT) Entropy of a matrix of individuals
#'
#' Calculates the HCDT, also known as Tsallis entropy of order \eqn{q} of a \code{\link{pattern_matrix_individuals}} object.
#'
#' Tsallis (Havrda and Charvat, 1967; Daroczy, 1970; Tsallis, 1988) generalized entropy is a generalized measure of diversity (Jost, 2006).
#' See \code{\link{Tsallis}} for more details.
#'
#' @param NorP An object of class "wmppp" (\code{\link{pattern_matrix_individuals}}).
#' @param q A number: the order of entropy. Some corrections allow only a positive number. Default is 1 for Shannon entropy.
#' @param Correction A string containing one of the possible corrections: \code{"None"} (no correction), \code{"ChaoShen"}, \code{"GenCov"}, \code{"Grassberger"}, \code{"Holste"}, \code{"Bonachela"}, \code{"ZhangGrabchak"}, or \code{"ChaoWangJost"}, \code{"Marcon"}, \code{"UnveilC"}, \code{"UnveiliC"}, \code{"UnveilJ"} or \code{"Best"}, the default value.  Currently, \code{"Best"} is \code{"ChaoWangJost"}.
#' @param ... Further arguments. Unsused.
#' @param CheckArguments If \code{TRUE} (default), the function arguments are verified. Should be set to \code{FALSE} to save time in simulations for example, when the arguments have been checked elsewhere.
#'
#' @return A named number equal to the calculated entropy. The name is that of the bias correction used.
#'
#' @importFrom entropart Tsallis
#' @method Tsallis pattern_matrix_individuals
#' @export
#'
#' @examples
Tsallis.pattern_matrix_individuals <-
function(NorP, q = 1, Correction = "Best", ..., CheckArguments = TRUE)
{
  return(entropart::bcTsallis(Ns=as.AbdVector.pattern_matrix_individuals(NorP), q=q, Correction=Correction, CheckArguments=CheckArguments))
}


#' HCDT diversity of a matrix of individuals
#'
#' \code{Diversity} calls \code{\link{Tsallis}} to calculate entropy and transforms it into diversity by calculating its deformed exponential.
#'
#' @inheritParams Tsallis.pattern_matrix_individuals
#'
#' @return A named number equal to the calculated diversity. The name is that of the bias correction used.
#'
#' @importFrom entropart Diversity
#' @method Diversity pattern_matrix_individuals
#' @export
#'
#' @examples
Diversity.pattern_matrix_individuals <-
function(NorP, q = 1, Correction = "Best", ..., CheckArguments = TRUE)
{
  return (entropart::bcDiversity(Ns=as.AbdVector.pattern_matrix_individuals(NorP), q=q, Correction=Correction, CheckArguments=CheckArguments))
}


#' Richness of a matrix of individuals
#'
#' \code{Richness} is the number of species.
#'
#' @inheritParams Tsallis.pattern_matrix_individuals
#' @param Correction A string containing one of the possible corrections: \code{"None"} (no correction), \code{"Jackknife"}, \code{"iChao1"}, or \code{"Chao1"}, the default value.
#' @param Alpha The risk level, 5\% by default, used to optimize the jackknife order.
#' @param JackOver If \code{TRUE}, retain the jackknife order immediately superior to the optimal one, usually resulting in the overestimation of the number of species. Default is \code{FALSE}.
#'
#' @return A named number equal to the calculated diversity. The name is that of the bias correction used.
#'
#' @importFrom entropart Richness
#' @method Richness pattern_matrix_individuals
#' @export
#'
#' @examples
Richness.pattern_matrix_individuals <-
function(NorP, Correction = "Chao1", Alpha = 0.05, JackOver = FALSE,  ..., CheckArguments = TRUE)
{
  return (entropart::bcRichness(Ns=as.AbdVector.pattern_matrix_individuals(NorP), Correction=Correction, Alpha=Alpha, JackOver=JackOver, CheckArguments=CheckArguments))
}




#' Shannon's Entropy of a matrix of individuals
#'
#' Calculates the Shannon entropy of a \code{\link{pattern_matrix_individuals}} object.
#'
#'
#' @param Correction A string containing one of the possible corrections: see \code{\link{Shannon}}.
#'
#' @inheritParams Tsallis.pattern_matrix_individuals
#'
#' @return A named number equal to the calculated entropy. The name is that of the bias correction used.
#'
#' @importFrom entropart Shannon
#' @method Shannon pattern_matrix_individuals
#' @export
#'
#' @examples
Shannon.pattern_matrix_individuals <-
function(NorP, Correction = "Best", ..., CheckArguments = TRUE)
{
  return(entropart::bcShannon(Ns=as.AbdVector.pattern_matrix_individuals(NorP), Correction=Correction, CheckArguments=CheckArguments))
}



#' Simpson Entropy of a matrix of individuals
#'
#' Calculates the Simpson entropy of a \code{\link{pattern_matrix_individuals}} object.
#'
#'
#' @param Correction A string containing one of the possible corrections: see \code{\link{Simpson}}.
#'
#' @inheritParams Tsallis.pattern_matrix_individuals
#'
#' @return A named number equal to the calculated entropy. The name is that of the bias correction used.
#'
#' @importFrom entropart Simpson
#' @method Simpson pattern_matrix_individuals
#' @export
#'
#' @examples
Simpson.pattern_matrix_individuals <-
function(NorP, Correction = "Lande", ..., CheckArguments = TRUE)
{
  return(entropart::bcSimpson(Ns=as.AbdVector.pattern_matrix_individuals(NorP), Correction=Correction, CheckArguments=CheckArguments))
}
