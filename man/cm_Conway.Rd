% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CM_Models.R
\docType{class}
\name{cm_Conway}
\alias{cm_Conway}
\title{Conway's game of life}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
cm_Conway
}
\value{
An \code{\link{R6Class}}.
}
\description{
A \code{\link{community_matrixmodel}} where each cell contains or not an individual.
At each generation, an individual may survive or not and empty cells be filled by a new individuals.
}
\details{
The survival and generation rules are fixed by the number of neighbors of each cell.
Edge effects are eliminated by a toroidal correction.
}
\section{Fields}{

\describe{
\item{\code{to_survive}}{The number of neighbors necessary for an individual to survive.}

\item{\code{to_generate}}{The number of neighbors necessary for an empty cell to generate an individual.}
}}

\section{Public Methods}{

\describe{
  \item{\code{initialize(pattern = NULL, timeline = 0)}}{Initialization.}
  \item{\code{plot(..., time=NULL, sleep=animation::ani.options("interval"))}}{Default plot method: plots the pattern.}
  \item{\code{autoplot(..., time=NULL)}}{Makes a \code{\link{ggplot}} of the pattern.}
  \item{\code{run(animate = FALSE, sleep = 0, save = FALSE, more_time = NULL)}}{Run the model.}
  \item{\code{saved_pattern(time)}}{Returns the pattern at the chosen time.}
  \item{\code{along_time(FUN, ...)}}{Applies the function FUN to the saved patterns along time and returns a dataframe with columns \code{x} for time and \code{y} for the results of FUN. FUN must return a single value.}
}
}

\keyword{datasets}