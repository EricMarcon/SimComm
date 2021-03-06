% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CM_Models.R
\docType{class}
\name{cm_drift}
\alias{cm_drift}
\title{Community Drift}
\value{
An \code{\link{R6Class}}.
}
\description{
A \code{\link{community_matrixmodel}} where each cell contains an individual. Marks are species.
At each generation, each individual is replaced by one of its neighbors.
}
\details{
Edge effects are eliminated by a toroidal correction.
}
\section{Public Methods}{

\describe{
  \item{\code{initialize(pattern = NULL, timeline = 0, type = "Species")}}{Initialization.}
  \item{\code{plot(time=NULL, sleep=animation::ani.options("interval"), ...)}}{Default plot method: plots the pattern.}
  \item{\code{autoplot(time=NULL, ...)}}{Makes a \code{\link{ggplot}} of the pattern.}
  \item{\code{run(animate = FALSE, sleep = animation::ani.options("interval"), save = FALSE, more_time = NULL)}}{Run the model.}
  \item{\code{saved_pattern(time)}}{Returns the pattern at the chosen time.}
  \item{\code{along_time(FUN, ...)}}{Applies the function FUN to the saved patterns along time and returns a dataframe with columns \code{x} for time and \code{y} for the results of FUN. FUN must return a single value.}
}
}

\examples{
myModel <- community_matrixmodel$new()
myModel$autoplot()
}
\section{Super classes}{
\code{\link[SimComm:community_model]{SimComm::community_model}} -> \code{\link[SimComm:community_matrixmodel]{SimComm::community_matrixmodel}} -> \code{cm_drift}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{neighborhood}}{A character string defining what is the neighborhood of a cell:
"von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
"Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
"Moore 2" or "24" for two rings of neighbors.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{neighborhood}}{A character string defining what is the neighborhood of a cell:
"von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
"Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
"Moore 2" or "24" for two rings of neighbors.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-clone}{\code{cm_drift$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="along_time">}\href{../../SimComm/html/community_model.html#method-along_time}{\code{SimComm::community_model$along_time()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="run">}\href{../../SimComm/html/community_model.html#method-run}{\code{SimComm::community_model$run()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="saved_pattern">}\href{../../SimComm/html/community_model.html#method-saved_pattern}{\code{SimComm::community_model$saved_pattern()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_matrixmodel" data-id="autoplot">}\href{../../SimComm/html/community_matrixmodel.html#method-autoplot}{\code{SimComm::community_matrixmodel$autoplot()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_matrixmodel" data-id="initialize">}\href{../../SimComm/html/community_matrixmodel.html#method-initialize}{\code{SimComm::community_matrixmodel$initialize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_matrixmodel" data-id="neighbors">}\href{../../SimComm/html/community_matrixmodel.html#method-neighbors}{\code{SimComm::community_matrixmodel$neighbors()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_matrixmodel" data-id="plot">}\href{../../SimComm/html/community_matrixmodel.html#method-plot}{\code{SimComm::community_matrixmodel$plot()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="SimComm" data-topic="community_matrixmodel" data-id="prepare_buffer">}\href{../../SimComm/html/community_matrixmodel.html#method-prepare_buffer}{\code{SimComm::community_matrixmodel$prepare_buffer()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{cm_drift$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
