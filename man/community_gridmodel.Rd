% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classes.R
\docType{class}
\name{community_gridmodel}
\alias{community_gridmodel}
\title{Community Grid Model Class}
\description{
Community Grid Model Class

Community Grid Model Class
}
\details{
A \link{community_model} whose pattern is a regular, rectangular grid of points.
}
\section{Super class}{
\code{\link[SimComm:community_model]{SimComm::community_model}} -> \code{community_gridmodel}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{tess}}{A tessellation of the window containing the points, used to plot the model.}

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
\item \href{#method-community_gridmodel-new}{\code{community_gridmodel$new()}}
\item \href{#method-community_gridmodel-neighbors}{\code{community_gridmodel$neighbors()}}
\item \href{#method-community_gridmodel-plot}{\code{community_gridmodel$plot()}}
\item \href{#method-community_gridmodel-clone}{\code{community_gridmodel$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="along_time"><a href='../../SimComm/html/community_model.html#method-community_model-along_time'><code>SimComm::community_model$along_time()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="autoplot"><a href='../../SimComm/html/community_model.html#method-community_model-autoplot'><code>SimComm::community_model$autoplot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="run"><a href='../../SimComm/html/community_model.html#method-community_model-run'><code>SimComm::community_model$run()</code></a></span></li>
<li><span class="pkg-link" data-pkg="SimComm" data-topic="community_model" data-id="saved_pattern"><a href='../../SimComm/html/community_model.html#method-community_model-saved_pattern'><code>SimComm::community_model$saved_pattern()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-community_gridmodel-new"></a>}}
\if{latex}{\out{\hypertarget{method-community_gridmodel-new}{}}}
\subsection{Method \code{new()}}{
Create a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{community_gridmodel$new(
  pattern = pattern_grid(),
  timeline = 0,
  type = "Species",
  neighborhood = "von Neumann 1"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pattern}}{The pattern which describes the location of agents.}

\item{\code{timeline}}{A numeric vector that contains the points of time of interest.}

\item{\code{type}}{The type of individuals. Informational only.}

\item{\code{neighborhood}}{A character string defining what is the neighborhood of a cell:
"von Neumann 1" or "4" for the closest four neighbors (North, West, South, East);
"Moore 1" or "8" for all adjacent cells (the first four and North-West, etc.);
"Moore 2" or "24" for two rings of neighbors.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-community_gridmodel-neighbors"></a>}}
\if{latex}{\out{\hypertarget{method-community_gridmodel-neighbors}{}}}
\subsection{Method \code{neighbors()}}{
Return a vector that contains the indices of neighbors in the point pattern.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{community_gridmodel$neighbors(point)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{point}}{The focal point around which a neighborhood is defined.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-community_gridmodel-plot"></a>}}
\if{latex}{\out{\hypertarget{method-community_gridmodel-plot}{}}}
\subsection{Method \code{plot()}}{
Plots the pattern.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{community_gridmodel$plot(
  time = NULL,
  sleep = 0,
  which.marks = "PointType",
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{time}}{The point of the timeline considered.
Its value should be in \code{timeline}.}

\item{\code{sleep}}{The time (in ms) to sleep between each step of the evolution of the model.}

\item{\code{which.marks}}{The marks to plot, that must be a the name of a column of the mark dataframe.}

\item{\code{...}}{Extra arguments to be passed to methods.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-community_gridmodel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-community_gridmodel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{community_gridmodel$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
