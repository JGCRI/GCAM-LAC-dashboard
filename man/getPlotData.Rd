% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server-helpers.R
\name{getPlotData}
\alias{getPlotData}
\title{Extract and format data for a plot}
\usage{
getPlotData(prjdata, query, pltscen, diffscen, key, filters,
  yearRange = c(2005, 2050))
}
\arguments{
\item{prjdata}{Project data structure}

\item{query}{Name of the query to plot}

\item{pltscen}{Name of the scenario to plot}

\item{diffscen}{Difference scenario, if any}

\item{key}{Aggregation variable.  (e.g., 'region' or 'sector')}

\item{yearRange}{A vector of two integers of form \code{c(start year, end
year)} to filter the data to.}

\item{filters:}{Named list of variables and values to filter on before
aggregating. Values can be character or a list.}
}
\description{
Extract and format data for a plot
}
\keyword{internal}
