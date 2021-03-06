% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server-helpers.R
\name{plotTime}
\alias{plotTime}
\title{Plot values over time as a bar chart}
\usage{
plotTime(prjdata, query, scen, diffscen, subcatvar, filters)
}
\arguments{
\item{prjdata}{A project data structure}

\item{query}{Name of the query to plot}

\item{scen}{Name of the scenario to plot}

\item{diffscen}{Name of the difference scenario, or NULL if none}

\item{subcatvar}{Variable to use for subcategories in the plot}

\item{filters}{Named list of variables and values to filter to}
}
\description{
If it is possible to build a plot with the data, this function will return
a list containing the data frame being plotted as the second element.
}
