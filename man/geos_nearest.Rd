% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geos-strtree.R
\name{geos_nearest}
\alias{geos_nearest}
\alias{geos_nearest_indexed}
\alias{geos_nearest_hausdorff}
\alias{geos_nearest_frechet}
\title{Find the closest feature}
\usage{
geos_nearest(geom, tree)

geos_nearest_indexed(geom, tree)

geos_nearest_hausdorff(geom, tree, densify = NULL)

geos_nearest_frechet(geom, tree, densify = NULL)
}
\arguments{
\item{geom}{A \link[=as_geos_geometry]{GEOS geometry vector}}

\item{tree}{A \code{\link[=geos_strtree]{geos_strtree()}}}

\item{densify}{A fraction between 0 and 1 denoting the degree to which
edges should be subdivided (smaller value means more subdivisions).
Use NULL to calculate the distance as-is.}
}
\value{
An integer vector of length \code{geom} containing the index
of \code{tree} that is closest to each feature in \code{geom}.
}
\description{
Finds the closest item index in \code{tree} to \code{geom}, vectorized along \code{geom}.
}
