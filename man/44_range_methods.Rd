\name{44_range_methods}
\alias{range.GeomArray}
\alias{range.Grid}
\alias{range.Line}
\alias{range.Points}
\alias{range.Polygon}
\alias{range.Text}
\alias{range.VImage}
\title{Object Spatial Ranges}
\description{
Compute the spatial ranges of GeomObject(s).

In principle, this corresponds to the xlim/ylim arguments in the base graphics system.\cr
(i.e. If you needed to initialize a plotting window, then the range methods give the xlim/ylim values).}
\usage{
\method{range}{GeomArray}(v, \dots)
\method{range}{Grid}(v, \dots)
\method{range}{Line}(v, \dots)
\method{range}{Points}(v, \dots)
\method{range}{Polygon}(v, \dots)
\method{range}{Text}(v, \dots)
\method{range}{VImage}(v, \dots)
}
\arguments{
\item{v}{A GeomObject.}
\item{\dots}{Ignored.}
}
\value{A two-column matrix.}
\examples{
range (TCUBE)
}
