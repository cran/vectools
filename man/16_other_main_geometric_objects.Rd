\name{16_other_main_geometric_objects}
\alias{Grid-class}
\alias{VImage-class}
\alias{Grid}
\alias{VImage}
\alias{as.Grid}
\title{Other Main Geometric Objects}
\description{
Geometric objects, resembling a grid, containing quadralaterals.

These can be plotting using either the base or grid graphics systems.

These functions take three matrices.\cr
For regularly-spaced or semi-regularly-spaced grids, it may be easier to use the \link{rectGrid} and \link{rectVImage} functions, which take two vectors and a matrix.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
Grid (x, y, gv=NULL, \dots, glist = list (), vlist=NULL)
VImage (x, y, gv=NULL, \dots, tf=FALSE, colm, glist = list () )

as.Grid (v, \dots, glist = list(), vlist = NULL)
}
\arguments{
\item{x, y}{
	Numeric matrices of coordinates.}
\item{gv}{
	A numeric matrix, giving the "z" value at each "x" and "y" pair.\cr
	If null, a 2D grid is created.\cr
	Can also be a scalar value, but this is mainly for testing and demo purposes.}
\item{v}{Currently, an FGrid object, such as one returned by the \link{cgrid} function.}
\item{glist}{Argument list for plotting functions.}
\item{vlist}{
	Same as glist, except that each element is a matrix, corresponding to each quadralateral.\cr
	The number of rows and columns for each element, should be one less than gv.\cr
	Note that glist and vlist should contain distinct names.}
\item{tf}{
	Logical, transpose and flip colm.\cr
	By default, x and y have their standard meanings.\cr
	Setting tf to true, allows raster-style data to be rendered properly.}
\item{colm}{
	A character matrix of colors.\cr
	The number of rows and columns for each element, should be one less than gv.}
\item{\dots}{Ignored.}
}
\details{
Note that the plot methods for VImage will plots the colors (like a heatmap) if colm is provided.\cr
Otherwise, it will plot a grid of lines.
}
\value{A Grid or VImage object.}
\seealso{
\link{Points}, \link{Line}, \link{Polygon}

\link{RImage-class}

\link{rectGrid}, \link{rectVImage}\cr
Refer to these functions, for examples.\cr
(These use simple vectors for x and y, rather than matrices).
}
