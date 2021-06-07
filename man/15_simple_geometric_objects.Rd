\name{15_simple_geometric_objects}
\alias{GeomObject-class}
\alias{Points-class}
\alias{Line-class}
\alias{Polygon-class}
\alias{Text-class}
\alias{Points}
\alias{Line}
\alias{Polygon}
\alias{Text}
\alias{as.Points}
\alias{as.Line}
\alias{as.Polygon}
\title{Simple Geometric Objects.}
\description{
Points, lines, polygons and text, as S4 objects.

These can be plotting using either the base or grid graphics systems.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
Points (x, y, z=NULL, \dots, glist = list () )
Line (x, y, z=NULL, \dots, glist = list () )
Polygon (x, y, z=NULL, \dots, glist = list () )
Text (text, x, y, z=NULL, \dots, glist = list () )

as.Points (v, \dots)
as.Line (v, \dots)
as.Polygon (v, \dots)
}
\arguments{
\item{x, y, z}{
	Equal length numeric vectors, giving coordinates.\cr
	If z missing, a 2d object, otherwise, a 3d object.}
\item{v}{Currently, a two or three column matrix.}
\item{text}{
	A character vector of labels.\cr
	x, y and z, need to be the same length.}
\item{glist}{Argument list for plotting functions.}
\item{\dots}{Ignored.}
}
\value{
Each function returns an S4 object with the corresponding class name.\cr
e.g. Line returns a Line object.
}
\seealso{
\link{Grid}, \link{VImage}

\link{regPolygon}
}
\examples{
plot (Points (1:4, 1:4) )
}
