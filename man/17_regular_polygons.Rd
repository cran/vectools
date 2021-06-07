\name{17_regular_polygons}
\alias{Cuboid-class}
\alias{Rect-class}
\alias{regPolygon}
\alias{Rect}
\alias{Cuboid}
\title{Regular Polygons}
\description{
Regular polygons.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
regPolygon (n=4, \dots, about.axis, d=1, stagger = (n \%\% 2 == 0) )

Rect (\dots, about.axis, center=FALSE, side.length=1,
    glist = list () )
Cuboid (\dots, center=FALSE, side.length=1, glist = list () )
}
\arguments{
\item{n}{Integer, the number of points.}
\item{d}{Numeric, the distance.}
\item{about.axis}{
	A single character equal to "x", "y", z".\cr
	If missing (the default), the object is in 2d space.\cr
	If true, a 2d object is created in 3d space.}
\item{stagger}{
	Logical, if false, the first point is at the top.\cr
	Otherwise, it's rotated half the angle.\cr
	(That is, the angle equal to 2pi divided by n).}
\item{center}{Logical, if true, the object is centered about the origin, otherwise, it has the lowermost corner at the origin.}
\item{side.length}{Numeric vector of length one or two, the x and y lengths.}
\item{glist}{
	Argument lists for plotting functions.\cr
	Note that, in Cuboid, it's applied to each face.}
\item{\dots}{Ignored.}
}
\value{A regPolygon, Rect or Cuboid object.}
\examples{
vt3.testplot (Rect (side.length = c (2, 1) ) )
}
