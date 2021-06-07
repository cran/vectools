\name{40_test_plots}
\alias{Test Plots}
\alias{vt3.testplot}
\alias{vt3.testplot3}
\alias{spin3d}
\title{Test Plots}
\description{
Simple plotting functions for testing both 2d and 3d transformations.

Note that currently, the spin3d function only doesn't work well for objects, where the x/y/z ranges are considerably different.\cr
e.g. An x and y range between -10 and 10, and a z range between 0 and 1000.
}
\usage{
vt3.testplot (v=SQUARE, \dots,
    main="", xlab="x", ylab="y",
    xlim = c (-4.5, 4.5), ylim=xlim,
    grid.lines=TRUE, cross.hairs=grid.lines,
    proj="xy", sort=TRUE)

vt3.testplot3 (v=CUBE, \dots,
    main = c ("proj=xy", "proj=xz", "proj=yz"),
    xlab="x", ylab="y", zlab="z",
    xlim = c (-4.5, 4.5), ylim=xlim, zlim=xlim,
    grid.lines=TRUE, cross.hairs=grid.lines,
    sort=TRUE)

spin3d (v=CUBE, \dots, t=5)
}
\arguments{
\item{v}{
	A GeomObject, which includes a GeomArray.}
\item{main, xlab, ylab, zlab}{
	Strings, the main, x axis and y axis titles.\cr
	Note that in 3D plots, these map from 3D to 2D.}
\item{xlim, ylim, zlim}{
	Numeric vectors of length two, giving the x, y and z ranges.\cr
	Note that in 3D plots, these map from the 3D coordinates to 2D coordinates.}
\item{grid.lines}{Logical, add grid lines.}
\item{cross.hairs}{Logical, add lines running through the origin.}
\item{proj}{String, either "xy", "xz" or "yz", projection for 3D objects.}
\item{sort}{Logical, if true (the default), depth-sort 3D objects.}
\item{t}{Numeric, approximate number of seconds.}
\item{\dots}{Ignored.}
}
\seealso{
\link{MatrixArray}

\link{Binary Operators}\cr
This describes how to perform operations on MatrixArray objects.

\link{Pre-Multiplication Transformation Matrices}

\link{Post-Multiplication Transformation Matrices}

\link{SQUARE}, \link{CUBE}
}
\examples{
vt3.testplot (SQUARE \%]*\% brot2 (pi / 8) )

vt3.testplot3 (TCUBE)

if (interactive () )
	spin3d (v=CUBE)
}
