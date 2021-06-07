\name{18_rectangular_grids}
\alias{rectGrid}
\alias{rectVImage}
\title{Regularly-Spaced Grids}
\description{
Regularly-spaced or semi-regularly-spaced grids.

These functions take two vectors and a matrix.\cr
For more general "grids", you can use the \link{Grid} and \link{VImage} functions, which take three matrices.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
rectGrid (x, y, gv=NULL, \dots, glist = list (),
    vlist=NULL)
rectVImage (x, y, gv=NULL, \dots, tf=FALSE,
    colm, glist = list () )
}
\arguments{
\item{x, y}{Equal length (ascending or descending) numeric vectors, giving the grid points, in x and y directions.}
\item{gv}{
	A numeric matrix, giving the "z" value at each "x" and "y" pair.\cr
	If null, a 2D grid is greated.\cr
	Can also be a scalar value, but this is mainly for testing and demo purposes.}
\item{glist, vlist}{
	Argument lists of graphical parameters using in the plotting functions.\cr
	glist applies to all polygons, in vlist the elements should be matrices, where each value is applied to separate polygons.}
\item{tf}{Logical, transpose and flip gv, for rendering raster-style images, using the standard coordinate system.}
\item{colm}{A character matrix, of colors.}
\item{\dots}{Ignored.}
}
\details{
Note that vlist and colm arguments should have dimensions one less than the nrow/ncol of gv.
}
\value{A Grid or VImage object.}
\seealso{
\link{Grid}, \link{VImage}

\link{as.Grid}\cr
Which can be used on \link{FGrid-class} objects.
}
\examples{
library (png)

#adapted from png package
im0 <- readPNG (system.file ("img", "Rlogo.png", package="png") )

#standard raster object
colm <- as.raster (im0)
headt (colm)

#smaller version
I <- seq (1, nrow (colm), 5)
J <- seq (1, ncol (colm), 5)
colm <- colm [I, J]
headt (colm)

#VImage
x <- seq (-1, 1,, ncol (colm) + 1)
y <- seq (-1, 1,, nrow (colm) + 1)
v <- rectVImage (x, y, 0, tf=TRUE, colm=colm)

if (interactive () )
    spin3d (v \%]*\% brot3x (pi / 4), t=10)
}
