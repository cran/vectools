\name{65_outer_generalizations_1}
\alias{FGrid-class}
\alias{DGrid-class}
\alias{CGrid-class}
\alias{StepGrid-class}
\alias{DGrid}
\alias{CGrid}
\alias{StepGrid}
\alias{dgrid}
\alias{cgrid}
\alias{stepgrid}
\title{Outer Generalizations 1}
\description{
Constructors for S4 objects, containing a matrix (gv) and coordinates (x and y), or (xb and yb).

Essentially, the gv slot is equivalent to the matrix returned by base::outer, over the x and y values.\cr
DGrid is for integer inputs and CGrid is for numeric inputs.

The StepGrid version is for step functions, where xb and yb represent breakpoints.\cr
The gv values are evaluated using the midpoints.

The \link{xgrid.hdim} function and related functions, can be used to produce arrays in higher dimensions.
}
\usage{
DGrid (x = 1:nrow (gv), y = 1:ncol (gv), gv)
CGrid (x = 1:nrow (gv), y = 1:ncol (gv), gv)
StepGrid (xb = 0:nrow (gv) + 0.5, yb = 0:ncol (gv) + 0.5, gv)

dgrid (sf=`*`, xlim, ylim=xlim, \dots, cf, iterate=FALSE, x, y)
cgrid (sf=`*`, xlim, ylim=xlim, \dots, n=8, cf, iterate=FALSE, x, y)
stepgrid (sf=`*`, xlim, ylim=xlim, \dots, n=8, cf, iterate=FALSE, xb, yb)
}
\arguments{
\item{sf}{
	A suitable function.\cr
	It's first argument should be a matrix/list/etc, if cf provided, otherwise it should have leading arguments, for x and y.}
\item{xlim, ylim}{
	Length-two integer (for DGrid) or numeric (for CGrid/SGrid) vectors.\cr
	These give the limits for the sequences.}
\item{n}{
	Integer vector of length one or two, the number or rows and columns.\cr
	For CGrid, this is also the length of the x and y sequences.\cr
	For StepGrid, xb and yb will be one longer.}
\item{iterate}{
	Logical, if true, evaluate the function with one bin/point, at a time.
	Note that this is not done in an efficient way, you'll get a faster result if you vectorize your function.}
\item{cf}{
	A function of two vectors, used to combine x and y into a single object.\cr
	The sf function is called using the result of the cf function.}
\item{x, y}{Integer or numeric vectors.}
\item{xb, yb}{Same as x and y, but one longer, for breakpoints/endpoints.}
\item{gv}{A matrix of grid values, equivalent to the result of sf.}
\item{\dots}{Currently, ignored.}
}
\value{
An DGrid, CGrid or SGrid object.
}
\seealso{
\link{dgrid.hdim}, \link{cgrid.hdim}, \link{xgrid.hdim}
}
\examples{
f <- function (v)
    v [,1] + v [,2]

#simple addition problem
dgrid (`+`, c (0, 4), c (1, 5) )

#same, but f operates on matrix
dgrid (f, c (0, 4), c (1, 5), cf=cbind)
}
