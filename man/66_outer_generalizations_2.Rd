\name{66_outer_generalizations_2}
\alias{dgrid.hdim}
\alias{cgrid.hdim}
\alias{xgrid.hdim}
\title{Outer_Generalizations}
\description{
Outer product generalizations, allowing an arbitrary function and an arbitrary number of variables.

Note that the \link{DGrid} and \link{CGrid} functions provide additional functionality in the 2D case.
}
\usage{
dgrid.hdim (sf=`*`, \dots, cf, lims = rbind (...), drop=TRUE, iterate=FALSE)
cgrid.hdim (sf=`*`, \dots, cf, lims = rbind (...), drop=TRUE, iterate=FALSE, n=10)

xgrid.hdim (sf=`*`, \dots, cf, seqs = list (...), drop=TRUE, iterate=FALSE)
}
\arguments{
\item{sf}{
	A suitable function.\cr
	By default, the number of arguments needs to match the main argument list.\cr
	(But a single argument is required if cf is not missing).\cr
	And the length of the return value needs to match the length of the arguments.}
\item{lims}{
	A two-column matrix of limits.\cr
	If a row is constant, then a constant (rather than sequence) is used for evaluation.\cr
	(And if drop is true, then there will be one less dimensions in the result).}
\item{seqs}{A list of vectors.}
\item{\dots}{
	Alternative ways of specifying lims and seqs, see above.\cr
	In dgrid.hdim and cgrid.hdim these should be pairs of integers and numerics, respectively.\cr
	(Or a single value, for a constant).}
\item{drop}{Logical, if true, array dimensions with one value are dropped.}
\item{iterate}{
	Logical, if true, evaluate the function with one bin/point, at a time.\cr
	Note that this is not done in an efficient way, you'll get a faster result if you vectorize your function.}
\item{cf}{A function used to combine the main arguments.}
\item{n}{
	Integer vector, of length one, or equal to the length of the main arguments, giving the number of evaluation points, in each dimension.\cr
	Ignored, where the main arguments are constants.\cr
	(Rather than pairs defining ranges, or sequences).}
}
\value{
An array.

By default (with drop=TRUE), the number of dimensions will equal the number of main arguments, except where any dimension has a single evaluation bin/point.
}
\seealso{
\link{FGrid-class}
}
\examples{
f3a <- function (x, y, z)
    x + 10 * y + 100 * z
f3b <- function (x)
    x [,1] + 10 * x [,2] + 100 * x [,3]

x <- seq (0, 10, length.out=4)
xgrid.hdim (f3a, x, x, x)

#same as above, but using xlim/ylim style input
cgrid.hdim (f3a, c (0, 10), c (0, 10), c (0, 10), n=4)
cgrid.hdim (f3b, c (0, 10), c (0, 10), c (0, 10), cf=cbind, n=4)

#drop argument
#(here, the default drops the first dimension)
cgrid.hdim (f3a, 0, c (0, 10), c (0, 10), drop=FALSE, n=4)
cgrid.hdim (f3a, 0, c (0, 10), c (0, 10), n=4)

#dropping two dimensions
cgrid.hdim (f3a, c (0, 10), 0, 0, n=4)

#different n values
cgrid.hdim (f3a, c (0, 10), c (0, 10), c (0, 10), n = c (2, 3, 4) )
}
