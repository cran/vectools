\name{70_outer_generalizations}
\alias{fgrid}
\alias{dgrid}
\alias{cgrid}
\title{Outer_Generalizations}
\description{Outer product generalizations, allowing an arbitrary function and an arbitrary number of variables.}
\usage{
fgrid (f=`*`, \dots, combine=FALSE, drop=TRUE, iterate=FALSE,
    combine.function=cbind)

dgrid (f=`*`, \dots, combine=FALSE, drop=TRUE, iterate=FALSE,
    combine.function=cbind)
cgrid (f=`*`, \dots, combine=FALSE, drop=TRUE, iterate=FALSE,
    combine.function=cbind, n)
}
\arguments{
\item{f}{
	A function.\cr
	By default, the number of arguments needs to match the main argument list.\cr
	And the length of the return value needs to match the length of the arguments.}
\item{\dots}{
	The main argument list.\cr
	By default, the number of arguments needs to match the function, f.\cr
	The fgrid function can take any non-empty vectors, giving the evaluation bins/points.\cr
	The dgrid function takes integer vectors, of length one or two, giving evaluation constants/intervals.\cr
	The cgrid function takes numeric vectors, of length one or two, giving evaluation constants/intervals.}
\item{combine}{Logical, if true, the main arguments are combined via combine.function, and then the function, f, is called with a single argument.}
\item{drop}{Logical, if true, array dimensions with one value are dropped.}
\item{iterate}{
	Logical, if true, evaluate the function with one bin/point, at a time.
	Note that this is not done in an efficient way, you'll get a faster result if you vectorize your function.}
\item{combine.function}{
	A function used to combine the main arguments.\cr
	Ignored, unless combine is true.}
\item{n}{
	Integer, of length one, or equal to the length of the main arguments, giving the number of evaluation points, in each dimension.\cr
	Ignored, where the main arguments are constants\cr
	(Rather than pairs defining ranges, or sequences).}
}
\value{
An array.

By default (with drop=TRUE), the number of dimensions will equal the number of main arguments, except where any dimension has a single evaluation bin/point.
}
\examples{
f3a <- function (x, y, z)
    x + 10 * y + 100 * z
f3b <- function (x)
    x [,1] + 10 * x [,2] + 100 * x [,3]

x <- seq (0, 10, length.out=4)
fgrid (f3a, x, x, x)

#same as above, but using xlim/ylim style input
cgrid (f3a, c (0, 10), c (0, 10), c (0, 10), n=4)
cgrid (f3b, c (0, 10), c (0, 10), c (0, 10), combine=TRUE, n=4)

#drop argument
#(here, the default drops the first dimension)
cgrid (f3a, 0, c (0, 10), c (0, 10), drop=FALSE, n=4)
cgrid (f3a, 0, c (0, 10), c (0, 10), n=4)

#dropping two dimensions
cgrid (f3a, c (0, 10), 0, 0, n=4)

#different n values
cgrid (f3a, c (0, 10), c (0, 10), c (0, 10), n = c (2, 3, 4) )
}
