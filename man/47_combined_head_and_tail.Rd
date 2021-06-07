\name{47_combined_head_and_tail}
\alias{Combined Head and Tail Functions}
\alias{headt.ObjectArray}
\alias{headt.MatrixLike}
\alias{headt.data.frame}
\alias{headt.matrix}
\alias{headt.raster}
\alias{headt.default}
\title{Combined Head and Tail Functions}
\description{Combined head and tail functions.}
\usage{
\method{headt}{ObjectArray}(v, nh=3, nt=nh, \dots)
\method{headt}{MatrixLike}(v, nh=3, nt=nh, \dots)
\method{headt}{data.frame}(v, nh=3, nt=nh, \dots)
\method{headt}{matrix}(v, nh=3, nt=nh, \dots)

\method{headt}{raster}(v, nh=3, nt=nh, \dots)

\method{headt}{default}(v, nh=3, nt=nh, \dots)
}
\arguments{
\item{v}{A suitable object.}
\item{nh, nt}{
	Integers, the number of head/tail elements/rows.\cr
	In matrix-based objects, can also be a vector of length two.\cr
	(In which case, the second value applies to the number of columns).\cr\cr
	Note that currently, in PartMatrix and SectMatrix objects, nh and nt includes formatted lines.\cr
	However, this may change, in the future.}
\item{\dots}{Ignored.}
}
\value{
Currently, some of the functions return formatted character matrices, however, it's possible this may change in the future.
}
\seealso{
\link{ghead.data.frame}
}
\examples{
x <- matrix (1:2700, 30, 90)
rsep <- seq (3, 27, 3)
csep <- seq (3, 87, 3)

pm <- as.PartMatrix (x, rsep, csep)

headt (pm, 10, c (5, 1) )
}
