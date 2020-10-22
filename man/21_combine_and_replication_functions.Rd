\name{21_combine_and_replication_functions}
\alias{c.MatrixArray}
\alias{rep.MatrixArray}
\alias{rep2.MatrixArray}
\title{Replication Functions}
\description{A combine and rep method for MatrixArray and a function similar to rep_len.}
\usage{
\method{c}{MatrixArray}(x, \dots)

\method{rep}{MatrixArray}(x, n=1, \dots, each=1, times=n)
\method{rep2}{MatrixArray}(x, n, \dots, n.out=n)
}
\arguments{
\item{x}{A 1d MatrixArray object.}
\item{\dots}{
	In the combine function, further MatrixArray objects.\cr
	In other functions, ignored.}
\item{n}{Refer to the each and n.out arguments.}
\item{each}{The number of times, each element is repeated, consecutively.}
\item{times}{The number of times, the sequence is repeated, periodically.}
\item{n.out}{The length of the output object.}
}
\value{A new MatrixArray object.}
\examples{
rep (eq.brot2 (4), 10)
}
