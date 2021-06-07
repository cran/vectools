\name{31_replication_functions}
\alias{rep.ObjectArray}
\alias{rep.ImageArray}
\alias{rep.GeomArray}
\alias{rep2.ObjectArray}
\alias{rep2.GeomArray}
\alias{rep2.ImageArray}
\alias{rep.MatrixArray}
\alias{rep2.MatrixArray}
\title{Replication Functions}
\description{rep methods, and rep2 methods, functions similar to rep_len.}
\usage{
\method{rep}{ObjectArray}(x, n=1, \dots, each=1, times=n)
\method{rep}{ImageArray}(x, n=1, \dots, each=1, times=n)
\method{rep}{GeomArray}(x, n=1, \dots, each=1, times=n)
\method{rep}{MatrixArray}(x, n=1, \dots, each=1, times=n)

\method{rep2}{ObjectArray}(v, n, \dots, n.out=n)
\method{rep2}{GeomArray}(v, n, \dots, n.out=n)
\method{rep2}{ImageArray}(v, n, \dots, n.out=n)
\method{rep2}{MatrixArray}(v, n, \dots, n.out=n)
}
\arguments{
\item{x, v}{A one-dimensional ObjectArray (incl subclasses).}
\item{n}{Default value for n.out and times, see below.}
\item{each}{Integer, the number of times, each element is repeated, consecutively.}
\item{times}{Integer, the number of times, the sequence is repeated, periodically.}
\item{n.out}{Integer, the length of the output object.}
\item{\dots}{Ignored.}
}
\value{
A new one-dimensional ImageArray, ObjectArray, GeomArray or MatrixArray object.
}
\examples{
rep (eq.brot2 (4), 10)
}
