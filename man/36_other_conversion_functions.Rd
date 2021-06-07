\name{36_other_conversion_functions}
\alias{as.list.ObjectArray}
\alias{as.list.MImage}
\alias{as.matrix.SImage}
\alias{as.matrix.NestMatrix}
\alias{as.matrix.SectMatrix}
\title{Other Conversion Functions}
\description{Functions to convert ObjectArray(s) to lists, and NestMatrix and SectMatrix objects to matrices.}
\usage{
\method{as.list}{ObjectArray}(x, \dots)

\method{as.list}{MImage}(x, \dots)
\method{as.matrix}{SImage}(x, \dots)

\method{as.matrix}{NestMatrix}(x, \dots)
\method{as.matrix}{SectMatrix}(x, \dots)
}
\arguments{
\item{x}{
	An ObjectArray/NestMatrix or PartMatrix/SectMatrix object. \cr
	Note that nested matrices need to be conformable and non-recursive.}
\item{\dots}{Ignored.}
}
\examples{
x <- NestMatrix (2, 2)
x [[1, 1]] <- matrix ("AA", 2, 2)
x [[1, 2]] <- matrix ("AB", 2, 2)
x [[2, 1]] <- matrix ("BA", 2, 2)
x [[2, 2]] <- matrix ("BB", 2, 2)

x
as.matrix (x)
}
