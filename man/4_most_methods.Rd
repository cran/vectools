\name{4_most_methods}
\alias{dim.ObjectArray}
\alias{dim.NestMatrix}
\alias{dim.SectMatrix}
\alias{print.VectorLike}
\alias{format.ObjectArray}
\alias{format.NestMatrix}
\alias{format.SectMatrix}
\alias{as.matrix.SectMatrix}
\alias{head.VectorLike}
\alias{tail.VectorLike}
\title{Most Methods}
\description{Dim, print, format, head and tail methods for vector-like objects.}
\usage{
\method{dim}{ObjectArray}(x)
\method{dim}{NestMatrix}(x)
\method{dim}{SectMatrix}(x)

\method{print}{VectorLike}(x, \dots)

\method{format}{ObjectArray}(x, na.string="", \dots)
\method{format}{NestMatrix}(x, na.string="", \dots)
\method{format}{SectMatrix}(x, na.string="", \dots)

\method{as.matrix}{SectMatrix}(x, \dots)

\method{head}{VectorLike}(x, n=6, \dots)
\method{tail}{VectorLike}(x, n=6, \dots)
}
\details{
Refer to vignette for more information.

Note that the dimensions of object arrays and nested matrices are the dimensions of the top level (or collapsed) object. However, the dimensions of sectioned matrices (including partitioned matrices) are the dimensions or the combined (or expanded) matrix.

Note that the format method for ObjectArray, calls the objtag function, for each of its elements.
}
\value{The format, head and tail functions return formatted character matrices.}
\arguments{
\item{x}{A VectorLike object.}
\item{na.string}{What to format NAs as.}
\item{n}{Number of items.}
\item{\dots}{Other arguments.}
}
\examples{
s = matrix (1:100, 10, 10)
x = as.PartMatrix (s, 5, c (2, 4, 6, 8) )

dim (x)
as.matrix (x)
head (x, 2)
}
