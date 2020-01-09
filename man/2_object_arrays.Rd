\name{2_object_arrays}
\alias{VectorLike-class}
\alias{ObjectArray-class}
\alias{ObjectArray}
\alias{as.ObjectArray}
\title{Object Arrays}
\description{One, two and higher-dimensional object arrays.}
\usage{
ObjectArray (dim, names, default.value=NA)
as.ObjectArray (x, dim, names)
}
\details{
ObjectArray(s) are similar to list matrices and list arrays, however, there are some differences, especially, in the way that they're formatted.
}
\arguments{
\item{dim}{Dimensions, same as dim, in standard array objects.}
\item{names}{List of names, which should match dim, above.}
\item{default.value}{Default value, for each element.}
\item{x}{A list with the same length as prod (dims).}
}
\examples{
x = ObjectArray (c (2, 2) )
x [[1, 1]] = x [[2, 2]] = x
x
}
