\name{01_subsetting_operators}
\alias{Subsetting Operators}
\alias{[.ObjectArray}
\alias{[[.ObjectArray}
\alias{[[<-.ObjectArray}
\alias{[[.NestMatrix}
\alias{[[<-.NestMatrix}
\alias{[[.PartMatrix}
\alias{[[<-.PartMatrix}
\alias{[.GeomArray}
\alias{[.ImageArray}
\alias{[.MatrixArray}
\alias{[[<-.GeomArray}
\alias{getSect}
\title{Subsetting Operators}
\description{Subsetting operators for vector-like objects.}
\usage{
\method{[}{ObjectArray}(v, \dots)
\method{[[}{ObjectArray}(v, \dots)
\method{[[}{ObjectArray}(v, \dots) <- value

\method{[[}{NestMatrix}(v, i, j, \dots, drop=TRUE, zero=TRUE)
\method{[[}{NestMatrix}(v, i, j) <- value

\method{[[}{PartMatrix}(v, i, j, \dots, drop=TRUE)
\method{[[}{PartMatrix}(v, i, j) <- value

\method{[}{GeomArray}(v, \dots)
\method{[}{ImageArray}(v, \dots)
\method{[}{MatrixArray}(v, \dots)
\method{[[}{GeomArray}(v, \dots) <- value

#for SectMatrix
getSect (v, \dots, drop=TRUE)
}
\arguments{
\item{v}{An ObjectArray (including subclasses) or block matrix object.}
\item{i, j, \dots}{The indices.}
\item{drop}{Logical, if true, reduce the number of dimensions, where possible.}
\item{zero}{Logical, if true, replace ZERO with zero, or a zero matrix.}
\item{value}{A suitable value to assign.}
}
\details{
ObjectArray (and NestMatrix), are similar to lists, so:\cr
Single bracket subsetting returns another ObjectArray, which is a subset of the original.\cr
Double bracket subsetting gets or sets a single element within the ObjectArray.

For PartMatrix:\cr
Currently, single bracket subsetting is not supported.\cr
Currently, double bracket subsetting indexes a single submatrix.

Note that the number of indices used in an ObjectArray depends on the number of dimensions.

The number of indices in PartMatrix objects is always two.\cr
And the number of indices for SectMatrix subsetting depends on the VMap object.

Note that there are restrictions on what can be assigned:\cr
An ObjectArray may be restricted by its CLASS slot, determined when the object is constructed.\cr
Other objects may have restrictions on the sizes of submatrices.
}
\seealso{
\link{ObjectArray}

\link{NestMatrix}

\link{PartMatrix}

\link{SectMatrix}
}
\examples{
x <- matrix (1:16, 4, 4)

pm <- as.PartMatrix (x, 2, 2)
nm <- as.NestMatrix (pm)

#all same
x [1:2, 1:2]
nm [[1, 1]]
pm [[1, 1]]

#all same
x [1, 2]
nm [[1, 1]][1, 2]
pm [[1, 1]][1, 2]
}
