\name{11_other_matrix-like_objects}
\alias{VMap-class}
\alias{PartMatrix-class}
\alias{SectMatrix-class}
\alias{VMap}
\alias{PartMatrix}
\alias{SectMatrix}
\alias{as.PartMatrix}
\alias{as.SectMatrix}
\alias{n22}
\title{Other Matrix-Like Objects}
\description{
Partitioned and sectioned matrices.

Note that PartMatrix objects maybe removed or deprecated, in the future.
}
\usage{
VMap (n)

PartMatrix (nr, nc, isep, jsep, \dots, rnames, cnames, default.value=0)
SectMatrix (nr, nc, \dots, vmap, rnames, cnames, default.value=0)

as.PartMatrix (x, isep, jsep, \dots, rnames, cnames)
as.SectMatrix (x, \dots, vmap, rnames, cnames)

n22 (\dots)
}
\arguments{
\item{n}{Integer vector, the dimensions.}
\item{nr, nc}{Integers, the number or rows and columns}
\item{isep, jsep}{
	Optional integer vectors, the inter-row and inter-column indices.\cr
	Ignored, if x is a NestMatrix}
\item{vmap}{A VMap object.}
\item{rnames, cnames}{Character vectors giving the row and column names.}
\item{default.value}{The default value of entries in the matrix.}
\item{x}{
	A matrix or data.frame.\cr
	as.PartMatrix also allows a NestMatrix, if it has conformable submatrices and is non-recursive.}
\item{\dots}{Ignored.}
}
\details{
A PartMatrix is a subclass of a SectMatrix.\cr
(These are S4 classes, which are similar to \link{NestMatrix}).

A PartMatrix contains a matrix, along with a set of separator lines.\cr
(These lines are included, when the object is formatted).

A SectMatrix is similar, but allows arbitrary rectangular sections, defined by the vmap argument.

A VMap object is a subclass of ObjectArray, that requires one or more two by two integer matrices.\cr
Each two by two matrices defines the indices of the section.\cr
The top row is the start and end indices for the rows, and the bottom row is the start and end indices for the columns.

A VMap can have two or more dimesions.\cr
And a VMap is created by the PartMatrix constructor, which is two dimensional.

The n22 function is a convenience function for constructing a one dimensional VMap.\cr
It takes a integer vector, where the length is a multiple of four.

Each consecutive set of four defines the indices for one section.\cr
Within each set, each value describes the row start, row end, column start and column end, in that order.

Note that PartMatrix objects maybe removed or deprecated, in the future.

Also note, do not to access slots directly.\cr
(But rather use functions within this package).
}
\value{
VMap, PartMatrix and SectMatrix are constructors for object of their classes.

n22 returns a VMAP object.
}
\seealso{
\link{ObjectArray}

\link{NestMatrix}\cr
Similar to PartMatrix.

\link{Subsetting Operators}

\link{Standard Methods}
}
\examples{
x <- matrix (1:100, 10, 10)

pm <- as.PartMatrix (x, 5, c (2, 4, 6, 8) )
pm

pm [[1, 4]]

vmap <- n22 (
    1, 10, #1
    1, 10,
    3, 10, #2
    3, 10,
    5, 10, #3
    5, 10,
    7, 10, #4
    7, 10,
    9, 10, #5
    9, 10)
sm <- as.SectMatrix (x, vmap=vmap)
sm

getSect (sm, 4)
}
