\name{25_block_matrices}
\alias{VMap-class}
\alias{NestMatrix-class}
\alias{PartMatrix-class}
\alias{SectMatrix-class}
\alias{VMap}
\alias{NestMatrix}
\alias{PartMatrix}
\alias{SectMatrix}
\alias{as.NestMatrix}
\alias{as.PartMatrix}
\alias{as.SectMatrix}
\alias{n22}
\title{Block Matrices}
\description{
Block matrices.\cr
(NestMatrix and PartMatrix objects).

PartMatrix objects are generalized by SectMatrix objects, which allow overlapping sections.

Note that PartMatrix objects maybe removed or deprecated, in the future.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
VMap (n)

NestMatrix (nr, nc, \dots, rnames, cnames, conform=TRUE, recursive=FALSE,
    default.value=ZERO)
as.NestMatrix (v, \dots, rnames, cnames, conform=TRUE, recursive=FALSE)

PartMatrix (nr, nc, rsep, csep, \dots, rnames, cnames, default.value=0)
SectMatrix (nr, nc, \dots, vmap, rnames, cnames, default.value=0)

as.PartMatrix (v, rsep, csep, \dots, rnames, cnames)
as.SectMatrix (v, \dots, vmap, rnames, cnames)

n22 (\dots)
}
\arguments{
\item{n}{Integer, the number of submatrices.}
\item{nr, nc}{Integers, the number or rows and columns}
\item{rsep, csep}{
	Optional integer vectors, the inter-row and inter-column indices.\cr
	Ignored, if v is a NestMatrix.}
\item{vmap}{A VMap object.}
\item{rnames, cnames}{
	Character vectors of nr row names and nc column names.\cr
	May be NULL.\cr
	Currently ignored, when a NestMatrix is constructed from a PartMatrix.}
\item{default.value}{The default value of entries in the matrix.}
\item{v}{
	A matrix or data.frame.\cr
	as.PartMatrix also allows a NestMatrix, if it has conformable submatrices and is non-recursive.}
\item{conform}{
	Logical, if true, submatrices are required to have conformable dimensions.\cr
	Note that a NestMatrix may contain ZERO(s) regardless of the whether there's a conformable requirement or not.}
\item{recursive}{
	Logical, if true, subelements can be nested matrices.\cr
	Otherwise, they must be ZERO or standard matrices.}
\item{\dots}{Ignored.}
}
\details{
A NestMatrix is a subclass of an ObjectArray that contains ZERO or standard matrices.\cr
If recursive is true, then it may also contain other NestMatrix objects.

Note that NestMatrix elements can have a scalar value assigned, but it will be converted to a 1x1 matrix.

A PartMatrix is a subclass of a SectMatrix.\cr
(These are similar to NestMatrix).

A PartMatrix contains a matrix, along with a set of separator lines.\cr
(These lines are included, when the object is printed/formatted).

A SectMatrix is similar, but allows arbitrary rectangular sections, defined by the vmap argument.

A VMap object is a subclass of ObjectArray, that requires one or more two by two integer matrices.\cr
Each two by two matrices defines the indices of the section.\cr
The top row is the start and end indices for the rows, and the bottom row is the start and end indices for the columns.

A VMap can have more than one dimensions.\cr
And a VMap is created by the PartMatrix constructor, which is two dimensional.

The n22 function is a convenience function for constructing a one dimensional VMap.\cr
It takes a integer vector, where the length is a multiple of four.

Each consecutive set of four defines the indices for one section.\cr
Within each set, each value describes the row start, row end, column start and column end, in that order.
}
\value{
VMap, NestMatrix, PartMatrix and SectMatrix are constructors for object of their classes.

n22 returns a VMAP object.
}
\seealso{
\link{ObjectArray}

\link{Subsetting Operators}

\link{Standard Methods}
}
\examples{
x <- matrix (1:100, 10, 10)

###########
#NestMatrix
###########
msub <- NestMatrix (4, 4)

nm1 <- NestMatrix (4, 4)
nm2 <- NestMatrix (4, 4, recursive=TRUE, default.value=msub)
nm3 <- as.NestMatrix (PartMatrix (5, 5, 2:4, 2:4) )

nm1
nm2
nm3

###########
#PartMatrix
###########
pm <- as.PartMatrix (x, 5, c (2, 4, 6, 8) )
pm

pm [[1, 4]]

###########
#SectMatrix
###########
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
