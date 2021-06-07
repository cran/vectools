\name{26_MatrixArray}
\alias{MatrixArray-class}
\alias{MatrixArray}
\alias{as.MatrixArray}
\title{Matrix Arrays}
\description{
A MatrixArray is a subclass of ObjectArray.\cr
These represent a (currently one dimensional) array of matrices.

The package provides binary operators to apply matrix multiplication over the entire array.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
MatrixArray (n, \dots, NR, NC, conform=TRUE, default.value=ZERO)
as.MatrixArray (v, \dots, n, NR, NC, conform=TRUE)
}
\arguments{
\item{n}{
	Integer, the length\cr
	Currently, the top-level object is constrained to one dimension.}
\item{v}{
	A suitable object.\cr
	Such as a list of matrices.}
\item{NR, NC}{Currently, these need to be set to NA.}
\item{conform}{
	Logical, if true, submatrices are required to have conformable dimensions.}
\item{default.value}{The default value.}
\item{\dots}{Ignored.}
}
\value{A MatrixArray object.}
\seealso{
\link{ObjectArray}

\link{NestMatrix}, \link{GeomArray}

\link{Binary Operators}
}
\examples{
v <- MatrixArray (2, NR=NA, NC=NA)
v [[1]] <- matrix (1:4, 2, 2)
v [[2]] <- matrix (5:8, 2, 2)
v

t <- diag (c (2, 2) )
u <- t %*% v

u
u [[1]]
u [[2]]
}
