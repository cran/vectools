\name{71_pooling}
\alias{Pooling}
\alias{vt2.pool.matrix}
\alias{vt2.pool2.matrix}
\title{Pooling}
\description{
Pooling functions for matrices.

POOLING AND CONVOLUTION FUNCTIONS ARE ONLY EARLY PROTOTYPES.\cr
THEY HAVE HAD LIMITED TESTING AND MAY BE CHANGED IN FUTURE UPDATES.
}
\usage{
vt2.pool.matrix (x, nsub, \dots, f=mean, edge.method="exact")
vt2.pool2.matrix (x, ntar, \dots, f=mean)
}
\arguments{
\item{x}{A matrix.}
\item{nsub}{An integer of length two, giving the size of the submatrices.}
\item{ntar}{
	An integer vector of length two.
	The preferred return matrix size.\cr
	(Currently, this is not exact, but should be in the future).}
\item{f}{
	A function of a matrix.\cr
	This matrix will be a submatrix of the matrix x.}
\item{edge.method}{
	String, either "exact" or "spart".
	Refer to details.}
\item{\dots}{Ignored.}
}
\details{
These functions partition a matrix into a set of smaller matrices.\cr
(The submatrices do not overlap).

In the pool function the user specifies the submatrix size, and in the pool2 function the user specifies the (approx) target size. 

In the "exact" edge method, the input matrix needs to divisible into equally sized matrices.\cr
An error is produced if this is not possible.

In the "spart" edge method, smaller equal or near-equal sized matrices are used for edge cases.
}
\value{
A matrix.
}
\seealso{
\link{Convolutions}

\link{Image Filters}
}
\examples{
x <- matrix (1:40, 4, 10)
vt2.pool.matrix (x, c (2, 2) )
}
