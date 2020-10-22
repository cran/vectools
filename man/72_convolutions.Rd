\name{72_convolutions}
\alias{Convolutions}
\alias{vt2.convolve.matrix}
\title{Convolutions}
\description{
Image-style convolutions of two matrices.

POOLING AND CONVOLUTION FUNCTIONS ARE ONLY EARLY PROTOTYPES.\cr
THEY HAVE HAD LIMITED TESTING AND MAY BE CHANGED IN FUTURE UPDATES.
}
\usage{
vt2.convolve.matrix (x, k, \dots, f, nsub)
}
\arguments{
\item{x}{A matrix, representing an image/etc.}
\item{k}{
	A matrix, the image filter.
	In general, this should sum to one.}
\item{f}{
	A function of a matrix.\cr
	Ignored, if k provided.}
\item{nsub}{
	An integer vector of length one or two, the size of the submatrices.\cr
	Ignored, if k provided.}
\item{\dots}{Ignored.}
}
\details{
If k provided, then each element of the new matrix is computed by multiplying k by a submatrix of x the same size as k.

The image filter slides over the matrix.

If k omitted, it's possible to use an arbitrary function.\cr
This will also slide over the matrix, calling the function with submatrices.
}
\value{A matrix.}
\seealso{
\link{Pooling}

\link{Image Filters}
}
\examples{
x <- matrix (1:100, 10, 10)
vt2.convolve.matrix (x, vt2.simple.matrix (5) )
}
