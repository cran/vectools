\name{73_image_filters}
\alias{Image Filters}
\alias{vt2.simple.matrix}
\title{Image Filters}
\description{
Kernels (here referred to as image filters), for convolutions.

POOLING AND CONVOLUTION FUNCTIONS ARE ONLY EARLY PROTOTYPES.\cr
THEY HAVE HAD LIMITED TESTING AND MAY BE CHANGED IN FUTURE UPDATES.
}
\usage{
vt2.simple.matrix (n, \dots, scale=TRUE)
}
\arguments{
\item{n}{Integer of length one or two, giving the size of matrix.}
\item{scale}{Logical, if true (the default), scale the matrix, such that matrix values sum to one.}
\item{\dots}{Ignored.}
}
\value{
A matrix to used with a convolution algorithm.

By default, the matrix values sum to one.
}
\seealso{
\link{Pooling}

\link{Convolutions}
}
\examples{
vt2.simple.matrix (5)
}
