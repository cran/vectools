\name{76_post-multiplication_transformation_matrices}
\alias{Post-Multiplication Transformation Matrices}
\alias{bscl2}
\alias{btrl2}
\alias{brot2}
\alias{bscl3}
\alias{btrl3}
\alias{brot3x}
\alias{brot3y}
\alias{brot3z}
\title{Post-Multiplication Transformation Matrices}
\description{Constructors for constructing one or more transformation matrices.}
\usage{
bscl2 (x, y=x, \dots, about)
btrl2 (x=0, y=0)
brot2 (theta, \dots, about)

bscl3 (x, y=x, z=x)
btrl3 (x=0, y=0, z=0)

brot3x (theta)
brot3y (theta)
brot3z (theta)
}
\arguments{
\item{x, y, z}{Numeric vectors, x, y and z transformation parameters.}
\item{theta}{Numeric vector, the angle in radians.}
\item{about}{Length-two numeric vector, giving center point for transformation.}
\item{\dots}{Ignored.}
}
\details{
These matrices are designed for post-multiplication.\cr
They go after the data matrix.

Pre and post scaling matrices are the same.

In 2d rotation matrices, positive theta values rotate counter-clockwise.

3d rotation matrices, are the same as the 2d matrices, except for having an extra row and column.\cr
(Whether they're clockwise or counter-clockwise, depends on one's coordinate system).
}
\value{
A matrix or a 1d MatrixArray.

2d transformations are 3x3 matrices and 3d transformations are 4x4 matrices.}
\seealso{
\link{MatrixArray}

\link{Binary Operators}\cr
This describes how to perform operations on MatrixArray objects.

\link{Pre-Multiplication Transformation Matrices}

\link{vt3.testplot}, \link{vt3.testplot}
}
\examples{
########################################
#2d static examples
########################################
#centered hexagon
v <- regPolygon (6)
vt3.testplot (v)

#four hexagons, different sizes
v2 <- v \%]*\% bscl2 (seq (1.3, 0.7,, 4) )
vt3.testplot (v2)
}
