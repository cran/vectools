\name{75_pre-multiplication_transformation_matrices}
\alias{Pre-Multiplication Transformation Matrices}
\alias{ascl2}
\alias{atrl2}
\alias{arot2}
\alias{ascl3}
\alias{atrl3}
\alias{arot3x}
\alias{arot3y}
\alias{arot3z}
\title{Pre-Multiplication Transformation Matrices}
\description{Constructors for constructing one or more transformation matrices.}
\usage{
ascl2 (x, y=x, \dots, about)
atrl2 (x=0, y=0)
arot2 (theta, \dots, about)

ascl3 (x, y=x, z=x)
atrl3 (x=0, y=0, z=0)

arot3x (theta)
arot3y (theta)
arot3z (theta)
}
\arguments{
\item{x, y, z}{Numeric vectors, x, y and z transformation parameters.}
\item{theta}{Numeric vector, the angle in radians.}
\item{about}{Length-two numeric vector, giving center point for transformation.}
\item{\dots}{Ignored.}
}
\details{
These matrices are designed for pre-multiplication.\cr
They go before the data matrix.

Pre and post scaling matrices are the same.

In 2d rotation matrices, positive theta values rotate counter-clockwise.

3d rotation matrices, are the same as the 2d matrices, except for having an extra row and column.\cr
(Whether they're clockwise or counter-clockwise, depends on one's coordinate system).
}
\value{
A matrix or a 1d MatrixArray.

2d transformations are 3x3 matrices and 3d transformations are 4x4 matrices.
}
\seealso{
\link{MatrixArray}

\link{Binary Operators}\cr
This describes how to perform operations on MatrixArray objects.

\link{Post-Multiplication Transformation Matrices}

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
v2 <- ascl2 (seq (1.3, 0.7,, 4) ) \%*[\% v
vt3.testplot (v2)
}
