\name{77_other_transformation_matrices}
\alias{Other Transformation Matrices}
\alias{eq.arot2}
\alias{eq.brot2}
\title{Other Transformation Matrices}
\description{Convenience function for a set of circular rotation matrices.}
\usage{
eq.arot2 (n, \dots, start=0)
eq.brot2 (n, \dots, start=0)
}
\arguments{
\item{n}{Integer, the number of distinct points.}
\item{start}{Numeric, giving the starting angle in radians.}
\item{\dots}{Ignored.}
}
\details{
These functions are wrappers for \link{arot2} and \link{brot2}.

They compute a vector of equally-spaced theta values, around a circle.\cr
Then construct the transformation matrices using the theta values.

This can be used to create polygons.

Note that swapping functions, reverses the direction of rotation.\cr
(e.g. Changing eq.arot2 to eq.brot2).
}
\value{
A MatrixArray of 3x3 rotation matrices.
}
\seealso{
\link{Binary Operators}

\link{MatrixArray}

\link{Pre-Multiplication Transformation Matrices}

\link{Post-Multiplication Transformation Matrices}

\link{vt3.testplot}
}
\examples{
#equivalent to:
#vt3.testplot (regPolygon (6, d=4) )
ps <- c (0, 4) \%|*\% eq.brot2 (6, start = pi / 6)
v <- as.Polygon (ps)
vt3.testplot (v)
}
