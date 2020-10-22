\name{61_post-multiplication_transformation_matrices}
\alias{Post-Multiplication Transformation Matrices}
\alias{btrl2}
\alias{bscl2}
\alias{brot2}
\alias{btrl3}
\alias{bscl3}
\alias{brot3x}
\alias{brot3y}
\alias{brot3z}
\title{Post-Multiplication Transformation Matrices}
\description{Constructors for constructing one or more transformation matrices.}
\usage{
btrl2 (x=0, y=0)
bscl2 (x, y=x)
brot2 (theta)

btrl3 (x=0, y=0, z=0)
bscl3 (x, y=x, z=x)

brot3x (theta)
brot3y (theta)
brot3z (theta)
}
\arguments{
\item{x, y, z}{Numeric vectors, x, y and z transformation parameters.}
\item{theta}{Numeric vector, the angle in radians.}
}
\details{
These matrices are designed for post-multiplication.\cr
They go after the data matrix.\cr
(Note that data, should have points as rows).

Pre and post scaling matrices are the same.

In 2d rotation matrices, positive theta values rotate counter-clockwise.

3d rotation matrices, are the same as the 2d matrices, except for having an extra row and column.\cr
(Whether they're clockwise or counter-clockwise, depends on one's coordinate system).
}
\value{
A matrix or a 1d MatrixArray.

2d transformations are 3x3 matrices and 3d transformations are 4x4 matrices.}
\references{.}
\seealso{
\link{MatrixArray}

\link{Binary Operators}\cr
This describes how to perform operations on MatrixArray objects.

\link{Pre-Multiplication Transformation Matrices}

\link{polyplot}, \link{cubplot}
}
\examples{
########################################
#2d static examples
########################################
#centered hexagon
ps <- c (0, 1) \%|*\% eq.brot2 (6, start = pi / 6)
polyplot (ps)

#four hexagons, different sizes
vm <- ps \%]*\% bscl2 (seq (1.3, 0.7,, 4) )
polyplot (vm)

#scaled, translated, then rotated (cartesian)
vm.c <- ps \%]*\% (
    bscl2 (seq (1.3, 0.7,, 4) ) \%*\%
    btrl2 (,3.75) \%{*}\%
    eq.brot2 (8) )
polyplot (vm.c)

#scaled, translated, then rotated (standard, but rev)
vm.s <- ps \%]*\% (
    bscl2 (seq (1.2, 0.6,, 5) ) \%*\%
    btrl2 (,4) \%*\%
    eq.arot2 (10) )
polyplot (vm.s)

########################################
#3d dynamic example
########################################
sim3d <- function (N=200, DELAY=0)
{   start <- runif (3, -4, 0)
    end <- runif (3, 0, 4)
    xpos <- seq (start [1], end [1],, N)
    ypos <- seq (start [2], end [2],, N)
    zpos <- seq (start [3], end [3],, N)

    nrots <- runif (3, 1, 4)
    start <- runif (3, 0, 2 * pi)
    end <- start + 2 * pi * nrots
    xtheta <- seq (start [1], end [1],, N)
    ytheta <- seq (start [2], end [2],, N)
    ztheta <- seq (start [3], end [3],, N)
	
    for (k in 1:N)
    {   ps <- cubps \%]*\% (
            btrl3 (xpos [k], ypos [k], zpos [k]) \%*\%
            brot3x (xtheta [k]) \%*\%
            brot3y (ytheta [k]) \%*\%
            brot3z (ztheta [k]) )
        cubplot (ps)
        if (DELAY > 0)
            Sys.sleep (DELAY)
    }
}

if (interactive () )
    sim3d (200, 0)
}
