\name{60_pre-multiplication_transformation_matrices}
\alias{Pre-Multiplication Transformation Matrices}
\alias{atrl2}
\alias{ascl2}
\alias{arot2}
\alias{atrl3}
\alias{ascl3}
\alias{arot3x}
\alias{arot3y}
\alias{arot3z}
\title{Pre-Multiplication Transformation Matrices}
\description{Constructors for constructing one or more transformation matrices.}
\usage{
atrl2 (x=0, y=0)
ascl2 (x, y=x)
arot2 (theta)

atrl3 (x=0, y=0, z=0)
ascl3 (x, y=x, z=x)

arot3x (theta)
arot3y (theta)
arot3z (theta)
}
\arguments{
\item{x, y, z}{Numeric vectors, x, y and z transformation parameters.}
\item{theta}{Numeric vector, the angle in radians.}
}
\details{
These matrices are designed for pre-multiplication.\cr
They go before the data matrix.\cr
(Note that data, should have points as columns).

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

\link{polyplot}, \link{cubplot}
}
\examples{
########################################
#2d static examples
########################################
#centered hexagon
ps <- eq.arot2 (6, start = pi / 6) \%*|\% c (0, 1)
polyplot (ps, transpose=TRUE)

#four hexagons, different sizes
vm <- ascl2 (seq (1.3, 0.7,, 4) ) \%*[\% ps
polyplot (vm, transpose=TRUE)

#scaled, translated, then rotated (cartesian)
vm.c <-
    eq.arot2 (8) \%{*}\%
    (   atrl2 (,3.75) \%*\%
        ascl2 (seq (1.3, 0.7,, 4) )
    ) \%*[\%
    ps
polyplot (vm.c, transpose=TRUE)

#scaled, translated, then rotated (standard, but rev)
vm.s <-
    eq.brot2 (8) \%{*}\%
    atrl2 (,3.75) \%*\%
    ascl2 (seq (1.3, 0.7,, 4) ) \%*[\%
    ps
polyplot (vm.s, transpose=TRUE)

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
    {   ps <-
            arot3z (ztheta [k]) \%*\%
            arot3y (ytheta [k]) \%*\%
            arot3x (xtheta [k]) \%*\%
            atrl3 (xpos [k], ypos [k], zpos [k]) \%*[\%
            t (cubps)
        cubplot (ps, transpose=TRUE)
        if (DELAY > 0)
            Sys.sleep (DELAY)
    }
}

if (interactive () )
    sim3d (200, 0)
}
