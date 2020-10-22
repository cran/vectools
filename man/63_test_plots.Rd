\name{63_test_plots}
\alias{Test Plots}
\alias{polyplot}
\alias{cubplot}
\title{Test Plots}
\description{Simple plotting functions for testing both 2d and 3d transformations.}
\usage{
polyplot (ps, \dots,
    main,
    grid=TRUE, add=FALSE, transpose=FALSE,
    xlim=c(-5, 5), ylim=c(-5, 5),
    line.color="black", fill.color="lightblue")

cubplot (ps, \dots,
    transpose=FALSE,
    xlim=c(-5, 5), ylim=c(-5, 5), zlim=c(-5, 5),
    line.color="black")
}
\arguments{
\item{ps}{
	A matrix or MatrixArray object.\cr
	By default:\cr
	In polyplot, each matrix should be a two-column matrix, representing points around a polygon.\cr
	In cubplot, each matrix should be equal to \link{cubps}, or a transformed version of it, via standard geometric transformation matrices.\cr
	Note that the first polygon (matrix) is plotted first, and the second polygon is plotted second, and so on.}
\item{main}{String, the main title.}
\item{grid}{Logical, add grid lines.}
\item{add}{Logical, if true, plot the polygons on top of an existing plot.}
\item{transpose}{Logical, if true, use the transpose of each matrix.}
\item{xlim, ylim, zlim}{Numeric vectors of length two, giving the x, y and z ranges.}
\item{line.color, fill.color}{Character vectors of length one, or the length of p, giving the the line and fill colors.}
\item{\dots}{Ignored.}
}
\seealso{
\link{MatrixArray}

\link{Binary Operators}\cr
This describes how to perform operations on MatrixArray objects.

\link{Pre-Multiplication Transformation Matrices}

\link{Post-Multiplication Transformation Matrices}

\link{sqps}, \link{cubps}
}
\examples{
########################################
#refer to:
#Pre-Multiplication or Post-Multiplication
#for better examples
########################################
polyplot (sqps \%]*\% brot2 (pi / 8) )
}
