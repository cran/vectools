\name{33_standard_methods}
\alias{print.GeomObject}
\alias{print.MImage}
\alias{print.SImage}
\alias{Standard Methods}
\alias{length.ObjectArray}
\alias{length.SectMatrix}
\alias{dim.ObjectArray}
\alias{dim.SectMatrix}
\alias{dimnames.ObjectArray}
\alias{dimnames.SectMatrix}
\alias{dimnames<-.ObjectArray}
\alias{dimnames<-.SectMatrix}
\alias{print.VecLike}
\alias{format.ObjectArray}
\alias{format.SectMatrix}
\alias{format.ZERO}
\alias{head.VecLike}
\alias{head.raster}
\alias{tail.VecLike}
\alias{tail.raster}
\title{Standard Methods}
\description{Length, dim, print, format, head and tail methods for vector-like objects, from this package.}
\usage{
\method{print}{GeomObject}(x, \dots)
\method{print}{MImage}(x, \dots)
\method{print}{SImage}(x, \dots)

\method{length}{ObjectArray}(x)
\method{length}{SectMatrix}(x)

\method{dim}{ObjectArray}(x)
\method{dim}{SectMatrix}(x)

\method{dimnames}{ObjectArray}(x)
\method{dimnames}{SectMatrix}(x)

\method{dimnames}{ObjectArray}(x) <- value
\method{dimnames}{SectMatrix}(x) <- value

\method{print}{VecLike}(x, \dots)

\method{format}{ObjectArray}(x, \dots)
\method{format}{SectMatrix}(x, \dots, na.string="")
\method{format}{ZERO}(x, \dots)

\method{head}{VecLike}(x, n=6, \dots)
\method{head}{raster}(x, \dots)
\method{tail}{VecLike}(x, n=6, \dots)
\method{tail}{raster}(x, \dots)
}
\arguments{
\item{x}{A suitable object}.
\item{na.string}{String, what to format NAs as.}
\item{n}{Integer, the number of items.}
\item{value}{A list of character vectors, matching the dimensions of the object.}
\item{\dots}{Ignored.}
}
\details{
Note that the dimensions of object arrays and nested matrices are the dimensions of the top level object. However, the dimensions of sectioned matrices (including partitioned matrices) are the dimensions of the combined matrix.

The format method for ObjectArray, calls the objtag function, for each of its elements.

The format method for sectioned and partitioned matrices, adds plain-text markup, for separators and section boxes.
}
\value{
Most functions perform the same action as standard R functions.

The format methods return formatted character matrices.

Currently, some of the head, tail functions return formatted character matrices, however, it's possible this may change in the future.}
\examples{
x <- matrix (1:100, 10, 10)
pm <- as.PartMatrix (x, 5, c (2, 4, 6, 8) )
nm <- as.NestMatrix (pm)

dim (nm)
dim (pm)

head (nm, 2)
head (pm, 2)

nm
pm
}
