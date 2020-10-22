\name{12_is_functions}
\alias{is.ObjectArray}
\alias{is.NestMatrix}
\alias{is.PartMatrix}
\alias{is.SectMatrix}
\alias{is.MatrixArray}
\alias{is.ZERO}
\title{Is Functions}
\description{
Functions to test if an object is of a particular class.

Note that NestMatrix and MatrixArray extend ObjectArray, and PartMatrix extends SectMatrix.}
\usage{
is.ObjectArray (x)
is.NestMatrix (x)
is.PartMatrix (x)
is.SectMatrix (x)
is.MatrixArray (x)

is.ZERO (x)
}
\arguments{
\item{x}{An object to test.}
}
\value{True or false.}
\examples{
v <- ObjectArray (,c (5, 10) )

is.ObjectArray (v)
}
