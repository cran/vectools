\name{30_is_functions}
\alias{is.ZERO}
\alias{is.Points}
\alias{is.Line}
\alias{is.Polygon}
\alias{is.Text}
\alias{is.Grid}
\alias{is.ObjectArray}
\alias{is.NestMatrix}
\alias{is.PartMatrix}
\alias{is.SectMatrix}
\alias{is.MatrixArray}
\alias{is.GeomArray}
\alias{is.GeomObject}
\alias{is.ImageArray}
\alias{is.RImage}
\alias{is.SImage}
\alias{is.MImage}
\title{Is Functions}
\description{
Functions to test if an object is of a particular class.

Note that ImageArray, GeomArray, NestMatrix and MatrixArray extend ObjectArray, and PartMatrix extends SectMatrix.}
\usage{
is.ZERO (object)

is.RImage (object)
is.GeomObject (object)

is.Points (object)
is.Line (object)
is.Polygon (object)
is.Text (object)
is.Grid (object)

is.SImage (object)
is.MImage (object)

is.ImageArray (object)
is.ObjectArray (object)
is.GeomArray (object)
is.NestMatrix (object)
is.PartMatrix (object)
is.SectMatrix (object)
is.MatrixArray (object)
}
\arguments{
\item{object}{An object to test.}
}
\value{TRUE or FALSE.}
\examples{
is.ZERO (ZERO)
}
