\name{34_as_raster_methods}
\alias{as.raster.MImage}
\alias{as.raster.SImage}
\title{As Raster Methods}
\description{
Create standard raster objects from RImage (SImage and MImage) objects, from this package.}
\usage{
as.raster.SImage (x, \dots, optfit=FALSE,
    colf = vt3.linear.shader (reverse=invert),
    invert=TRUE)

as.raster.MImage (x, \dots, which, invert.gs=TRUE)
}
\arguments{
\item{x}{An RImage object.}
\item{which}{
	Integer, which channel.\cr
	If missing, all channels are used.}
\item{invert, invert.gs}{
	Logical, if true, reverse the colors.\cr
	In MImage, ignored, except for GS and GS-A (one and two channel images), or single-channel images via the which arg.}
\item{optfit}{Logical, if true, fit colors to a reduced interval (min value to max value), increasing the visible color variation.}
\item{colf}{
	A function that maps a numeric matrix to a character matrix of R colors.\cr
	This includes a \link{shader} function from this package.}
\item{\dots}{Ignored.}
}
\seealso{
\link{RImage}

Refer to above link for example.
}

