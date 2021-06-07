\name{21_ImageArray}
\alias{ImageArray-class}
\alias{ImageArray}
\alias{as.ImageArray}
\title{Image Arrays}
\description{
ImageArray objects, where ImageArray is a subclass of \link{ObjectArray}, containing \link{RImage} (raster image) objects.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
ImageArray (n)
as.ImageArray (im, \dots, n)
}
\arguments{
\item{n}{Integer vector, the dimensions.}
\item{im}{A list of RImage objects.}
\item{\dots}{Ignored.}
}
\value{
An ImageArray object.
}
\seealso{
\link{ObjectArray}

\link{RImage}

\link{VImage}
}
\examples{
library (png)
library (colorspace)

#adapted from png package
im0 <- readPNG (system.file ("img", "Rlogo.png", package="png") )

#multichannel images
im <- ImageArray (2)
im [[1]] <- as.MImage (im0)
im [[2]] <- as.MImage (im0, storage="HCL")

decomp.plot (im)
}
