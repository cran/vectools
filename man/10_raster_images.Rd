\name{10_raster_images}
\alias{RImage}
\alias{RImage-class}
\alias{SImage-class}
\alias{MImage-class}
\alias{as.SImage}
\alias{as.MImage}
\alias{get.channel}
\title{Raster Objects}
\description{
Objects representing raster image data.

The class RImage has two subclasses SImage (for single channel images) and MImage (for multiple channel images).

Currently, pixel values (im) need to be between zero and one.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.

NOTE:\cr
USING NON-sRGB COLOR SPACES REQUIRES THE colorspace PACKAGE TO BE INSTALLED AND LOADED.

Also, note that there's also a \link{VImage} object, which uses vector graphics.
}
\usage{
as.SImage (im)
as.MImage (im, \dots, input="sRGB", storage=input)

get.channel (im, which, \dots, as.matrix=FALSE)
}
\arguments{
\item{im}{
	For SImage:\cr
	A matrix or raster object.\cr\cr
	For MImage:\cr
	A matrix, an array, a list, a SImage object or a raster object.\cr
	Arrays can have two or three dimensions.\cr
	In the 3d case, the last dimension can have 1 to 4 items, each representing a channel.\cr
	Lists can have 1 to 4 equally-sized matrices, each representing one channel.\cr\cr
	For get.channel:\cr
	An MImage object.}
\item{input, storage}{
	Single characters, giving the name of a valid color space from the colorspace package.\cr
	Refer to \link{mapcol}.\cr
	The input argument is ignored, unless the input is a three or four channel 3D array.\cr\cr
	Input is the input color space, and storage is the storage color space.\cr\cr
	Note that the colorspace package needs to be installed to use non-sRGB colorspaces.\cr
	Also, note that the same name is used for three channel images, and four channel images (with an alpha channel).\cr
	i.e. "sRGB" can be used for sRGB and sRGB-A.}
\item{which}{Integer, which channel.}
\item{as.matrix}{Logical, if false (default), return an SImage object, If true, return a matrix.}
\item{\dots}{Ignored.}
}
\value{An SImage, MImage or matrix object.}
\examples{
library (png)

#adapted from png package
im0 <- readPNG (system.file ("img", "Rlogo.png", package="png") )

#multichannel image
im <- as.MImage (im0)

plot (im)

headt (as.raster (im) )
}
