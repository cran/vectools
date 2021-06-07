\name{84_shaders}
\alias{shader}
\alias{vt3.hcl.shaders}
\alias{vt3.hsv.shaders}
\alias{vt3.hue.shader}
\alias{vt3.linear.shader}
\alias{vt3.rgb.shaders}
\title{Single Channel Shaders}
\description{
These functions return functions (or a list of functions).\cr
The resulting functions, map a matrix of numeric values to a character matrix of colors.

They are designed for rendering single channel images, and also for rendering multiple channels, separately.\cr
(In contrast to rendering a multichannel image, as a single raster image).
}
\usage{
vt3.linear.shader (col0="black", col1="white", min=0, max=1, reverse=FALSE)

vt3.rgb.shaders ()
vt3.hcl.shaders ()
vt3.hsv.shaders ()

vt3.hue.shader (hmin=0, hmax=360, \dots, c=50, l=50, na.col="white")
}
\arguments{
\item{hmin, hmax}{Single numeric values, the min and max hue values.}
\item{c, l}{Single numeric values, the C and L values in HCL color space.}
\item{na.col}{The color returned for NA values.}
\item{col0, col1}{Strings, the lower and upper colors.}
\item{min, max}{The min and max values to interpolate between.}
\item{reverse}{Logical, if true, it's equivalent to swapping min and max.}
\item{\dots}{Ignored.}
}
\value{
vt3.linear.shader returns a function.

The others return a list of four functions.
}
\seealso{
\link{as.raster.SImage}, \link{as.raster.MImage}
}
\examples{
colf <- vt3.linear.shader ()
colf (c (0.33, 0.67) )
}
