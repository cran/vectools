\name{86_color_transformations}
\alias{mapcol}
\alias{mapcol3}
\title{Color Conversion}
\description{
Functions to map vectors/matrices/arrays of colors between color spaces.

THESE FUNCTIONS REQUIRE THE colorspace PACKAGE TO BE INSTALLED AND LOADED.

One exception to this requirement is mapping numeric values to strings, if the "from" color space equals the "to" color space.
}
\usage{
mapcol (colv, \dots, as.list = is.list(colv), as.string=FALSE,
    from="sRGB", to=from, correction=TRUE)
mapcol3 (x, y, z, alpha, \dots, as.list=FALSE, as.string=FALSE,
    from="sRGB", to=from, correction=TRUE)
}
\arguments{
\item{colv}{
	A numeric array with two or more dimensions.\cr
	(Such as the result of \link{standardize.cols} with as.array=TRUE.\cr\cr
	The last dimension should have three or four levels, which represent channels.}
\item{x, y, z, alpha}{
	Numeric vectors.\cr
	Alternative way of specifying color.}
\item{as.list, as.string}{Logical, if true, return a list or string, see value section.}
\item{from}{String, the input color space.\cr}
\item{to}{
	String, the output color space.\cr
	Ignored, if as.string is true.}
\item{correction}{
	Logical, correct the sRGB values.\cr
	Ignored, unless converting HCL to sRGB.}
\item{\dots}{Ignored.}
}
\details{
The functions wrap functions from the colorspace package.

The to and from arguments, use names of color spaces from the colorspace package.

This includes:\cr
"XYZ", "RGB", "sRGB", "LAB", "polarLAB", "HSV", "HLS", "LUV" and "polarLUV".

In addition to HCL, with is the same as polarLUV, but the the arguments in a different order.

The same labels are used for three channel and four channel images.
}
\value{
By default, these functions return arrays.\cr
Where the last dimension corresponds to channel.

If as.string is true, a character vector/matrix/array is returned.\cr
If as.list is true, a list is returned, with one element per channel.
}
\seealso{
\link{standardize.cols}
}
\examples{
library (colorspace)

cols <- standardize.cols (
    c ("black", "white", "#000000", "#FFFFFF"),
    as.array=TRUE)

#hues
mapcol (cols, to="HCL")[,1]
}
