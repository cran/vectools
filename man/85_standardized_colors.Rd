\name{85_standardized_colors}
\alias{is.opaque}
\alias{standardize.cols}
\alias{vt3.prep.cols}
\title{Standardize Colors}
\description{
Functions for standardizing R color strings.

Note that the \link{mapcol} function can used to map between color spaces.
}
\usage{
is.opaque (cols, ..., single.flag=TRUE)
standardize.cols (cols, \dots, nchannels, as.list=FALSE, as.array=FALSE)
vt3.prep.cols (cols, nchannels, single.flag=TRUE, \dots, as.list=FALSE,
    as.array=FALSE)
}
\arguments{
\item{cols}{A character vector/matrix/array, including color names or hex values for sRGB/sRGB-A space.}
\item{single.flag}{Logical, if true, return a single TRUE/FALSE value.}
\item{nchannels}{
	Integer, either three or four, determining the number of output channels.\cr
	If missing, there will be three if all input colors are opaque, otherwise four.}
\item{as.list, as.array}{Logical values, if true, return a list or array, see value section.}
\item{\dots}{Ignored.}
}
\details{
The is.opaque function is used to test if the input contains any non-opaque colors.\cr

The standardize.cols function converts all colors into a hex strings.\cr
The result will be all 3 channel or 4 channel values, not a mixture of two.

The vt3.prep.cols function is a combination of the two functions above.
}
\value{
By default, is.opaque returns a single logical value.\cr
If single.flag is false, it returns a logical vector/matrix/array matching the dimensions of cols.

By default, standardize.cols returns a character vector/matrix/array matching the dimensions of cols.\cr
If as.list or as.array are true, it returns a list or array, respectively.\cr
In lists, each element corresponds to each channel, and in arrays, the last dimension corresponds to each channel.

vt3.prep.cols returns a length two list, containing the opaque flags and the standardized colors.
}
\examples{
mixed.cols <- c ("black", "white", "#000000", "#FFFFFF")

standardize.cols (mixed.cols)
standardize.cols (mixed.cols, as.list=TRUE)
}
