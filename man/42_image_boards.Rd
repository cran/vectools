\name{42_image_boards}
\alias{decomp.plot}
\alias{init.image.board}
\title{Image Boards}
\description{
Plot multiple images, or multiple channels.

Note that init.image.board initializes the plot (in base graphics only) and returns the previous par settings.\cr
Subsequent calls are required to plot each image.

Alternatively, you can use the plot method for ImageArray(s).
}
\usage{
init.image.board (nr, nc, \dots, rby=TRUE) 
decomp.plot (im, \dots, rby=TRUE, colfs)
}
\arguments{
\item{im}{An RImage or ImageArray}
\item{nr, nc}{Integers, number of rows and columns.}
\item{rby}{Logical, if true, fill by row first.}
\item{colfs}{
A list of color functions.
Refer to \link{shader}.}
\item{\dots}{Ignored.}
}
\seealso{
\link{RImage} and \link{ImageArray}.

Refer to the above links for examples.
}
