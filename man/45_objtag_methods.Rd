\name{45_objtag_methods}
\alias{objtag methods}
\alias{objtag.ObjectArray}
\alias{objtag.NestMatrix}
\alias{objtag.PartMatrix}
\alias{objtag.SectMatrix}
\alias{objtag.MatrixArray}
\alias{objtag.Zero}
\alias{objtag.function}
\alias{objtag.list}
\alias{objtag.data.frame}
\alias{objtag.matrix}
\alias{objtag.default}
\alias{objtag.GeomObject}
\alias{objtag.GeomArray}
\alias{objtag.RImage}
\alias{objtag.ImageArray}
\title{objtag Methods}
\description{Element-level formatting functions for object arrays.}
\usage{
\method{objtag}{ObjectArray}(v, \dots)
\method{objtag}{NestMatrix}(v, \dots)
\method{objtag}{PartMatrix}(v, \dots)
\method{objtag}{SectMatrix}(v, \dots)
\method{objtag}{MatrixArray}(v, \dots)

\method{objtag}{Zero}(v, \dots)

\method{objtag}{function}(v, \dots)
\method{objtag}{list}(v, \dots)
\method{objtag}{data.frame}(v, \dots)
\method{objtag}{matrix}(v, \dots)

\method{objtag}{default}(v, \dots)

\method{objtag}{GeomObject}(v, \dots)
\method{objtag}{GeomArray}(v, \dots)
\method{objtag}{RImage}(im, \dots)
\method{objtag}{ImageArray}(im, \dots)
}
\arguments{
\item{im, v}{An object.}
\item{\dots}{Ignored.}
}
\details{
These functions map an object to a single compact string, regardless of the length of the object.

The format method for ObjectArray calls the objtag function for each of its elements.

To format an object of a different class, you need to write an (S3) objtag method for that class.
}
\value{
Each method returns a single compact string.\cr
(i.e. A length-one character vector, that's relatively short).

If you write a new objtag method, it should do the same.}
\seealso{
\link{format.ObjectArray}
}
\examples{
#simple classes
alphabet.1 <- function ()
    structure (LETTERS, class="alphabet.1")
alphabet.2 <- function ()
    structure (sample (LETTERS), class="alphabet.2")

#simple 2x2 ObjectArray
x <- ObjectArray (c (2, 2) )
x [[1, 1]] <- alphabet.1 ()
x [[2, 1]] <- alphabet.1 ()
x [[1, 2]] <- alphabet.2 ()
x [[2, 2]] <- alphabet.2 ()

#printed with default formatting
x

#objtag methods
objtag.alphabet.1 <- function (x)
    paste ("<A1 ", x [1], ":", x [26], ">", sep="")
objtag.alphabet.2 <- function (x)
    paste ("<A2 ", x [1], ":", x [26], ">", sep="")

#reprinted with custom formatting
x
}
