\name{5_element_level_formatting}
\alias{objtag}
\alias{objtag.default}
\alias{objtag.ObjectArray}
\alias{objtag.NestMatrix}
\alias{objtag.SectMatrix}
\alias{objtag.PartMatrix}
\alias{objtag.function}
\alias{objtag.list}
\alias{objtag.matrix}
\alias{objtag.data.frame}
\title{Object Array Formatting}
\description{Element-level formatting functions for object arrays.}
\usage{
objtag (\dots)

\method{objtag}{default}(x, \dots)
\method{objtag}{ObjectArray}(x, \dots)
\method{objtag}{NestMatrix}(x, \dots)
\method{objtag}{SectMatrix}(x, \dots)
\method{objtag}{PartMatrix}(x, \dots)
\method{objtag}{function}(x, \dots)
\method{objtag}{list}(x, \dots)
\method{objtag}{matrix}(x, \dots)
\method{objtag}{data.frame}(x, \dots)
}
\details{
These functions map an object to a single compact string, regardless of the length of the object.

The format method for ObjectArray calls the objtag function for each of its elements.

To format an object of a different class, you need to write an (S3) objtag method for that class.
}
\value{
Each method returns a single compact string.\cr
(i.e. A length-one character vector, that's relatively short).

If you write a new objtag method, it should do the same.
}
\arguments{
\item{x}{An object.}
\item{\dots}{.}
}

\examples{
#near-trivial classes
alphabet.1 = function ()
    structure (LETTERS, class="alphabet.1")
alphabet.2 = function ()
    structure (sample (LETTERS), class="alphabet.2")

#near-trivial 2x2 object array
x = ObjectArray (c (2, 2) )
x [[1, 1]] = alphabet.1 ()
x [[2, 1]] = alphabet.1 ()
x [[1, 2]] = alphabet.2 ()
x [[2, 2]] = alphabet.2 ()

#printed with default formatting
x

#objtag methods
objtag.alphabet.1 = function (x)
    paste ("<A1 ", x [1], ":", x [26], ">", sep="")
objtag.alphabet.2 = function (x)
    paste ("<A2 ", x [1], ":", x [26], ">", sep="")

#reprinted with custom formatting
x
}
