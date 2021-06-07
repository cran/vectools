\name{03_constants}
\alias{Zero-class}
\alias{ZERO}
\alias{SQUARE}
\alias{CUBE}
\alias{TCUBE}
\title{Constant Values}
\description{
The ZERO constant is for \link{NestMatrix}.

The other constants are mainly for test plots.\cr
Refer to \link{vt3.testplot} and \link{vt3.testplot3} functions.
}
\usage{
ZERO

SQUARE
CUBE

#tilted
TCUBE
}
\details{
ZERO is an instance of the class Zero.\cr
This is a generalization of zero matrices.\cr
Which can be used in NestMatrix objects, without being constrained by dimensions.
}
\seealso{
\link{NestMatrix}

\link{vt3.testplot}, \link{vt3.testplot3}, \link{spin3d}
}
\examples{
vt3.testplot3 (CUBE \%]*\% btrl3 (0, 2, 2.5) )
}
