\name{51_sequence_properties}
\alias{Sequence Properties}
\alias{is.sttincreasing}
\alias{is.sttdecreasing}
\alias{is.nondecreasing}
\alias{is.nonincreasing}
\title{Sequence Properties}
\description{Functions to test basic sequence properties, excluding unique-related properties.}
\usage{
is.sttincreasing (v)
is.sttdecreasing (v)
is.nondecreasing (v)
is.nonincreasing (v)
}
\arguments{
\item{v}{An integer/numeric vector.}
}
\value{
All functions return true or false.
}
\seealso{
\link{Unique-Related Functions}

\link{Optimization-Related Functions}

\link{Other Sequence Functions}
}
\examples{
x <- c (0, 0, 1, 2)

is.sttincreasing (x)
is.nondecreasing (x)
}
