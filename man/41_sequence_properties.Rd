\name{41_sequence_properties}
\alias{Sequence Properties}
\alias{is.increasing}
\alias{is.decreasing}
\alias{is.nondecreasing}
\alias{is.nonincreasing}
\title{Sequence Properties}
\description{Functions to test basic sequence properties, excluding unique-related properties.}
\usage{
is.increasing (x)
is.decreasing (x)
is.nondecreasing (x)
is.nonincreasing (x)
}
\arguments{
\item{x}{An integer/numeric vector.}
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

is.increasing (x)
is.nondecreasing (x)
}
