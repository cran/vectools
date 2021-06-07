\name{55_sequence_properties_2}
\alias{Other Sequence Functions}
\alias{midpoints}
\alias{endpoints}
\title{Other Sequence Properties}
\description{Compute midpoints and endpoints from a vector.}
\usage{
midpoints (v)
endpoints (v)
}
\arguments{
\item{v}{An integer/numeric vector.}
}
\value{
The midpoints function returns a vector, with a length one less than the original vector.

The endpoints function returns a length two vector.
}
\seealso{
\link{Unique-Related Functions}

\link{Sequence Properties}\cr
(Other than unique-related properties).

\link{Optimization-Related Functions}
}
\examples{
x <- 1:4

midpoints (x)
endpoints (x)
}
