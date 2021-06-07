\name{50_unique-related_functions}
\alias{Unique-Related Functions}
\alias{is.each.equal}
\alias{is.each.unique}
\alias{n.unique}
\alias{n.duplicated}
\alias{which.unique}
\alias{usv}
\title{Unique-Related Functions}
\description{
Convenience functions to test uniqueness, or to return the number of unique/duplication values, or a unique sorted vector.

The which.unique function returns a vector of indices.\cr
This will produce a vector of unique values, if the original vector is subsetted using the indices.
}
\usage{
is.each.equal (v)
is.each.unique (v)

n.unique (v)
n.duplicated (v)

which.unique (v)

usv (v)
}
\arguments{
\item{v}{An integer/numeric vector.}
}
\seealso{
\link{Sequence Properties}

\link{Optimization-Related Functions}

\link{Other Sequence Functions}
}
\examples{
x <- c (2, 2, 1, 1, 10, 12)

n.unique (x)
usv (x)
}
