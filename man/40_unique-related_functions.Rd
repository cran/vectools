\name{40_unique-related_functions}
\alias{Unique-Related Functions}
\alias{is.each.equal}
\alias{is.each.unique}
\alias{n.unique}
\alias{n.duplicated}
\alias{which.unique}
\alias{usv}
\title{Unique-Related Functions}
\description{
Convenience functions to test uniqueness, or to return the number of unique or duplication values, or a unique sorted vector.

The which.unique function returns a vector of indices.\cr
This will be produce a vector of unique values, if the original vector is subsetted using the indices.
}
\usage{
is.each.equal (x)
is.each.unique (x)

n.unique (x)
n.duplicated (x)

which.unique (x)

usv (x)
}
\arguments{
\item{x}{An integer/numeric vector.}
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
