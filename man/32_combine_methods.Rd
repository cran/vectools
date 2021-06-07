\name{32_combine_methods}
\alias{c.ObjectArray}
\title{Combine Functions}
\description{Combine one-dimensional ObjectArray(s).}
\usage{
\method{c}{ObjectArray}(\dots)
}
\arguments{
\item{\dots}{An argument list of one-dimensional ObjectArray(s), including subclasses.}
}
\value{A new ObjectArray object.}
\examples{
v1 <- rep (eq.brot2 (4), 2)
v2 <- rep (eq.brot2 (6), 2)

c (v1, v2)
}
