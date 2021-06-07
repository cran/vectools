\name{61_special_sequences}
\alias{seqt}
\title{Special Sequences}
\description{
The seqt function is just a wrapper for the base::seq function, which multiples the result by 2 pi.

Note that the \link{eq.arot2} and \link{eq.brot2} functions can be used to generate a circular transformation matrix.
}
\usage{
seqt (\dots)
}
\arguments{
\item{\dots}{Arguments for the base::seq function}
}
\examples{
seqt (0, 1,, 10)
}
