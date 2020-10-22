\name{51_grouped_head}
\alias{ghead.data.frame}
\title{Grouped Head}
\description{Grouped head function for data.frames.}
\usage{
\method{ghead}{data.frame}(x, nh=3, \dots, group.by)
}
\arguments{
\item{x}{A data.frame.}
\item{nh}{Integer, the number of rows per group.}
\item{group.by}{
	A string, giving the name of the grouping variable.\cr
	If missing, defaults to the first string or factor, with non-unique values.}
\item{\dots}{Ignored.}
}
\value{
Currently, returns a formatted character matrix, however, it's possible this may change in the future.
}
\seealso{
\link{select}, \link{selectf}

\link{headt.data.frame}
}
\examples{
ghead (iris)
}
