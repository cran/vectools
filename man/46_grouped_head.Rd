\name{46_grouped_head}
\alias{ghead.data.frame}
\title{Grouped Head}
\description{Grouped head function for data.frame(s).}
\usage{
\method{ghead}{data.frame}(v, nh=3, \dots, gname)
}
\arguments{
\item{v}{A data.frame.}
\item{nh}{Integer, the number of rows per group.}
\item{gname}{
	A string, giving the name of the grouping variable.\cr
	If missing, defaults to the first string or factor, with non-unique values.}
\item{\dots}{Ignored.}
}
\value{
Currently, returns a formatted character matrix, however, it's possible this may change in the future.
}
\seealso{
\link{headt.data.frame}
}
\examples{
ghead (iris)
}
