\name{60_cartesian_products}
\alias{cprod}
\title{Cartesian Product}
\description{
Computes the cartesian product of one or more sets.

By default, a matrix is returned, if possible.\cr
Each row, represents one combination.

In some cases, as.list (to return lists) needs to be set to true.\cr
(If there's a lack of supporting methods).
}
\usage{
cprod (\dots, sets = list(\dots), gby.last=FALSE, as.list=FALSE)
}
\arguments{
\item{sets}{
	List containing one or more vectors.\cr
	Note that standard vectors can be replaced with other objects.\cr
	However, each object requires a rep method with an each and times argument, and you need to set as.list to true.}
\item{gby.last}{
	Logical, if false (default), group by the first set, then the second, and so on.\cr
	If true, group by the last set, then the second to last, and so on.}
\item{as.list}{
	Logical, if true, return a list, rather than a matrix.\cr
	Each element corresponds to one column from the corresponding matrix.}
\item{\dots}{
	Argument list containing one or more standard vectors.\cr
	Often, easier way to specify sets, above.}
}
\examples{
cprod (1:2, 1:4)
cprod (1:2, 1:4, gby.last=TRUE)
}
