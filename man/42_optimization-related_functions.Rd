\name{42_optimization-related_functions}
\alias{Optimization-Related Functions}
\alias{which.mins}
\alias{which.maxs}
\alias{which.opts}
\alias{mins}
\alias{maxs}
\alias{opts}
\title{Optimization-Related Functions}
\description{Sequence based functions, involving indices (or intervals) of minima or maxima.}
\usage{
which.mins (x, \dots, ret.type="I", endpoints=FALSE)
which.maxs (x, \dots, ret.type="I", endpoints=FALSE)
which.opts (x, \dots, ret.type="I", endpoints=FALSE)

mins (x, \dots, intervals.ok=FALSE, endpoints=FALSE)
maxs (x, \dots, intervals.ok=FALSE, endpoints=FALSE)
opts (x, \dots, intervals.ok=FALSE, endpoints=FALSE)
}
\arguments{
\item{x}{An integer/numeric vector.}
\item{intervals.ok}{Logical, if true, optimal regions are allowed, if false (and there are optimal regions), an error is generated.}
\item{ret.type}{
	String, either:\cr
	"I"         (integer vector of indices)\cr
	"intervals" (two-column integer matrix, representing intervals)\cr
	"NMP"       (integer vector, same as "NMP-")\cr
	"NMP-"      (integer vector of indices, near mid points, rounds down given even length subintervals)\cr
	"NMP+"      (integer vector of indices, near mid points, rounds up given even length subintervals)\cr
	"MP"        (numeric vector of exact midpoints)}
\item{endpoints}{Logical vector, of length one or two, to include the first/last values as possible optima.}
\item{\dots}{Ignored.}
}
\details{
These functions are designed to find indices or subintervals matching a pattern.

When ret.type="I", the solutions need to be single indices.\cr
Otherwise, an error is generated.

When ret.type="intervals" the functions return the starting and ending indices of intervals.

For other return types, a single index is returned for each interval.\cr
}
\value{
All "which" functions return an integer vector, which the exception of ret.type="intervals" which returns a two-column integer matrix and ret.type="MP" which returns a numeric vector.

The mins and maxs functions return values from the original vector.\cr
They may contain duplicated values.
}
\seealso{
\link{Unique-Related Functions}

\link{Sequence Properties}\cr
(Other than unique-related properties).

\link{Other Sequence Functions}
}
\examples{
x <- c (2, 1, 0, 1, 2, 1, 0, 1, 2)
which.mins (x)
mins (x)
}
