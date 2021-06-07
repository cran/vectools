\name{52_which_functions}
\alias{Optimization-Related Functions}
\alias{which.mins}
\alias{which.maxs}
\alias{which.opts}
\alias{which.level}
\alias{which.decreasing}
\alias{which.increasing}
\alias{which.nondecreasing}
\alias{which.nonincreasing}
\alias{which.nonlevel}
\alias{mins}
\alias{maxs}
\alias{opts}
\title{Which Functions}
\description{Sequence based functions, involving indices (or intervals) of minima or maxima.}
\usage{
which.mins (v, \dots, what="I", ends=FALSE)
which.maxs (v, \dots, what="I", ends=FALSE)
which.opts (v, \dots, what="I", ends=FALSE)

which.level (v, \dots, what="intervals")

which.decreasing (v, \dots, what="intervals")
which.increasing (v, \dots, what="intervals")
which.nondecreasing (v, \dots, what="intervals")
which.nonincreasing (v, \dots, what="intervals")
which.nonlevel (v, \dots, what="intervals")

mins (v, \dots, allow.intervals=FALSE, ends=FALSE)
maxs (v, \dots, allow.intervals=FALSE, ends=FALSE)
opts (v, \dots, allow.intervals=FALSE, ends=FALSE)
}
\arguments{
\item{v}{An integer/numeric vector, with length two or more.}
\item{allow.intervals}{Logical, if true, optimal sections (which two or more equal values) are allowed, if false and there are optimal sections, an error is generated.}
\item{what}{
	String, either:\cr
	"I"         (integer vector of indices)\cr
	"intervals" (two-column integer matrix, representing intervals)\cr
	"NMP"       (integer vector, same as "NMP-")\cr
	"NMP-"      (integer vector of indices, near mid points, rounds down given even length subintervals)\cr
	"NMP+"      (integer vector of indices, near mid points, rounds up given even length subintervals)\cr
	"XMP"       (numeric vector of exact midpoints)\cr
	"first"     (integer vector of first indices)\cr
	"last"      (integer vector of last indices)}
\item{ends}{Logical vector, of length one or two, to include the first/last values as possible optima.}
\item{\dots}{Ignored.}
}
\details{
These functions are designed to find indices or subintervals matching a pattern.

When what="I", the solutions need to be single indices.\cr
Otherwise, an error is generated.\cr
(Note that "I" is not allowed, for the which.level function).

When what="intervals" the functions return the starting and ending indices of intervals.

For other return types, a single index is returned for each interval.
}
\value{
All "which" functions return an integer vector, which the exception of what="intervals" which returns a two-column integer matrix and what="XMP" which returns a numeric vector.

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
