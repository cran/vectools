\name{04_new_generic_functions}
\alias{plotv}
\alias{plot.VecLike}
\alias{rep2}
\alias{objtag}
\alias{ghead}
\alias{headt}
\alias{vt3.proj}
\title{New Generic Functions}
\description{New generic functions.}
\usage{
plotv (\dots)
\method{plot}{VecLike}(x, \dots)

rep2 (\dots)

vt3.proj (\dots)

objtag (\dots)

ghead (\dots)
headt (\dots)
}
\arguments{
\item{x, \dots}{Arguments for the method.}
}
\details{
In general, it's not necessary to call the plotv function.\cr
Calls to plot should call plotv, which in turn, should call the correct method.

The rep2 function is similar to rep_len.\cr
(It recycles arguments to a particular length).

The objtag function is used inside formatted object arrays.

The ghead and headt functions are for grouped head and combined head and tail.
}
\seealso{
\link{rep.MatrixArray}, \link{rep2.MatrixArray}

\link{objtag methods}

\link{ghead.data.frame}

\link{Combined Head and Tail Functions}
}
\examples{
plot (TCUBE)
plotv (TCUBE)

ghead (iris)
}
