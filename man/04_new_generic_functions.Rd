\name{04_new_generic_functions}
\alias{rep2}
\alias{objtag}
\alias{ghead}
\alias{headt}
\alias{pool}
\alias{pool2}
\title{New Generic Functions}
\description{New generic functions.}
\usage{
rep2 (\dots)

objtag (\dots)

ghead (\dots)
headt (\dots)

pool (\dots)
pool2 (\dots)
}
\arguments{
\item{\dots}{Arguments for the method.}
}
\details{
The rep2 function is similar to rep_len.\cr
(It recycles arguments to a particular length).

The objtag function is used inside formatted object arrays.

The ghead and headt functions are for groupd head and combined head and tail.

The pool function is for pooling.\cr
The pool2 function is for pooling, but with a target size.
}
\seealso{
\link{rep.MatrixArray}, \link{rep2.MatrixArray}

\link{objtag methods}

\link{ghead.data.frame}

\link{Combined Head and Tail Functions}

\link{vt2.pool.matrix}

\link{vt2.convolve.matrix}
}
\examples{
ghead (iris)
}
