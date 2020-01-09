\name{7_head_and_tail_generalizations}
\alias{headg}
\alias{headt}
\alias{headt.default}
\alias{headt.ObjectArray}
\alias{headt.MatrixLike}
\alias{headt.matrix}
\alias{headg.data.frame}
\alias{headt.data.frame}
\title{Head and Tail Generalizations}
\description{Combined head and tail functions, and a grouped head function for data.frames.}
\usage{
headg (\dots)
headt (\dots)

\method{headt}{default}(x, nh=3, nt=nh, \dots)
\method{headt}{ObjectArray}(x, nh=3, nt=nh, \dots)
\method{headt}{MatrixLike}(x, nh=3, nt=nh, \dots)
\method{headt}{matrix}(x, nh=3, nt=nh, \dots)

\method{headg}{data.frame}(x, group.by, n=3, \dots)
\method{headt}{data.frame}(x, nh=3, nt=nh, \dots)
}
\details{Refer to vignette for more information.}
\value{These functions return formatted character matrices.}
\arguments{
\item{x}{An object.}
\item{group.by}{A string, giving the name of the grouping variable.}
\item{n, nh, nt}{Similar to head and tail.}
\item{\dots}{Ignored.}
}
\examples{
s = matrix (1:100, 10, 10)
x = as.PartMatrix (s, 5, c (2, 4, 6, 8) )
headt (x, c (2, 6) , 2)

headg (iris, "Species")

headt (trees)
}
