\name{3_matrix_like_objects}
\alias{MatrixLike-class}
\alias{NestMatrix-class}
\alias{SectMatrix-class}
\alias{PartMatrix-class}
\alias{NestMatrix}
\alias{SectMatrix}
\alias{PartMatrix}
\alias{as.NestMatrix}
\alias{as.NestMatrix.2}
\alias{as.SectMatrix}
\alias{as.PartMatrix}
\alias{setmap<-}
\alias{getmap}
\alias{rnames}
\alias{cnames}
\alias{rnames<-}
\alias{cnames<-}
\title{Matrix-Like Objects}
\description{Nested, sectioned and partitioned matrices.}
\usage{
NestMatrix (nr, nc, rnames, cnames, default.value=NA)
SectMatrix (nsect, nr, nc, rnames, cnames, default.value=NA)
PartMatrix (Rb, Cb, nr, nc, rnames, cnames, default.value=NA)

as.NestMatrix (x, Rb, Cb, rnames, cnames)
as.NestMatrix.2 (x, rnames, cnames)

as.SectMatrix (x, nsect, rnames, cnames)
as.PartMatrix (x, Rb, Cb, rnames, cnames)

setmap (x, \dots) <- value
getmap (x, \dots)

rnames (x)
cnames (x)
rnames (x, \dots) <- value
cnames (x, \dots) <- value
}
\details{Refer to the vignette for more information.}
\arguments{
\item{nsect}{The number (possibly in two or more dimensions) of submatrices.}
\item{Rb, Cb}{The inter-row and inter-column indices.}
\item{nr, nc}{The dimensions.}
\item{rnames, cnames}{Character vectors giving the row and column names.}
\item{default.value}{The default value of entries in the matrix.}
\item{value}{For setmap, a two by two integer-valued matrix, or a length-4 vector, giving top-left row index, top-left column index, bottom-right row index and bottom-right column index. For the rnames/cnames functions, a character vector.}
\item{x}{For the as functions, a standard matrix or data.frame. For the getmap/setmap functions, a SectMatrix object. For the rnames/cnames functions, any matrix-like object from above.}
\item{\dots}{For getmap/setmap, the indices of the submatrix.}
}
\examples{
s = matrix (1:100, 10, 10)

x = as.SectMatrix (s, 2)
setmap (x, 1) = c (2, 2, 8, 8)
setmap (x, 2) = c (3, 3, 9, 9)
x

x = as.PartMatrix (s, 5, c (2, 4, 6, 8) )
x
}
