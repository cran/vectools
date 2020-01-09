\name{1_subsetting_operators}
\alias{[.SectMatrix}
\alias{[<-.SectMatrix}
\alias{[[.ObjectArray}
\alias{[[<-.ObjectArray}
\alias{[[.NestMatrix}
\alias{[[<-.NestMatrix}
\alias{[[.SectMatrix}
\alias{[[<-.SectMatrix}
\title{Subsetting Operators}
\description{Subsetting operators for vector-like objects.}
\usage{
\method{[[}{ObjectArray}(x, \dots)
\method{[[}{ObjectArray}(x, \dots) <- value

\method{[[}{NestMatrix}(x, i, j)
\method{[[}{NestMatrix}(x, i, j) <- value

\method{[}{SectMatrix}(x, \dots)
\method{[}{SectMatrix}(x, \dots) <- value
\method{[[}{SectMatrix}(x, i, j)
\method{[[}{SectMatrix}(x, i, j) <- value
}
\details{
ObjectArray and NestMatrix, are similar to lists, so:\cr
Double bracket subsetting gets or sets a single element.

For SectMatrix (including PartMatrix):\cr
Single bracket subsetting indexes submatrices and double bracket subsetting indexes the combined matrix.

Note that this may be changed in the future.
}
\arguments{
\item{x}{A vector-like object.}
\item{i, j, \dots}{The indices.}
\item{value}{The value to assign. For SectMatrix, the dimensions of the value need to match the dimensions set by setmap.}
}
\examples{
s = matrix (1:16, 4, 4)

#2x2 nested matrix, with 4 2x2 submatrices
x = as.NestMatrix (s, 2, 2)
x

x [[1, 1]]       #2x2 matrix
x [[1, 1]][2, 2] #6

#4x4 nested matrix, with 16 scalar values
x = as.NestMatrix.2 (s)
x

x [[2, 2]]       #6

#4x4 partitioned matrix, partitioned into 4 2x2 submatrices
x = as.PartMatrix (s, 2, 2)
x

x [1, 1]        #2x2 matrix
x [[2, 2]]      #6
x [1, 1][2, 2]  #6
}
