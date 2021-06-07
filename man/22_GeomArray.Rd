\name{22_GeomArray}
\alias{GeomArray-class}
\alias{GeomArray}
\alias{GeomArray2}
\alias{GeomArray3}
\alias{as.GeomArray}
\title{Geometric Arrays}
\description{
A GeomArray is a subclass of ObjectArray for storing GeomObject(s).

Note that GeomArray is also a GeomObject, and contain other GeomArray(s).\cr
i.e. They can be recursive.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
GeomArray (n, \dots, D)

GeomArray2 (n)
GeomArray3 (n)

as.GeomArray (v, \dots, n, D)
}
\arguments{
\item{n}{
	Single integer, the dimensions.\cr
	Currently, restricted to one dimensional arrays.}
\item{D}{
	Integer, two or three, for the number of spatial dimensions.\cr
	Any elements in the GeomArray will need to match this.\cr
	For the as function, D is optional but all elements need to match.}
\item{v}{A list of GeomObject(s).}
\item{\dots}{Ignored.}
}
\value{A GeomObject.}
\seealso{
\link{VMap}\cr
Another subclass of ObjectArray.

\link{PartMatrix}\cr
Similar to NestMatrix.

\link{SectMatrix}\cr
A generalization of PartMatrix

\link{Subsetting Operators}

\link{Standard Methods}

\link{Binary Operators}\cr
Operations on MatrixArray objects.
}
\examples{
#explicit construction
v <- GeomArray (2, D=2)
v [[1]] <- regPolygon (6, d=1)
v [[2]] <- regPolygon (6, d=2)
plot (v)

#construction via matrix array multiplication
v <- regPolygon (6, d=1) %]*% bscl2 (1:2)
plot (v)
}
