\name{20_ObjectArray}
\alias{VecLike-class}
\alias{MatrixLike-class}
\alias{ObjectArray-class}
\alias{ObjectArray}
\alias{as.ObjectArray}
\title{Object Arrays}
\description{
One, two and higher-dimensional object arrays.

NOTE:\cr
INTERNAL STRUCTURE OF OBJECTS IS SUBJECT TO CHANGE.\cr
DO NOT USE SLOTS, DIRECTLY.
}
\usage{
ObjectArray (n, \dots, CLASS="<OBJECT>", names, default.value=NULL)

as.ObjectArray (v, \dots, n, CLASS="<OBJECT>", names)
}
\arguments{
\item{CLASS}{
	A character vector of classes.\cr
	Every object in the object array, needs to be of these classes.\cr
	Note that the order is not important.\cr\cr
	The default, "<OBJECT>", means that objects can be of any class.}
\item{n}{Positive integer vector, the dimensions.}
\item{names}{
	List of character vectors, of names in each dimension, which should match n, above.\cr
	May be NULL or contain NULL values.}
\item{default.value}{Default value, for each element.}
\item{v}{
	In as.ObjectArray, a list.\cr
	In as.NestMatrix, a matrix or a PartMatrix.}
\item{\dots}{Ignored.}
}
\details{
ObjectArray(s) are similar to list matrices and list arrays.\cr
(But these are S4 classes, with a number of differences).

Setting individual elements to NULL, doesn't remove them.\cr
And they support formatting, via objtag methods.

By default, an ObjectArray can contain any objects, however, this can be restricted by the CLASS argument.
}
\value{
An ObjectArray object.
}
\seealso{
Subclasses:\cr
\link{ImageArray}\cr
\link{GeomArray}\cr
\link{NestMatrix}\cr
\link{MatrixArray}

\link{PartMatrix}, \link{SectMatrix}\cr
Similar to NestMatrix.

\link{Subsetting Operators}

\link{Standard Methods}

\link{Binary Operators}\cr
Operations on MatrixArray objects.
}
\examples{
v <- ObjectArray (c (2, 2), CLASS="list")
v [[1, 1]] <- list (1)
v [[2, 1]] <- list (1, 2)
v [[1, 2]] <- list (1, 2, 3)
v [[2, 2]] <- list (1, 2, 3, 4)
v

v [[1, 2]]
}
