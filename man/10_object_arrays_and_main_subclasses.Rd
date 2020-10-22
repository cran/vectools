\name{10_object_arrays_and_main_subclasses}
\alias{VectorLike-class}
\alias{MatrixLike-class}
\alias{ObjectArray-class}
\alias{NestMatrix-class}
\alias{MatrixArray-class}
\alias{ObjectArray}
\alias{NestMatrix}
\alias{MatrixArray}
\alias{as.ObjectArray}
\alias{as.NestMatrix}
\title{Object Arrays and Main Subclasses}
\description{One, two and higher-dimensional object arrays, along with their main subclasses.}
\usage{
ObjectArray (CLASS="<OBJECT>", n, \dots, names, default.value=NULL)
NestMatrix (nr, nc, \dots, rnames, cnames, conform=TRUE, recursive=FALSE,
    default.value=ZERO)
MatrixArray (n, \dots, conform=TRUE, default.value=ZERO)

as.ObjectArray (x, CLASS="<OBJECT>", n, \dots, names)
as.NestMatrix (x, \dots, rnames, cnames, conform=TRUE, recursive=FALSE)
}
\arguments{
\item{CLASS}{
	A character vector of classes.\cr
	Every object in the object array, needs to be of these classes.\cr
	Note that the order is not important.\cr\cr
	The default, "<OBJECT>", means that objects can be of any class.}
\item{n}{Integer, the dimensions.}
\item{nr, nc}{Integers, the dimensions, same as n, where n is of length two.}
\item{names}{
	List of character vectors, of names in each dimension, which should match n, above.\cr
	May be NULL or contain NULL values.}
\item{rnames, cnames}{
	Character vectors of nr row names and nc column names.\cr
	May be NULL.\cr
	Currently ignored, when a NestMatrix is constructed from a PartMatrix.}
\item{conform}{
	Logical, if true, submatrices are required to have conformable dimensions.\cr
	Note that a NestMatrix may contain ZERO(s) regardless of the whether there's a conformable requirement or not.}
\item{recursive}{
	Logical, if true, sub elements can be nested matrices.\cr
	Otherwise, they must be standard matrices or ZERO.}
\item{default.value}{
	Default value, for each element.\cr
	In NestMatrix, the defaul value is ZERO.}
\item{x}{
	In as.ObjectArray, a list.\cr
	In as.NestMatrix, a matrix or a PartMatrix.}
\item{\dots}{Ignored.}
}
\details{
ObjectArray(s) are similar to list matrices and list arrays.\cr
(But these are S4 classes, with a number of differences).

Setting individual elements to NULL, doesn't remove them.\cr
And they support formatting, via objtag methods.

By default, and ObjectArray can contain any objects, however, this can be restricted by the CLASS argument.

A NestMatrix is a subclass of an ObjectArray that contains ZERO or standard matrices.\cr
If recursive is true, then it may also contain other NestMatrix objects.

Note that NestMatrix elements can have a scalar value assigned, but it will be converted to a 1x1 matrix.

A MatrixArray is also a subclass of an ObjectArray, with contains standard matrices.\cr
And is designed for matrix array multiplication.

ZERO is an instance of the class ZERO.\cr
This is a generalization of zero matrices.\cr
Which can be used in NestMatrix objects, without being constrained by conformable dimensions.

Note, do not to access slots directly.\cr
(But rather use functions within this package).
}
\value{
All functions return objects of their classes.
}
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
v <- ObjectArray ("list", c (2, 2) )
v [[1, 1]] <- list (1)
v [[2, 1]] <- list (1, 2)
v [[1, 2]] <- list (1, 2, 3)
v [[2, 2]] <- list (1, 2, 3, 4)
v

v [[1, 2]]

nm1 <- NestMatrix (4, 4)
nm1

msub <- NestMatrix (4, 4)
nm2 <- NestMatrix (4, 4, recursive=TRUE, default.value=msub)
nm2

nm3 <- as.NestMatrix (PartMatrix (5, 5, 2:4, 2:4) )
nm3
}
