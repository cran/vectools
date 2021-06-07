\name{70_transformation_functions}
\alias{Transformation Functions}
\alias{vt3.cmult.pre}
\alias{vt3.mmult}
\alias{vt3.mmultc}
\alias{vt3.gmult.post}
\alias{vt3.gmult.pre}
\alias{vt3.gmultc.post}
\alias{vt3.gmultc.pre}
\alias{vt3.rmult.post}
\title{Transformation Functions.}
\description{
Transformation functions for \link{GeomArray} and \link{MatrixArray} objects.

The generalize the binary operators, used for \link{Matrix Array Multiplication}.
}
\usage{
vt3.mmult (a, b)
vt3.mmultc (a, b, \dots, gby.rhs=FALSE)

vt3.gmult.pre (a, b)
vt3.gmultc.pre (a, b, \dots, gby.trans=TRUE, rolling=TRUE)

vt3.gmult.post (a, b)
vt3.gmultc.post (a, b, \dots, gby.trans=TRUE, rolling=TRUE)

vt3.cmult.pre (a, b, \dots, pack=TRUE)
vt3.rmult.post (a, b, pack=TRUE)
}
\arguments{
\item{a, b}{
	In simple matrix-array multiplication, matrices or MatrixArray(s).\cr\cr
	In pre-multiplication, a is the transformation object and b is the data object.\cr
	In post-multiplication, a is the data object and b is the transformation object.\cr\cr
	The transformation object should be a matrix or MatrixArray.\cr\cr
	The data object should a matrix, GeomObject, GeomArray or MatrixArray.\cr
	Except for row-wise and col-wise functions (vt3.cmult.pre/vt3.rmult.post), where the data object should be a matrix.}
\item{gby.rhs}{Logical, if true, group by the sub-objects within the RHS object.}
\item{gby.trans}{Logical, if true (default), group by (or nest by) sub-objects within the transformation object.}
\item{rolling}{Logical, if true (default), extend the arrays (repeating where necessary), otherwise, nest the expansions.}
\item{pack}{
	Logical, if true (the default) add/strip and extra row/column to the data object.\cr
	This allows a 3 by 3 transformation matrix to be multiply a two-colum data matrix.\cr\cr
	Ignored, for GeomObject(s).}
\item{\dots}{Ignored.}
}
\details{
Reiterating, these functions generalize the binary operators, used for \link{Matrix Array Multiplication}.

Please refer to them, for an overview.
}
\value{
A matrix for the vt3.cmult.pre/vt3.rmult.post functions.\cr
(These are used for row-wise and column-wise matrix multiplication).

A matrix, GeomObject, GeomArray or MatrixArray for other functions.

By default, cartesian-based multiplication uses the rolling option, where the length of the output will match the length of the cartesian product.

If the nested/nonrolling option is used, then the length will match the length of the grouping operand.\cr
(This is determined by the gby.rhs and gby.trans args).
}
\seealso{
\link{Matrix Array Multiplication}
}
\examples{
x <- matrix (1:4, 2, 2)
vt3.gmult.pre (ascl2 (1:4), x)
}
