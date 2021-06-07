\name{00_binary_operators}
\alias{Binary Operators}
\alias{Matrix Array Multiplication}
\alias{\%*\%,MatrixArray,MatrixArray-method}
\alias{\%*\%,MatrixArray,matrix-method}
\alias{\%*\%,matrix,MatrixArray-method}
\alias{\%*[\%}
\alias{\%*\{\%}
\alias{\%*|\%}
\alias{\%]*\%}
\alias{\%\}*\%}
\alias{\%|*\%}
\alias{\%*||\%}
\alias{\%||*\%}
\title{Matrix Array Multiplication}
\description{
Multiplication on \link{GeomArray} and \link{MatrixArray} objects.

These functions call the \link{Transformation Functions}.
}
\usage{
#simple matrix-array multiplication
\S4method{\%*\%}{MatrixArray,MatrixArray}(x, y)
\S4method{\%*\%}{MatrixArray,matrix}(x, y)
\S4method{\%*\%}{matrix,MatrixArray}(x, y)

#pre-multiplication
#(with right-side data objects)
a \%*[\% b
a \%*\{\% b
a \%*|\% b

#post-multiplication
#(with left-side data objects)
a \%]*\% b
a \%\}*\% b
a \%|*\% b

a \%*||\% b
a \%||*\% b
}
\arguments{
\item{a, b}{
	In simple matrix-array multiplication, matrices or MatrixArray(s).\cr\cr
	In pre-multiplication, a is the transformation object and b is the data object.\cr
	In post-multiplication, a is the data object and b is the transformation object.\cr\cr
	The transformation object should be a matrix or MatrixArray.\cr\cr
	The data object should a matrix, GeomObject, GeomArray or MatrixArray.\cr
	Except for row-wise and col-wise functions (with the vertical bars), where the data object should be a matrix.}
\item{x, y}{Same as a and b.}
}
\details{
The operators are designed for matrix array multiplication.\cr
But can also be used to transform GeomObject(s) and GeomArray(s).

If there are two matrix arrays, each with ten matrices of the same size:\cr
And they are multiplied using the \%*\% operator, then each pair of matrices is multiplied using matrix multiplication, and a new matrix array is returned, of length ten.

It's possible to use a single matrix, in which case, its handled the same as a matrix array of length one.\cr
Each pair of matrices needs to be conformable.\cr
And if the lengths of the matrix arrays are different, then one matrix array is recycled.\cr
(But currently, the length longer one needs to a multiple of the length shorter one).

Often these operators can be used to apply transformation matrices to data.\cr
In which case, the transformation matrices will often have one more row or column than the data.

With the right-pad operator \%*[\%, an extra row of ones is rbinded to the right hand matrices before multiplication, and strips it off the result. Likewise, with the left-pad operator \%]*\%, an extra column of ones is cbinded to the left hand matrices before multiplication, and then strips it off.\cr
The padded operators can also used on GeomObject(s), including GeomArray(s).

There are a number of variations on this approach.

In cartesian-based matrix array multiplication, the resulting object is the product over the cartesian set of both matrix arrays.\cr
If the matrix arrays have length 10 and 20, then the resulting matrix array will have length 200, not 20.

The \%*\{\% and \%\}*\% operators perform cartesian matrix array multiplication, using a right or left data object, respectively.\cr
These call the \link{vt3.gmultc.pre} and \link{vt3.gmultc.post} functions, which provide some more information.

The \%*|\% and \%|*\% operators, perform column-wise and row-wise matrix array multiplication, with right or left padding.\cr
The data object, which should be a matrix, is handled as a MatrixArray of padded column or row vectors.\cr
The double bar operators are the same, except that the don't pad the operands.

In post-multiplication (but not pre-multiplication) with two or more transformations and left-padding of the left-most matrix array, we need to put all the transformations in parentheses, otherwise the result after the first multiplication will be non-conformable with the subsequent multiplications.
}
\value{
Column-wise and row-wise operators return a single matrix.

All other operators should return a GeomArray or MatrixArray.

Note that the recycling order, is subject to change.
}
\seealso{
\link{MatrixArray}

\link{Pre-Multiplication Transformation Matrices}

\link{Post-Multiplication Transformation Matrices}

\link{vt3.testplot}, \link{vt3.testplot3}
}
\examples{
########################################
#refer to:
#Pre-Multiplication or Post-Multiplication
#for better examples
########################################
vt3.testplot (SQUARE \%]*\% brot2 (pi / 8) )
}
