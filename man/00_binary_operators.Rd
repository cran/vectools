\name{00_binary_operators}
\alias{Binary Operators}
\alias{\%*\%,MatrixArray,MatrixArray-method}
\alias{\%*\%,MatrixArray,matrix-method}
\alias{\%*\%,matrix,MatrixArray-method}
\alias{\%\{*\}\%}
\alias{\%*[\%}
\alias{\%*\{\%}
\alias{\%*|\%}
\alias{\%]*\%}
\alias{\%\}*\%}
\alias{\%|*\%}
\title{Binary Operators}
\description{
MatrixArray multiplication.

That is, multiplication of arrays of matrices.
}
\usage{
\S4method{\%*\%}{MatrixArray,MatrixArray}(x, y)
\S4method{\%*\%}{MatrixArray,matrix}(x, y)
\S4method{\%*\%}{matrix,MatrixArray}(x, y)

#cartesian
a \%\{*\}\% b

#right padding
#(for pre-multiplication)
a \%*[\% b
a \%*\{\% b
a \%*|\% b

#left padding
#(for post-multiplication)
a \%]*\% b
a \%\}*\% b
a \%|*\% b
}
\arguments{
\item{a, b}{
	Comformable matrices or MatrixArray objects.\cr
	If pad operators are used, then the objects need to be conformable after padding.}
\item{x, y}{Same as a and b.}
}
\details{
The operators are designed for matrix array multiplication.

If there are two matrix arrays, each with ten matrices of the same size:\cr
And they are multiplied using the \%*\% operator, then each pair of matrices is multiplied using matrix multiplication, and a new matrix array is returned, of length ten.

It's possible to use a single matrix, in which case, its handled the same as a matrix array of length one.\cr
Each pair of matrices needs to be conformable.\cr
And if the lengths of the matrix arrays are different, then one matrix array is recycled.\cr
(But currently, the length longer one needs to a multiple of the length shorter one).

Often these operators can be used to apply transformation matrices to data.\cr
In which case, the transformation matrices will often have one more row or column than the data.

With the right-pad operator \%*[\%, an extra row of ones is rbinded to the right hand matrices before multiplication, and strips it off the result. Likewise, with the left-pad operator \%]*\%, an extra column of ones is cbinded to the left hand matrices before multiplication, and then strips it off.

There are a number of variations on this approach.

The operator \%\{*\}\% performs cartesian matrix array multiplication, which works out the cartesian product of both matrix arrays, and multiplies all of them.\cr
If the matrix arrays have length 10 and 20, then the resulting matrix array will have length 200, not 20.

The \%*\{\% and \%\}*\% operators also perform cartesian matrix array multiplication, but pad the right or left matrix arrays, respectively.

The \%*|\% and \%|*\% operators, perform column-wise and row-wise matrix array multiplication, with right or left padding.

The \%*|\% operator changes the initial padded matrix array on the right, into a second padded matrix array of column vectors. Likewise, the \%|*\% changes the initial padded matrix array on the left, into a second padded matrix array of row vectors.

Unlike the other operators, these operators will allow the data object to be a vector, and the result that's returned will be a single matrix.

Note that results from the \%\{*\}\% operator are dependent on parentheses.

Also, in post-multiplication with two or more transformations and left-padding of the left-most matrix array, we need to put all the transformations in parentheses, otherwise the result after the first multiplication will be non-conformable with the subsequent multiplications.

Column-wise and row-wise multiplication can be used with the eq.arot2 or eq.brot2 functions to define a polygon easily.
}
\value{
Column-wise and row-wise operators return a single matrix.

All other operators return a MatrixArray.

Note that the recycling order, is subject to change.
}
\seealso{
\link{MatrixArray}

\link{Pre-Multiplication Transformation Matrices}

\link{Post-Multiplication Transformation Matrices}

\link{polyplot}, \link{cubplot}
}
\examples{
########################################
#refer to:
#Pre-Multiplication or Post-Multiplication
#for better examples
########################################
polyplot (sqps \%]*\% brot2 (pi / 8) )
}
