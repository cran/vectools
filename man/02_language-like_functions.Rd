\name{02_language-like_functions}
\alias{CLASS}
\alias{ndim}
\title{Language-Like Functions}
\description{Return the CLASS property or number of dimensions.}
\usage{
CLASS (v)

ndim (v)
}
\arguments{
\item{v}{An ObjectArray.}
}
\details{
CLASS returns a character giving the CLASS property of the ObjectArray.\cr
This is the class or classes of the objects within the array.

<OBJECT> means that the ObjectArray can contain any objects.

The ndim function returns an integer, giving the number of dimensions.
}
\seealso{
\link{ObjectArray}
}
\examples{
v <- ObjectArray (c (2, 5), CLASS="MyClass")

CLASS (v)
class (v)

ndim (v)
}
