\name{02_language-like_functions}
\alias{CLASS}
\title{Language-Like Functions}
\description{
Return the CLASS property of the ObjectArray.

This is the class or classes of the objects within the array.

<OBJECT> means that the ObjectArray can contain any objects.
}
\usage{
CLASS (x)
}
\arguments{
\item{x}{An ObjectArray.}
}
\seealso{
\link{ObjectArray}
}
\examples{
v <- ObjectArray ("MyClass", 10)
CLASS (v)
class (v)
}
