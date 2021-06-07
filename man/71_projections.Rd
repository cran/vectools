\name{71_projections}
\alias{vt3.proj.GeomArray}
\alias{vt3.proj.Grid}
\alias{vt3.proj.Line}
\alias{vt3.proj.Point}
\alias{vt3.proj.Polygon}
\alias{vt3.proj.Text}
\title{Projections}
\description{Functions for Mapping 3D GeomObject(s) to 2D GeomObject(s).}
\usage{
\method{vt3.proj}{GeomArray}(v, proj="xy", sort=TRUE, \dots)

\method{vt3.proj}{Grid}(v, proj="xy", \dots)
\method{vt3.proj}{Line}(v, proj="xy", \dots)
\method{vt3.proj}{Point}(v, proj="xy", \dots)
\method{vt3.proj}{Polygon}(v, proj="xy", \dots)
\method{vt3.proj}{Text}(v, proj="xy", \dots)
}
\arguments{
\item{v}{A 3D GeomObject.}
\item{proj}{String, including "xy", "xz" or "yz"}
\item{sort}{Logical, if true, sort the GeomArray, which is necessary to plot the corresponding 2D-based arrays correctly.}
\item{\dots}{Ignored.}
}
\value{
A 2D GeomObject.

Note that plotting a 3D object, automatically projects it.
}
\seealso{
\link{range.GeomArray}
}
\examples{
range (TCUBE)

range (vt3.proj (TCUBE) )
range (vt3.proj (TCUBE, "xz") )
}
