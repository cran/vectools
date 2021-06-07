\name{41_plot_methods}
\alias{plotv.GeomArray}
\alias{plotv.Grid}
\alias{plotv.ImageArray}
\alias{plotv.Line}
\alias{plotv.Points}
\alias{plotv.Polygon}
\alias{plotv.RImage}
\alias{plotv.Text}
\alias{plotv.VImage}
\alias{plot_RImage}
\title{Plot Methods}
\description{
Plotting methods for GeomObject(s).

These use a dual approach that supports both base graphics and grid.\cr
There are plotting methods for both 2D and 3D objects.
}
\usage{
\method{plotv}{RImage}(im, \dots)

\method{plotv}{Points}(v, \dots, gsys="graphics", add=FALSE, proj="xy")
\method{plotv}{Line}(v, \dots, gsys="graphics", add=FALSE, proj="xy")
\method{plotv}{Polygon}(v, \dots, gsys="graphics", add=FALSE, proj="xy")
\method{plotv}{Text}(v, \dots, gsys="graphics", add=FALSE, proj="xy")

\method{plotv}{Grid}(v, \dots, gsys="graphics", add=FALSE, proj="xy")
\method{plotv}{VImage}(v, \dots, gsys="graphics", add=FALSE, proj="xy")

\method{plotv}{ImageArray}(im, \dots, rby=TRUE, add=FALSE)
\method{plotv}{GeomArray}(v, \dots, gsys="graphics", add=FALSE, proj="xy", sort=TRUE)

plot_RImage (im, \dots, gsys="graphics", 
    xlim = c (0, 1), ylim = c (0, 1),
    orient = "r",
    add=FALSE, interpolate=FALSE)
}
\arguments{
\item{v}{A GeomObject.}
\item{xlim, ylim}{Length-two numeric vectors, giving the boundaries of the image.}
\item{gsys}{String, graphics system, either "graphics" or "grid".}
\item{add}{Logical, if true, add to existing plot, if false, a new plot is created.}
\item{proj}{
	String, the projection, for 3D objects.\cr
	Either "xy", "xz", "yz".}
\item{sort}{
	Logical, if true, sort the objects, first.\cr
	Note that 3D objects need to be sorted to render correctly.\cr
	However, they only need to be sorted once.}
\item{interpolate}{Logical, if true, interpolate.}
\item{orient}{
	String, the orientation.\cr
	Either "v" for standard vector graphics (such as plotting heatmaps), "r", for standard raster graphics, but with the vertical axis running down, or "r.flip", for the same but with the vertical axis running up.\cr
	Note that "r" and "r.flip" should produce the same result, if add=FALSE.}
\item{im}{An RImage (raster image) or ImageArray object.}
\item{rby}{Logical, if true, plot by rows.}
\item{\dots}{
	Ignored.\cr
	Except for plot_RImage, where they're passed to the as.raster method.\cr
	And also for the other plotting functions for RImage objects, where they're passed to plot_RImage, and then to the as.raster method.}
}
\examples{
plot (TCUBE)
}
