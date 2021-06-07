#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

is.ZERO = function (object) is (object, "Zero")
is.GeomObject = function (object) is (object, "GeomObject")
is.RImage = function (object) is (object, "RImage")

is.Points = function (object) is (object, "Points")
is.Line = function (object) is (object, "Line")
is.Polygon = function (object) is (object, "Polygon")
is.Grid = function (object) is (object, "Grid")
is.Text = function (object) is (object, "Text")

is.SImage = function (object) is (object, "SImage")
is.MImage = function (object) is (object, "MImage")

is.ObjectArray = function (object) is (object, "ObjectArray")
is.ImageArray = function (object) is (object, "ImageArray")
is.GeomArray = function (object) is (object, "GeomArray")
is.NestMatrix = function (object) is (object, "NestMatrix")
is.SectMatrix = function (object) is (object, "SectMatrix")
is.PartMatrix = function (object) is (object, "PartMatrix")
is.MatrixArray = function (object) is (object, "MatrixArray")

length.ObjectArray = function (x) prod (x@n)
length.SectMatrix = function (x) x@nr * x@nc

dim.ObjectArray = function (x) x@n
dim.SectMatrix = function (x) c (x@nr, x@nc)

dimnames.ObjectArray = function (x)
	x@names

"dimnames<-.ObjectArray" = function (x, value)
{	if (is.null (value) )
		x@names = value
	else
	{	for (k in 1:x@N)
		{	v = value [[k]]
			if (is.null (v) || x@n [k] == length (v) )
				0
			else
				stop ("names don't match dimensions")
		}
		x@names <- value
	}
	x
}

dimnames.SectMatrix = function (x)
{	if (is.null (x@rnames) && is.null (x@cnames) )
		NULL
	else
		list (x@rnames, x@cnames)
}

"dimnames<-.SectMatrix" = function (x, value)
{	x@rnames = value [[1]]
	x@cnames = value [[2]]
	x
}

print.VecLike = function (x, ...)
	print (format (x, ...), quote=FALSE, ...)

print.GeomObject = function (x, ...)
	cat (class (x)[1], "\n", sep="")

print.SImage = function (x, ...)
	cat (class (x)[1], " ", x@nr, "x", x@nc, "\n", sep="")

print.MImage = function (x, ...)
{	if (x@nchannels == 1)
		type = "G"
	else if (x@nchannels == 2)
		type = "G-A"
	else if (x@nchannels == 3)
		type = x@storage
	else if (x@nchannels == 4)
		type = paste0 (x@storage, "-A")
	else
		stop ("invalid object")
	cat (class (x)[1], " (", type, ", ", x@nchannels, ") ", x@nr, "x", x@nc, "\n", sep="")
}

format.ZERO = function (x, ...)
	"<scalable zero matrix>"

head.VecLike = function (x, n=6, ...)
	noquote (head (format (x, ...), n) )

tail.VecLike = function (x, n=6, ...)
	noquote (tail (format (x, ...), n) )

head.raster = function (x, ...)
{	x = as.matrix (x)
	head (x, ...)
}

tail.raster = function (x, ...)
{	x = as.matrix (x)
	tail (x, ...)
}
