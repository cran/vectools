#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

is.ZERO = function (x) is (x, "ZERO")
is.ObjectArray = function (x) is (x, "ObjectArray")
is.NestMatrix = function (x) is (x, "NestMatrix")
is.SectMatrix = function (x) is (x, "SectMatrix")
is.PartMatrix = function (x) is (x, "PartMatrix")
is.MatrixArray = function (x) is (x, "MatrixArray")

length.ObjectArray = function (x) prod (x@n)
length.SectMatrix = function (x) x@nr * x@nc

dim.ObjectArray = function (x) x@n
dim.SectMatrix = function (x) c (x@nr, x@nc)

print.VectorLike = function (x, ...)
	print (format (x, ...), quote=FALSE, ...)

format.ZERO = function (x, ...)
	"<scalable zero matrix>"

head.VectorLike = function (x, n=6, ...)
	noquote (head (format (x, ...), n) )

tail.VectorLike = function (x, n=6, ...)
	noquote (tail (format (x, ...), n) )

as.list.ObjectArray = function (x, ...)
	x@data

as.matrix.NestMatrix = function (x, ...)
	as.matrix (as.PartMatrix (x), ...)

as.matrix.SectMatrix = function (x, ...)
{	y = x@data
	rownames (y) = rownames (x)
	colnames (y) = rownames (x)
	y
}

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
