#vectools: Supplementary Vector-Related Tools
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

dim.ObjectArray = function (x) x@dim
dim.NestMatrix = function (x) x@data@dim
dim.SectMatrix = function (x) c (x@nr, x@nc)

print.VectorLike = function (x, ...)
	print (format (x, ...), quote=FALSE, ...)

head.VectorLike = function (x, n=6, ...)
	noquote (head (format (x, ...), n) )

tail.VectorLike = function (x, n=6, ...)
	noquote (tail (format (x, ...), n) )

as.matrix.SectMatrix = function (x, ...)
{	y = x@data
	rnames = rnames (x)
	cnames = cnames (x)
	if (! is.na (rnames [1]) )
		rownames (y) = rnames
	if (! is.na (cnames [1]) )
		colnames (y) = cnames
	y
}
