#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

as.list.ObjectArray = function (x, ...)
	x@data

as.matrix.SImage = function (x, ...)
	x@colm

as.list.MImage = function (x, ...)
	x@colms

as.matrix.NestMatrix = function (x, ...)
	as.matrix (as.PartMatrix (x), ...)

as.matrix.SectMatrix = function (x, ...)
{	y = x@data
	rownames (y) = rownames (x)
	colnames (y) = rownames (x)
	y
}
