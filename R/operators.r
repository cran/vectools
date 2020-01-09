#vectools: Supplementary Vector-Related Tools
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

"[[.ObjectArray" = function (x, ...)
{	I = unlist (list (...) )
	x@data [[.mi2si (x@dim, I)]]
}

"[[<-.ObjectArray" = function (x, ..., value)
{	I = unlist (list (...) )
	x@data [[.mi2si (x@dim, I)]] = value
	x
}

"[[.NestMatrix" = function (x, i, j)
	x@data [[i, j]]

"[[<-.NestMatrix" = function (x, i, j, value)
{	if (! (is.vector (value) || is.matrix (value) || inherits (value, "NestMatrix") ) )
		value = as.matrix (value)
	x@data [[i, j]] = value
	x
}

"[.SectMatrix" = function (x, ...)
{	vmap = x@vmap [[...]]
	I = vmap [1]:vmap [3]
	J = vmap [2]:vmap [4]
	x@data [I, J]
}

"[<-.SectMatrix" = function (x, ..., value)
{	vmap = x@vmap [[...]]
	I = vmap [1]:vmap [3]
	J = vmap [2]:vmap [4]
	x@data [I, J] = value
	x
}

"[[.SectMatrix" = function (x, i, j)
	x@data [[i, j]]

"[[<-.SectMatrix" = function (x, i, j, value)
{	x@data [[i, j]] = value
	x
}

.prod = function (x)
{	if (length (x) == 0)
		0
	else
		prod (x)
}

.mi2si = function (dim, I)
{	ndim = length (dim)
	if (ndim <= 1)
		I
	else
		sum ( (I - 1) * cumprod (c (1, dim [-ndim]) ) ) + 1
}
