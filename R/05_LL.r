#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.is.vector = function (x)
	(is.logical (x) || is.integer (x) || is.character (x) ||is.numeric (x) || is.complex (x) )

CLASS = function (x)
{	if (is.ObjectArray (x) )
		x@CLASS
	else
		stop ("CLASS can only be called on ObjectArray objects")
}

n22 = function (...)
{	v = unlist ( list (...) )
	nv = length (v)
	n = nv %/% 4
	if (nv %% 4 != 0)
		stop ("n22 needs a multiple of four values")
	v = array (v, c (2, 2, n) )
	vmap = VMap (n)
	for (i in 1:n)
		vmap [[i]] = t (v [,,i])
	vmap
}

c.MatrixArray = function (x, ...)
{	y = list (...)
	N = length (y)
	if (N == 0)
		x
	else
	{	n = integer (N + 1)
		n [1] = length (x)
		for (i in 1:N)
		{	n [i + 1] = length (y [[i]])
			if (! is.MatrixArray (y [[i]]) )
				stop ("two (not one) matrix arrays required")
		}
		v = MatrixArray (sum (n) )
		b = cumsum (n)
		a = c (1, b [-(N + 1)] + 1)
		v@data [a [1]:b [1] ] = x@data
		for (i in 1:N)
			v@data [a [i + 1]:b [i + 1] ] = y [[i]]@data
		v
	}
}

rep.MatrixArray = function (x, n=1, ..., each=1, times=n)
{	if (x@N == 1)
	{	if (n < 1)
			stop ("n needs to be positive")
		nx = length (x)
		I = rep (1:nx, each=each, times=times)
		v = MatrixArray (nx * each * times)
		v@data = x@data [I]
		v
	}
	else
		stop ("rep method can only be called on 1-dim objects")

}

rep2.MatrixArray = function (x, n, ..., n.out=n)
{	if (x@N == 1)
	{	if (n.out < 1)
			stop ("n needs to be positive")
		v = MatrixArray (n.out)
		for (i in 1:n.out)
			v [[i]] = x [[(i - 1) %% x@n + 1]]
		v
	}
	else
		stop ("rep2 method can only be called on 1-dim objects")
}
