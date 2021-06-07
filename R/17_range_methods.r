#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.nrng = function (x)
{	nr = nrow (x)
	rownames (x) = letters [23 + 1:nr]
	colnames (x) = c ("a", "b")
	x
}

range.Points = function (v, ...)
{	mins = apply (v@data, 2, min)
	maxs = apply (v@data, 2, max)
	.nrng (cbind (mins, maxs) )
}

range.Line = function (v, ...) range.Points (v)
range.Polygon = function (v, ...) range.Points (v)
range.Text = function (v, ...) range.Points (v)

range.Grid = function (v, ...)
{	xrng = range (v@x)
	yrng = range (v@y)
	rngs = rbind (x=xrng, y=yrng)
	if (v@D == 3)
	{	zrng = range (v@y)
		rngs = rbind (rngs, z=zrng)
	}
	.nrng (rngs)
}

range.VImage = function (v, ...) range.Grid (v)

range.GeomArray = function (v, ...)
{	n = length (v)
	N = v@D
	
	mins = maxs = matrix (0, n, N)
	I = rep (TRUE, n)
	for (i in seq_len (n) )
	{	if (is.null (v [[i]]) )
			I [i] = FALSE
		else
		{	rng = range (v [[i]])
			mins [i,] = rng [,1]
			maxs [i,] = rng [,2]
		}
	}
	nI = sum (I)
	if (nI == 0)
		stop ("GeomArray, empty")
	if (nI < n)
	{	warning ("GeomArray contains null values.")
		mins = mins [I,, drop=FALSE]
		maxs = maxs [I,, drop=FALSE]
	}
	rngs = matrix (0, N, 2)
	rngs [,1] = apply (mins, 2, min)
	rngs [,2] = apply (maxs, 2, max)
	.nrng (rngs)
}
