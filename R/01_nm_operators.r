#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.assert.class.conform = function (CLASS, x)
{	if (CLASS != "<OBJECT>" && ! is.null (x) )
	{	if (! all (CLASS %in% class (x) ) )
			stop ("input doesn't match class restrictions")
	}
}

.assert.nsub.conform = function (i, j, nrs, ncs, conform, recursive, value)
{	if (is.ZERO (value) )
	{	nrs [i] = ncs [j] = 0
		list (value, TRUE, TRUE, nrs, ncs)
	}
	else
	{	if (is.vector (value) )
		{	if (length (value) == 1)
				value = matrix (value, 1, 1)
			else
				stop ("standard (non-matrix) vectors need to length one")
		}
		else if (recursive)
		{	if (! (is.NestMatrix (value) || is.matrix (value) ) )
				stop ("recursive NestMatrix needs ZERO, NestMatrix, matrix or vector")
		}
		else
		{	if (! is.matrix (value) )
				stop ("non-recursive NestMatrix needs ZERO, matrix or vector")
		}
		update.r = update.c = FALSE
		if (conform)
		{	n = dim (value)
			if (any (n == 0) )
				stop ("value needs non-zero dims, use ZERO instead")
			if (all (nrs == 0) )
			{	update.r = TRUE
				nrs [i] = n [1]
			}
			else
			{	nrs2 = nrs [nrs != 0]
				if (any (n [1] != nrs2) )
					stop ("value non-conformable in rows")
			}
			if (all (ncs == 0) )
			{	update.c = TRUE
				ncs [j] = n [2]
			}
			else
			{	ncs2 = ncs [ncs != 0]
				if (any (n [2] != ncs2) )
					stop ("value non-conformable in columns")
			}
		}
		list (value, update.r, update.c, nrs, ncs)
	}
}

"[.ObjectArray" = function (x, ...)
{	if (x@N != 1)
		stop ("single bracket subsetting, supports 1-dim only")
	I = unlist (list (...) )
	y = x@data [I]
	as.ObjectArray (y, x@CLASS, length (y) )
}

"[[.ObjectArray" = function (x, ...)
{	I = unlist (list (...) )
	x@data [[.mi2si (x@n, I)]]
}

"[[<-.ObjectArray" = function (x, ..., value)
{	I = unlist (list (...) )
	.assert.class.conform (x@CLASS, value)
	x@data [.mi2si (x@n, I)] = list (value)
	x
}

"[[.NestMatrix" = function (x, i, j, ..., drop=TRUE, zero=TRUE)
{	y = x@data [[.mi2si (x@n, c (i, j) )]]
	if (is.ZERO (y) && zero)
	{	nrs = apply (x@nrs, 1, max)
		ncs = apply (x@ncs, 2, max)
		y = matrix (0, nrs [i], ncs [j])
	}
	if (drop && ! is.ZERO (y) )
	{	if (nrow (y) == 1 || ncol (y) == 1)
			as.vector (y)
		else
			y
	}
	else
		y
}

"[[<-.NestMatrix" = function (x, i, j, value)
{	r = .assert.nsub.conform (i, j, x@nrs [i,], x@ncs [,j], x@conform, x@recursive, value)
	if (r [[2]]) x@nrs [i,] = r [[4]]
	if (r [[3]]) x@ncs [,j] = r [[5]]
	x@data [[.mi2si (x@n, c (i, j) )]] = r [[1]]
	x
}

"[[.PartMatrix" = function (x, i, j, ..., drop=TRUE)
	`.sSectMatrix` (x, i, j, drop=drop)

"[[<-.PartMatrix" = function (x, i, j, value)
	`.sSectMatrix.assign` (x, i, j, value=value)

getSect = function (x, ..., drop=TRUE)
	`.sSectMatrix` (x, ..., drop=drop)

".sSectMatrix" = function (x, ..., drop=TRUE)
{	vmap = x@vmap [[...]]
	I = vmap [1]:vmap [3]
	J = vmap [2]:vmap [4]
	x@data [I, J, drop=drop]
}

".sSectMatrix.assign" = function (x, ..., value)
{	vmap = x@vmap [[...]]
	I = vmap [1]:vmap [3]
	J = vmap [2]:vmap [4]
	x@data [I, J] = value
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
